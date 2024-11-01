library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(patchwork)
library(stargazer)
library(plotly)
library(knitr)
library(kableExtra)
library(caret)
library(Rborist)
library(e1071)
library(reshape2)
library(klaR)
library(rpart)
library(rpart.plot)
library(factoextra)
library(cluster)
library(class)
library(randomForest)
library(gam)
library(pROC)
library(shiny)


# load --------------------------------------------------------------------

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- read_csv("TelCo_Customer_Data.csv")

# tidy --------------------------------------------------------------------

# clean up column names
data <- data |> janitor::clean_names()

# remove non-essential variables
data <- data |>
  dplyr::select(-customer_id)

data$gender <- ifelse(data$gender == "Male", 0, 1)
data$churn_status <- ifelse(data$churn_status == "No", 0, 1)

# remove outliers ------------------------------------------------------------

# function to logically identify outlier rows as FALSE
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x >= lower_bound & x <= upper_bound
}

# removing outlier rows
annual_income_outlier_data <- remove_outliers(data$annual_income)
monthly_charge_outlier_data <- remove_outliers(data$monthly_charge)
data_usage_outlier_data <- remove_outliers(data$data_usage)
data <- data[annual_income_outlier_data &
               monthly_charge_outlier_data &
               data_usage_outlier_data, ]





# tidy is complete .... start the server function





shiny_data <- data

server <- function(input, output) {
  # OUTPUT: Distribution Charts
  output$histograms <- renderPlot({
    ggplot(shiny_data, aes(x = shiny_data[[input$predictor]])) +
      geom_histogram(aes(fill = after_stat(x)),
                     binwidth = input$bins,
                     color = "black") +
      scale_fill_gradient(
        low = "green",
        high = "blue",
        name = input$predictor
      ) +
      labs(title = input$predictor,
           x = input$predictor,
           y = "Count") +
      theme_minimal()
  })
  
  # OUTPUT: Bar Charts for Churn by Group
  output$churn_by_group <- renderPlot({
    # cut the data in a group
    shiny_data <- shiny_data %>%
      mutate(group = cut(
        shiny_data[[input$predictor_group]],
        breaks = c(-Inf, quantile(shiny_data[[input$predictor_group]], probs = c(0.33, 0.66)), Inf),
        labels = c("Low", "Medium", "High"),
        ordered_result = TRUE
      ))
    
    # Calculate churn rate by group
    the_group <- shiny_data %>%
      group_by(group) %>%
      summarise(churn_rate = mean(churn_status))
    
    # remove the temp column
    shiny_data <- shiny_data |>
      dplyr::select(-group)
    
    # Create the bar plot for churn rate by age group
    ggplot(the_group,
           aes(x = group, y = churn_rate, fill = group)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#008080", "coral", "goldenrod"),
                        name = input$predictor_group) +
      labs(title = paste("Churn Rate by", input$predictor_group, "Group"), x = paste(input$predictor_group, "Group"), y = "Churn Rate") +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format(),
                         limits = c(0, max(the_group$churn_rate) * 1.2)) +
      geom_text(aes(label = scales::percent(churn_rate)),
                vjust = -0.5,
                size = 5)
    #scale_y_continuous(labels = scales::percent_format())
  })
  
  # OUTPUT: Logistic_cv table
  output$logistic_cv_table <- renderTable({
    as.data.frame(logistic_cv$results[, c("Accuracy", "Kappa")])
  })
  
  # OUTPUT: ROC curve
  output$roc <- renderPlot({
    plot(roc_curve, col = "blue", main = "ROC Curve for Logistic Regression Model")
  })
  
  # OUTPUT: Prediction Metrics
  output$matrix_table <- renderTable({
    metrics_table
  })
}