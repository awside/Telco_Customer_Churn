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
#-----
library(naivebayes)
library(kernlab)
library(mboost)
library(import)
library(kknn)
library(ranger)
library(wsrf)
library(RSNNS)
library(monmlp)
library(adabag)
#-----
library(corrplot)
library(MASS)
library(glmnet)
library(Metrics)
library(gbm)
library(xgboost)
library(pROC)
#-----
library(palmerpenguins)
library(ggthemes)
library(dslabs)


# load --------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- read_csv("TelCo_Customer_Data.csv")


# summary -----------------------------------------------------------------

skim(data)
any(is.na(data))
unique_columns <- sapply(data, unique)




# Preprocessing and EDA:

# Column names where cleaned and standardized using the janitor package to 
# improve readability and consistency.

# The two categorical variables where converted to binary format to simplify 
# the modeling process.

# Outlier detection and removal was used based on the interquartile range 
# method. Reducing the influence of extreme values that could skew the results.


# tidy --------------------------------------------------------------------

# clean up column names
data <- data |> janitor::clean_names()

# remove non-essential variables
data <- data |>
  dplyr::select(-customer_id)

data$gender <- ifelse(data$gender == "Male", 0, 1)
data$churn_status <- ifelse(data$churn_status == "No", 0, 1)

# remove outliers ------------------------------------------------------------

boxplot(data[, sapply(data, is.numeric)])

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






# EDA:

# The age distribution of customers was visualized making it easier to see the 
# distinction in the age range.

# Several other plots where used to examine the dataset. An important 
# distinction was found between age groups and churn levels, indicating that 
# different age groups could be predict costumer churn.


# EDA ---------------------------------------------------------------------

# histogram with gradient fill based on age
ggplot(data, aes(x = age)) +
  geom_histogram(aes(fill = after_stat(x)), binwidth = 5, color = "black") +
  scale_fill_gradient(low = "green", high = "blue", name = "Age") +
  geom_vline(xintercept = 40, linetype = "dotted", color = "black", linewidth = 1) +
  geom_vline(xintercept = 60, linetype = "dotted", color = "black", linewidth = 1) +
  labs(title = "Age Distribution of Customers", x = "Age", y = "Count") +
  theme_minimal()

# boxplot of annual income by gender
ggplot(data, aes(x = as.factor(gender), y = annual_income, fill = as.factor(gender))) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightblue", "pink"), name = "Gender", labels = c("Male", "Female")) +
  labs(title = "Annual Income by Gender", x = "Gender", y = "Annual Income") +
  theme_minimal() +
  scale_x_discrete(labels = c("Male", "Female"))

# Calculate the average income by age
avg_income_by_age <- data %>%
  group_by(age) %>%
  summarise(average_income = mean(annual_income))

# line chart of average income by age with smoothing loess
ggplot(avg_income_by_age, aes(x = age, y = average_income)) +
  geom_line(color = "lightgreen", size = 1) +
  geom_point(color = "darkgreen", size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen", size = 1.2) +
  labs(title = "Average Annual Income by Age", x = "Age", y = "Average Annual Income") +
  theme_minimal()

# Calculate churn rate by gender
churn_rate_by_gender <- data %>%
  group_by(gender) %>%
  summarise(churn_rate = mean(churn_status))

# bar plot for churn rate by gender with enhancements
ggplot(churn_rate_by_gender, aes(x = as.factor(gender), y = churn_rate, fill = as.factor(gender))) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("lightblue", "pink"), 
                    name = "Gender", 
                    labels = c("Male", "Female")) +
  labs(title = "Churn Rate by Gender", x = "Gender", y = "Churn Rate") +
  theme_minimal() +
  scale_x_discrete(labels = c("Male", "Female")) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, max(churn_rate_by_gender$churn_rate) * 1.2)) +
  geom_text(aes(label = scales::percent(churn_rate)), vjust = -0.5, size = 5)

# scatter plot of annual income vs. usage with a linear regression line
ggplot(data, aes(x = annual_income, y = data_usage)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot of Annual Income vs. Usage with Linear Regression", 
       x = "Annual Income", y = "Usage") +
  theme_minimal()

# Create age group
data$age_group <- cut(data$age, 
                      breaks = c(-Inf, 39, 60, Inf), 
                      labels = c("Below 40", "40 to 60", "Above 60"))

# Calculate churn rate by age group
churn_rate_by_age_group <- data %>%
  group_by(age_group) %>%
  summarise(churn_rate = mean(churn_status))

# Create the bar plot for churn rate by age group
ggplot(churn_rate_by_age_group, aes(x = age_group, y = churn_rate, fill = age_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#008080", "coral", "goldenrod"), name = "Age Group") +
  labs(title = "Churn Rate by Age Group", x = "Age Group", y = "Churn Rate") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, max(churn_rate_by_age_group$churn_rate) * 1.2)) +
  geom_text(aes(label = scales::percent(churn_rate)), vjust = -0.5, size = 5)
  #scale_y_continuous(labels = scales::percent_format())

# Create the stacked bar plot for churn status by gender and age group
ggplot(data, aes(x = age_group, fill = as.factor(churn_status))) +
  geom_bar(position = "fill") +
  facet_wrap(~ gender, labeller = labeller(gender = c("0" = "Male", "1" = "Female"))) +
  scale_fill_manual(values = c("#A8E0FF", "#F8DDA4"), name = "Churn Status", labels = c("Not Churned", "Churned")) +
  labs(title = "Churn Status by Gender and Age Group", x = "Age Group", y = "Proportion") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format())

# remove age_group
data <- data |>
  dplyr::select(-age_group)


# data split --------------------------------------------------------------

# training test split
set.seed(123)
trainIndex <- createDataPartition(data$churn_status, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]



# GLM ---------------------------------------------------------------------

# Logistic model
logistic_model <- glm(churn_status ~ age:gender, 
                      data = train_data, 
                      family = binomial)
summary(logistic_model)

# Predict on the test set
predictions <- predict(logistic_model, newdata = test_data, type = "response")

# Convert probabilities to binary
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# ROC curve
roc_curve <- roc(test_data$churn_status, predicted_classes)

# Plot ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve for Logistic Regression Model")

# confusion matrix
cm <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$churn_status))
print(cm)

# metrics
accuracy <- cm$overall['Accuracy']
precision <- cm$byClass['Pos Pred Value']
recall <- cm$byClass['Sensitivity']
f1_score <- 2 * ((precision * recall) / (precision + recall))

# put metrics in a table
metrics_table <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy, precision, recall, f1_score)
)

# round values to 4 decimals
metrics_table$Value <- round(metrics_table$Value, 4)
print(metrics_table)




# Model Evaluation and Refinement:

# Cross-Validation was used because it would make the model more robust and 
# create a more generalizable model to avoid overfitting.

# The ROC curve, though imperfect, shows the model's ability to distinguish 
# between the two classes.

# There is room for improvement based on the confusion matrix’s metrics. 
# However, the model is currently slightly better than random guessing.

# To enhance the model's accuracy, additional machine learning algorithms 
# should be explored.


# CV ----------------------------------------------------------------------

# Ensure churn_status is a factor
train_data$churn_status <- as.factor(train_data$churn_status)

# Cross-Validation (GLM)
set.seed(123)
logistic_cv <- caret::train(
  churn_status ~ age,
  data = train_data, 
  method = "glm", 
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

# CV results
print(logistic_cv)

# Predict probabilities on the test set
predicted_probs <- predict(logistic_cv, newdata = test_data, type = "prob")[, 2]

# ROC curve
roc_curve <- roc(test_data$churn_status, predicted_probs)

# Plot ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve for Logistic Regression Model")

# Convert probabilities to binary class predictions
predicted_classes <- ifelse(predicted_probs >= 0.5, 1, 0)

# confusion matrix
cm <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$churn_status))

# metrics
accuracy <- cm$overall['Accuracy']
precision <- cm$byClass['Pos Pred Value']
recall <- cm$byClass['Sensitivity']
f1_score <- 2 * ((precision * recall) / (precision + recall))

# put metrics in a table
metrics_table <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy, precision, recall, f1_score)
)

# round values to 4 decimals
metrics_table$Value <- round(metrics_table$Value, 4)
print(metrics_table)



# shiny -------------------------------------------------------------------

shiny_data <- data

ui <- fluidPage(
  tags$style(HTML("
    body {
      background-color: #f5f5f5;
      .custom-chart {
        border-radius: 15px;
        box-shadow: 2px 2px 12px rgba(0, 0, 0, 0.2);
        padding: 10px;
        background-color: #ffffff;
      }
      hr.custom-line {
        border: 0;
        height: 2px;
        background-color: rgba(0, 0, 0, 0.2); /* Darker color for the line */
        margin: 20px 0; /* Adjust the spacing around the line */
      }
    }
  ")),
  
  titlePanel("TelCo Customer Dashboard"),
  hr(class = "custom-line"),
  h3("Demographics Distribution"),
  fluidRow(
    column(width = 4,
           # INPUT : selector for dependent variable
           selectInput("predictor", "Choose a Predictor:",
                       choices = c("age", "account_length", "monthly_charge", "data_usage"),
                       selected = "age"),
           # INPUT : slider for bins
           sliderInput(inputId = "bins",
                       label = "Bin Width:",
                       min = 1,
                       max = 10,
                       value = 5)
    ),
    column(width = 8,
           div(class = "custom-chart",
               plotOutput(outputId = "histograms")
           )
    )
  ),
  hr(class = "custom-line"),
  h3("Churn Status by Group"),
  fluidRow(
    column(width = 4,
           # INPUT : selector for dependent variable
           selectInput("predictor_group", "Choose a Group:",
                       choices = c("age", "annual_income", "account_length", "monthly_charge", "data_usage"),
                       selected = "age"),
    ),
    column(width = 8,
           div(class = "custom-chart",
               plotOutput(outputId = "churn_by_group")
           )
    )
  ),
  hr(class = "custom-line"),
  h3("Churn Prediction Model"),
  fluidRow(
    column(width = 3,
           h4("Logistic Cross Validation (10-fold)"),
           tableOutput("logistic_cv_table"),
           h4("Confusion Matrix Metrics"),
           tableOutput("matrix_table")
    ),
    column(width = 9,
           div(class = "custom-chart",
               plotOutput(outputId = "roc")
           )
    )
  ),
  hr(class = "custom-line"),
  h3("Recommendation"),
  fluidRow(
    column(width = 6,
           p("The visual analysis highlights a significant trend: churn rates 
             are notably higher among customers aged 40-60 and above 60. This 
             finding suggests that these age groups might be experiencing 
             challenges or unmet needs that lead them to discontinue their 
             service. Understanding and addressing these factors could greatly 
             reduce churn and improve overall customer retention."),
           p("The data suggests that older customers might have specific needs 
             or concerns leading to higher churn rates. Continuous monitoring 
             and analysis of these churn rates, alongside the feedback 
             collected, can help in iterating and refining the strategies 
             employed."),
           p("In summary, by focusing on targeted retention strategies that 
             address the unique needs and preferences of customers aged 40-60 
             and above 60, the company can significantly reduce churn rates. 
             This approach not only helps in maintaining a stable customer base 
             but also builds long-term loyalty by demonstrating a commitment to 
             meeting the specific needs of these valuable customer segments.")
    )
  ),
  hr(class = "custom-line"),
)



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

# Combine the UI and server to create the Shiny app
shinyApp(ui = ui, server = server)




























