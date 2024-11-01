# ui.R

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