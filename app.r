# Load necessary libraries
library(ggplot2)
library(dplyr)
library(caTools)
library(car)
library(caret)
library(shiny)
library(shinythemes)
library(plotly)
library(randomForest)

# Load Dataset
df <- read.csv("AmesHousing.csv")

# Feature Selection: High-Correlation Factors
cor_matrix <- cor(df %>% select_if(is.numeric), use = "complete.obs")
cor_target <- cor_matrix["SalePrice", ]
high_corr_features <- names(cor_target[abs(cor_target) > 0.4]) # Select highly correlated features

# Ensure important categorical variables are included
selected_features <- c("SalePrice", "Gr.Liv.Area", "Lot.Area", "Overall.Qual", "Bldg.Type", 
                       "House.Style", "Exter.Qual", "Bsmt.Cond", "Bedroom.AbvGr", "Garage.Cars", 
                       "Year.Built", "Full.Bath", "Kitchen.Qual")

# Filter only selected columns
selected_features <- selected_features[selected_features %in% colnames(df)]
df <- df %>% select(all_of(selected_features)) %>% na.omit()

# Convert categorical features to factors
df <- df %>%
  mutate(
    Bldg.Type = as.factor(Bldg.Type),
    House.Style = as.factor(House.Style),
    Exter.Qual = factor(Exter.Qual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
    Bsmt.Cond = factor(Bsmt.Cond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
    Kitchen.Qual = factor(Kitchen.Qual, levels = c("Fa", "TA", "Gd", "Ex"), ordered = TRUE)
  )

# Train-Test Split
set.seed(42)
trainIndex <- createDataPartition(df$SalePrice, p = 0.8, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

# Train Model with Tuned Parameters
model <- randomForest(SalePrice ~ ., data = trainData, ntree = 300, mtry = 5, importance = TRUE)

# Define the tuning grid for Random Forest
tune_grid <- expand.grid(
  mtry = c(3, 5, 7),          # Number of variables randomly sampled at each split
  splitrule = c("variance"),  # Regression uses "variance"
  min.node.size = c(5, 10, 15) # Minimum observations per terminal node
)

# Train the model using caret's train() function with cross-validation
control <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
rf_tuned <- train(
  SalePrice ~ ., 
  data = trainData, 
  method = "ranger",   # Fast implementation of Random Forest
  trControl = control, 
  tuneGrid = tune_grid,
  importance = "impurity"
)

# Define the best hyperparameters
best_tune <- rf_tuned$bestTune

# SHINY UI - Modernized
ui <- fluidPage(
  theme = shinytheme("darkly"),
  
  titlePanel(tags$h2("🏡 AI-Powered Home Valuation", align = "center")),
  
  sidebarLayout(
    sidebarPanel(
      h4("🏠 Property Details"),
      numericInput("lot_area", "Lot Area (sqft):", 8000),
      numericInput("gr_liv_area", "Living Area (sqft):", 1500),
      sliderInput("overall_qual", "Overall Quality:", min = 1, max = 10, value = 5),
      numericInput("year_built", "Year Built:", 2000),
      numericInput("bedroom_abvgr", "Bedrooms Above Ground:", 3),
      numericInput("full_bath", "Full Bathrooms:", 2),
      numericInput("garage_cars", "Garage Capacity:", 2),
      selectInput("exterior_qual", "Exterior Quality:", choices = c("Po", "Fa", "TA", "Gd", "Ex")),
      selectInput("bsmt_cond", "Basement Condition:", choices = c("Po", "Fa", "TA", "Gd", "Ex")),
      selectInput("kitchen_qual", "Kitchen Quality:", choices = c("Fa", "TA", "Gd", "Ex")),
      selectInput("bldg_type", "Building Type:", choices = unique(df$Bldg.Type)),
      selectInput("house_style", "House Style:", choices = unique(df$House.Style)),
      actionButton("predict", "🔮 Predict Price", class = "btn-primary"),
      actionButton("recommend", "💡 Renovation Recommendations", class = "btn-info")
    ),
    
    mainPanel(
      tags$h3("🏡 Property Valuation"),
      textOutput("predicted_price"),
      hr(),
      tags$h3("📊 Price Factors Impact"),
      plotlyOutput("feature_importance"),
      hr(),
      tags$h3("💡 Renovation Recommendations"),
      textOutput("renovation_suggestion"),
      hr(),
      tags$h3("📈 ROI Analysis - Renovation Impact"),
      textOutput("roi_estimate"),
      plotlyOutput("roi_plot")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  # Reactive expression to store predicted price
  predicted_price <- reactiveVal(NULL)
  
  observeEvent(input$predict, {
    
    input_data <- data.frame(
      Lot.Area = input$lot_area,
      Gr.Liv.Area = input$gr_liv_area,
      Overall.Qual = input$overall_qual,
      Year.Built = input$year_built,
      Full.Bath = input$full_bath,
      Garage.Cars = input$garage_cars,
      Bedroom.AbvGr = input$bedroom_abvgr,
      Exter.Qual = factor(input$exterior_qual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
      Bsmt.Cond = factor(input$bsmt_cond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE),
      Kitchen.Qual = factor(input$kitchen_qual, levels = c("Fa", "TA", "Gd", "Ex"), ordered = TRUE),
      Bldg.Type = factor(input$bldg_type, levels = unique(df$Bldg.Type)),
      House.Style = factor(input$house_style, levels = unique(df$House.Style))
    )
    
    # Store the predicted price in the reactive variable
    predicted_price(predict(rf_tuned, newdata = input_data))
    
    output$predicted_price <- renderText({ 
      paste("💰 Predicted Price: Rs.", format(round(predicted_price(), 2), big.mark = ",")) 
    })
    
    # Feature Importance Plot
    output$feature_importance <- renderPlotly({
      importance_data <- data.frame(Feature = rownames(importance(model)), Importance = importance(model)[, 1])
      importance_data <- importance_data %>% arrange(desc(Importance))
      
      ggplot(importance_data, aes(x = reorder(Feature, Importance), y = Importance, fill = Feature)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Feature Importance in Price Prediction", x = "Feature", y = "Importance") +
        theme_minimal()
    })
  })
  
  observeEvent(input$recommend, {
    # Check if predicted price is available
    if (!is.null(predicted_price())) {
      renovation <- "🔹 Consider upgrading kitchen or adding a garage for higher ROI.\n"
      roi <- 0
      
      # Kitchen Renovation
      if (input$kitchen_qual == "Fa") {
        renovation <- "✅ Upgrading the kitchen will significantly increase home value.\n"
        roi <- 0.10
      }
      if (input$kitchen_qual == "TA" | input$kitchen_qual == "Gd") {
        renovation <- paste(renovation, "✅ Upgrading the interior of kitchen will be beneficial.\n")
        roi <- 0.10
      }
      
      # Garage Renovation
      if (input$garage_cars == 0) {
        renovation <- paste(renovation, "✅ Adding a garage will significantly increase home value.\n")
        roi <- roi + 0.07
      }
      
      # Living Area vs. Lot Area
      if (input$gr_liv_area < (0.6 * input$lot_area)) {
        renovation <- paste(renovation, "✅ Your home has a large lot with a small living area. Consider adding a garden, fireplace, or pool for better ROI.\n")
        roi <- roi + 0.05
      }
      
      # Overall Quality-Based Recommendations
      if (input$overall_qual < 3) {
        renovation <- paste(renovation, "✅ Your home's overall quality is low. Painting and wall proofing are recommended to boost property value.\n")
        roi <- roi + 0.08
      } else if (input$overall_qual < 7) {
        renovation <- paste(renovation, "✅ Interior decoration and home improvements are suggested for better resale value.\n")
        roi <- roi + 0.06
      }
      
      # Basement Condition Recommendations
      if (input$bsmt_cond == "Po") {
        renovation <- paste(renovation, "✅ Your basement condition is poor. Consider improving the basement to enhance overall property value.\n")
        roi <- roi + 0.05
      } else if (input$bsmt_cond == "Fa") {
        renovation <- paste(renovation, "✅ Improving basement conditions will boost the home value.\n")
        roi <- roi + 0.05
      }
      
      # ROI Estimate
      price_after_renovation <- predicted_price() * (1 + roi)
      output$renovation_suggestion <- renderText({ renovation })
      output$roi_estimate <- renderText({ paste("📈 Estimated Price After Renovation: Rs.", format(round(price_after_renovation, 2), big.mark = ",")) })
      
      # ROI Plot (Bar Chart)
      renovation_costs <- data.frame(
        Renovation = c("Kitchen Upgrade", "Garage Addition", "Living Area Expansion", "Overall Quality Improvement", "Basement Improvement"),
        Impact = c(roi * 100, 0.07 * 100, 0.05 * 100, 0.08 * 100, 0.05 * 100)
      )
      
      output$roi_plot <- renderPlotly({
        ggplot(renovation_costs, aes(x = Renovation, y = Impact, fill = Renovation)) +
          geom_bar(stat = "identity") +
          labs(title = "Cost vs Impact of Renovations",
               x = "Renovation Type",
               y = "Price Impact (%)") +
          scale_y_continuous(labels = scales::percent) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
    } else {
      output$renovation_suggestion <- renderText({ "Please click the Predict button to get the predicted price." })
      output$roi_estimate <- renderText({ "Please click the Predict button to get the predicted price." })
      output$roi_plot <- renderPlotly({ ggplot() + theme_minimal() })
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)