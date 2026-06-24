# Load necessary libraries
library(shiny)
library(shinythemes)
library(randomForest)
library(plotly)
library(ggplot2)
library(dplyr)
library(pagedown)   # Chrome-based HTML → PDF 

# Load trained model and sample data
model <- readRDS("rf_tuned.rds")
df <- read.csv("AmesHousing.csv")

# Define UI
ui <- fluidPage(
  theme = shinytheme("darkly"), # set the custom theme from shinytheme
  
  # set the page title & window title
  titlePanel(
    title       = div(tags$h2("🏡 AI-Powered Home Valuation", style = "text-align:center;")),
    windowTitle = "HousePredX – AI Home Valuation"
  ),
  
  # UI theme - sidebar layout
  sidebarLayout(
    # set the sidebar panel elements (used to take inputs)
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
      
      # actionButton to predict & recommend
      actionButton("predict",   "🔮 Predict Price",              class = "btn-primary"),
      actionButton("recommend", "💡 Renovation Recommendations", class = "btn-info")
    ),
    
    # main panel to display outputs
    mainPanel(
      
      tags$h3("🏡 Property Valuation"),
      textOutput("predicted_price"), # predicted price from model
      hr(), # <hr/> tag for horizontal tags
      
      tags$h3("📊 Price Factors Impact"),
      plotlyOutput("feature_importance"), # feature importance for interpretability
      hr(),
      
      tags$h3("💡 Renovation Recommendations"),
      textOutput("renovation_suggestion"), # recommendations
      hr(),
      
      tags$h3("📈 ROI Analysis - Renovation Impact"),
      textOutput("roi_estimate"), # roi plot for visual informatics
      plotlyOutput("roi_plot"),
      hr(),
      
      # download button to save the report as .pdf file
      downloadButton("download_report", "⬇️ Download PDF Report", class = "btn-success")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # define reactive variables to render
  predicted_price        <- reactiveVal(NULL)
  renovation_text        <- reactiveVal(NULL)
  price_after_renovation <- reactiveVal(NULL)
  roi_val                <- reactiveVal(0)
  
  
  # handler for predict actionButton
  observeEvent(input$predict, {
    # takes input from frontend and makes it to a dataframe
    input_data <- data.frame(
      Lot.Area      = input$lot_area,
      Gr.Liv.Area   = input$gr_liv_area,
      Overall.Qual  = input$overall_qual,
      Year.Built    = input$year_built,
      Full.Bath     = input$full_bath,
      Garage.Cars   = input$garage_cars,
      Bedroom.AbvGr = input$bedroom_abvgr,
      Exter.Qual    = factor(input$exterior_qual, levels = c("Po","Fa","TA","Gd","Ex"), ordered = TRUE),
      Bsmt.Cond     = factor(input$bsmt_cond,     levels = c("Po","Fa","TA","Gd","Ex"), ordered = TRUE),
      Kitchen.Qual  = factor(input$kitchen_qual,  levels = c("Fa","TA","Gd","Ex"),      ordered = TRUE),
      Bldg.Type     = factor(input$bldg_type,  levels = unique(df$Bldg.Type)),
      House.Style   = factor(input$house_style, levels = unique(df$House.Style))
    )
    
    # model prediction
    pred <- predict(model, newdata = input_data)
    predicted_price(pred) # set the predicted_price reactiveVal with pred
    
    # render predicted_price text to mainPanel
    output$predicted_price <- renderText({
      paste("💰 Predicted Price: $", format(round(pred, 2), big.mark = ","))
    })
    
    # render the plot for feature importance to mainPanel
    output$feature_importance <- renderPlotly({
      importance_data <- data.frame(
        Feature    = rownames(varImp(model)),
        Importance = varImp(model)[, 1]
      ) %>% arrange(desc(Importance))
      
      ggplot(importance_data, aes(x = reorder(Feature, Importance), y = Importance, fill = Feature)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Feature Importance in Price Prediction", x = "Feature", y = "Importance") +
        theme_minimal()
    })
  })
  
  # prepares roi, renovation to render on actionButton recommend click.
  observeEvent(input$recommend, {
    if (!is.null(predicted_price())) {
      renovation <- ""
      roi <- 0
      
      if (input$kitchen_qual == "Fa") {
        renovation <- "Upgrading the kitchen will significantly increase home value.\n"
        roi <- roi + 0.10
      } else if (input$kitchen_qual %in% c("TA", "Gd")) {
        renovation <- paste(renovation, "Upgrading the interior of the kitchen will be beneficial.\n")
        roi <- roi + 0.10
      }
      
      if (input$garage_cars == 0) {
        renovation <- paste(renovation, "Adding a garage will significantly increase home value.\n")
        roi <- roi + 0.07
      }
      
      if (input$gr_liv_area < (0.6 * input$lot_area)) {
        renovation <- paste(renovation, "Consider utilizing lot space (garden, pool, fireplace).\n")
        roi <- roi + 0.05
      }
      
      if (input$overall_qual < 3) {
        renovation <- paste(renovation, "Wall proofing and basic upgrades recommended.\n")
        roi <- roi + 0.08
      } else if (input$overall_qual < 7) {
        renovation <- paste(renovation, "Moderate interior upgrades advised.\n")
        roi <- roi + 0.06
      }
      
      if (input$bsmt_cond == "Po") {
        renovation <- paste(renovation, "Improve basement condition for better value.\n")
        roi <- roi + 0.05
      } else if (input$bsmt_cond == "Fa") {
        renovation <- paste(renovation, "Enhancing basement will boost value.\n")
        roi <- roi + 0.05
      }
      
      new_price <- predicted_price() * (1 + roi) # new price based on rule-based calculation
      renovation_text(renovation) # set the reactive variable rennovation_text
      price_after_renovation(new_price) # set the reactive variable
      roi_val(roi) # set the reactive variable
      
      # rendering rennovation to mainPanel
      output$renovation_suggestion <- renderText({ renovation })
      
      #rendering price after rennovation to mainPanel
      output$roi_estimate <- renderText({
        paste("📈 Estimated Price After Renovation: $", format(round(new_price, 2), big.mark = ","))
      })
      
      # render the roi Plot to mainPanel
      output$roi_plot <- renderPlotly({
        renovation_costs <- data.frame(
          Renovation = c("Kitchen Upgrade", "Garage Addition", "Lot Utilization", "Overall Quality", "Basement"),
          Impact     = c(0.10, 0.07, 0.05, 0.08, 0.05)
        )
        ggplot(renovation_costs, aes(x = Renovation, y = Impact, fill = Renovation)) +
          geom_bar(stat = "identity") +
          labs(title = "Renovation ROI Impact", y = "Impact", x = "Type") +
          scale_y_continuous(labels = scales::percent) +
          theme_minimal()
      })
    }
  })
  
  
  # Download handler — uses pagedown::chrome_print() to convert the
  # rendered HTML report to PDF via Chrome. No LaTeX/TinyTeX required.
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("HousePredX_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      req(predicted_price(), price_after_renovation())
      
      # Step 1: copy template to temp dir
      tmp_dir    <- tempdir()
      tmp_report <- file.path(tmp_dir, "report_template.Rmd")
      tmp_html   <- file.path(tmp_dir, "report_template.html")
      file.copy("report_template.Rmd", tmp_report, overwrite = TRUE)
      
      # Step 2: knit Rmd → HTML (pagedown::html_paged format)
      rmarkdown::render(
        input       = tmp_report,
        output_file = tmp_html,
        params      = list(
          predicted_price = as.numeric(predicted_price()),
          new_price       = as.numeric(price_after_renovation()),
          renovation_text = renovation_text(),
          roi             = roi_val()
        ),
        envir = new.env(parent = globalenv())
      )
      
      # Step 3: Chrome prints the paged HTML → PDF
      pagedown::chrome_print(
        input  = tmp_html,
        output = file,
        wait   = 5   # seconds to wait for page to fully load before printing
      )
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
