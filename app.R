# ================================
# Dental Health and Diabetes Shiny App
# Created by: George Eid
# ================================

# 1️⃣ Load libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinythemes)

# ================================
# 2️⃣ Sample medical dataset
# ================================
set.seed(123)

medical_data <- data.frame(
  PatientID = 1:100,
  Age = sample(30:80, 100, replace = TRUE),
  Gender = sample(c("Male", "Female"), 100, replace = TRUE),
  Diabetes = sample(c("Yes", "No"), 100, replace = TRUE),
  GumDisease = sample(c("Yes", "No"), 100, replace = TRUE),
  ToothLoss = sample(0:10, 100, replace = TRUE),
  Cavities = sample(0:8, 100, replace = TRUE)
)

# ================================
# 3️⃣ UI
# ================================
ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  
  titlePanel("Dental Health and Diabetes Visualization"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "varX",
        "Select X Variable:",
        choices = c("Age", "ToothLoss", "Cavities"),
        selected = "Age"
      ),
      
      selectInput(
        "varY",
        "Select Y Variable:",
        choices = c("Age", "ToothLoss", "Cavities"),
        selected = "ToothLoss"
      ),
      
      checkboxGroupInput(
        "genderFilter",
        "Select Gender:",
        choices = c("Male", "Female"),
        selected = c("Male", "Female")
      ),
      
      checkboxGroupInput(
        "diabetesFilter",
        "Diabetes Status:",
        choices = c("Yes", "No"),
        selected = c("Yes", "No")
      ),
      
      checkboxGroupInput(
        "gumFilter",
        "Gum Disease Status:",
        choices = c("Yes", "No"),
        selected = c("Yes", "No")
      ),
      
      sliderInput(
        "ageFilter",
        "Select Age Range:",
        min = min(medical_data$Age),
        max = max(medical_data$Age),
        value = c(min(medical_data$Age), max(medical_data$Age))
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot", plotOutput("scatterPlot")),
        tabPanel("Boxplot", plotOutput("boxPlot")),
        tabPanel("Histogram", plotOutput("histPlot")),
        tabPanel("Data Table", DTOutput("dataTable"))
      )
    )
  )
)

# ================================
# 4️⃣ Server
# ================================
server <- function(input, output) {
  
  # Filtered data
  filtered_data <- reactive({
    medical_data %>%
      filter(
        Gender %in% input$genderFilter,
        Diabetes %in% input$diabetesFilter,
        GumDisease %in% input$gumFilter,
        Age >= input$ageFilter[1],
        Age <= input$ageFilter[2]
      )
  })
  
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    data <- filtered_data()
    
    ggplot(
      data,
      aes_string(
        x = input$varX,
        y = input$varY,
        color = "Diabetes"
      )
    ) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
      facet_wrap(~Gender) +
      scale_color_manual(values = c("Yes" = "red", "No" = "blue")) +
      theme_minimal() +
      labs(
        title = paste(input$varX, "vs", input$varY, "by Gender"),
        x = input$varX,
        y = input$varY,
        color = "Diabetes"
      )
  })
  
  # Boxplot
  output$boxPlot <- renderPlot({
    data <- filtered_data()
    
    ggplot(data, aes(x = Diabetes, y = ToothLoss, fill = Diabetes)) +
      geom_boxplot(alpha = 0.7) +
      facet_wrap(~Gender) +
      scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +
      theme_minimal() +
      labs(
        title = "Tooth Loss by Diabetes and Gender",
        x = "Diabetes",
        y = "Tooth Loss"
      )
  })
  
  # Histogram
  output$histPlot <- renderPlot({
    data <- filtered_data()
    
    ggplot(data, aes(x = ToothLoss, fill = Diabetes)) +
      geom_histogram(bins = 10, position = "dodge", alpha = 0.7) +
      facet_wrap(~Gender) +
      scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +
      theme_minimal() +
      labs(
        title = "Distribution of Tooth Loss by Gender and Diabetes",
        x = "Tooth Loss",
        y = "Count"
      )
  })
  
  # Data Table
  output$dataTable <- renderDT({
    datatable(
      filtered_data(),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
}

# ================================
# 5️⃣ Run App
# ================================
shinyApp(ui = ui, server = server)
