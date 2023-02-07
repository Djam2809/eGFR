#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(shiny)
library(shinydashboard)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      column(width = 6,
             radioButtons("sex", "Sex", c("Male" = 1, "Female" = 0)),
             numericInput("creat", "Creatinine (μmol/L)", value = 85, min = 0, max = 500),
             numericInput("height", "Height (cm)", value = 158, min = 0, max = 250),
             numericInput("bmi", "BMI (kg/m^2)", value = 18, min = 0, max = 100),
             numericInput("CystC", "Cystatin C (mg/L)", value = 1.29, min = 0, max = 5)),
      column(width = 6,
             plotlyOutput("egfr"))
    )
  )
)

server <- function(input, output) {
  output$egfr <- renderPlotly({
    sexM <- as.numeric(input$sex)
    creat <- as.numeric(input$creat)
    height <- as.numeric(input$height)
    bmi <- as.numeric(input$bmi)
    CystC <- as.numeric(input$CystC)
    
    eGFR <- (73.7 - 0.546 * max(0, creat - 72) + 1.4 * sexM * max(0, height - 158) + 75.7 - 27.3 * max(0, CystC - 1.29) - 0.367 * max(0, height - 136) + 6.57 * bmi * max(0, 1.29 - CystC) * sexM) / 2
    
    
    plot_ly(type = "indicator",
            mode = "gauge+number",
            value = eGFR,
            title = "eGFR (ml/min/1.73 m2)",
            gauge = list(axis = list(range = c(0, 130),layout = list(width = 50, height = 50)),
                         bar = list(color = "darkgray"),
                         steps = list(list(range = c(0, 15), color = "black",name = "stage 5"),
                                      list(range = c(15, 30), color = "red",name = "stage 4"),
                                      list(range = c(30, 45), color = "orange",name = "stage 3B"),
                                      list(range = c(45, 60), color = "#FAD7A0",name = "stage 3A"),
                                      list(range = c(60, 90), color = "yellow",name = "stage 2"),
                                      list(range = c(90, 130), color = "green",name = "stage 1")),
                         threshold = list(value = 130, line = list(color = "green", width = 2))))
  })
}

shinyApp(ui, server)