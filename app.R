#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

  sidebarLayout(
    sidebarPanel(numericInput(inputId = "preg",
                              label = "Pregnancies:",
                              min = 0,
                              max = 17,
                              value = 1),
                 numericInput(inputId = "glu",
                              label = "Glucose:",
                              min = 0,
                              max = 199,
                              value = 5),
                 numericInput(inputId = "BP",
                              label = "Blood Pressure:",
                              min = 0,
                              max = 122,
                              value = 100),
                 numericInput(inputId = "BMI",
                              label = "BMI:",
                              min = 0,
                              max = 67,
                              value = 20),
                 numericInput(inputId = "DPF",
                              label = "DegreePedigreeFunction:",
                              min = 0,
                              max = 2.4,
                              value = 2),
                 numericInput(inputId = "Age",
                              label = "Age:",
                              min = 21,
                              max = 81,
                              value = 50)
    ),
    mainPanel(h3(textOutput("results"))
    )
  )
)
server <- function(input, output) {
  
  output$results <- renderText({
    odds <- 0.433 + 0.021 * (input$preg) + 0.006 * (input$glu) - 0.002 * (input$BP) +  0.013 * (input$BMI) + 0.151 * (input$DPF) + 0.003 * (input$Age)   
    results <-  odds/(1+odds)
    ##(pi/1−pi)=1.021(Pregnancies)+1.006(Glucose)+0.998(BloodPressure)+1.013(BMI)+1.151(DiabetesPedigreeFunction)+1.003(Age)
    results
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
