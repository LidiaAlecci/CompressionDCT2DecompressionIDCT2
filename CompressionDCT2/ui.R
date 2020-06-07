#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard) 

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tags$head(
        tags$style(HTML("
      .col-sm-6, .col-sm-8 {
	    padding-left: 3em;
      }
    "))
    ),
    # Application title
    titlePanel("Image Compression"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        fluidRow(
            # Sidebar panel for inputs ----
            sidebarPanel(
                # Input: Select a file ----
                fileInput("image", "Choose Bitmap File",
                          multiple = FALSE,
                          accept = c(".bmp")),
                
                # Horizontal line ----
                tags$hr(),
                numericInput(inputId = "f", label = "Choose the value of F", 20, min=0),
                numericInput(inputId = "d", label = "Choose the value of d", 0, min=0),
                submitButton("Submit")
            ),
            # Main panel for displaying outputs ----
            mainPanel(
                
                # Output: ----
                plotOutput("tablePlot", width = 400, height = 300),
            )
        ),
        fluidRow(
            box(title = "Input", plotOutput("oldImage", width = 400, height = 300)),
            box(title = "Output", plotOutput("newImage", width = 400, height = 300))
        )
    )
))
