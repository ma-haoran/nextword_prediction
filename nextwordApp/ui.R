library(shiny)


shinyUI(navbarPage("Coursera Data Science Capstone: Course Project",
                   tabPanel("Predict the Next Word",
                            HTML("<strong>Author: Ma Haoran</strong>"),
                            br(),
                            HTML("<strong>Date: 21 Feb 2021</strong>"),
                          
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("x",
                        "numbers of prediction",
                        min = 1,
                        max = 20,
                        value = 3),
            textInput("inputtext","Tap")
        ),
        
        
        mainPanel(
            textOutput("text")
        )
    )
    ),
    tabPanel("About",
             mainPanel(includeHTML("C:/Users/Apple/Desktop/RStudio Tour/assignment/assignment10/nextword_prediction/about.HTML")))
))
