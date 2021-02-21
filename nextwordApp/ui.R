library(shiny)


shinyUI(fluidPage(
    
    # Application title
    titlePanel("NextWord"),
    
    
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
))
