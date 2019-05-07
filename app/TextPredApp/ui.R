library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Text Prediction App"),
    # first row - large text input
    helpText("Just start typing to see the predictions at the bottom. Click on a prediction use it in your text"),
    fluidRow(
        column(6, offset = 1,
            textAreaInput("text_input", "", value="", width = "49vw", height = "300px"),
        # second row - three best predictions
            fluidRow(
                div(style="display:inline-block;width:32%;text-align: center;",uiOutput("prediction2_button")),
                div(style="display:inline-block;width:32%;text-align: center;",uiOutput("prediction1_button")),
                div(style="display:inline-block;width:32%;text-align: center;",uiOutput("prediction3_button"))
                   )
        )
    )
    
))
