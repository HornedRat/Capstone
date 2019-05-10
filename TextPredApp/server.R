library(shiny)
source("SBO model.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    prediction1 <- reactiveVal()
    prediction2 <- reactiveVal()
    prediction3 <- reactiveVal()
    
    observe({
        predictions <- predict_from_text(input$text_input)
        
        #predictions
        prediction1(predictions$word[1])
        prediction2(predictions$word[2])
        prediction3(predictions$word[3])
        
        #reactive buttons
        output$prediction1_button <- renderUI({
            actionButton("prediction1_button", label = prediction1(),
                         style ='font-size:150%;
                             font-weight:bold;')
        })
        output$prediction2_button <- renderUI({
            actionButton("prediction2_button", label = prediction2())
        })
        output$prediction3_button <- renderUI({
            actionButton("prediction3_button", label = prediction3())
        })
        
        })
    
    
        #button events
        observeEvent(input$prediction1_button, {
            newText <- paste(input$text_input, prediction1())
            updateTextAreaInput(session, "text_input", value = newText)
        })
        
        observeEvent(input$prediction2_button, {
            newText <- paste(input$text_input, prediction2())
            updateTextAreaInput(session, "text_input", value = newText)
        })
        
        observeEvent(input$prediction3_button, {
            newText <- paste(input$text_input, prediction3())
            updateTextAreaInput(session, "text_input", value = newText)
        })

  
})
