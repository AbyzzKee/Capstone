source('Capstone2Predict.R')

shinyServer(function(input, output) {
  
  result <- reactive({ backoffPredict(input$entry); })
  output$predictedText1 <- renderText(paste("[1]", result()[1]));
  output$predictedText2 <- renderText(paste("[2]", result()[2]));
  output$predictedText3 <- renderText(paste("[3]", result()[3]));
  output$predictedText4 <- renderText(paste("[4]", result()[4]));
  output$predictedText5 <- renderText(paste("[5]", result()[5]));
})