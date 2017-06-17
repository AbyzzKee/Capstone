shinyUI(fluidPage(

    titlePanel("Word Prediction ShinyApp"),
    sidebarLayout(
      sidebarPanel(
        
        strong("Next Word Prediction using Back-Off Algorithm on n-grams"),
        p('Base on your input text, this ShinyApp will try to predict the next word, you might want to enter next.'),
        
        a("Click to show Documentation Slide", 
          href="http://rpubs.com/abyzzkee/CapstonePresentation")
      ),
      mainPanel(
        
        textInput("entry",
                  "Enter your text here:",
                  "who is going to"),
        br(),
        strong("The predicted words are:"),
        h3(textOutput('predictedText1')),
        br(),
        p(textOutput('predictedText2')),
        p(textOutput('predictedText3')),
        p(textOutput('predictedText4')),
        p(textOutput('predictedText5'))
        
      )
    )
    
))