#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$inputs <- renderUI({
      lapply(featuresWt.nonZero$featuresOnPath, function(feature) {
          if(class(dt.try[[feature]]) == "numeric"){
              sliderInput(inputId = paste0("input_", feature)
                          , label = feature
                          , min = min(data[[feature]], na.rm = T)
                          , max = max(data[[feature]], na.rm = T)
                          , value = dt.try[[feature]])
          } else if(class(dt.try[[feature]]) %in% c("factor", "character")){
              selectInput(inputId = paste0("input_", feature)
                          , label = feature
                          , choices = as.list(levels(data[[feature]]))
                          , selected = match(as.character(dt.try$Pclass), levels(data$Pclass)))
          }
          
      })
      
      
      
      
      
  })
  
  output$distPlot <- renderPlot({
      
      # generate bins based on input$bins from ui.R
      x  <- faithful[, 2] 
      range01 <- function(x){(x-min(x))/(max(x)-min(x))}
      x <- range01(x) 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      dt.play <- dt.try
      for(feature in featuresWt.nonZero$featuresOnPath){
          dt.play[[feature]] <- input[[paste0("input_", feature)]]
      }
      
      colClass <- sapply(dt.play, class)
      if("integer" %in% colClass){
          dt.play[, colnames(dt.play[, colClass == "integer", with = F]) := lapply(.SD, as.numeric)
                  , .SDcols = colnames(dt.play[, colClass == "integer", with = F])]
      }
      
      m.play <- data.matrix(dt.play)
      dplay <- xgb.DMatrix(data = m.play)
      pred.play <- predict(md.xgb, dplay)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
      abline(v = pred.play, col = "red")
      
  })
  
})



