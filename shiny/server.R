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
  
  output$expPlot <- renderPlot({
      
      # generate bins based on input$bins from ui.R
      x  <- faithful[, 2] 
      range01 <- function(x){(x-min(x))/(max(x)-min(x))}
      x <- range01(x) 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      dt.play <- dt.try
      for(feature in featuresWt.nonZero$featuresOnPath){
          dt.play[[feature]] <- input[[paste0("input_", feature)]]
      }
      
      # colClass <- sapply(dt.play, class)
      # if("integer" %in% colClass){
      #     dt.play[, colnames(dt.play[, colClass == "integer", with = F]) := lapply(.SD, as.numeric)
      #             , .SDcols = colnames(dt.play[, colClass == "integer", with = F])]
      # }
      # 
      # m.play <- data.matrix(dt.play)
      # dplay <- xgb.DMatrix(data = m.play)
      # pred.play <- predict(md.xgb, dplay)
      
      new.ret.scoreExplain <- xgboostScoreExplain(md.xgb, dt.play)
      
      # exp plot
      print(ggplot(data.exp, aes(x = x, y = y)) + 
                geom_line() +
                xlab("gain/quality/raw score") +
                ylab("score") +
                geom_hline(yintercept = new.ret.scoreExplain$pred) +
                geom_text(data = data.table(x = 0, y = new.ret.scoreExplain$pred)
                          , aes(x, y)
                          , label = new.ret.scoreExplain$pred
                          , hjust = 0
                          , vjust = -1)
      )
      
      # # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
      # abline(v = new.ret.scoreExplain$pred, col = "red")
      # text(new.ret.scoreExplain$pred)
      
      output$featuresWtPlot <- renderPlot({
          
          dt.play <- dt.try
          for(feature in featuresWt.nonZero$featuresOnPath){
              dt.play[[feature]] <- input[[paste0("input_", feature)]]
          }
          
          new.ret.scoreExplain <- xgboostScoreExplain(md.xgb, dt.play)
          
          new.featuresWt.nonZero <- new.ret.scoreExplain$featuresWt[N != 0]
          
          new.featuresWt.nonZero
          
          new.new.featuresWt.nonZero <- new.featuresWt.nonZero
          new.new.featuresWt.nonZero$featuresOnPath <- as.factor(new.new.featuresWt.nonZero$featuresOnPath)
          new.new.featuresWt.nonZero$featuresOnPath <- factor(new.new.featuresWt.nonZero$featuresOnPath
                                                      , levels = levels(new.new.featuresWt.nonZero$featuresOnPath)[order(abs(new.new.featuresWt.nonZero$N))])
          ggplot(new.new.featuresWt.nonZero
                 , aes(x = featuresOnPath
                       , y = N
                       , fill = featuresOnPath)) +
              geom_bar(stat = "identity", position = "identity", show.legend = F) +
              ylim(c(-.05, .05)) +
              geom_text(aes(label = N,
                            vjust = ifelse(N >= 0, 0, 1)))
          print(last_plot() + coord_flip())
          
          
      })
      
  })
  
})



