
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
  
  ls.featuresWt <- reactive({
      dt.play <- dt.try
      for(feature in featuresWt.nonZero$featuresOnPath){
          dt.play[[feature]] <- input[[paste0("input_", feature)]]
      }
      
      new.ret.scoreExplain <- xgboostScoreExplain(md.xgb, dt.play)

      list(pred = new.ret.scoreExplain$pred
           , dt = new.ret.scoreExplain$featuresWt[N != 0])
  })
  
  output$plots <- renderPlot({
      
      ls.ret <- ls.featuresWt()
      
      # exp plot
      plot.exp <- ggplot(data.exp, aes(x = x, y = y)) + 
          geom_line() +
          xlab("gain/quality/raw score") +
          ylab("score") +
          geom_hline(yintercept = ls.ret$pred) +
          geom_text(data = data.table(x = 0, y = ls.ret$pred)
                    , aes(x, y)
                    , label = ls.ret$pred
                    , hjust = 0
                    , vjust = -1)
      
      ls.ret$dt$featuresOnPath <- factor(ls.ret$dt$featuresOnPath
                                         , levels = unique(ls.ret$dt$featuresOnPath)[order(abs(ls.ret$dt$N))])
      
      plot.featuresWt <- ggplot(ls.ret$dt
                                , aes(x = featuresOnPath
                                      , y = N
                                      , fill = featuresOnPath)) +
          geom_bar(stat = "identity", position = "identity", show.legend = F) +
          ylim(c(-.05, .05)) +
          geom_text(aes(label = N, vjust = ifelse(N >= 0, 0, 1))) +
          coord_flip()
      
      require(gridExtra)
      grid.arrange(plot.exp, plot.featuresWt
                   , ncol = 1)
      
  }
  , height = 700, width = 700)
  
  output$texts <- renderUI({
      
      ls.ret <- ls.featuresWt()
      
      str <- character()
      for(i in 1:nrow(ls.ret$dt)){
          str <- paste(str, paste0(ls.ret$dt[i]$featuresOnPath, ": ", ls.ret$dt[i]$N), "<br/>")
      }
      str <- paste(str, "------------------------------------", "<br/>")
      str <- paste(str, sum(ls.ret$dt$N), "<br/>")
      str <- paste("<div align='right' style = 'background-color = #F5F6CE'>", str, "</div>")
      HTML(str)
  })
})



