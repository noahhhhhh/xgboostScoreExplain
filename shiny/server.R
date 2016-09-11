require(shiny)
require(grid)
require(gridExtra)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
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
          geom_hline(yintercept = ls.ret$pred, colour="#990000", linetype="dashed") +
          annotate("text", label = ls.ret$pred, x = -4, y = ls.ret$pred + .1, colour = "red")
      
      ls.ret$dt$featuresOnPath <- factor(ls.ret$dt$featuresOnPath
                                         , levels = unique(ls.ret$dt$featuresOnPath)[order(abs(ls.ret$dt$N))])
      
      plot.featuresImp <- ggplot(ls.ret$dt
                                , aes(x = featuresOnPath
                                      , y = abs(N)
                                      , fill = featuresOnPath)) +
          geom_bar(stat = "identity", position = "identity", show.legend = F) +
          xlab("Important Features") +
          ylab("Individual Importance") +
          coord_flip() +
          theme_bw() +
          theme(axis.text.x = element_blank())
      
      plot.featuresWt <- ggplot(ls.ret$dt
                                , aes(x = featuresOnPath
                                      , y = N
                                      , fill = featuresOnPath)) +
          geom_bar(stat = "identity", position = "identity", show.legend = F) +
          geom_hline(yintercept = 0, colour = "#990000", linetype = "dashed") +
          ylim(c(-.05, .05)) +
          geom_text(aes(label = N, vjust = ifelse(N >= 0, 0, 1))) +
          ylab("Gain (Quantified Importance)") +
          coord_flip() +
          theme_bw() +
          theme(axis.title.y = element_blank(),
                axis.text.x = element_blank())
      
      str <- character()
      for(i in 1:nrow(ls.ret$dt)){
          str <- paste(str, paste0(ls.ret$dt[i]$featuresOnPath, ": ", ls.ret$dt[i]$N), "<br/>")
      }
      
      grid.arrange(plot.exp, arrangeGrob(plot.featuresImp, plot.featuresWt, ncol = 2), ncol = 1)
      
  })
  
  output$texts <- renderUI({
      
      ls.ret <- ls.featuresWt()
      
      str <- character()
      for(i in 1:nrow(ls.ret$dt)){
          str <- paste(str, paste0(ls.ret$dt[i]$featuresOnPath, ": ", ls.ret$dt[i]$N), "<br/>")
      }
      str <- paste(str, "--------------------------", "<br/>")
      str <- paste(str, "<strong>SUM </strong>:", sum(ls.ret$dt$N), "<br/>")
      str <- paste(str, "---------------------------------------------------------------------", "<br/>")
      str <- paste0(str, " exp(<strong>", sum(ls.ret$dt$N), "</strong>) /", " (1 + exp(<strong>", sum(ls.ret$dt$N), "</strong>)) = ", ls.ret$pred)
      str <- paste("<div align='right' style = 'background-color = #F5F6CE'>", str, "</div>")
      HTML(str)
  })
})



