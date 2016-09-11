rm(list = ls()); gc();
require(xgboost)
source("xgboostScoreExplain.R")

md.xgb <- readRDS("md.rds")
dt.try <- readRDS("dtTry.rds")
data <- readRDS("data.rds")

m.try <- data.matrix(dt.try)
dtry <- xgb.DMatrix(data = m.try)
pred.try <- predict(md.xgb, dtry)
pred.try

ret.scoreExplain <- xgboostScoreExplain(md.xgb, dt.try)
featuresWt.nonZero <- ret.scoreExplain$featuresWt[N != 0]

x <- seq(-5, 5, by = .01)
y <- logit(x)
data.exp <- data.table(x = x, y = y)

str <- character()
for(i in 1:nrow(featuresWt.nonZero)){
    str <- paste(str, paste0(featuresWt.nonZero[i]$featuresOnPath, ": ", featuresWt.nonZero[i]$N), "<br/>")
}
str <- paste(str, "------------------------------------", "<br/>")
str <- paste(str, sum(featuresWt.nonZero$N), "<br/>")
str <- paste("<div align='center'>", str, "</div>")
HTML(str)
