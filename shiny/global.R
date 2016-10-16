require(xgboost)
source("../xgboostScoreExplain.R")

md.xgb <- readRDS("../md.rds")
dt.try <- readRDS("../dtTry.rds")
data <- readRDS("../data.rds")

m.try <- data.matrix(dt.try)
dtry <- xgb.DMatrix(data = m.try)
pred.try <- predict(md.xgb, dtry)
pred.try

m.data <- data.matrix(data[, !c("Survived"), with = F])
ddata <- xgb.DMatrix(data = m.data, missing = NaN)
pred.data <- predict(md.xgb, ddata)
pred.data

ret.scoreExplain <- xgboostScoreExplain(md.xgb, dt.try)
featuresWt.nonZero <- ret.scoreExplain$featuresWt[N != 0]

x <- seq(-5, 5, by = .01)
y <- logit(x)
data.exp <- data.table(x = x, y = y)

