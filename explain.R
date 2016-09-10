rm(list = ls()); gc();
require(xgboost)
source("xgboostScoreExplain.R")

md.xgb <- readRDS("md.rds")
dt.try <- readRDS("dtTry.rds")

m.try <- data.matrix(dt.try)
dtry <- xgb.DMatrix(data = m.try)
pred.try <- predict(md.xgb, dtry)
pred.try

xgboostScoreExplain(md.xgb, dt.try)