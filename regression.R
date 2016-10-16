require(data.table)
require(xgboost)
require(caret)
data(mtcars)

setDT(mtcars)

# add some noise
# set.seed(1)
# iris <- rbind(iris
#               , data.table(Sepal.Length = rnorm(20, mean = 4, 1)
#                            , Sepal.Width = rnorm(20, mean = 2, .5)
#                            , Petal.Length = rnorm(20, mean = 2, .1)
#                            , Petal.Width = rnorm(20, mean = 1, .01)
#                            , target = sample(0:2, 20, replace = T)
#               )
# )

set.seed(1)
ind.train <- createDataPartition(mtcars$mpg, p = .8, list = F)
dt.train <- mtcars[ind.train]
dt.valid <- mtcars[!ind.train]

mx.train <- as.matrix(dt.train[, !c("mpg"), with = F])
mx.valid <- as.matrix(dt.valid[, !c("mpg"), with = F])

dmx.train <- xgb.DMatrix(data = mx.train, label = dt.train$mpg)
dmx.valid <- xgb.DMatrix(data = mx.valid, label = dt.valid$mpg)

watchlist <- list(valid = dmx.valid, train = dmx.train)
params <- list(objective = "reg:linear"
               , booster = "gbtree"
               , eta = 1)

set.seed(1)
mod <- xgb.train(params = params
                 , data = dmx.train
                 , watchlist = watchlist
                 , nrounds = 10
                 , verbose = 1
                 , print.every.n = 1
                 , early.stop.round = 3
                 , maximize = F)


# explain -----------------------------------------------------------------

dt.try <- dt.valid[, !c("mpg"), with = F][1]
dt.try
#    cyl  disp hp drat   wt qsec vs am gear carb
# 1:   4 140.8 95 3.92 3.15 22.9  1  0    4    2

predict(mod, dmx.valid, outputmargin = F)[1]
# 20.79204
predict(mod, dmx.valid, outputmargin = T)[1]
# 20.79204

# tree
featureNames <- names(dt.try)
dt.tree <- xgb.model.dt.tree(feature_names = featureNames, model = mod)
dt.tree <- dt.tree[order(as.double(dt.tree$Tree), as.double(gsub("[[:digit:]]-", "", dt.tree$ID)))]

# 20.935 + 0.12470200 + -0.7317360 + 0.015625000 + 8.17056e-02-0.007303870
# sum will do





