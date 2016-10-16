logistic <- function(x){
    exp(x) / (1 + exp(x))
}

require(data.table)
require(xgboost)
require(caret)
data(iris)

setDT(iris)

iris[, target := ifelse(Species == "setosa", 0
                        , ifelse(Species == "versicolor", 1, 2))]
iris[, Species := NULL]

# add some noise
set.seed(1)
iris <- rbind(iris
              , data.table(Sepal.Length = rnorm(20, mean = 4, 1)
                           , Sepal.Width = rnorm(20, mean = 2, .5)
                           , Petal.Length = rnorm(20, mean = 2, .1)
                           , Petal.Width = rnorm(20, mean = 1, .01)
                           , target = sample(0:2, 20, replace = T)
              )
)

set.seed(1)
ind.train <- createDataPartition(iris$target, p = .8, list = F)
dt.train <- iris[ind.train]
dt.valid <- iris[!ind.train]

mx.train <- as.matrix(dt.train[, !c("target"), with = F])
mx.valid <- as.matrix(dt.valid[, !c("target"), with = F])

dmx.train <- xgb.DMatrix(data = mx.train, label = dt.train$target)
dmx.valid <- xgb.DMatrix(data = mx.valid, label = dt.valid$target)

watchlist <- list(valid = dmx.valid, train = dmx.train)
params <- list(objective = "multi:softmax"
               , booster = "gbtree"
               , num_class = 3
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

# tree
featureNames <- names(dt.try)
dt.tree <- xgb.model.dt.tree(feature_names = featureNames, model = mod)
dt.tree <- dt.tree[order(as.double(dt.tree$Tree), as.double(gsub("[[:digit:]]-", "", dt.tree$ID)))]

# split by class
nclasses <- 3
ntrees <- max(dt.tree$Tree) + 1
nsteps <- ntrees / nclasses

ls.multiTrees <- list()
for(class in 1:nclasses){
    tree <- seq(class, ntrees, nclasses)
    ls.multiTrees[[class]] <- dt.tree[Tree %in% (tree - 1)]
}

dt.try1 <- dt.valid[, !c("target"), with = F][33]
dt.try1
#    Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1:     4.593901    2.381588     1.986495   0.9941048

predict(mod, dmx.valid, outputmargin = F)[33]
# 0
predict(mod, dmx.valid, ntreelimit = 1, outputmargin = T)[33]
# 0.07142854
predict(mod, dmx.valid, ntreelimit = 2, outputmargin = T)[33]
# -0.003004551
predict(mod, dmx.valid, ntreelimit = 3, outputmargin = T)[33]
# -0.02223086
predict(mod, dmx.valid, ntreelimit = 4, outputmargin = T)[33]
# -0.1819847

# 1
-7.32284e-01 -5.31554e-01 -4.71174e-01 -4.12748e-01
# 0.1045407
# exp
# 0.1167454

# 2
-0.7150260 -0.0499992 -0.4296610 -0.3436280
# 0.1767805
# exp
# 0.2147428

# 3
0.95121900 + 0.09121610 -0.15046700 + 0.22677400
# 0.7537553
# exp
# 3.061001

##########################################################################################

dt.try <- dt.valid[, !c("target"), with = F][1]
dt.try
#    Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1:          5.1         3.5          1.4         0.2

predict(mod, dmx.valid, outputmargin = F)[1]
# 0
predict(mod, dmx.valid, ntreelimit = 1, outputmargin = T)[1]
# 1.921965
predict(mod, dmx.valid, ntreelimit = 2, outputmargin = T)[1]
# 2.49486
predict(mod, dmx.valid, ntreelimit = 3, outputmargin = T)[1]
# 2.954991
predict(mod, dmx.valid, ntreelimit = 4, outputmargin = T)[1]
# 3.326782

# 1
1.42197e+00 + 5.72894e-01 + 4.60131e-01 + 3.71792e-01
# 0.9441063
# exp
# 16.8911

# 2
-0.7150260 -0.4843730 -0.4584300 -0.3436280
# 0.11905
# exp
# 0.1351382

# 3
-0.72786900 -0.53164200 -0.41280800 -0.34392900
# 0.1175075
# exp
# 0.1331541


# prob --------------------------------------------------------------------

params <- list(objective = "multi:softprob"
               , booster = "gbtree"
               , num_class = 3
               , eta = 1)

set.seed(1)
mod.prob <- xgb.train(params = params
                 , data = dmx.train
                 , watchlist = watchlist
                 , nrounds = 10
                 , verbose = 1
                 , print.every.n = 1
                 , early.stop.round = 3
                 , maximize = F)

predict(mod.prob, dmx.valid)


# put into function -------------------------------------------------------


ret <- list()
featureNames <- names(dt.try)
for(class in 1:length(ls.multiTrees)){
    
    trees <- unique(ls.multiTrees[[class]]$Tree)
    for(i in 1:length(trees)){
        ret[class][i] <- walkThroughTree(ls.multiTrees[[class]][Tree == trees[i]], featureNames)
    }
}










