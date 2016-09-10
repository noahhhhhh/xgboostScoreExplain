require(data.table)
require(xgboost)
require(caret)

source("utilities.R")

data <- fread("data/train.csv")
setDT(data)

colClass <- unlist(lapply(data, class))
colClass[colClass == "integer"]
data$PassengerId <- as.numeric(data$PassengerId)
data$Survived <- as.numeric(data$Survived)
data$Pclass <- as.factor(data$Pclass)
data$SibSp <- as.numeric(data$SibSp)
data$Parch <- as.numeric(data$Parch)

data[, Name := NULL]
data[, Cabin := NULL]
data[, Ticket := NULL]
data[, PassengerId := NULL]
data[, Embarked := NULL]

ColNAs(data, method = "sum", output = "all")
data[, Age := ifelse(is.na(Age), median(data$Age), Age)]
data[, Sex := ifelse(Age == "mail", 1, 0)]


ind.train <- createDataPartition(data$Survived, p = .7, list = F)
dt.train <- data[ind.train]
dt.eval <- data[!ind.train]

m.train <- data.matrix(dt.train[, !c("Survived"), with = F])
m.eval <- data.matrix(dt.eval[, !c("Survived"), with = F])

dtrain <- xgb.DMatrix(data = m.train, label = dt.train$Survived, missing = NaN)
deval <- xgb.DMatrix(data = m.eval, label = dt.eval$Survived, missing = NaN)

watchlist <- list(valid = deval)
params <- list(objective = "binary:logistic", 
                booster = "gbtree",
                eval_metric = "auc",
                eta                 = 0.02, # 0.06, #0.01,
                max_depth           = 4, #changed from default of 8
                subsample           = 0.83, # 0.7
                colsample_bytree    = 0.77 # 0.7
                #num_parallel_tree   = 2
                # alpha = 0.0001, 
                # lambda = 1
)
set.seed(1231231)
md.xgb <- xgb.train(params = params
                    , data = dtrain
                    , nrounds = 500
                    , watchlist = watchlist
                    , maximize = F
                    , print.every.n = 1
                    , early.stop.round = 1)
dt.test <- dt.train[1, ]
dt.try <- dt.test[, !c("Survived"), with = F]

saveRDS(md.xgb, file = "md.rds")
saveRDS(dt.try, file = "dtTry.rds")
saveRDS(data, file = "data.rds")
