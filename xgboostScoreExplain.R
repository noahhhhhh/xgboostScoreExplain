xgboostScoreExplain <- function(model, dt.try){
    ## TODO: different classes of cols, now it only supports numbers
    ## TODO: binary is no quite accurate
    require(plyr)
    require(data.table)
    featureNames <- names(dt.try)
    dt.tree <- xgb.model.dt.tree(feature_names = featureNames, model = model)
    dt.tree <- dt.tree[order(as.double(dt.tree$Tree), as.double(gsub("[[:digit:]]-", "", dt.tree$ID)))]
    
    tbTodtWithMoreCols <- function(tb, names){
        dt <- as.data.table(tb)
        
        diff.names <- setdiff(names, dt$featuresOnPath)
        
        dt.featuresWtMore <- rbind(dt, data.table(featuresOnPath = diff.names, N = rep(0, length(diff.names))))
        return(dt.featuresWtMore)
    }
    
    
    walkThroughTree <- function(tree, featureNames){
        cur.id <- tree[1]$ID
        next.Feature <- "!Leaf"
        
        featuresOnPath <- character()
        gain <- 0
        
        while(next.Feature != "Leaf"){
            cur.row <- tree[ID == cur.id]
            cur.id <- cur.row$ID
            
            featuresOnPath <- c(featuresOnPath, cur.row$Feature)
            
            next.ID <- ifelse(as.double(dt.try[[cur.row$Feature]]) < as.double(cur.row$Split)
                              , cur.row$Yes
                              , cur.row$No)
            next.Feature <- ifelse(as.double(dt.try[[cur.row$Feature]]) < as.double(cur.row$Split)
                                   , cur.row$Yes.Feature
                                   , cur.row$No.Feature)
            
            cur.id <- next.ID
            
            if(next.Feature == "Leaf"){
                gain <- tree[ID == next.ID]$Quality
            }
        }
        dt.featuresWt <- gain * (table(featuresOnPath) / length(featuresOnPath))
        featuresWt <- tbTodtWithMoreCols(dt.featuresWt, featureNames)
        return(list(gain = gain, featuresWt = featuresWt, featuresOnPath = featuresOnPath))
        
    }
    
    ls.tree <- list()
    ret <- list()
    for(i in 1:(max(dt.tree$Tree) + 1)){
        ls.tree[[i]] <- dt.tree[Tree == i - 1]
        ret[[i]] <- walkThroughTree(ls.tree[[i]], featureNames)
    }
    
    sumOnCommonCols <- function(d1, d2){
        ddply(rbind(d1, d2), .(featuresOnPath), summarize, N = sum(N))
    }
    
    ls.featuresWt <- lapply(ret, function(x)x$featuresWt)
    
    dt.featuresWt <- Reduce(sumOnCommonCols, ls.featuresWt)
    dt.featuresWt <- dt.featuresWt[order(-abs(dt.featuresWt$N)), ]
    setDT(dt.featuresWt)
    logit <- function(x){
        exp(x) / (1 + exp(x))
    }
    
    pred <- logit(sum(dt.featuresWt$N))
    
    importance <- xgb.importance(feature_names = featureNames, model = model)
    
    return(list(summary = ret
                , featuresWt = dt.featuresWt
                , pred = pred
                , importance = importance))
}
