
###########################################################################
sample_data <- function(dfid, dfall, folds){
  # Sampling data for cross-validation
  # Every rows of 1 uid belong to one subset
  # Args:
  # - dfid: list of unique uid 
  # - dfall: dataframe of preprocessed data
  # - folds: number of folds
  # Return data frame with sample id (sample id indicates which subset each row belongs to)
  
  dfid$sampleid <- sample(c(1:folds), nrow(dfid), replace = TRUE)
  
  n = nrow(dfall)
  dfall$sampleid <- 0
  for (i in c(1:n)){
    sampleid = dfid[dfid$uid==dfall[i,]$uid,]$sampleid
    dfall[i,]$sampleid <- sampleid
    print (i)
  }
  return (dfall)
}

pred_performance <- function(dfdata, pred, dependent_col){
  # Compute mean, std of residual
  # Args:
  # - dfdata: dataframe of input data
  # - pred: predicted data
  # - dependent_col
  setClass("pred_metric",
           representation(
             res = "vector",
             res_by_uid = "vector",
             meanres = "numeric",
             meanres_by_uid = "numeric",
             stdres = "numeric",
             stdres_by_uid = "numeric"
           )
  )
  
  Residual <- cbind(dfdata$uid, pred, dfdata[[dependent_col]])
  names(Residual) <- c("uid", "Predicted", "Actual")
  
  Residual$Difference <- (Residual$Actual - Residual$Predicted)
  res_peruid <- aggregate(Residual$Difference, list(Residual$uid), mean)
  
  res_peruid <- aggregate(Residual$Difference, list(Residual$uid), mean)
  names(res_peruid) <- c("uid", "res")
  metric <- new("pred_metric", res = Residual$Difference, res_by_uid = res_peruid$res, meanres=mean(Residual$Difference), meanres_by_uid = mean(res_peruid$res), stdres = sd(Residual$Difference), stdres_by_uid = sd(res_peruid$res))
  return (metric)
}

crossvalidate <- function(trainingset, cvset, testset, independent_col, dependent_col, model){
  # Measure performance when apply model to predict dependent variable based on independent variable
  # Args:
  # - trainingset: training set
  # - cvset: cv set
  # - testset: test set
  # - independent_col, dependent_col, model
  # Return measurements
  
  setClass("pred_metric",
           representation(
             res = "vector",
             res_by_uid = "vector",
             meanres = "numeric",
             meanres_by_uid = "numeric",
             stdres = "numeric",
             stdres_by_uid = "numeric"
           )
  )
  
  setClass("crossvalidate_metric",
           representation(
             train = "pred_metric",
             cv = "pred_metric",
             test = "pred_metric"))
  
  # predict data on test set
  cvpred <- as.data.frame(predict(model, cvset[c(independent_col,dependent_col)]))
  testpred <- as.data.frame(predict(model, testset[c(independent_col,dependent_col)]))
  trainpred <- as.data.frame(predict(model, trainingset[c(independent_col,dependent_col)]))
  
  
  cv_metric <- pred_perforance(cvset, cvpred, dependent_col)
  train_metric <- pred_perforance(trainingset, trainpred, dependent_col)
  test_metric <- pred_perforance(testset, testpred, dependent_col)
  
  cv_result <- new("crossvalidate_metric", train = train_metric, cv = cv_metric, test = test_metric)
  
  return (cv_result)
}

config_factor_col <- function (subset, data){
  # Configure fator variable of subset of data to be consistent with the data
  # Args:
  # - Subset, data
  # Return: subset with fator variable configured
  
  subset$activity <- factor(subset$activity, levels = levels(data$activity))
  subset$sex <- factor(subset$sex, levels = levels(data$sex))
  subset$inj <- factor(subset$inj, levels = levels(data$inj))
  return (subset)
}


logcv <- function(metric, fpath, fold){
  # Write performance measurement (metric) to file
  # Args:
  # - metric
  # - fpath
  # - fold
  
  setClass("pred_metric",
           representation(
             res = "vector",
             res_by_uid = "vector",
             meanres = "numeric",
             meanres_by_uid = "numeric",
             stdres = "numeric",
             stdres_by_uid = "numeric"
           )
  )
  
  setClass("crossvalidate_metric",
           representation(
             train = "pred_metric",
             cv = "pred_metric",
             test = "pred_metric"))
  
  line1 <- paste("train", metric@train@meanres, metric@train@meanres_by_uid, metric@train@stdres, metric@train@stdres_by_uid)
  line2 <- paste("cv", metric@cv@meanres, metric@cv@meanres_by_uid, metric@cv@stdres, metric@cv@stdres_by_uid)
  line3 <- paste("test", metric@test@meanres, metric@test@meanres_by_uid, metric@test@stdres, metric@test@stdres_by_uid)
  
  print ("log cross validation")
  write(paste("fold",fold) , file=fpath, append=TRUE)
  write(line1, file=fpath, append=TRUE)
  write(line2, file=fpath, append=TRUE)
  write(line3, file=fpath, append=TRUE)
  
}


