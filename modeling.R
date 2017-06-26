#library(plyr)
setwd("D:\\Programs\\Dropbox\\R_project\\Misfit")
fdata = "./data/profile_acc.csv"

lmlog = "./data/lm.log"
aiclog = "./data/aic.log"
rflog = "./data/rf.log"


read_data <- function(fdata){
  #-------------------------------------------------------------------------------------------------
  # Read data file including demographics and accel data
  # - Convert sex, inj, activity to categorical variable
  # Args:
  # - Path of data file
  # Return data frame: uid, new feature of accel, demographics
  #-------------------------------------------------------------------------------------------------
  
  data <- data.frame(read.csv(fdata, stringsAsFactors = F, header = T, sep=c(",",";")))
  nbin=15
  freq_xacc_names <- apply(as.array(as.character(c(1:nbin))), 1, function(x) paste0("xacc",x))
  freq_yacc_names <- apply(as.array(as.character(c(1:nbin))), 1, function(x) paste0("yacc",x))
  freq_zacc_names <- apply(as.array(as.character(c(1:nbin))), 1, function(x) paste0("zacc",x))
  att <- c("uid", "activity", "meanx", "meany", "meanz", "stdx", "stdy", "stdz", "maxx", "maxy", "maxz", "minx", "miny", "minz", "meandiffx", "meandiffy", "meandiffz", "avgresultant", "timepeaks", freq_xacc_names, freq_yacc_names, freq_zacc_names, "height", "sex", "age", "weight", "inj")
  names(data) <- att
  
  data$sex <- as.factor(data$sex)
  data$activity <- as.factor(data$activity)
  data$inj <- as.factor(data$inj)
  
  data <- na.omit(data)
  return (data)
}


modeling <- function(data, features){
  #-------------------------------------------------------------------------------------------------
  # Apply model linear regression, aic method for linear regression, random forest
  # Use cross-validation 5 folds
  # Assess std of error on training, validate, test set
  # - Args: data frame: uid, demographics, accel features
  # Return: Write perpormance measure to log file 
  # - Measurements: Residual mean, Residual std, Residual (by uid) mean, Residual (by uid) std
  #------------------------------------ Create sample ----------------------------------------------
  # sampling 
  folds = 5
  
  dfid <- data.frame(unique(data$uid))
  names(dfid) <- "uid"
  tmp <- sample_data(dfid, data, folds)
  data <- tmp
  
  #------------------------------------ setting attributes for --------------------------------------
  # rescale height and age feature
  data$height <- data$height/100
  data$age <- data$age/10
  
  nbin=15
  freq_xacc_names <- apply(as.array(as.character(c(1:nbin))), 1, function(x) paste0("xacc",x))
  freq_yacc_names <- apply(as.array(as.character(c(1:nbin))), 1, function(x) paste0("yacc",x))
  freq_zacc_names <- apply(as.array(as.character(c(1:nbin))), 1, function(x) paste0("zacc",x))
  
  independent_col = features
  #independent_col = c("activity", "meanx", "meany", "meanz", "stdx", "stdy", "stdz", "maxx", "maxy", "maxz", "minx", "miny", "minz", "meandiffx", "meandiffy", "meandiffz", "avgresultant", "timepeaks", freq_xacc_names, freq_yacc_names, freq_zacc_names, "height", "sex", "age", "inj") # 
  dependent_col = "weight"
  
  
  dfstdlm = data.frame()
  dfstdrf = data.frame()
  dfstdaic = data.frame()
  list = c(1:folds)
  h = folds-1
  #par(mfrow = c(2,2))
  
  for (i in c(1:h)){
    # remove rows with id i from dataframe to create training set
    
    j = i+1
    trainingset <- subset(data, sampleid %in% list[-c(i,j)])
    cvset <- subset(data, sampleid==i)
    testset <- subset(data, sampleid==j)
    
    if(length(unique(trainingset$sex)) == 2 && length(unique(trainingset$inj)) == 2 && length(unique(trainingset$activity)) == 6){
      trainingset <- config_factor_col(trainingset, data)
      cvset <- config_factor_col(cvset, data)
      testset <- config_factor_col(testset, data)
      print ("in if")#
     # Linear Regression
     lmmodel <- lm(weight ~ ., data = trainingset[c(independent_col,dependent_col)])
     lmmodel.metric <- crossvalidate(trainingset, cvset, testset, independent_col, dependent_col, lmmodel)
     logcv(lmmodel.metric, lmlog, i)
     
     
      library(MASS)
      aic <- stepAIC(lmmodel, direction="both")
      aic.metric <- crossvalidate(trainingset, cvset, testset, independent_col, dependent_col, aic)
      logcv(aic.metric, aiclog, i)      
      
      # Random Forest
      library(randomForest)
      forestmodel <- randomForest(weight ~ ., data = trainingset[c(independent_col,dependent_col)], maxnodes=20)
      forest.metric <- crossvalidate(trainingset, cvset, testset,independent_col, dependent_col, forestmodel)
      logcv(forest.metric, rflog, i)
    }
  }  
}

#feature = c("activity", "meanx", "meany", "meanz", "stdx", "stdy", "stdz", "maxx", "maxy", "maxz", "minx", "miny", "minz", "meandiffx", "meandiffy", "meandiffz", "avgresultant", "timepeaks", freq_xacc_names, freq_yacc_names, freq_zacc_names, "height", "sex", "age", "inj")
#data = read_data(fdata)
#modeling(data,feature)
