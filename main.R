
############################################## Preprocessing demographic data ######################################
setwd ("...")

source("./R/preprocessing.R")
source("./R/modeling.R") 
source("./R/crossvalidate.R")

lmlog <- "./data/lm.log"
aiclog <- "./data/aic.log"
rflog <- "./data/rf.log"
fprofile <- "./data/WISDM_at_v2.0_demographics.txt"
facc <- "./data/WISDM_at_v2.0_raw.txt"

create_demographic_data <- function(fprofile){
  # Create data frame about demographics
  # - Read data from file
  # - Convert sex and inj to factor
  # - Remove NA
  # - Remove duplicated rows
  # - Remove outliers corresponding to height and weight
  # Arg: 
  # - fprofile: txt file with sep = ","
  
  profile = read.csv(fprofile, stringsAsFactors = F,header = T, sep=",")
  attprofile <- c("uid", "height", "sex", "age", "weight", "inj")
  names(profile) <- attprofile
  
  profile$sex <- as.factor(profile$sex)
  profile$inj <- as.factor(profile$inj)
  profile <- na.omit(profile)
  dfprofile <- profile[!duplicated(profile), ]
  dfprofile <- remove_outlier(dfprofile, c("height", "weight"))
  
  return (dfprofile)
}

############################################## Preprocessing acc data ##############################################
create_acc_data <- function(facc){
  # Create data acc 
  # - Read data from file
  # - Convert Activity to factor
  # - Remove NA
  # - Remove duplicated rows
  # - Remove outliers of each activity
  # Arg:
  # - facc: text file with sep = "," and ";" at the end of lines
  
  acc <- data.frame(read.csv(facc, stringsAsFactors = F, header = T, sep=c(",",";")))
  attacc <- c("uid", "activity", "time", "xacc", "yacc", "zacc")
  names(acc) <- attacc
  acc$activity <- as.factor(acc$activity)
  acc$time <- as.numeric(acc$time)
  acc <- na.omit(acc)
  acc$zacc <- apply(data.frame(acc$zacc), 1, function(x) return (substr(x,1,nchar(x)-1)))
  
  acc$xacc <- as.numeric(acc$xacc)
  acc$yacc <- as.numeric(acc$yacc)
  acc$zacc <- as.numeric(acc$zacc)
  
  act <- levels(acc$activity)
  dfacc = data.frame()
  
  for (a in act){
    dftmp <- remove_outlier(acc[acc$activity==a,], c("xacc","yacc","zacc"))
    dfacc <- rbind(dfacc,dftmp)
  }
  
  dfacc <- dfacc[!duplicated(dfacc), ]
  return (dfacc)
}

################################################ aggregate data in 10s ################################################

aggregate_acc <- function(dfacc, nagr){
  # Aggregate 200 continuous rows to produce new features
  # Args:
  # - dfacc: data frame of accel data
  # - nagr: Number of continuous rows
  # Return: Data frame of accel with new feature
  
  newdfacc <- data.frame()
  i = 1
  nbin = 15
  nrow = nrow(dfacc)
  
  minx = min(dfacc["xacc"])
  maxx = max(dfacc["xacc"])
  
  miny = min(dfacc["yacc"])
  maxy = max(dfacc["yacc"])
  
  minz = min(dfacc["zacc"])
  maxz = max(dfacc["zacc"])
  
  while(i <= nrow){
    dftmp = data.frame()
    count = 1
    dftmp <- rbind(dftmp, dfacc[i,])
    i=i+1
    
    if(i <= nrow){
      while((i <= nrow) && (count < 200) && (dfacc$uid[i-1] == dfacc$uid[i]) && (dfacc$activity[i-1] == dfacc$activity[i])){
        print (i)
        dftmp <- rbind(dftmp, dfacc[i,])
        count = count+ 1
        i = i+1
      }
      if(nrow(dftmp)>1){
        mean <- apply(dftmp[c("xacc","yacc","zacc")], 2, mean)
        std <- apply(dftmp[c("xacc","yacc","zacc")], 2, sd)
        max <- apply(dftmp[c("xacc","yacc","zacc")], 2, max)
        min <- apply(dftmp[c("xacc","yacc","zacc")], 2, min)
        
        # Average Absolute Difference
        meandiff <- apply(dftmp[c("xacc","yacc","zacc")], 2, function(x) mean(abs(x[1:length(x)-1]-x[2:length(x)])))
        
        # Average Resultant Acceleration
        avgresultant <- mean(apply(dftmp[c("xacc","yacc","zacc")], 1, function(x) sqrt(mean(x[1]*x[1] + x[2]*x[2] + x[3]*x[3]))))
        
        # Time Between Peaks
        timepeaks <- compute_time_peaks(dftmp)
        
        # Binned Distribution
        freq_xacc <- binned_dist(dftmp[["xacc"]], minx, maxx, nbin)
        freq_yacc <- binned_dist(dftmp[["yacc"]], miny, maxy, nbin)
        freq_zacc <- binned_dist(dftmp[["zacc"]], minz, maxz, nbin)
        
        newrow <- c(dftmp$uid[count-1], dftmp$activity[count-1], mean, std, max, min, meandiff, avgresultant, timepeaks, freq_xacc, freq_yacc, freq_zacc)
        newdfacc <- rbind(newdfacc, newrow)
      }
    }
  }
  
  ibin <- c(1:nbin)
  freq_xacc_names <- apply(as.array(as.character(c(1:nbin))), 1, function(x) paste0("xacc",x))
  freq_yacc_names <- apply(as.array(as.character(c(1:nbin))), 1, function(x) paste0("yacc",x))
  freq_zacc_names <- apply(as.array(as.character(c(1:nbin))), 1, function(x) paste0("zacc",x))
  
  names(newdfacc) <- c("uid", "activity", "meanx", "meany", "meanz", "stdx", "stdy", "stdz", "maxx", "maxy", "maxz", "minx", "miny", "minz", "meandiffx", "meandiffy", "meandiffz", "avgresultant", "timepeaks", freq_xacc_names, freq_yacc_names, freq_zacc_names)
  return (newdfacc)
}

main <- function(){
  # Read data
  dfprofile <- create_demographic_data(fprofile)
  dfacc <- create_acc_data(facc)
  newdfacc <- aggregate_acc(dfacc, 200)
  df_all_feature <- merge(newdfacc, dfprofile, by=("uid"))
  na.omit(df_all_feature)
  #write.csv(newdfacc, file = "../data/accdata_ver2.csv")
  
  nbin = 15
  ibin <- c(1:nbin)
  freq_xacc_names <- apply(as.array(as.character(c(1:nbin))), 1, function(x) paste0("xacc",x))
  freq_yacc_names <- apply(as.array(as.character(c(1:nbin))), 1, function(x) paste0("yacc",x))
  freq_zacc_names <- apply(as.array(as.character(c(1:nbin))), 1, function(x) paste0("zacc",x))
  
  # MOdelling
  # featuresset1 = c("activity", "meanx", "meany", "meanz", "stdx", "stdy", "stdz", "maxx", "maxy", "maxz", "minx", "miny", "minz", "meandiffx", "meandiffy", "meandiffz", "avgresultant", "timepeaks", freq_xacc_names, freq_yacc_names, freq_zacc_names, "height", "sex", "age", "inj")
  featuresset2 = c("activity", "meanx", "meany", "meanz", "stdx", "stdy", "stdz", "maxx", "maxy", "maxz", "minx", "miny", "minz", "meandiffx", "meandiffy", "meandiffz", "avgresultant", "timepeaks", "height", "sex", "age", "inj")
  modeling(data,featuresset2)
}


main()
