library(tidyr)
#library(BayesFactor)

##functions for calculating and plotting adjusted CIs and SEs for repeated measures design 
#1. for accuracy
# m is condition number overall
acc_adj_ci <- function(df, con1, subject, ci_threshold, m, group){
  if (missing(group)){
    # performance by subject
    bySub <- df %>% group_by(subject) %>%
      summarise(
        meanRT = mean(rt, na.rm = TRUE),
        sdRT = sd(rt, na.rm = TRUE),
        total = length(correct),
        numCorrect = sum(correct),
        numIncorrect = total - numCorrect,
        errorRate = numIncorrect/total
      )
  
    # by condition and sub
    accByConSub <- df %>% group_by({{con1}}, subject) %>% 
      summarise(
        numCorrect = sum(correct),
        numIncorrect = length(correct)-sum(correct),
        total = length(correct),
        errorRate = (length(correct)-sum(correct))/length(correct)
        )
    # take out NA condition
    accByConSub <- drop_na(accByConSub)
  
    #remove between subject variance
    bySub <- bySub %>% mutate(grandMean = mean(errorRate),
                            diff = grandMean - errorRate)
  
    accByConSub <- accByConSub %>% 
      mutate(adj = errorRate + bySub$diff)
  
    accByCon <- accByConSub %>% group_by({{con1}}) %>%
      summarise(
        N = n_distinct(subject),
        errorRate = mean(adj),
        SD = sd(adj, na.rm = TRUE),
        SE = SD/sqrt(N),
        ci_original = qnorm(ci_threshold)*SE,     #0.975
        ci = ci_original*sqrt(m/(m-1))           #adjusted based on Morey(2008)    
      )
    return(accByCon)
  } else {
    # performance by subject
    bySub <- df %>% group_by(group, subject) %>%
      summarise(
        meanRT = mean(rt, na.rm = TRUE),
        sdRT = sd(rt, na.rm = TRUE),
        total = length(correct),
        numCorrect = sum(correct),
        numIncorrect = total - numCorrect,
        errorRate = numIncorrect/total
      )
    
    # by condition and sub
    accByConSub <- df %>% group_by({{con1}},group,subject) %>% 
      summarise(
        numCorrect = sum(correct),
        numIncorrect = length(correct)-sum(correct),
        total = length(correct),
        errorRate = (length(correct)-sum(correct))/length(correct)
      )
    # take out NA condition
    accByConSub <- drop_na(accByConSub)
    
    #remove between subject variance
    bySub <- bySub %>% group_by(group) %>%
      mutate(grandMean = mean(errorRate),
             diff = grandMean - errorRate)
    
    accByConSub <- accByConSub %>% group_by({{con1}}) %>%
      mutate(adj = errorRate + bySub$diff)
    
    accByCon <- accByConSub %>% group_by({{con1}}, group) %>%
      summarise(
        N = n_distinct(subject),
        errorRate = mean(adj),
        SD = sd(adj, na.rm = TRUE),
        SE = SD/sqrt(N),
        ci_original = qnorm(ci_threshold)*SE,     #0.975
        ci = ci_original*sqrt(m/(m-1))           #adjusted based on Morey(2008)    
      )
    return(accByCon)
  }
  
}



#2. for rts
# m is condition number overall
rt_adj_ci <- function(df, con1, subject, RT, ci_threshold, m, group){  
  if(missing(group)){
    rtBySub <- df %>% group_by(subject)%>% 
      summarise(
        N    = length({{RT}}),
        meanRT = mean({{RT}}, na.rm = TRUE),
        sdRT = sd({{RT}}, na.rm = TRUE),
        seRT = sdRT / sqrt(N))
    
    rtByConSub <- df %>% group_by({{con1}},subject) %>% 
      summarise(
        N    = length({{RT}}),
        meanRT = mean({{RT}}, na.rm = TRUE),
        sdRT = sd({{RT}}, na.rm = TRUE),
        seRT = sdRT / sqrt(N))
    
    rtByConSub <- drop_na(rtByConSub)
    
    #remove between subject variance 
    rtBySub <- rtBySub %>% mutate(grandMean = mean(meanRT),
                                    diff = grandMean - meanRT)
    rtByConSub <- rtByConSub %>% mutate(adj = meanRT + rtBySub$diff)
    
    #get rt by condition
    rtByCon <- rtByConSub %>% group_by({{con1}}) %>% 
      summarise(
        N    = length(adj),
        meanRT = mean(adj, na.rm = TRUE),
        sdRT = sd(adj, na.rm = TRUE),
        seRT = sdRT / sqrt(N),
        ci_original = qnorm(ci_threshold)*seRT, 
        ci = ci_original*sqrt(m/(m-1)) 
      )
    return(rtByCon)
  } else{
    rtBySub <- df %>% group_by(group, subject)%>% 
      summarise(
        N    = length({{RT}}),
        meanRT = mean({{RT}}, na.rm = TRUE),
        sdRT = sd({{RT}}, na.rm = TRUE),
        seRT = sdRT / sqrt(N))
    
    rtByConSub <- df %>% group_by({{con1}}, group, subject) %>% 
      summarise(
        N    = length({{RT}}),
        meanRT = mean({{RT}}, na.rm = TRUE),
        sdRT = sd({{RT}}, na.rm = TRUE),
        seRT = sdRT / sqrt(N))
    
    rtByConSub <- drop_na(rtByConSub)
    
    #remove between subject variance 
    rtBySub <- rtBySub %>% group_by(group) %>%
      mutate(grandMean = mean(meanRT),
             diff = grandMean - meanRT)
    
    rtByConSub <- rtByConSub %>% group_by({{con1}}) %>%
      mutate(adj = meanRT + rtBySub$diff)
    
    #get rt by condition
    rtByCon <- rtByConSub %>% group_by({{con1}}, group) %>% 
      summarise(
        N    = length(adj),
        meanRT = mean(adj, na.rm = TRUE),
        sdRT = sd(adj, na.rm = TRUE),
        seRT = sdRT / sqrt(N),
        ci_original = qnorm(ci_threshold)*seRT, 
        ci = ci_original*sqrt(m/(m-1))
      )
    return(rtByCon)
  }
}
  

#3. for ratings
rating_adj_ci <- function(df, con1, subject, ci_threshold, m){
  ratingBySub <- df %>% group_by(subject) %>% 
    summarise(
                         meanScore = mean(score, na.rm = TRUE),
                         sdScore = sd(score, na.rm = TRUE),
                         seScore = sdScore / sqrt(length(score))
                         )
  
  ratingBySubCon <- df %>% group_by({{con1}},subject) %>% 
    summarise(
                            meanScore = mean(score, na.rm = TRUE),
                            sdScore = sd(score, na.rm = TRUE),
                            seScore = sdScore / sqrt(length(score))
                            )
  
  #remove between subject variance
  ratingBySub$grandMean <- mean(ratingBySub$meanScore)
  ratingBySub$diff <- ratingBySub$grandMean - ratingBySub$meanScore 
  ratingBySubCon$adj <- ratingBySub$diff + ratingBySubCon$meanScore
  
  
  ratingByCondition <- ratingBySubCon %>% group_by({{con1}}) %>% 
    summarise(
                               N = length(unique(subject)),
                               meanScore = mean(adj),
                               SD = sd(adj, na.rm = TRUE),
                               SE = SD/sqrt(N),
                               ci_original = qnorm(ci_threshold)*SE, 
                               ci = ci_original*sqrt(m/(m-1))
              )
  return(ratingByCondition)
}



### functions for transformation between normal and logit scale
inv.logit <- function(x){ exp(x)/(1+exp(x)) }
logit <- function(p){log(p/(1-p))}



### function for simulating sample size
# reportBF <- function(x, digits){
#   # This function extracts a BF from a BFBayesFactor object and rounds it
#   if(missing(digits)){
#     digits <- 2
#   }
#   round(as.numeric(as.vector(x)), digits)
# }
# 
# sim_sampleSize <- function(minN, batchSize, crit1, crit2, nIter, d, Seed){
#   # # Parameters
#   # minN      <- 10   # Staring sample size
#   # batchSize <- 5    # How often is the BF checked
#   # crit1     <- 10   # Upper criterion: evidence for H1
#   # crit2     <- 1/6  # Lower criterion: evidence for H0 
#   # nIter     <- 100  # Number of iterations for simulation 
#   # d         <- 0.652    # Effect size
#   set.seed(Seed)
#   for(i in 1:nIter){
#     # First iteration
#     data <- rnorm(minN, d, 1)
#     n    <- minN
#     bf   <- reportBF(ttestBF(data))
#     
#     # Loop
#     while(bf[length(bf)] < crit1 & bf[length(bf)] > crit2){
#       data <- c(data, rnorm(minN, d, 1))
#       bf[length(bf) + 1] <- reportBF(ttestBF(data))
#       n[length(n) + 1] <- n[length(n)] + batchSize
#     }
#     
#     if(i == 1){
#       df <- data.frame(id = rep(i, length(bf)), n = n, bf = bf)
#     } else {
#       df <- rbind(df, data.frame(id =  rep(i, length(bf)), n = n, bf = bf))
#     }
#   }
#   maxN <- max(df$n)
#   medianN <- median(df$n)
#   quantile <- as.vector(quantile(df$n, p=0.95))
#   return(paste0('max N: ', maxN, '; median N: ', medianN, '; 95%: ', quantile))
# }