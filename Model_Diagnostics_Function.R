###Function that calculates common statistics
###for model diagnostics.

#Function inputs: data, observation column name, prediction column name
model_diag <- function(df, obs, pred) { 
 
  difference <- df[ , pred]-df[ , obs] #difference between predictions and observations
  
  Mean_AE <- sum(abs(difference))/nrow(difference) #Calc for Mean Absolute Error
  
  MeanSE <- sum(difference**2)/nrow(difference)
  
  RootMean_SE <- sqrt(MeanSE) #Calc for Root Mean Square Error
  
  Pbias <- sum(difference)/sum(df[ , obs]) * 100 #Calc for Percent Bias
  
  results <- data.frame(Mean_AE, MeanSE, RootMean_SE,Pbias) #aggregate results to a dataframe
  
  print(results)
}
