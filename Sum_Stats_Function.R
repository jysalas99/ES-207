#my function that computes summary statistics over 
#A given variable
library(moments)

sum.data <- function(x,y) {
  data <- x %>%              #Filter out NA values; avoids NA.Rm repetition 
    filter(x[y] != "NA")
  results <- c(MEAN = sum(data[y])/nrow(data[y]), MEDIAN = median(data[[y]]),
               STDEV = sd(data[[y]]), InterQ_Range = IQR(data[[y]]), 
               SKWNS = skewness(data[[y]]))
  return(results)
}


