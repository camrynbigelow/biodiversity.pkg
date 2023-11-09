#'Create a linear model regression summary for your data.
#'
#'Determine if your variables have a relationship. Summary will provide statistical values for residuals, estimates, standard error, t-value and p-values.
#'@param x independent value
#'@param y dependent value
#'@param data The dataset you read in (dataframe)
#'@return summary The summary of your linear model regression.
#'
#'@export

linear_model<- function(x, y, data){
  model_fit<-lm(paste(y, "~", x), data=data)
    summ<-summary(model_fit)
    return(summ)
}

#model_fit<-lm(paste(count~ salinity), data=fake)
#summ<-summary(model_fit)


