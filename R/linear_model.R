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
  model_fit<-lm(eval(as.name(y)) ~ eval(as.name(x)), data=data)
    summ<-summary(model_fit)
    if (!is.numeric(x))
      {print("Predictor value is not numeric!")
    }else{
    return(summ)
    }
}

model_fit<-lm(paste(count~ salinity), data=fake)
#summ<-summary(model_fit)

#y parameter not found!
