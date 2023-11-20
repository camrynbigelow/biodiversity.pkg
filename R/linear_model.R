#'Create a linear model regression summary for your data.
#'
#'Determine if your variables have a relationship. Summary will provide statistical values for residuals, estimates, standard error, t-value and p-values.
#'@param independent_cols independent columns
#'@param dependent dependent column
#'@param data The dataset you read in (dataframe)
#'@return summary The summary of your linear model regression.
#'
#'@export

linear_model<-function(data, dependent, independent){
  if(!is.numeric(data[[dependent]])){
    print("Dependent variable is not numeric")
    return(NULL)
  } else {
    formula<-paste(dependent, "~", independent)
    model_fit<-lm(formula, data=data)
    return(summary(model_fit))
  }
}
