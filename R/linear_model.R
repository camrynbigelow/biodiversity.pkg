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
  model_fit<-lm({{y}}~{{x}}, data) %>% 
    summary<-summary(model_fit)
    return(summary)
}

#linear_model(data=surveys, x=sex, y=weight)
#^ object not found

#model_fit<-lm(CPD~Sex, data=GM)

#summary(model_fit)

