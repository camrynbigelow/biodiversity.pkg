#'Create a linear model regression summary for your data.
#'
#'Determine if your variables have a relationship. Summary will provide statistical values for residuals, estimates, standard error, t-value and p-values.
#'@param independent_cols independent columns
#'@param dependent dependent column
#'@param data The dataset you read in (dataframe)
#'@return summary The summary of your linear model regression.
#'
#'@export

linear_model<-function(data, dependent, independent_cols){
 data %>% 
  if(is.numeric({{dependent}})){
    summ<- data %>% 
      select(a=quo_name(dependent), starts_with(independent_cols)) %>% 
      lm(a~., data=.) %>% 
      summary()
    return(summ)
 } else {
   print("Dependent variable needs to be numeric!")
 }
}
