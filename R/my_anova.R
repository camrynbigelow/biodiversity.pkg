#'Create an anova summary for your data
#'
#'Compare relationships with a categorical independent variable using ANOVA
#'@param data The dataset you read in
#'@param x The independent variable
#'@param y The dependent variable
#'@return summary The ANOVA summary
#'
#'@export

my_anova<-function(data, x, y){

 model_fit<-lm({{y}}~{{x}}, data)
  anova_model_fit<-aov(model_fit)
  summary<-summary(anova_model_fit)
  return(summary)
  
}

#my_anova(data=GM, Sex, CPD)
#^ object not found