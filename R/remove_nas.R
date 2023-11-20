#' Clean your data by removing NAs
#' 
#' Use your data that you read in to clean data
#' Returns your data without NAs
#' Save as a variable to be able to use this clean data
#' @param data The data set you read in to remove NAs (dataframe)
#' @return clean The "cleaned" dataset without NAs (dataframe)
#' 
#' @export

remove_nas<-function(data){
  clean<-data %>% 
    na.omit(data)
  if(all(is.na(clean))){
    print("NAs remain!")
  }else{
  return(clean)
}
}
