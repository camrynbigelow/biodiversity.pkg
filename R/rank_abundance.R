#'Show rank abundance by plotting your data in descending order by phylum
#'
#'Use your data that you read in and the phylogenetic rank you want to count.
#'
#'@param data The dataset you read in (dataframe)
#'@param x The rank of organisms you want to count. Default is phylum. (category)
#'@param xlabel Name of x-axis label, should be the same as x (String)
#'@return rank_plot The rank abundance plot (bar plot)
#'
#'@export

rank_abundance<-function(data, x, xlabel){
 sorted<-data %>% 
  count ({{x}}) %>% 
  arrange(desc(n))
 rank_plot<-ggplot(data=sorted, mapping=aes(x= fct_reorder({{x}},n, desc), y = n))+geom_col()
 rank_plot<-rank_plot+labs(x=xlabel, y="Count", title="Rank Abundance")
  return(rank_plot)
}

#This function works!!
#sorted<-surveys %>% 
 # count (genus) %>% 
#  arrange(desc(n))
#rank_plot<-ggplot(data=sorted, mapping=aes(x= fct_reorder(genus,n, desc), y = n))+geom_col()
#rank_plot<-rank_plot+labs(x="Genus", y="Count")


