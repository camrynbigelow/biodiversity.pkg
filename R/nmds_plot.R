#'Non-metric Multidimensional Scale plot
#'
#'Creates a ggplot based on non-metric multidimensional scale to show similarity between groups. 
#'@param data The data you cleaned (dataframe)
#'@param Species Your species name column
#'@param Sector Your sector column
#'@return plot Your nmds plot using ggplot
#'
#'@export

nmds_plot<-function(data, Species, Sector){
  speciesCount <- data %>% 
    select(Species, Count, Sector) %>% 
    group_by(Species, Sector) %>% 
    summarize(totalCount = sum(Count)) %>%
    spread(Species, totalCount, fill = 0) 
  speciesMatrix <- data.matrix(speciesCount, rownames.force = unique(data$Sector))
  NMDS <- metaMDS(speciesMatrix)
  data.scores = as.data.frame(scores(NMDS)$sites)
  names <- data.scores  %>%
    mutate(Location  = speciesCount$Sector)
  plot <- ggplot(names, aes(x = NMDS1, y = NMDS2)) +
    geom_point(size = 4, aes(colour = Location))
  return(plot)
}

#nmds works, but gives warning about insufficient data
#can force it outside of function but not within, ie got a plot during test