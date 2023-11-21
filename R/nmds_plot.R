#'Non-metric Multidimensional Scale prep
#'
#'Prepares data for NMDS plotting.
#'@param data The data you cleaned (dataframe)
#'@param Species Your species name column
#'@param Sector Your sector column
#'@return speciesMatrix 
#'
#'@export

nmds_plot<-function(data, Species, Sector){
  if(!is.numeric(data$Sector)){
    print("Sector is not numeric")
    return(NULL)
  } else {
  speciesCount <- data %>% 
    select(Species, Count, Sector) %>% 
    group_by(Species, Sector) %>% 
    summarize(totalCount = sum(Count)) %>%
    spread(Species, totalCount, fill = 0) 
  speciesMatrix <- data.matrix(speciesCount, rownames.force = unique(data$Sector))
  return(speciesMatrix)
  }
}


    speciesCount <- invert %>% 
      select(Species, Count, Sector) %>% 
      group_by(Species, Sector) %>% 
      summarize(totalCount = sum(Count)) %>%
      spread(Species, totalCount, fill = 0) 
    speciesMatrix <- data.matrix(speciesCount, rownames.force = unique(invert$Sector))
    NMDS <- metaMDS(speciesMatrix)
    data.scores = as.data.frame(scores(NMDS)$sites)
    names <- data.scores  %>%
      mutate(Location  = speciesCount$Sector)
    plot <- ggplot(names, aes(x = NMDS1, y = NMDS2)) +
      geom_point(size = 4, aes(colour = Location))

  

