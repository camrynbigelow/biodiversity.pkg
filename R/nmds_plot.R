#'Non-metric Multidimensional Scale plot
#'
#'Creates a ggplot based on non-metric multidimensional scale to show similarity between groups. 
#'@param data The data you cleaned (dataframe)
#'@param Species Your species name column
#'@param y parameter or sector
#'@return plot Your nmds plot using ggplot
#'
#'@export

nmds_plot<-function(data, Species, y){
  speciesCount <- data %>% 
  select(Species, count, {{y}}) %>% 
  group_by(Species, {{y}}) %>% 
  summarize(totalCount = sum(count)) %>%
  spread(Species, totalCount, fill = 0) 
speciesMatrix <- data.matrix(speciesCount, rownames.force = unique(paste(data,y)))
nmds_plot <- metaMDS(speciesMatrix)
data.scores = as.data.frame(scores(paste(nmds_plot,y)))
names <- data.scores  %>%
  mutate(Location  = paste(speciesCount, {{y}}))
plot <- ggplot(names, aes(x = NMDS1, y = NMDS2)) +
  geom_point(size = 4, aes(colour = Location))
return(plot)
  }


#Test- worked!
#speciesCount <- cam %>% 
 # select(Common.Name, Count, Subproject) %>% 
  #group_by(Common.Name, Subproject) %>% 
  #summarize(totalCount = sum(Count)) %>%
  #spread(Common.Name, totalCount, fill = 0) 
#speciesMatrix <- data.matrix(speciesCount, rownames.force = unique(cam$Subproject))
#eDNA_NMDS <- metaMDS(speciesMatrix)
#data.scores = as.data.frame(scores(eDNA_NMDS)$sites)
#names <- data.scores  %>%
#  mutate(Location  = speciesCount$Subproject)
#plot <- ggplot(names, aes(x = NMDS1, y = NMDS2)) +
 # geom_point(size = 4, aes(colour = Location))