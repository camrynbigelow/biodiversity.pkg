#'Non-metric Multidimensional Scale plot
#'
#'Creates a ggplot based on non-metric multidimensional scale to show similarity between groups. 
#'@param data The data you cleaned (dataframe)
#'@param x Your species name column
#'@param y Your site name
#'@return plot Your nmds plot using ggplot
#'
#'@export

nmds_plot<-function(data,x,y){
  speciesCount <- data %>% 
  select({{x}}, Count, {{y}}) %>% 
  group_by({{x}}, {{y}}) %>% 
  summarize(totalCount = sum(Count)) %>%
  spread({{x}}, totalCount, fill = 0) 
speciesMatrix <- data.matrix(speciesCount, rownames.force = unique(data${{Y}}))
nmds_plot <- metaMDS(speciesMatrix)
data.scores = as.data.frame(scores(eDNA_NMDS)$sites)
names <- data.scores  %>%
  mutate(Location  = speciesCount${{y}})
plot <- ggplot(names, aes(x = NMDS1, y = NMDS2)) +
  geom_point(size = 4, aes(colour = Location))
return(plot)
  }

Test- worked!
speciesCount <- cam %>% 
  select(Common.Name, Count, Subproject) %>% 
  group_by(Common.Name, Subproject) %>% 
  summarize(totalCount = sum(Count)) %>%
  spread(Common.Name, totalCount, fill = 0) 
speciesMatrix <- data.matrix(speciesCount, rownames.force = unique(cam$Subproject))
eDNA_NMDS <- metaMDS(speciesMatrix)
data.scores = as.data.frame(scores(eDNA_NMDS)$sites)
names <- data.scores  %>%
  mutate(Location  = speciesCount$Subproject)
plot <- ggplot(names, aes(x = NMDS1, y = NMDS2)) +
  geom_point(size = 4, aes(colour = Location))
