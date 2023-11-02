#' Mapping abundance function
#' 
#' Plot abundances in a map. Need the following columns Actual.Lon, Actual.Lat, Subproject, Deployment.ID, and a species name column
#' @param data
#' @param species The species or common name column
#' @return abundanceMap The interactive map of abundances
#' 
#' @export

mapping_abundance<-function(data, species){
  speciesCount<-data %>%
    select({{species}}, Count, Subproject) %>%
    group_by({{species}}, Subproject) %>% 
    summarize(totalCount=sum(Count)) 
  ggplot(data, aes(x=Actual.Long, y=Actual.Lat, color = Subproject))+
    geom_point()+
    theme_bw()
  deployCount <- data %>% dplyr::select({{species}}, Count, Deployment.ID, Actual.Lon, Actual.Lat, Subproject) %>% 
    group_by(Deployment.ID, Actual.Lon, Actual.Lat, Subproject) %>% 
    summarize(totalCount = sum(Count))
  
  ggplot(deployCount)+
    geom_point(aes(x=Actual.Lon, y=Actual.Lat, size=totalCount, color=Subproject), alpha=0.5)+
    theme_bw()
  
  deployCount_sf <- deployCount %>% st_as_sf(coords = c("Actual.Lon", "Actual.Lat"), crs=4326)
  
  abundanceMap <- mapview(deployCount_sf, zcol="Subproject", cex="totalCount", layer.name = "Habitat Type")
  
  return(abundanceMap)
}



