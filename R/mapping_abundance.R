#' Mapping abundance function
#' 
#' Plot abundances in a map. Need the following packages: tidyverse, sf, mapview, and vegan
#' @param data
#' @param Species The species or common name column
#' @param Lat The column with Lat coordinates
#' @param Long The column with Long coordinates
#' @param Sector From Sectors 1-6
#' @return abundanceMap The interactive map of abundances
#' 
#' @export

mapping_abundance<-function(data, Species, Lat, Long, Sector){
  speciesCount<-fake %>%
    select(Species, count, Sector)%>%
    group_by(Species, Sector) %>% 
    summarize(totalCount=sum(count)) 
  ggplot(data, aes(x=Long, y=Lat, color = Sector))+
    geom_point()+
    theme_bw()
  deployCount <- fake %>% dplyr::select(Species, count, Long, Lat, Sector) %>% 
    group_by(Long, Lat, Sector) %>% 
    summarize(totalCount = sum(count))
  
  ggplot(deployCount)+
    geom_point(aes(x=Long, y=latitiude, size=totalCount, color=Sector), alpha=0.5)+
    theme_bw()
  
  deployCount_sf <- deployCount %>% st_as_sf(coords = c("Long", "Lat"), crs=4326)
  
  abundanceMap <- mapview(deployCount_sf, zcol="Sector", cex="totalCount", layer.name = "Sector Name")
  
  return(abundanceMap)
}
#mapping_abundance(fake, Species, Lat, Long, Sector)

#Need help making Sector a character

