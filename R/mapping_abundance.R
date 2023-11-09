#' Mapping abundance function
#' 
#' Plot abundances in a map. Need the following packages: tidyverse, sf, mapview, and vegan
#' @param data
#' @param species.name The species or common name column
#' @param latitude The column with latitude coordinates
#' @param longitude The column with longitude coordinates
#' @param sector From sectors 1-6
#' @return abundanceMap The interactive map of abundances
#' 
#' @export

mapping_abundance<-function(data, species.name, latitude, longitude, sector){
  speciesCount<-fake %>%
    select(species.name, count, sector)%>%
    group_by(species.name, sector) %>% 
    summarize(totalCount=sum(count)) 
  ggplot(data, aes(x=longitude, y=latitude, color = sector))+
    geom_point()+
    theme_bw()
  deployCount <- fake %>% dplyr::select(species.name, count, longitude, latitude, sector) %>% 
    group_by(longitude, latitude, sector) %>% 
    summarize(totalCount = sum(count))
  
  ggplot(deployCount)+
    geom_point(aes(x=longitude, y=latitiude, size=totalCount, color=sector), alpha=0.5)+
    theme_bw()
  
  deployCount_sf <- deployCount %>% st_as_sf(coords = c("longitude", "latitude"), crs=4326)
  
  abundanceMap <- mapview(deployCount_sf, zcol="sector", cex="totalCount", layer.name = "Sector Name")
  
  return(abundanceMap)
}
#mapping_abundance(fake, species.name, latitude, longitude, sector)

#Need help making sector a character

