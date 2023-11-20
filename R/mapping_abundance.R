#' Mapping abundance function
#' 
#' Plot abundances in a map. Need the following packages: tidyverse, sf, mapview, and vegan
#' @param data Dataframe you cleaned
#' @param Species The species or common name column
#' @param Lat The column with Lat coordinates
#' @param Long The column with Long coordinates
#' @param Sector From Sectors 1-6
#' @return abundanceMap The interactive map of abundances
#' 
#' @export

mapping_abundance<-function(data, Species, Lat, Long, Sector){
  if(!is.numeric(data$Sector)){
    print("Sector is not numeric")
    return(NULL)
  } else {
  speciesCount<-data %>%
    select(Species, Count, Sector)%>%
    mutate(Sector = as.character(Sector))
  speciesCount %>% 
    group_by(Species, Sector) %>% 
    summarize(totalCount=sum(Count)) 
  ggplot(data, aes(x=Long, y=Lat, color = Sector))+
    geom_point()+
    theme_bw()
  deployCount <- data %>% dplyr::select(Species, Count, Long, Lat, Sector) %>% 
    group_by(Long, Lat, Sector) %>% 
    summarize(totalCount = sum(Count))
  
  ggplot(deployCount)+
    geom_point(aes(x=Long, y=latitiude, size=totalCount, color=factor(Sector)), alpha=0.5)+
    theme_bw()
  
  deployCount_sf <- deployCount %>% st_as_sf(coords = c("Long", "Lat"), crs=4326)
  
  abundanceMap <- mapview(deployCount_sf, zcol="Sector", cex="totalCount", layer.name = "Sector Name")
  
  return(abundanceMap)
  }
}


