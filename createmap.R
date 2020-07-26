library(sf)

create_map <- function(data, buffer, roadsize = 1, othersize = 0.8) {
  pt <- data.frame(lat = lat, long = long)
  pt <- pt %>% 
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
    st_transform(crs)
  
  circle <- st_buffer(pt, dist = buffer)
  circle <- circle %>% st_transform(st_crs(data))
  
  clipped <- st_intersection(circle, data)
  
  clipped <- clipped[!is.na(clipped$name),]
  clipped$len <- st_length(clipped)
  
  clipped <- clipped[!(clipped$fclass == "footway" & is.na(clipped$name)), ]
  
  clipped$TYPE <- substr(clipped$name, stri_locate_last(clipped$name, regex = " ")[, 1] + 1,
                         nchar(clipped$name)) %>% 
    stri_trans_general(id = "Title")
  
  plottypes <- clipped %>% 
    st_drop_geometry() %>% 
    filter(!is.na(TYPE)) %>% 
    group_by(TYPE) %>% 
    summarize(length = sum(len)) %>% 
    arrange(desc(length)) %>% 
    top_n(8)
  
  plottypes <- as.character(plottypes$TYPE)
  plottypes <- c(plottypes, 'Motorway', 'Other')
  
  plotcolors <-  c('#59c8e5', '#fed032', '#4cb580', '#fe4d64', '#0a7abf',
                   '#2e968c', '#fe9ea5', '#fe9ea5', '#ff9223', '#cccccc')
  
  names(plotcolors) <- plottypes
  
  clipped$TYPE[clipped$fclass == 'motorway' & !(clipped$TYPE %in% plottypes)] <- "Motorway"
  clipped$TYPE[!(clipped$TYPE %in% plottypes) & clipped$TYPE != 'Motorway'] <- "Other"
  clipped$TYPE[!(clipped$TYPE %in% plottypes)] <- "Other"
  
  otherroads <- clipped[(clipped$TYPE == "Other"), ]
  clipped <- clipped[(clipped$TYPE != "Other"), ]
  
  blankbg <-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                  axis.title.x=element_blank(), axis.title.y=element_blank(),
                  panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),plot.background=element_blank(), #rect(fill = "#595959"),
                  plot.margin = unit(c(0,0,0,0), "mm"), legend.position = "bottom")
  
  map <- ggplot() + blankbg + theme(panel.grid.major = element_line(colour = "transparent")) + 
    geom_sf(data=otherroads, size = othersize, aes(color=TYPE)) + 
    geom_sf(data=clipped, size = roadsize, aes(color=TYPE)) +
    scale_color_manual(values = plotcolors) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0))
  
  return(map)
}

create_map(allroads, 5000, 0.3, 0.7)
