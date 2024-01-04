source('./libraryList.R')

track <- read_json("data/circuits/tracks/bh-2002.geojson")

for (i in c(1:94)){
        
        if (i == 1) {
                coords <- data.frame()
        }
        
        lon <- track$features[[1]]$geometry$coordinates[[i]][[1]][1]
        lat <- track$features[[1]]$geometry$coordinates[[i]][[2]][1]
        
        coords <- rbind(
                coords,
                data.frame(
                        Point = i,
                        NextPoint = (i %% 94) + 1,
                        Lat = lat,
                        Lon = lon
                )
        )
        
        if (i == 94) {
                
                coords <- coords %>% 
                        left_join(
                                by = c("NextPoint" = "Point"),
                                coords %>% 
                                        select(
                                                Point,
                                                NextLat = Lat,
                                                NextLon = Lon
                                        )
                        ) %>% 
                        mutate(
                                CoordDist = sqrt((NextLat - Lat) ^ 2 + (NextLon - Lon) ^ 2),
                                Direction = ifelse(
                                        (NextLon - Lon) >= 0 & (NextLat - Lat) >= 0,
                                        atan((NextLon - Lon) / (NextLat - Lat)),
                                        ifelse(
                                                (NextLon - Lon) >= 0 & (NextLat - Lat) <= 0,
                                                pi + atan((NextLon - Lon) / (NextLat - Lat)),
                                                ifelse(
                                                        (NextLon - Lon) <= 0 & (NextLat - Lat) <= 0,
                                                        pi + atan((NextLon - Lon) / (NextLat - Lat)),
                                                        ifelse(
                                                                (NextLon - Lon) <= 0 & (NextLat - Lat) >= 0,
                                                                (2 * pi) + atan((NextLon - Lon) / (NextLat - Lat)),
                                                                atan((NextLon - Lon) / (NextLat - Lat))
                                                        )
                                                )
                                        )
                                ),
                                Direction = round((180 / pi) * Direction, 1)
                        )
                
        }
}

coords %>% 
        ggplot() +
        geom_segment(aes(x = Lon, xend = NextLon, y = Lat, yend = NextLat)) +
        geom_text(aes(x = Lon, y = Lat, label = Direction), size = 2.5)
