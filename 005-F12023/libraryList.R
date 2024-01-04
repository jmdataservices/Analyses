rm(list = ls())
library(tidyverse)
library(jsonlite)

track_view_raw <- function(file) {
        
        track <- read_json(file)
        
        track_length <- track$features[[1]]$properties$length
        track_alt <- track$features[[1]]$properties$altitude
        track_points <- length(track$features[[1]]$geometry$coordinates)
        track_name <- track$features[[1]]$properties$Name
        track_location <- track$features[[1]]$properties$Location
        
        for (i in c(1:track_points)){
                
                if (i == 1) {
                        coords <- data.frame()
                }
                
                lon <- track$features[[1]]$geometry$coordinates[[i]][[1]][1]
                lat <- track$features[[1]]$geometry$coordinates[[i]][[2]][1]
                
                coords <- rbind(
                        coords,
                        data.frame(
                                Point = i,
                                NextPoint = (i %% track_points) + 1,
                                Lat = lat,
                                Lon = lon
                        )
                )
                
                if (i == track_points) {
                        
                        rm(i, lat, lon)
                }
        }
        
        coords <- coords %>% 
                left_join(
                        by = c("NextPoint" = "Point"),
                        coords %>% 
                                select(
                                        Point,
                                        NextLat = Lat,
                                        NextLon = Lon
                                )
                )
        
        visual <- coords %>% 
                ggplot() +
                geom_segment(aes(x = Lon, xend = NextLon, y = Lat, yend = NextLat)) +
                geom_text(aes(x = Lon, y = Lat, label = Point), size = 3.5) +
                theme_minimal() +
                theme(
                        panel.grid = element_blank()
                ) +
                labs(
                        
                )
        
        return(
                list(
                        raw_coords = coords,
                        raw_visual = visual,
                        raw_length = track_length,
                        raw_altitude = track_alt,
                        raw_points = track_points
                )
        )
        
}

track_view_detailed <- function(raw_coords, corners, straights, track_length) {
        
        coords <- raw_coords %>% 
                mutate(
                        CoordDist = sqrt((NextLat - Lat) ^ 2 + (NextLon - Lon) ^ 2),
                        CumCoordDist = cumsum(CoordDist),
                        TotalCoordDist = max(CumCoordDist),
                        PercCoordDist = round(100 * CoordDist / TotalCoordDist, 2),
                        PercTrack = round(100 * CumCoordDist / TotalCoordDist, 2),
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
                        Direction = round((180 / pi) * Direction, 1),
                        PreviousDirection = lag(Direction, n = 1),
                        isNewStraight = ifelse(
                                Point %in% straights,
                                1,
                                0
                        ),
                        isNewCorner = ifelse(
                                Point %in% corners,
                                1,
                                0
                        ),
                        SegmentType = ifelse(isNewStraight == 1, "Straight", ifelse(isNewCorner == 1, "Corner", NA))
                ) %>% 
                fill(SegmentType) %>% 
                mutate(
                        StraightID = cumsum(isNewStraight == 1),
                        StraightID = ifelse(SegmentType == "Corner", 0, StraightID),
                        StraightID = ifelse(StraightID == max(StraightID), 1, StraightID),
                        CornerID = cumsum(isNewCorner == 1),
                        CornerID = ifelse(SegmentType == "Straight", 0, CornerID),
                        TrackDistance = track_length,
                        SectionDistance = (PercCoordDist / 100) * TrackDistance,
                        CornerID = ifelse(isNewStraight == 1, lag(CornerID, default = 0), CornerID),
                        DirectionChange = Direction - PreviousDirection
                )
        
        visual <- coords %>% 
                ggplot() +
                geom_segment(aes(x = Lon, xend = NextLon, y = Lat, yend = NextLat, color = SegmentType)) +
                geom_text(aes(x = Lon, y = Lat, label = ifelse(isNewCorner == 1, CornerID, NA)), size = 3.5) +
                #geom_text(aes(x = Lon, y = Lat, label = Point), size = 3.5) +
                theme_minimal() +
                theme(
                        panel.grid = element_blank()
                ) +
                labs(
                        
                )
        
        return(
                list(
                        detailed_coords = coords,
                        detailed_visual = visual
                )
        )
        
}
