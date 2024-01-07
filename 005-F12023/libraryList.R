rm(list = ls())
library(tidyverse)
library(jsonlite)

track_view_raw <- function(file, reverse_points = F) {
        
        track <- read_json(file)
        
        track_length <- track$features[[1]]$properties$length
        track_alt <- track$features[[1]]$properties$altitude
        track_points <- length(track$features[[1]]$geometry$coordinates)
        track_name <- track$features[[1]]$properties$Name
        track_location <- track$features[[1]]$properties$Location
        
        if (reverse_points == F) {
                start <- 1
                stop <- track_points
        } else if (reverse_points == T) {
                start <- track_points
                stop <- 1
        }
        
        for (i in c(start:stop)){
                
                if (i == start) {
                        coords <- data.frame()
                        j <- 1
                }
                
                lon <- track$features[[1]]$geometry$coordinates[[i]][[1]][1]
                lat <- track$features[[1]]$geometry$coordinates[[i]][[2]][1]
                
                coords <- rbind(
                        coords,
                        data.frame(
                                Point = j,
                                NextPoint = (j %% track_points) + 1,
                                Lat = lat,
                                Lon = lon
                        )
                )
                
                j <- j + 1
                
                if (i == stop) {
                        
                        rm(i, j, lat, lon)
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
                        raw_points = track_points,
                        raw_name = track_name,
                        raw_location = track_location
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
                        DirectionChange = Direction - PreviousDirection,
                        DirectionChange = ifelse(
                                abs(DirectionChange) >= 180,
                                ifelse(
                                        DirectionChange < 0,
                                        DirectionChange + 360,
                                        DirectionChange - 360
                                ),
                                DirectionChange
                        ),
                        SegmentCode = paste0(str_sub(SegmentType, 1, 1), ifelse(StraightID == 0, CornerID, StraightID)),
                        PreviousSegmentCode = ifelse(lag(SegmentCode) != SegmentCode, lag(SegmentCode), NA)
                ) %>% 
                fill(PreviousSegmentCode) %>% 
                group_by(
                        CornerID
                ) %>% 
                mutate(
                        CornerTotalLength = sum(SectionDistance[SegmentType=="Corner"]),
                        CornerPointNumber = row_number(),
                        CornerPoints = max(CornerPointNumber, na.rm = T),
                        CornerPoints = ifelse(CornerID == 0, 0, CornerPoints),
                        CornerPointsProgress = round(CornerPointNumber / CornerPoints, 1),
                        CornerTurnProgress = cumsum(abs(DirectionChange)),
                        CornerLengthProgress = cumsum(SectionDistance),
                        VectorDelta = sum(DirectionChange),
                        TurnDirection = ifelse(VectorDelta < 0, "Left", "Right"),
                        TurnAngle = abs(VectorDelta),
                        PercCornerTurnProgress = round(100 * CornerTurnProgress / TurnAngle, 1),
                        PercCornerLengthProgress = ifelse(CornerTotalLength == 0 | SegmentType == "Straight", NA, round(100 * CornerLengthProgress / CornerTotalLength, 1))
                ) %>% 
                group_by(
                        StraightID
                ) %>% 
                mutate(
                        StraightTotalLength = sum(SectionDistance[SegmentType=="Straight"]),
                        StraightPointNumber = row_number(),
                        StraightPoints = max(StraightPointNumber, na.rm = T),
                        StraightPoints = ifelse(StraightID == 0, 0, StraightPoints),
                        StraightPointsProgress = round(StraightPointNumber / StraightPoints, 1),
                        StraightTurnProgress = cumsum(abs(DirectionChange)),
                        StraightLengthProgress = cumsum(SectionDistance),
                        VectorDelta = sum(DirectionChange),
                        StraightStraightness = sum(abs(DirectionChange))
                ) %>% as.data.frame()
        
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

profile_corners <- function(track_details) {
        
        corner_profiles <- track_details$detailed_coords %>% 
                filter(
                        CornerID > 0,
                        SegmentType == "Corner"
                ) %>% 
                select(
                        CornerID,
                        SegmentCode,
                        PreviousSegmentCode,
                        TurnDirection,
                        TurnAngle,
                        CornerTotalLength
                ) %>% 
                distinct() %>% 
                left_join(
                        by = "PreviousSegmentCode",
                        det$detailed_coords %>% 
                                mutate(
                                        SegmentLength = ifelse(StraightTotalLength > 0, StraightTotalLength, CornerTotalLength)
                                ) %>% 
                                select(
                                        PreviousSegmentCode = SegmentCode,
                                        PreviousSegment = SegmentType,
                                        PreviousSegmentLength = SegmentLength
                                ) %>% 
                                distinct()
                ) %>% 
                mutate(
                        CornerCoefficient = round(
                                log(PreviousSegmentLength * (TurnAngle / CornerTotalLength)),
                                2
                        )
                )
        
        return(corner_profiles)
        
}
