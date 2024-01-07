source('./libraryList.R')

tracks <- list.files("data/circuits/tracks")

track_details <- data.frame()
corner_profiles <- data.frame()

for (track in tracks) {
        
        track_path <- paste0("data/circuits/tracks/", track)
        
        raw <- track_view_raw(track_path)
        
        points <- read.csv("data/circuits/points.csv") %>% filter(Track == raw$raw_location)
        corners <- points$Point[points$Segment=="Corner"]
        straights <- points$Point[points$Segment=="Straight"]
        
        if (!is.null(dim(points)[1]) & dim(points)[1] != 0) {
                
                det <- track_view_detailed(
                        raw$raw_coords,
                        corners,
                        straights,
                        raw$raw_length
                )
                
                track_details <- rbind(
                        track_details,
                        det$detailed_coords %>% 
                                mutate(
                                        Track = raw$raw_location
                                )
                )
                
                corner_profiles <- rbind(
                        corner_profiles,
                        profile_corners(det) %>% 
                                mutate(
                                        Track = raw$raw_location
                                )
                )
                
                print(paste0("Completed ingest for ", raw$raw_location, "."))
                
        } else {
                
                print(paste0("No ingest completed for ", raw$raw_location, "."))
                
        }
        
}

rm(corners, straights, track, track_path, det, points, raw, tracks)

all_corners <- corner_profiles %>% 
        group_by(
                Track
        ) %>% 
        summarise(
                Corners = n(),
                MinimumCorner = min(CornerCoefficient, na.rm = T),
                MeanCorner = mean(CornerCoefficient, na.rm = T),
                MedianCorner = median(CornerCoefficient, na.rm = T),
                MaximumCorner = max(CornerCoefficient, na.rm = T)
        ) %>% 
        arrange(
                desc(MeanCorner)
        )