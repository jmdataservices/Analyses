source('./libraryList.R')

#bh-2002
#at-1969
track_code <- "at-1969"
track_path <- paste0("data/circuits/tracks/", track_code, ".geojson")

raw <- track_view_raw(track_path)

points <- read.csv("data/circuits/points.csv") %>% filter(Track == raw$raw_location)
corners <- points$Point[points$Segment=="Corner"]
straights <- points$Point[points$Segment=="Straight"]

det <- track_view_detailed(
        raw$raw_coords,
        corners,
        straights,
        raw$raw_length
)

det$detailed_coords %>% head(30)
det$detailed_coords %>% tail(10)

det$detailed_coords[det$detailed_coords$CornerID==8,] %>% head(10)


