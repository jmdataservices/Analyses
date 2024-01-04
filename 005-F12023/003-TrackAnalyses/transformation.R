source('./libraryList.R')
list.files("./data/circuits/tracks/")

track_code <- "jp-1962"
track_path <- paste0("data/circuits/tracks/", track_code, ".geojson")
track_path

raw <- track_view_raw(track_path, reverse_points = F)
raw$raw_visual
raw$raw_name
raw$raw_location

points <- read.csv("data/circuits/points.csv")

local_points <- points %>% 
        filter(
                Track == raw$raw_location
        )

corners <- local_points$Point[local_points$Segment=="Corner"]
straights <- local_points$Point[local_points$Segment=="Straight"]

det <- track_view_detailed(raw$raw_coords, corners, straights, raw$raw_length)

det$detailed_visual
