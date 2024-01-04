source('./libraryList.R')

bahrain_info <- track_view_raw("data/circuits/tracks/bh-2002.geojson")

#bahrain_info$raw_visual
#bahrain_info$raw_coords

bahrain_straights <- c(
        1, 13, 22, 36, 42, 52, 76, 85, 91
)

bahrain_corners <- c(
        3, 8, 11, 14, 24, 27, 32, 37, 44, 49, 57, 68, 77, 86, 90
)

bahrain_detailed_info <- track_view_detailed(bahrain_info$raw_coords, bahrain_corners, bahrain_straights, bahrain_info$raw_length)

#tail(bahrain_detailed_info$detailed_coords)
#bahrain_detailed_info$detailed_visual

austria_info <- track_view_raw("data/circuits/tracks/at-1969.geojson")

austria_info$raw_visual

austria_straights <- c(
        1, 5, 20, 33, 40, 50, 64, 70, 78
)

austria_corners <- c(
        3, 16, 28, 36, 41, 51, 58, 66, 72
)

austria_detailed_info <- track_view_detailed(austria_info$raw_coords, austria_corners, austria_straights, austria_info$raw_length)

austria_detailed_info$detailed_visual
