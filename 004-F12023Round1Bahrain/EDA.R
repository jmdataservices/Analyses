#############################################################################
# Ingest
#############################################################################

# https://en.wikipedia.org/wiki/2023_Bahrain_Grand_Prix

rm(list = ls())
library(mossR)
library(gridExtra)
library(cowplot)

color_list <- c(
        "Alfa Romeo" = "firebrick4",
        "AlphaTauri" = "darkblue",
        "Alpine F1 Team" = "hotpink",
        "Aston Martin" = "forestgreen",
        "Ferrari" = "red",
        "Haas F1 Team" = "black",
        "McLaren" = "orange",
        "Mercedes" = "grey50",
        "Red Bull" = "navy",
        "Williams" = "cyan"
)

lap_times <- read.csv("data/lap_times.csv")
pit_stops <- read.csv("data/pit_stops.csv")
drivers <- read.csv("data/drivers.csv")
races <- read.csv("data/races.csv")
circuits <- read.csv("data/circuits.csv")
results <- read.csv("data/results.csv")
constructors <- read.csv("data/constructors.csv")
status <- read.csv("data/status.csv")

laptimes <- lap_times %>% 
        select(
                driverId,
                RaceID = raceId,
                Lap = lap,
                CurrentPosition = position,
                Laptime = milliseconds
        ) %>% 
        left_join(
                by = "driverId",
                drivers %>% 
                        mutate(
                                DriverName = paste(sep = " ", forename, surname)
                        ) %>% 
                        select(
                                driverId,
                                DriverName,
                                DriverCode = code
                        )
        ) %>% 
        left_join(
                by = c("RaceID" = "raceId"),
                races %>% 
                        select(
                                raceId,
                                circuitId,
                                Year = year,
                                Round = round,
                                RaceDate = date,
                                RaceName = name
                        )
        ) %>% 
        left_join(
                by = "circuitId",
                circuits %>% 
                        select(
                                circuitId,
                                CircuitRef = circuitRef,
                                CircuitName = name,
                                Lat = lat,
                                Lng = lng,
                                Alt = alt,
                                Location = location,
                                Country = country
                        )
        ) %>% 
        left_join(
                by = c("RaceID" = "raceId", "driverId"),
                results %>% 
                        select(
                                raceId,
                                driverId,
                                constructorId,
                                GridSpot = grid,
                                statusId
                        ) %>% 
                        left_join(
                                by = "statusId",
                                status %>% 
                                        select(
                                                statusId,
                                                Status = status
                                        )
                        )
        ) %>% 
        left_join(
                by = c("constructorId"),
                constructors %>% 
                        select(
                                constructorId,
                                TeamName = name
                        )
        ) %>% 
        left_join(
                by = c("RaceID" = "raceId", "driverId", "Lap"),
                pit_stops %>% 
                        select(
                                raceId,
                                driverId,
                                Lap = lap,
                                StopNumber = stop,
                                StopTime = milliseconds
                        )
        ) %>% 
        left_join(
                by = c("RaceID" = "raceId", "driverId", "Lap"),
                pit_stops  %>% 
                        mutate(
                                OutLap = T,
                                Lap = lap + 1
                        ) %>% 
                        select(
                                raceId,
                                driverId,
                                Lap,
                                OutLap
                        )
        ) %>% 
        select(
                -driverId,
                -constructorId,
                -circuitId,
                -statusId
        ) %>% 
        group_by(
                RaceID
        ) %>% 
        mutate(
                RaceLaps = max(Lap, na.rm = T)
        ) %>% 
        group_by(
                RaceID,
                DriverName
        ) %>% 
        mutate(
                RaceTime = cumsum(Laptime)
        )

laptimes_2023_1 <- laptimes %>% 
        filter(
                Year == 2023,
                Round == 1
        )

#############################################################################
# Racing Lap Dataset
#############################################################################

laptimes_2023_1 <- laptimes_2023_1 %>% as.data.frame() %>% 
        mutate(
                UniqueStint = cumsum(!is.na(StopNumber)) + 1
        ) %>% 
        group_by(
                DriverName
        ) %>% 
        mutate(
                Stint = cumsum(!is.na(StopNumber)) + 1
        )

laptimes_2023_1 <- laptimes_2023_1 %>% as.data.frame() %>% 
        group_by(
                UniqueStint
        ) %>% 
        mutate(
                StintLap = row_number()
        )

laptimes_racing <- laptimes_2023_1 %>% 
        left_join(
                by = "DriverName",
                laptimes_2023_1 %>% 
                        group_by(
                                DriverName
                        ) %>% 
                        summarise(
                                LastLap = max(Lap, na.rm = T)
                        )
        ) %>% 
        filter(
                Lap != 1,
                (Lap != LastLap | (grepl("Lap", Status) | Status == "Finished")),
                !Lap %in% c(40:42), #VSC
                is.na(OutLap),
                is.na(StopNumber)
        )

laptimes_racing <- laptimes_racing %>% 
        left_join(
                by = c("RaceID", "TeamName", "DriverName"),
                laptimes_racing %>% 
                        group_by(
                                RaceID,
                                TeamName,
                                DriverName
                        ) %>% 
                        summarise(
                                DriverMeanLaptime = mean(Laptime, na.rm = T),
                                DriverMedianLaptime = median(Laptime, na.rm = T),
                                DriverSDLaptime = sd(Laptime, na.rm = T)
                        )
        ) %>% 
        left_join(
                by = c("RaceID", "TeamName"),
                laptimes_racing %>% 
                        group_by(
                                RaceID,
                                TeamName
                        ) %>% 
                        summarise(
                                TeamMeanLaptime = mean(Laptime, na.rm = T),
                                TeamMedianLaptime = median(Laptime, na.rm = T),
                                TeamSDLaptime = sd(Laptime, na.rm = T)
                        )
        ) %>% 
        left_join(
                by = "RaceID",
                laptimes_racing %>% 
                        group_by(
                                RaceID
                        ) %>% 
                        summarise(
                                RaceMeanLaptime = mean(Laptime, na.rm = T),
                                RaceMedianLaptime = median(Laptime, na.rm = T),
                                RaceSDLaptime = sd(Laptime, na.rm = T)
                        )
        )

#############################################################################
# Stint Lap Model
#############################################################################

mod_stintlap <- lm(
        data = laptimes_racing,
        formula = Laptime ~ StintLap
)

mod_stintlap %>% summary()

laptimes_racing %>% 
        ggplot() +
        geom_point(aes(x = StintLap, y = Laptime, color = TeamName)) +
        geom_smooth(aes(x = StintLap, y = Laptime), color = "firebrick", method = "lm", se = F) +
        theme_moss() +
        theme(
                panel.grid.major.y = element_line(linewidth = 0.8, linetype = "dashed", color = "grey90"),
                axis.title = element_text(size = 11),
                axis.text = element_text(size = 10),
                strip.text = element_text(size = 10),
                legend.position = "top"
        ) +
        labs(
                title = "Lap Times Modelled Against Stint Laps",
                subtitle = "Increased tyre wear suggests a ~71 millisecond lap time reduction per lap",
                x = "Stint Lap",
                y = "Lap Time"
        ) +
        scale_x_continuous(
                breaks = c(seq(0, 100, 10))
        ) +
        scale_y_continuous(
                labels = function(x) {paste0(floor(x / 60000), ":", str_pad((x / 1000) %% 60, 2, pad = "0"))},
                breaks = c(seq(0, 250000, 1000))
        ) +
        scale_color_manual(
                values = color_list
        )

#############################################################################
# Fuel Cost Model
#############################################################################

mod_fuelcost <- lm(
        data = laptimes_racing,
        formula = Laptime ~ Lap
)

mod_fuelcost %>% summary()

laptimes_racing %>% 
        ggplot() +
        geom_point(aes(x = Lap, y = Laptime, color = TeamName)) +
        geom_smooth(aes(x = Lap, y = Laptime), color = "firebrick", method = "lm", se = F) +
        theme_moss() +
        theme(
                panel.grid.major.y = element_line(linewidth = 0.8, linetype = "dashed", color = "grey90"),
                axis.title = element_text(size = 11),
                axis.text = element_text(size = 10),
                strip.text = element_text(size = 10),
                legend.position = "top"
        ) +
        labs(
                title = "Lap Times Modelled Against Lap Number",
                subtitle = "Reduced fuel load suggests a ~56 millisecond lap time improvement per lap",
                x = "Lap Number",
                y = "Lap Time"
        ) +
        scale_x_continuous(
                breaks = c(seq(0, 100, 10)),
                limits = c(0, max(laptimes_racing$RaceLaps) + 6)
        ) +
        scale_y_continuous(
                labels = function(x) {paste0(floor(x / 60000), ":", str_pad((x / 1000) %% 60, 2, pad = "0"))},
                breaks = c(seq(0, 250000, 1000))
        ) +
        scale_color_manual(
                values = color_list
        )

#############################################################################
# Composite Model
#############################################################################

mod_lap <- lm(
        data = laptimes_racing,
        formula = Laptime ~ StintLap + Lap
)

mod_lap %>% summary()

mod_lap$coefficients[[2]]
mod_lap$coefficients[[3]]

laptimes_racing <- laptimes_racing %>% 
        mutate(
                AdjustedLaptimeReduction = (RaceLaps - Lap) * -mod_lap$coefficients[[3]],
                WearAdjustedLaptimeReduction = (StintLap - 1) * mod_lap$coefficients[[2]],
                Laptime_Adjusted = Laptime - AdjustedLaptimeReduction - WearAdjustedLaptimeReduction
        )

#### ARCHIVE (NEW MODEL FOR WEAR)

laptimes_racing <- laptimes_racing %>% 
        left_join(
                by = c("RaceID", "TeamName", "DriverName"),
                laptimes_racing %>% 
                        group_by(
                                RaceID,
                                TeamName,
                                DriverName
                        ) %>% 
                        summarise(
                                DriverMeanLaptime_Adjusted = mean(Laptime_Adjusted, na.rm = T),
                                DriverMedianLaptime_Adjusted = median(Laptime_Adjusted, na.rm = T),
                                DriverSDLaptime_Adjusted = sd(Laptime_Adjusted, na.rm = T)
                        )
        ) %>% 
        left_join(
                by = c("RaceID", "TeamName"),
                laptimes_racing %>% 
                        group_by(
                                RaceID,
                                TeamName
                        ) %>% 
                        summarise(
                                TeamMeanLaptime_Adjusted = mean(Laptime_Adjusted, na.rm = T),
                                TeamMedianLaptime_Adjusted = median(Laptime_Adjusted, na.rm = T),
                                TeamSDLaptime_Adjusted = sd(Laptime_Adjusted, na.rm = T)
                        )
        ) %>% 
        left_join(
                by = "RaceID",
                laptimes_racing %>% 
                        group_by(
                                RaceID
                        ) %>% 
                        summarise(
                                RaceMeanLaptime_Adjusted = mean(Laptime_Adjusted, na.rm = T),
                                RaceMedianLaptime_Adjusted = median(Laptime_Adjusted, na.rm = T),
                                RaceSDLaptime_Adjusted = sd(Laptime_Adjusted, na.rm = T)
                        )
        ) %>% as.data.frame()

#############################################################################
# Racing Laptimes
#############################################################################

laptimes_racing %>% 
        filter(
                
        ) %>% 
        ggplot() +
        geom_line(aes(x = Lap, y = Laptime, group = DriverName, color = TeamName)) +
        geom_point(aes(x = Lap, y = Laptime, color = TeamName)) +
        geom_text(aes(x = LastLap + 3, y = ifelse(Lap == LastLap, Laptime, NA), label = DriverCode), hjust = 0, size = 3, face = "bold") +
        theme_moss() +
        theme(
                panel.grid.major.y = element_line(linewidth = 0.8, linetype = "dashed", color = "grey90"),
                axis.title = element_text(size = 11),
                axis.text = element_text(size = 10),
                strip.text = element_text(size = 10),
                legend.position = "top"
        ) +
        labs(
                title = "Pace During Racing Laps",
                subtitle = "All compromised laptimes are removed to retain only 'fast' laps",
                x = "Lap Number",
                y = "Lap Time",
                color = ""
        ) +
        scale_x_continuous(
                breaks = c(seq(0, 100, 10)),
                limits = c(0, max(laptimes_racing$RaceLaps) + 6)
        ) +
        scale_y_continuous(
                labels = function(x) {paste0(floor(x / 60000), ":", str_pad((x / 1000) %% 60, 2, pad = "0"))},
                breaks = c(seq(0, 250000, 1000))
        ) +
        scale_color_manual(
                values = color_list
        ) +
        facet_wrap(
                TeamName ~ .
        )

laptimes_racing %>% 
        filter(
                
        ) %>% 
        ggplot() +
        geom_line(aes(x = Lap, y = Laptime_Adjusted, group = DriverName, color = TeamName)) +
        geom_point(aes(x = Lap, y = Laptime_Adjusted, color = TeamName)) +
        geom_text(aes(x = LastLap + 3, y = ifelse(Lap == LastLap, Laptime, NA), label = DriverCode), hjust = 0, size = 3, face = "bold") +
        theme_moss() +
        theme(
                panel.grid.major.y = element_line(linewidth = 0.8, linetype = "dashed", color = "grey90"),
                axis.title = element_text(size = 11),
                axis.text = element_text(size = 10),
                strip.text = element_text(size = 10),
                legend.position = "top"
        ) +
        labs(
                title = "Fuel Adjusted Pace During Racing Laps",
                subtitle = "All compromised laptimes are removed to retain only 'fast' laps",
                x = "Lap Number",
                y = "Lap Time",
                color = ""
        ) +
        scale_x_continuous(
                breaks = c(seq(0, 100, 10)),
                limits = c(0, max(laptimes_racing$RaceLaps) + 6)
        ) +
        scale_y_continuous(
                labels = function(x) {paste0(floor(x / 60000), ":", str_pad((x / 1000) %% 60, 2, pad = "0"))},
                breaks = c(seq(0, 250000, 1000))
        ) +
        scale_color_manual(
                values = color_list
        ) +
        facet_wrap(
                TeamName ~ .
        )

#############################################################################
# Average Lap Times
#############################################################################

laptimes_racing %>% 
        select(
                DriverName,
                TeamName,
                DriverMeanLaptime,
                DriverMedianLaptime
        ) %>% 
        distinct() %>% 
        ggplot() +
        geom_segment(aes(x = reorder(DriverName, DriverMeanLaptime), xend = reorder(DriverName, DriverMeanLaptime), y = 0, yend = DriverMeanLaptime, color = TeamName), linewidth = 2) +
        geom_point(aes(x = reorder(DriverName, DriverMeanLaptime), y = DriverMeanLaptime), color = "black", size = 8) +
        geom_point(aes(x = reorder(DriverName, DriverMeanLaptime), y = DriverMeanLaptime), color = "white", size = 6.5) +
        geom_point(aes(x = reorder(DriverName, DriverMeanLaptime), y = DriverMeanLaptime, color = TeamName), size = 5) +
        theme_moss() +
        theme(
                panel.grid.major.y = element_line(linewidth = 0.8, linetype = "dashed", color = "grey90"),
                axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
                title = "Average Lap Time by Driver",
                subtitle = "Measurement constitutes only 'racing laps', and excludes compromised laptimes",
                x = "Driver",
                y = "Lap Time"
        ) +
        scale_y_continuous(
                labels = function(x) {paste0(floor(x / 60000), ":", str_pad((x / 1000) %% 60, 2, pad = "0"))},
                breaks = c(seq(0, 250000, 1000))
        ) +
        scale_color_manual(
                values = color_list
        ) +
        coord_cartesian(
                ylim = c(96000, 101500)
        )

#############################################################################
# Average Lap Times - Fuel Adjusted
#############################################################################

laptimes_racing %>% 
        select(
                DriverName,
                TeamName,
                DriverMeanLaptime_Adjusted,
                DriverMedianLaptime_Adjusted
        ) %>% 
        distinct() %>% 
        ggplot() +
        geom_segment(aes(x = reorder(DriverName, DriverMeanLaptime_Adjusted), xend = reorder(DriverName, DriverMeanLaptime_Adjusted), y = 0, yend = DriverMeanLaptime_Adjusted, color = TeamName), linewidth = 2) +
        geom_point(aes(x = reorder(DriverName, DriverMeanLaptime_Adjusted), y = DriverMeanLaptime_Adjusted), color = "black", size = 8) +
        geom_point(aes(x = reorder(DriverName, DriverMeanLaptime_Adjusted), y = DriverMeanLaptime_Adjusted), color = "white", size = 6.5) +
        geom_point(aes(x = reorder(DriverName, DriverMeanLaptime_Adjusted), y = DriverMeanLaptime_Adjusted, color = TeamName), size = 5) +
        theme_moss() +
        theme(
                panel.grid.major.y = element_line(linewidth = 0.8, linetype = "dashed", color = "grey90"),
                axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
                title = "Average Lap Time by Driver - Fuel Adjusted",
                subtitle = "Measurement constitutes only 'racing laps', and excludes compromised laptimes\nWe have also reduced the lap times at the beginning of the race, according to the fuel model",
                x = "Driver",
                y = "Lap Time"
        ) +
        scale_y_continuous(
                labels = function(x) {paste0(floor(x / 60000), ":", str_pad((x / 1000) %% 60, 2, pad = "0"))},
                breaks = c(seq(0, 250000, 1000))
        ) +
        scale_color_manual(
                values = color_list
        ) +
        coord_cartesian(
                ylim = c(95000, 99000)
        )

laptimes_racing %>% 
        select(
                DriverName,
                TeamName,
                DriverMeanLaptime_Adjusted,
                DriverMedianLaptime_Adjusted,
                DriverSDLaptime_Adjusted
        ) %>% 
        distinct() %>% 
        ggplot() +
        geom_segment(aes(x = reorder(DriverName, DriverMeanLaptime_Adjusted), xend = reorder(DriverName, DriverMeanLaptime_Adjusted), y = DriverMeanLaptime_Adjusted - DriverSDLaptime_Adjusted, yend = DriverMeanLaptime_Adjusted + DriverSDLaptime_Adjusted, color = TeamName), alpha = 0.1, linewidth = 2) +
        geom_point(aes(x = reorder(DriverName, DriverMeanLaptime_Adjusted), y = DriverMeanLaptime_Adjusted), color = "black", size = 8) +
        geom_point(aes(x = reorder(DriverName, DriverMeanLaptime_Adjusted), y = DriverMeanLaptime_Adjusted), color = "white", size = 6.5) +
        geom_point(aes(x = reorder(DriverName, DriverMeanLaptime_Adjusted), y = DriverMeanLaptime_Adjusted, color = TeamName), size = 5) +
        theme_moss() +
        theme(
                panel.grid.major.y = element_line(linewidth = 0.8, linetype = "dashed", color = "grey90"),
                axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
                title = "Lap Consistency by Driver - Fuel Adjusted",
                subtitle = "Measurement includes only 'racing laps', fuel model adjustments and excludes compromised laptimes\nConsistency is much lower at the back of the field - perhaps unsurprisingly.",
                x = "Driver",
                y = "Lap Time"
        ) +
        scale_y_continuous(
                labels = function(x) {paste0(floor(x / 60000), ":", str_pad((x / 1000) %% 60, 2, pad = "0"))},
                breaks = c(seq(0, 250000, 1000))
        ) +
        scale_color_manual(
                values = color_list
        ) +
        coord_cartesian(
                ylim = c(95000, 99000)
        )

#############################################################################
# Tyre Age Analysis
#############################################################################

laptimes_freshtyrelaps <- laptimes_2023_1 %>% 
        filter(
                OutLap == T
        ) %>% 
        mutate(
                FreshTyreLap01 = 2,
                FreshTyreLap02 = 3,
                FreshTyreLap03 = 4,
                FreshTyreLap04 = 5,
                FreshTyreLap1 = Lap + 1,
                FreshTyreLap2 = Lap + 2,
                FreshTyreLap3 = Lap + 3,
                FreshTyreLap4 = Lap + 4,
                FreshTyreLap5 = Lap + 5
        ) %>% 
        select(
                RaceID,
                DriverName,
                FreshTyreLap1,
                FreshTyreLap2,
                FreshTyreLap3,
                FreshTyreLap4,
                FreshTyreLap5,
                FreshTyreLap01,
                FreshTyreLap02,
                FreshTyreLap03,
                FreshTyreLap04
        ) %>% as.data.frame() %>% 
        pivot_longer(
                cols = -c(RaceID, DriverName),
                names_to = "FreshLapNumber",
                values_to = "Lap"
        )

laptimes_racing_fresh <- laptimes_racing %>% 
        inner_join(
                by = c("DriverName", "Lap"),
                laptimes_freshtyrelaps %>% 
                        select(
                                DriverName,
                                Lap
                        ) %>% 
                        distinct()
        )

laptimes_racing_fresh %>% 
        group_by(
                TeamName,
                DriverName
        ) %>% 
        summarise(
                FreshLaps = n(),
                MeanDriverFreshLaptime_Adjusted = mean(Laptime_Adjusted, na.rm = T),
                MedianDriverFreshLaptime_Adjusted = median(Laptime_Adjusted, na.rm = T),
                SDDriverFreshLaptime_Adjusted = sd(Laptime_Adjusted, na.rm = T)
        ) %>% 
        ggplot() +
        geom_segment(aes(x = reorder(DriverName, MeanDriverFreshLaptime_Adjusted), xend = reorder(DriverName, MeanDriverFreshLaptime_Adjusted), y = MeanDriverFreshLaptime_Adjusted - SDDriverFreshLaptime_Adjusted, yend = MeanDriverFreshLaptime_Adjusted + SDDriverFreshLaptime_Adjusted, color = TeamName), alpha = 0.1, linewidth = 2) +
        geom_point(aes(x = reorder(DriverName, MeanDriverFreshLaptime_Adjusted), y = MeanDriverFreshLaptime_Adjusted), color = "black", size = 8) +
        geom_point(aes(x = reorder(DriverName, MeanDriverFreshLaptime_Adjusted), y = MeanDriverFreshLaptime_Adjusted), color = "white", size = 6.5) +
        geom_point(aes(x = reorder(DriverName, MeanDriverFreshLaptime_Adjusted), y = MeanDriverFreshLaptime_Adjusted, color = TeamName), size = 5) +
        geom_text(aes(x = reorder(DriverName, MeanDriverFreshLaptime_Adjusted), y = 94750, label = paste0(FreshLaps, "\nLaps"))) +
        #geom_text(aes(x = 0, y = 94750, label = "Fresh Laps:")) +
        theme_moss() +
        theme(
                panel.grid.major.y = element_line(linewidth = 0.8, linetype = "dashed", color = "grey90"),
                axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
                title = "Fresh Tyre Lap Times - Fuel Adjusted",
                subtitle = "Measurement includes only 'fresh tyre laps', fuel model adjustments and excludes compromised laptimes\nConsistency is much lower at the back of the field.",
                x = "Driver",
                y = "Lap Time"
        ) +
        scale_y_continuous(
                labels = function(x) {paste0(floor(x / 60000), ":", str_pad((x / 1000) %% 60, 2, pad = "0"))},
                breaks = c(seq(0, 250000, 1000))
        ) +
        scale_color_manual(
                values = color_list
        ) +
        coord_cartesian(
                ylim = c(94500, 98000)
        )

laptimes_racing_dead <- laptimes_racing %>% 
        anti_join(
                by = c("DriverName", "Lap"),
                laptimes_freshtyrelaps %>% 
                        select(
                                DriverName,
                                Lap
                        ) %>% 
                        distinct()
        )

laptimes_racing_dead %>% 
        group_by(
                TeamName,
                DriverName
        ) %>% 
        summarise(
                DeadLaps = n(),
                MeanDriverDeadLaptime_Adjusted = mean(Laptime_Adjusted, na.rm = T),
                MedianDriverDeadLaptime_Adjusted = median(Laptime_Adjusted, na.rm = T),
                SDDriverDeadLaptime_Adjusted = sd(Laptime_Adjusted, na.rm = T)
        ) %>% 
        ggplot() +
        geom_segment(aes(x = reorder(DriverName, MeanDriverDeadLaptime_Adjusted), xend = reorder(DriverName, MeanDriverDeadLaptime_Adjusted), y = MeanDriverDeadLaptime_Adjusted - SDDriverDeadLaptime_Adjusted, yend = MeanDriverDeadLaptime_Adjusted + SDDriverDeadLaptime_Adjusted, color = TeamName), alpha = 0.1, linewidth = 2) +
        geom_point(aes(x = reorder(DriverName, MeanDriverDeadLaptime_Adjusted), y = MeanDriverDeadLaptime_Adjusted), color = "black", size = 8) +
        geom_point(aes(x = reorder(DriverName, MeanDriverDeadLaptime_Adjusted), y = MeanDriverDeadLaptime_Adjusted), color = "white", size = 6.5) +
        geom_point(aes(x = reorder(DriverName, MeanDriverDeadLaptime_Adjusted), y = MeanDriverDeadLaptime_Adjusted, color = TeamName), size = 5) +
        geom_text(aes(x = reorder(DriverName, MeanDriverDeadLaptime_Adjusted), y = 94750, label = paste0(DeadLaps, "\nLaps"))) +
        #geom_text(aes(x = 0, y = 94750, label = "Dead Laps:")) +
        theme_moss() +
        theme(
                panel.grid.major.y = element_line(linewidth = 0.8, linetype = "dashed", color = "grey90"),
                axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
                title = "Dead Tyre Lap Times - Fuel Adjusted",
                subtitle = "Measurement includes only 'dead tyre laps', fuel model adjustments and excludes compromised laptimes\nConsistency is much lower at the back of the field.",
                x = "Driver",
                y = "Lap Time"
        ) +
        scale_y_continuous(
                labels = function(x) {paste0(floor(x / 60000), ":", str_pad((x / 1000) %% 60, 2, pad = "0"))},
                breaks = c(seq(0, 250000, 1000))
        ) +
        scale_color_manual(
                values = color_list
        ) +
        coord_cartesian(
                ylim = c(95500, 100000)
        )

#############################################################################
# Car Adjusted Lap Times
#############################################################################

laptimes_racing %>% 
        mutate(
                DriverDiff = DriverMeanLaptime_Adjusted - TeamMeanLaptime_Adjusted
        ) %>% 
        ggplot() +
        geom_segment(aes(x = reorder(DriverName, DriverDiff), xend = reorder(DriverName, DriverDiff), y = 0, yend = DriverDiff, color = TeamName), alpha = 0.1, linewidth = 2) +
        geom_point(aes(x = reorder(DriverName, DriverDiff), y = DriverDiff), color = "black", size = 8) +
        geom_point(aes(x = reorder(DriverName, DriverDiff), y = DriverDiff), color = "white", size = 6.5) +
        geom_point(aes(x = reorder(DriverName, DriverDiff), y = DriverDiff, color = TeamName), size = 5) +
        #geom_text(aes(x = 0, y = 94750, label = "Dead Laps:")) +
        theme_moss() +
        theme(
                panel.grid.major.y = element_line(linewidth = 0.8, linetype = "dashed", color = "grey90"),
                axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
                title = "Dead Tyre Lap Times - Fuel Adjusted",
                subtitle = "Measurement includes only 'dead tyre laps', fuel model adjustments and excludes compromised laptimes\nConsistency is much lower at the back of the field.",
                x = "Driver",
                y = "Lap Time"
        ) +
        scale_y_continuous(
                #labels = function(x) {paste0(floor(x / 60000), ":", str_pad((x / 1000) %% 60, 2, pad = "0"))},
                breaks = c(seq(-10000, 10000, 1000))
        ) +
        scale_color_manual(
                values = color_list
        ) +
        coord_cartesian(
                ylim = c(-1500, 1500)
        )

laptimes_racing %>% as.data.frame() %>% 
        select(
                TeamName,
                TeamMeanLaptime_Adjusted
        ) %>% 
        arrange(
                TeamMeanLaptime_Adjusted
        ) %>% 
        distinct()
