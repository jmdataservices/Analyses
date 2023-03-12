#############################################################################
# Ingest
#############################################################################

rm(list = ls())
library(mossR)
library(gridExtra)

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
                                DriverName
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

laptimes %>% glimpse()

#############################################################################
# Max vs Lewis 2021
#############################################################################

laptimes_lm2021 <- laptimes %>% 
        filter(
                Year == 2021,
                DriverName %in% c("Lewis Hamilton", "Max Verstappen")
        ) %>% as.data.frame()

laptimes_lm2021_diffs <- laptimes_lm2021 %>% 
        filter(
                !(Round %in% c(10, 12))
        ) %>% 
        pivot_wider(
                id_cols = c(RaceID, Round, RaceName, Lap, RaceLaps),
                names_from = DriverName,
                values_from = RaceTime
        ) %>% 
        mutate(
                TimeDiff = (`Lewis Hamilton` - `Max Verstappen`) / 1000
        ) %>% 
        group_by(
                Round
        ) %>% 
        mutate(
                MaxDiff = max(abs(TimeDiff), na.rm = T)
        )

laptimes_lm2021_diffs %>% 
        mutate(
                RoundRace = paste(sep = " ", str_pad(Round, 2, pad = "0"), RaceName)
        ) %>% 
        ggplot() +
        geom_hline(aes(yintercept = 0), linetype = "dotted", color = "grey50") +
        geom_line(aes(x = Lap, y = TimeDiff), color = "black", linewidth = 0.8) +
        #geom_point(aes(x = Lap, y = MaxDiff), color = "navy") +
        #geom_point(aes(x = Lap, y = -MaxDiff), color = "seagreen") +
        geom_text(aes(x = 1, y = -50, label = "Lewis"), color = "seagreen", hjust = 0) +
        geom_text(aes(x = 1, y = 50, label = "Max"), color = "navy", hjust = 0) +
        theme_msc() +
        theme(
                axis.text.x = element_blank(),
                axis.text.y = element_text(size = 9),
                strip.text = element_text(size = 10),
                strip.background = element_rect(color = "grey50")
        ) +
        labs(
                title = "Race Length Time Differences",
                subtitle = "Race 10 saw Max crash out on the opening lap at Silverstone,\nwhile race 12 at Spa was cancelled.",
                x = "Time",
                y = "Lead"
        ) +
        scale_y_continuous(
                limits = c(-80, 80),
                labels = function(x) {paste0(abs(x), " secs")}
        ) +
        facet_wrap(
                RoundRace ~ .
        )

ggsave("images/TimeDiffInfographic.png", device = "png", height = 14, width = 24)

plt_multi01 <- list(
        laptimes_lm2021 %>% 
                filter(
                        Round == 1
                ) %>% 
                ggplot() +
                geom_line(aes(x = Lap, y = Laptime, group = DriverName, color = DriverName), size = 1) +
                geom_point(aes(x = Lap, y = Laptime, color = DriverName), size = 3) +
                theme_msc() +
                theme(
                        panel.grid.major.y = element_line(linewidth = 0.5, linetype = "dashed", color = "grey80"),
                        legend.position = "top"
                ) +
                labs(
                        title = "Lap Times During Round 1",
                        subtitle = "Bahrain Grand Prix",
                        x = "Lap Time",
                        y = "Lap",
                        color = ""
                ) +
                scale_y_continuous(
                        labels = function(x) {paste0(floor(x / 60000), ":", (x / 1000) %% 60)}
                ) +
                scale_color_manual(
                        values = c(
                                "Lewis Hamilton" = "seagreen",
                                "Max Verstappen" = "navy"
                        )
                ) +
                coord_cartesian(
                        ylim = c(92500, 100000)
                ),
        laptimes_lm2021_diffs %>% 
                filter(
                        Round == 1
                ) %>% 
                mutate(
                        RoundRace = paste(sep = " ", str_pad(Round, 2, pad = "0"), RaceName)
                ) %>% 
                ggplot() +
                geom_hline(aes(yintercept = 0), linetype = "dotted", color = "grey50") +
                geom_line(aes(x = Lap, y = TimeDiff), color = "black", linewidth = 0.8) +
                #geom_point(aes(x = Lap, y = MaxDiff), color = "navy") +
                #geom_point(aes(x = Lap, y = -MaxDiff), color = "seagreen") +
                geom_text(aes(x = 1, y = -MaxDiff, label = "Lewis"), color = "seagreen", hjust = 0) +
                geom_text(aes(x = 1, y = MaxDiff, label = "Max"), color = "navy", hjust = 0) +
                geom_text(aes(x = Lap, y = ifelse(Lap == RaceLaps, TimeDiff + ifelse(TimeDiff < 0, -3, 3), NA), label = TimeDiff, color = ifelse(TimeDiff < 0, "Lewis", "Max")), size = 5) +
                theme_msc() +
                theme(
                        axis.text = element_text(size = 9),
                        strip.text = element_text(size = 10),
                        strip.background = element_rect(color = "grey50")
                ) +
                labs(
                        x = "Time",
                        y = "Lead"
                ) +
                scale_color_manual(
                        values = c(
                                "Lewis" = "seagreen",
                                "Max" = "navy"
                        )
                )
)

grid.arrange(
        plt_multi01[[1]],
        plt_multi01[[2]]
)

laptimes_lm2021 %>% 
        select(
                Round,
                RaceName
        ) %>% 
        distinct()

pit_stops


mossR::jmds_push_analysis(
        input_filename = "2021MaxLewis.Rmd",
        output_filename = "F1TestAnalysis.html",
        analysis_name = "F1TestAnalysis",
        analysis_desc = "A test run to include the F1 analysis"
)
