source("dependencies.R")

dat_adults <- read.csv(
        "001-AdultEarnings/src/adult.data",
        col.names = c(
                "Age",
                "WorkClass",
                "fnlwgt",
                "Education",
                "EducationNum",
                "MaritalStatus",
                "Occupation",
                "RelationshipStatus",
                "Race",
                "Sex",
                "CapitalGain",
                "CapitalLoss",
                "HoursWorked",
                "NativeCountry",
                "EarningsClass"
        )
)
dat_test <- read.csv("001-AdultEarnings/src/adult.test", skip = 1)

dat_adults <- dat_adults %>%
        mutate(
                across(.cols = c(EarningsClass, MaritalStatus, WorkClass, Education, Occupation, RelationshipStatus, Race, Sex, NativeCountry), .fns = trimws),
                HoursWorkedGroup = ifelse(
                        HoursWorked < 15, "Minimal", ifelse(
                                HoursWorked < 35, "PartTime", ifelse(
                                        HoursWorked < 45, "FullTime", ifelse(
                                                HoursWorked < 52, "OverTime", "XOverTime"
                                        )
                                )
                        )
                )
        )

dat_adults %>% head(3)

dat_adults %>%
        ggplot(aes(x = EarningsClass)) +
        geom_jitter(aes(y = EducationNum), width = 0.5, alpha = 0.05) +
        geom_boxplot(aes(y = EducationNum)) +
        theme_moss()

dat_adults %>%
        ggplot(aes(x = EarningsClass)) +
        geom_violin(aes(y = EducationNum), alpha = 0.05) +
        theme_moss()

dat_adults %>%
        select(
                Age,
                EducationNum,
                CapitalGain,
                CapitalLoss,
                HoursWorked
        ) %>%
        ggpairs()

dat_adults %>% head(3)

for (i in c(1,2,5,6,7,8,9,10,14,16)) {

        if (i == 1) {
                dat_adults_supertotals <- data.frame()
        }

        tmp <- dat_adults[,c(i,15)] %>%
                group_by(
                        ExplanatoryValues = dat_adults[,i],
                        Earnings = dat_adults[,15]
                ) %>%
                summarise(
                        TotalWorkers = n()
                ) %>%
                mutate(
                        ExplanatoryVariable = colnames(dat_adults)[i],
                        ExplanatoryValues = as.factor(ExplanatoryValues)
                )

        dat_adults_supertotals <- dat_adults_supertotals %>%
                rbind(
                        tmp
                )

}

dat_adults_supertotals %>%
        ggplot(aes(x = ExplanatoryValues)) +
        geom_bar(aes(weight = TotalWorkers, group = Earnings, fill = Earnings), position = "dodge") +
        theme_moss() +
        theme(
                panel.background = element_rect(fill = "white", color = "white"),
                plot.background = element_rect(fill = "white"),
                strip.background = element_rect(color = "white"),
                axis.text.x = element_text(angle = 60, hjust = 1)
        ) +
        labs(

        ) +
        facet_wrap(. ~ ExplanatoryVariable, scales = "free", ncol = 1)

ggsave("001-AdultEarnings/testimage.png", device = "png", width = 20, height = 30, units = "in")

