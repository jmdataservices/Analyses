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
