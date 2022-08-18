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
        ggplot() +
        geom_jitter(aes(x = EarningsClass, y = EducationNum), width = 0.5, alpha = 0.05) +
        theme_minimal()

table(dat_adults$Sex, dat_adults$EarningsClass)
table(dat_adults$Race, dat_adults$EarningsClass)
