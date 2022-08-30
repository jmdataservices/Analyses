dir <- gsub("/Analyses.*$", "/Analyses", getwd())

source(paste0(dir, "/dependencies.R"))

dat_adults <- read.csv(
        paste0(dir, "/001-AdultEarnings/src/adult.data"),
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

dat_test <- read.csv(
        paste0(dir, "/001-AdultEarnings/src/adult.test"),
        skip = 1
)

dat_adults <- dat_adults %>%
        mutate(
                across(
                        .cols = c(
                                EarningsClass,
                                MaritalStatus,
                                WorkClass,
                                Education,
                                Occupation,
                                RelationshipStatus,
                                Race,
                                Sex,
                                NativeCountry
                        ),
                        .fns = trimws
                ),
                HoursWorkedGroup = ifelse(
                        HoursWorked < 15, "Minimal", ifelse(
                                HoursWorked < 35, "PartTime", ifelse(
                                        HoursWorked < 45, "FullTime", ifelse(
                                                HoursWorked < 52, "OverTime", "XOverTime"
                                        )
                                )
                        )
                ),
                HoursWorkedGroup = factor(HoursWorkedGroup, levels = c(
                        "Minimal",
                        "PartTime",
                        "FullTime",
                        "OverTime",
                        "XOverTime"
                )),
                EarningsClassBinary = ifelse(EarningsClass == "<=50K", 0, 1)
        )

dat_adults %>%
        ggplot(aes(x = EarningsClass)) +
        geom_jitter(aes(y = EducationNum), width = 0.5, alpha = 0.05) +
        geom_boxplot(aes(y = EducationNum)) +
        theme_moss()

dat_adults %>%
        ggplot(aes(x = EarningsClass)) +
        geom_violin(aes(y = EducationNum), alpha = 0.05) +
        theme_moss()

# dat_adults %>%
#         select(
#                 Age,
#                 EducationNum,
#                 CapitalGain,
#                 CapitalLoss,
#                 HoursWorked
#         ) %>%
#         ggpairs()

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

plt_eda <- dat_adults_supertotals %>%
        ggplot(aes(x = ExplanatoryValues)) +
        geom_bar(aes(weight = TotalWorkers, group = Earnings, fill = Earnings), position = "dodge") +
        theme_moss() +
        theme(
                plot.title = element_blank(),
                plot.subtitle = element_blank(),
                axis.text.x = element_text(angle = 60, hjust = 1),
                legend.position = "top",
                legend.text = element_text(size = 18, face = "bold")
        ) +
        labs(
                title = "Large Bucket EDA Visual",
                subtitle = "Earnings by Various Metrics",
                x = "",
                y = "Total Workers",
                caption = "Figure 1"
        ) +
        scale_fill_manual(
                values = c(
                        ">50K" = "#628395",
                        "<=50K" = "#CF995F"
                )
        ) +
        facet_wrap(. ~ ExplanatoryVariable, scales = "free", ncol = 1)

## ggsave(plt_eda, filename = "001-AdultEarnings/testimage.png", device = "png", width = 20, height = 30, units = "in")

dat_adults_meanage <- dat_adults %>%
        group_by(
                EarningsClass
        ) %>%
        summarise(
                MeanAge = mean(Age, na.rm = T)
        )

plt_age <- dat_adults %>%
        ggplot(aes(x = EarningsClass)) +
        geom_jitter(aes(y = Age, color = EarningsClass), width = 0.2, alpha = 0.04) +
        geom_segment(data = dat_adults_meanage, aes(x = EarningsClass, xend = EarningsClass, y = 0, yend = MeanAge), color = "black", size = 1) +
        geom_hline(aes(yintercept = 0), color = "black", size = 1) +
        geom_point(data = dat_adults_meanage, aes(x = EarningsClass, y = MeanAge), color = "black", size = 10) +
        geom_point(data = dat_adults_meanage, aes(x = EarningsClass, y = MeanAge), color = "white", size = 8) +
        geom_text(data = dat_adults_meanage, aes(x = EarningsClass, y = MeanAge + 5, label = round(MeanAge, 1)), size = 5) +
        theme_moss() +
        theme(
                panel.grid.major.y = element_line(size = 0.75, linetype = "dashed", color = "grey90")
        ) +
        labs(
                x = "Earning Class",
                y = "Age"
        ) +
        scale_y_continuous(
                breaks = c(seq(0, 100, 10))
        ) +
        scale_color_manual(
                values = c(
                        ">50K" = "#628395",
                        "<=50K" = "#CF995F"
                )
        )

logisticmodel_age <- glm(
        data = dat_adults,
        family = "binomial",
        formula = EarningsClassBinary ~ Age
)

logisticmodel_age %>% summary()

logisticmodel_age_residuals <- data.frame(
        Res = logisticmodel_age$residuals,
        Rnk = c(1:length(logisticmodel_age$residuals))
) %>%
        ggplot() +
        geom_point(aes(x = Rnk, y = Res), alpha = 0.1) +
        geom_hline(aes(yintercept = 0), linetype = "dashed", size = 0.75, color = "firebrick1") +
        theme_moss() +
        theme(
                axis.text.x = element_blank()
        ) +
        labs(
                x = "",
                y = "Residuals"
        )

logisticmodel_age_residuals2 <- data.frame(
        Res = logisticmodel_age$residuals,
        Rnk = c(1:length(logisticmodel_age$residuals))
) %>%
        ggplot() +
        geom_histogram(aes(x = Res), bins = 100) +
        theme_moss() +
        theme(
                #axis.text.x = element_blank()
        ) +
        labs(
                x = "Residuals",
                y = ""
        )

dat_adults_meanedunum <- dat_adults %>%
        group_by(
                EarningsClass
        ) %>%
        summarise(
                MeanEducationNum = mean(EducationNum, na.rm = T)
        )

plt_edunum <- dat_adults %>%
        ggplot(aes(x = EarningsClass)) +
        geom_jitter(aes(y = EducationNum, color = EarningsClass), width = 0.2, alpha = 0.04) +
        geom_segment(data = dat_adults_meanedunum, aes(x = EarningsClass, xend = EarningsClass, y = 0, yend = MeanEducationNum), color = "black", size = 1) +
        geom_hline(aes(yintercept = 0), color = "black", size = 1) +
        geom_point(data = dat_adults_meanedunum, aes(x = EarningsClass, y = MeanEducationNum), color = "black", size = 10) +
        geom_point(data = dat_adults_meanedunum, aes(x = EarningsClass, y = MeanEducationNum), color = "white", size = 8) +
        geom_text(data = dat_adults_meanedunum, aes(x = EarningsClass, y = MeanEducationNum + 1, label = round(MeanEducationNum, 1)), size = 5) +
        theme_moss() +
        theme(
                panel.grid.major.y = element_line(size = 0.75, linetype = "dashed", color = "grey90")
        ) +
        labs(
                x = "Earning Class",
                y = "Education Level"
        ) +
        scale_y_continuous(
                breaks = c(seq(0, 20, 4))
        ) +
        scale_color_manual(
                values = c(
                        ">50K" = "#628395",
                        "<=50K" = "#CF995F"
                )
        )

plt_age_density <- dat_adults %>%
        ggplot(aes(x = Age)) +
        geom_density(aes(group = EarningsClass, fill = EarningsClass), alpha = 0.4) +
        geom_vline(data = dat_adults_meanage, aes(xintercept = MeanAge, color = EarningsClass), linetype = "dashed", size = 1) +
        theme_moss() +
        theme(
                panel.grid.major.x = element_line(size = 0.75, linetype = "dashed", color = "grey90"),
                axis.title.y = element_blank(),
                axis.text.y = element_blank()
        ) +
        labs(
                x = "Age"
        ) +
        scale_fill_manual(
                values = c(
                        ">50K" = "#628395",
                        "<=50K" = "#CF995F"
                )
        ) +
        scale_color_manual(
                values = c(
                        ">50K" = "#628395",
                        "<=50K" = "#CF995F"
                )
        )

plt_edunum_density <- dat_adults %>%
        ggplot(aes(x = EducationNum)) +
        geom_histogram(aes(group = EarningsClass, fill = EarningsClass), alpha = 0.4, bins = 16) +
        geom_vline(data = dat_adults_meanedunum, aes(xintercept = MeanEducationNum, color = EarningsClass), linetype = "dashed", size = 1) +
        theme_moss() +
        theme(
                panel.grid.major.x = element_line(size = 0.75, linetype = "dashed", color = "grey90"),
                axis.title.y = element_blank(),
                axis.text.y = element_blank()
        ) +
        labs(
                x = "Education Level"
        ) +
        scale_fill_manual(
                values = c(
                        ">50K" = "#628395",
                        "<=50K" = "#CF995F"
                )
        ) +
        scale_color_manual(
                values = c(
                        ">50K" = "#628395",
                        "<=50K" = "#CF995F"
                )
        ) +
        facet_wrap(.~EarningsClass, ncol = 1)

dat_adults_edunum_ords <- dat_adults %>%
        select(
                Education,
                EducationNum
        ) %>%
        distinct() %>%
        arrange(
                EducationNum
        )






