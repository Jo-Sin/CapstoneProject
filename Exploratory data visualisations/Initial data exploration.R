library(tidyverse)

aggregated_data_full <- read_csv("Final Aggregated Data.csv")
summary(aggregated_data_full)
aggregated_data <- aggregated_data_full[, c(1:2, 4, 9, 11:22, 27:29)]
aggregated_data$`Intervention Type (general)` <- factor(aggregated_data$`Intervention Type (general)`, 
                                                        levels = c("Physical Barriers", "Encouraging Help Seeking", "Blue Lights", "Multiple Interventions"))
aggregated_data$`Intervention Type` <- factor(aggregated_data$`Intervention Type`)
aggregated_data$`Intervention Type` <- factor(aggregated_data$`Intervention Type`, levels = levels(aggregated_data$`Intervention Type`)[c(4,2,1,5,7,3,6)])
aggregated_data$`Reduction in Yearly Suicide Rate` <- aggregated_data$`Yearly Pre-Intervention Rate` - aggregated_data$`Yearly Post-Intervention Rate`
plot(aggregated_data$`Pre-Intervention Suicide`, aggregated_data$`Post-Intervention Suicide`)
plot(aggregated_data$`Reduction in Yearly Suicide Rate`)
hist(aggregated_data$`Reduction in Yearly Suicide Rate`)
qplot(factor(aggregated_data$Country), aggregated_data$`Reduction in Yearly Suicide Rate`)
qplot(factor(aggregated_data$`Location Type`), aggregated_data$`Reduction in Yearly Suicide Rate`)
qplot(factor(aggregated_data$`Suicide Method`), aggregated_data$`Reduction in Yearly Suicide Rate`)

qplot(factor(aggregated_data$`Restriction Type`), aggregated_data$`Reduction in Yearly Suicide Rate`)
# Reduced number in average yearly suicide after installing physical restriction over Restriction height
qplot(`Restriction Height (m)`, `Reduction in Yearly Suicide Rate`, data = aggregated_data[!aggregated_data$`Restriction Type` %in% c('None', 'Blocked Road Access'),], colour = `Restriction Type`) +
  labs(title = "Reduced number of yearly suicides after installing physical restriction over restriction height", y = "Reduction in number of yearly suicides post-intervention")
# Reduced number in average yearly suicide over yearly number of suicide pre-intervention
qplot(`Yearly Pre-Intervention Rate`, `Reduction in Yearly Suicide Rate`, data = aggregated_data, colour = `Intervention Type (general)`) +
  labs(title = "Reduced number of yearly suicides post-intervention versus yearly suicides pre-intervention", 
       x = "Yearly number of suicides pre-intervention", y = "Reduction in number of yearly suicides post-intervention")
qplot(`Pre-Intervention Years`, `Reduction in Yearly Suicide Rate`, data = aggregated_data)
# Reduced number in average yearly suicide over post-intervention years - higher reduction if post-intervention period is short - reliability issue
qplot(`Post-Intervention Years`, `Reduction in Yearly Suicide Rate`, data = aggregated_data) +
  labs(title = "Reduced number of yearly suicides post-intervention versus duration of post-intervention period", y = "Reduction in number of yearly suicides post-intervention")

aggregated_data$`Percentage Reduction in Yearly Suicide` <- 
  (aggregated_data$`Yearly Pre-Intervention Rate` - aggregated_data$`Yearly Post-Intervention Rate`)/aggregated_data$`Yearly Pre-Intervention Rate`
hist(aggregated_data$`Percentage Reduction in Yearly Suicide`)

pre_post_suicide <- aggregated_data %>% 
  select(`Location Type`, `Suicide Method`, `Intervention Type`, `Intervention Type (general)`, `Yearly Pre-Intervention Rate`, `Yearly Post-Intervention Rate`) %>% 
  gather(key = "Type of observation period", value = "Yearly Number of suicides", `Yearly Pre-Intervention Rate`, `Yearly Post-Intervention Rate`)
pre_post_suicide$`Type of observation period` <- 
  factor(ifelse(pre_post_suicide$`Type of observation period` == "Yearly Pre-Intervention Rate", "Pre-Intervention", "Post-Intervention"), 
         levels = c("Pre-Intervention", "Post-Intervention"))
qplot(`Type of observation period`, `Yearly Number of suicides`, data = pre_post_suicide, colour = `Suicide Method`)
qplot(`Type of observation period`, `Yearly Number of suicides`, data = pre_post_suicide, colour = `Location Type`, geom = "jitter")
# Suicide method
pre_post_suicide %>% ggplot(aes(`Type of observation period`, `Yearly Number of suicides`, colour = `Suicide Method`)) + geom_jitter(width = 0.1, size = 2) +
  labs(title = "Comparing yearly number of suicides pre- and post-intervention across different suicide methods")
# Location type
pre_post_suicide %>% ggplot(aes(`Type of observation period`, `Yearly Number of suicides`, colour = `Location Type`)) + geom_jitter(width = 0.1, size = 2) +
  labs(title = "Comparing yearly number of suicides pre- and post-intervention across different location types")

# Comparing average yearly number of suicides pre and post intervention across different intervention types
pre_post_suicide %>% ggplot(aes(`Type of observation period`, `Yearly Number of suicides`, colour = `Intervention Type (general)`)) + geom_jitter(width = 0.1, size = 2) +
  labs(title = "Comparing yearly number of suicides pre- and post-intervention across different intervention types")
qplot(`Type of observation period`, `Yearly Number of suicides`, data = pre_post_suicide, geom = c("boxplot", "jitter"))
qplot(aggregated_data$`Reduction in Yearly Suicide Rate`, binwidth = 5)
qplot(aggregated_data$`Reduction in Yearly Suicide Rate`, geom = "dotplot")
aggregated_data %>% ggplot(aes(x = `Reduction in Yearly Suicide Rate`, fill = `Location Type`)) + 
  geom_dotplot(binwidth = 1, dotsize = 1, stackgroups = TRUE, binpositions="all")
yheight <- max(dplyr::count(aggregated_data, `Reduction in Yearly Suicide Rate`,)["n"])
aggregated_data %>% ggplot(aes(x = `Reduction in Yearly Suicide Rate`)) + geom_histogram(colour = "black", fill = "lightgrey", binwidth = 1, center = 0.5) +
  scale_x_continuous(breaks = seq(-10,25,5)) + scale_y_continuous(breaks = seq(0,14,2), labels = seq(0,14,2))
aggregated_data %>% ggplot(aes(x = `Reduction in Yearly Suicide Rate`, fill = `Location Type`)) + 
  geom_dotplot(binwidth = 1, dotsize = 1, method = "histodot", origin = -7, stackgroups = TRUE, binpositions="all") + coord_fixed(ratio = 14) +
  scale_x_continuous(breaks = seq(-10,25,5)) + scale_y_continuous(limits=c(0, 1), expand = c(0, 0), breaks = seq(0, 1, 2/14), labels=seq(0,14,2))

# Distribution of reduced number of average yearly suicide
aggregated_data %>% ggplot(aes(x = `Reduction in Yearly Suicide Rate`, fill = `Intervention Type (general)`)) + 
  geom_dotplot(binwidth = 1, dotsize = 1, stackgroups = TRUE, binpositions="all")
qplot(`Intervention Type (general)`, `Reduction in Yearly Suicide Rate`, data = aggregated_data, colour = `Intervention Type`)
aggregated_data %>% ggplot(aes(`Intervention Type (general)`, `Reduction in Yearly Suicide Rate`, colour = `Intervention Type`)) + geom_jitter(width = 0.1, height = 0)

aggregated_data %>% ggplot(aes(x = `Reduction in Yearly Suicide Rate`, fill = `Intervention Type (general)`)) + 
  geom_dotplot(binwidth = 1, dotsize = 1, method = "histodot", origin = -7, stackgroups = TRUE, binpositions="all") + coord_fixed(ratio = 14) +
  scale_x_continuous(breaks = seq(-10,25,5)) + scale_y_continuous(limits=c(0, 1), expand = c(0, 0), breaks = seq(0, 1, 2/14), labels=seq(0,14,2)) +
  labs(title="Distribution of the reduced number of yearly suicides post-intervention", x ="Reduction in number of yearly suicides post-intervention")

aggregated_data %>% ggplot(aes(x = `Reduction in Yearly Suicide Rate`, fill = `Intervention Type`)) + 
  geom_dotplot(binwidth = 1, dotsize = 1, method = "histodot", origin = -7, stackgroups = TRUE, binpositions="all") + coord_fixed(ratio = 14) +
  scale_x_continuous(breaks = seq(-10,25,5)) + scale_y_continuous(limits=c(0, 1), expand = c(0, 0), breaks = seq(0, 1, 2/14), labels=seq(0,14,2)) +
  labs(title="Distribution of the reduced number of yearly suicides post-intervention", x ="Reduction in number of yearly suicides post-intervention")


write.csv(aggregated_data, "initial_data_exploration_aggregated.csv", row.names = FALSE)



aggregated_data <- read_csv("initial_data_exploration_aggregated.csv")
