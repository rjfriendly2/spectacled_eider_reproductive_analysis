###Create a figure that shows variables (spring temp, spring wind, winter ice days, and ice index) for each year

###Load Packages
library(tidyverse)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)


#Read in environmental data
env_data <- read.csv("nest_survival/data/ice_wx.csv", header = TRUE)

#This data doesn't have year 2020. This is because time passed while working on the ananlysis and decided
#to add another year in my study. I was new to data wrangling in r so I updated the sea ice and st paul weather data
#in excel and manually added year 2020 to the data. 

# 1. Lets edit this environmental dataframe to only variables of interest. Subset columns
env_data2 <- env_data[, c(2,5,10,14,19)]

# 2. Add values for year 2020. Refer back to two data sets. For sea ice, navigate to
#sea_ice_bs_core/output/sea_ice_vars_1979_2020.csv and
#weather_st_paul/output/wx_st_paul_1993_2020.csv. Find out the values for year 2020 and
#input them to env_data2
env2020 <- c(2020,0,2,44,151.1152824) #values corresponding to environemental data
env_data3 <- rbind(env_data2, env2020)
head(env_data3)
summary(env_data3)


# 3. Plot the variables for each year in 4 panels

env_long <- tidyr::pivot_longer(env_data3, cols = c("sxt","sxw","w95","wsi"), names_to = "variable")


################################ IGNORE THIS ###############################################################################
### Can get the plot but how do you title each plot in your four panels?
ggplot(env_long) +
  geom_col(aes(x = factor(year_winter), y = value, fill = variable), color = "black") +
  facet_wrap(~ variable, scales = "free", labeller = labeller(variable = c(sxt = "Spring Extreme Temperature Days",
                                                                           sxw = "Spring Extreme Wind Days",
                                                                           w95 = "Winter Ice Days",
                                                                           wsi = "Winter Ice Index")),
             strip.position = "left") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
  ) +
  theme(strip.placement = "outside") +
  scale_fill_manual(values = c("sxt" = "deepskyblue", "sxw" = "deepskyblue", "w95" = "deepskyblue", "wsi" = "deepskyblue")) +
  scale_x_discrete(breaks = seq(1993, 2020, by = 5)) +
  guides(fill = "none") +
  xlab("Year") + ylab("")
ggsave("sea_ice_bs_core/output/envrionmental_variables.jpg", width = 7, height = 5, dpi = 600)

### Same as above, this was a experiment section
ggplot(env_long) +
  geom_col(aes(x = factor(year_winter), y = value, fill = variable), color = "black") +
  facet_wrap(~ variable, scales = "free", labeller = labeller(variable = c(sxt = "Spring Extreme Temperature Days",
                                                                           sxw = "Spring Extreme Wind Days",
                                                                           w95 = "Winter Ice Days",
                                                                           wsi = "Winter Ice Index")),
             strip.position = "left") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
  ) +
  theme(strip.placement = "outside") +
  scale_fill_manual(values = c("sxt" = "palegreen", "sxw" = "olivedrab1", "w95" = "blue", "wsi" = "deepskyblue")) +
  scale_x_discrete(breaks = seq(1993, 2020, by = 5)) +
  guides(fill = "none") +
  xlab("Year") + ylab("")
ggsave("sea_ice_bs_core/output/envrionmental_variables2.jpg", width = 7, height = 5, dpi = 600)

###########################################################################################################################






############################################### WE WILL USE THIS METHODS INSTED ###########################################

###Look at individual graphs through ggplot with different colors
plot_1 <- ggplot(data = env_data3) +
  geom_col(data = env_data3, aes(x = factor(year_winter), y = w95), fill ="blue", color = "black") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylab("Winter Ice Days") + xlab("") +
  scale_x_discrete(breaks = seq(1993, 2020, by = 5)) +
  labs(title = "(A)")

plot_2 <- ggplot(data = env_data3) +
  geom_col(data = env_data3, aes(x = factor(year_winter), y = wsi), fill ="deepskyblue", color = "black") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylab("Winter Ice Index") + xlab("") +
  scale_x_discrete(breaks = seq(1993, 2020, by = 5)) +
  labs(title = "(B)")
  
plot_3 <- ggplot(data = env_data3) +
  geom_col(data = env_data3, aes(x = factor(year_winter), y = sxt), fill ="palegreen", color = "black") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylab("Spring Extreme Temperature Days") + xlab("") +
  scale_x_discrete(breaks = seq(1993, 2020, by = 5)) +
  labs(title = "(C)")

plot_4 <- ggplot(data = env_data3) +
  geom_col(data = env_data3, aes(x = factor(year_winter), y = sxw), fill ="olivedrab1", color = "black") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylab("Spring Extreme Wind Days") + xlab("") +
  scale_x_discrete(breaks = seq(1993, 2020, by = 5)) +
  labs(title = "(D)")


###Create bottom text for "xlab"
bottom_text <- textGrob("Year")


###use package pathcwork to combine them
grid.arrange(plot_1, plot_2, plot_3, plot_4, bottom = bottom_text)  
ggsave("sea_ice_bs_core/output/envrionmental_panels_colors.jpg", grid.arrange(plot_1, plot_2, plot_3, plot_4,
                                                                              bottom = bottom_text),
       width = 7, height = 5, dpi = 600)




###Look at individual graphs through ggplot with same colors
plot_5 <- ggplot(data = env_data3) +
  geom_col(data = env_data3, aes(x = factor(year_winter), y = w95), fill ="deepskyblue", color = "black") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylab("Winter Ice Days") + xlab("") +
  scale_x_discrete(breaks = seq(1993, 2020, by = 5)) +
  labs(title = "(A)")

plot_6 <- ggplot(data = env_data3) +
  geom_col(data = env_data3, aes(x = factor(year_winter), y = wsi), fill ="deepskyblue", color = "black") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylab("Winter Ice Index") + xlab("") +
  scale_x_discrete(breaks = seq(1993, 2020, by = 5)) +
  labs(title = "(B)")

plot_7 <- ggplot(data = env_data3) +
  geom_col(data = env_data3, aes(x = factor(year_winter), y = sxt), fill ="deepskyblue", color = "black") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylab("Spring Extreme Temperature Days") + xlab("") +
  scale_x_discrete(breaks = seq(1993, 2020, by = 5)) +
  labs(title = "(C)")

plot_8 <- ggplot(data = env_data3) +
  geom_col(data = env_data3, aes(x = factor(year_winter), y = sxw), fill ="deepskyblue", color = "black") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylab("Spring Extreme Wind Days") + xlab("") +
  scale_x_discrete(breaks = seq(1993, 2020, by = 5)) +
  labs(title = "(D)")


###use package pathcwork to combine them
grid.arrange(plot_5, plot_6, plot_7, plot_8, bottom = bottom_text)
ggsave("sea_ice_bs_core/output/envrionmental_panels_samec.jpg", grid.arrange(plot_5, plot_6, plot_7, plot_8,
                                                                             bottom = bottom_text),
       width = 7, height = 5, dpi = 600)
