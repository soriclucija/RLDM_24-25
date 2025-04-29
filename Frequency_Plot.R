library('ggplot2')
source("C:/Users/lucij/Desktop/University/Leiden/Reinforcement Learning/helper_functions.R")


rawdata <- read.csv("C:/Users/lucij/Desktop/University/Leiden/Reinforcement Learning/dataset18.csv")
rawdata

View(rawdata)
nrow(rawdata)


rawdata$condition <- as.factor(rawdata$condition)
rawdata$correct <- as.factor(rawdata$correct)

#outliers (Berger & Kiefer, 2021)
library(dplyr)
mean_rt <- mean(rawdata$rt, na.rm = TRUE)
sd_rt <- sd(rawdata$rt, na.rm = TRUE)

lower_bound <- mean_rt - 2 * sd_rt
upper_bound <- mean_rt + 2 * sd_rt

filtered_data <- rawdata %>%
  filter(rt >= lower_bound & rt <= upper_bound)

nrow(filtered_data)


#plots

ggplot(filtered_data, aes(x = rt, fill = condition)) +
  geom_histogram(
    position = "identity",    
    binwidth = 30,         
    alpha = 0.7,
    color = "white"
  ) +
  scale_fill_manual(values = c("#002060", "#E97132")) +
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "none", 
    plot.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.title.x = element_text(face = "bold", family = "serif", margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", family = "serif", margin = margin(r = 15)),
    axis.text.x = element_text(size = 11, family = "serif", color = "black"),
    axis.text.y = element_text(size = 11, family = "serif", color = "black")
  ) +
  xlab("Reaction Time (ms)") +
  ylab("Frequency") +
  scale_x_continuous(n.breaks = 10)


