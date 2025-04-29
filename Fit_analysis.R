library('ggplot2')
source("C:/Users/lucij/Desktop/University/Leiden/Reinforcement Learning/helper_functions.R")
library(tidyr)  
library(dplyr)  

rawdata <- read.csv("C:/Users/lucij/Desktop/University/Leiden/Reinforcement Learning/dataset18.csv")
rawdata

View(rawdata)
nrow(rawdata)

rawdata$condition <- as.factor(rawdata$condition)
rawdata$correct <- as.factor(rawdata$correct)


ggplot(rawdata, aes(x = rt, fill = correct)) +
  geom_histogram(alpha = 0.5)

ggplot(rawdata, aes(x = rt, fill = condition)) +
  geom_histogram(alpha = 0.5)


#paired t-test over RT data by condition
#aggregating the data by subject and condition
aggregated_data <- rawdata %>%
  group_by(ID, condition) %>%
  summarise(median_rt = median(rt, na.rm = TRUE)) %>%
  ungroup()

t_test_result <- t.test(
  aggregated_data$median_rt[aggregated_data$condition == 1],  
  aggregated_data$median_rt[aggregated_data$condition == 2],  
  paired = TRUE
)

print(t_test_result) #p < .001

#accuracy by condition
rawdata$condition <- as.numeric(rawdata$condition)
rawdata$correct <- as.numeric(rawdata$correct)
accuracy_data <- rawdata %>%
  group_by(ID, condition) %>%
  summarise(mean_accuracy = mean(correct, na.rm = TRUE)) %>%
  ungroup()

accuracy_t_test <- t.test(
  accuracy_data$mean_accuracy[accuracy_data$condition == 1],  
  accuracy_data$mean_accuracy[accuracy_data$condition == 2],  
  paired = TRUE
)

print(accuracy_t_test) #p = 0.002


### fit model for each participant x condition

typeof(rawdata)
class(rawdata)

rawdata <- as.data.frame(rawdata)
class(rawdata$ID)
class(rawdata$correct)
class(rawdata$rt)
class(rawdata$condition)

rawdata$condition <- as.numeric(rawdata$condition)
rawdata$correct <- as.numeric(rawdata$correct)
participants <- unique(rawdata$ID)
rawdata$ID = as.numeric(rawdata$ID)

View(rawdata)

rawdata$correct = rawdata$correct - 1

accuracy_perct <- rawdata %>% group_by(condition) %>% 
  summarise(mean_age=mean(correct),
            .groups = 'drop')
accuracy_perct  #accuracy for first condition - 0.915, accuracy for second condition - .939


bias_z = NDT_ter = threshold_a = drift_rate_v = data.frame(matrix(NA, 12, 2))



#start model fitting for each participant x condition using fit_data() function
for (i in 1:12) {
  for (j in 1:2) {
    
    fit = fit_data(
      subset(
        rawdata, 
        rawdata$ID == i & rawdata$condition == j
      )
    )
    
    bias_z[i, j] = as.numeric(fit[1])
    NDT_ter[i, j] = as.numeric(fit[2])
    threshold_a[i, j] = as.numeric(fit[3])
    drift_rate_v[i, j] = as.numeric(fit[4])
    
  }
}
### which parameter(s) differ between conditions

colnames(bias_z) <- c("stress", "no_stress")
colnames(NDT_ter) <- c("stress", "no_stress")
colnames(threshold_a) <- c("stress", "no_stress")
colnames(drift_rate_v) <- c("stress", "no_stress")


#bias z
bias_mean_stress <- mean(bias_z$stress)
bias_sd_stress <- sd(bias_z$stress)

bias_mean_no_stress <- mean(bias_z$no_stress)
bias_sd_no_stress <- sd(bias_z$no_stress)

bias_ttest <- t.test(bias_z$stress, bias_z$no_stress, paired = TRUE)

bias_ttest #p = .5991
 
bias_long <- bias_z %>%
  pivot_longer(cols = c("stress", "no_stress"), names_to = "Condition", values_to = "Bias") %>%
  mutate(Participant = rep(1:nrow(bias_z), each = 2))

bias_long$Condition <- factor(bias_long$Condition, levels = c("stress", "no_stress"))

ggplot(bias_long, aes(x = Condition, y = Bias, color = Condition)) +
  geom_boxplot(fill = NA, width = 0.5, size = 1.0, fatten = 1.5) +
  geom_point(size = 2) +
  geom_line(aes(group = Participant), color = "black", linetype = "33", alpha = 0.4) +
  scale_color_manual(values = c("stress" = "#002060", "no_stress" = "#E97132")) +
  scale_x_discrete(labels = c("stress" = "Exam Stress", "no_stress" = "No Stress")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 14, family = "serif", face = "bold"),  
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        )+
  labs(x = "", y = "") 

#Non-decision time
ndt_mean_stress <- mean(NDT_ter$stress)
ndt_sd_stress <- sd(NDT_ter$stress)

ndt_mean_no_stress <- mean(NDT_ter$no_stress)
ndt_sd_no_stress <- sd(NDT_ter$no_stress)

ndt_ttest <- t.test(NDT_ter$stress, NDT_ter$no_stress, paired = TRUE)

ndt_ttest #p = .6255

ndt_long <- NDT_ter %>%
  pivot_longer(cols = c("stress", "no_stress"), names_to = "Condition", values_to = "NDT") %>%
  mutate(Participant = rep(1:nrow(NDT_ter), each = 2))

ndt_long$Condition <- factor(ndt_long$Condition, levels = c("stress", "no_stress"))

ggplot(ndt_long, aes(x = Condition, y = NDT, color = Condition)) +
  geom_boxplot(fill = NA, width = 0.5, size = 1.0, fatten = 1.5) +
  geom_point(size = 2) +
  geom_line(aes(group = Participant), color = "black", linetype = "33", alpha = 0.4) +
  scale_color_manual(values = c("stress" = "#002060", "no_stress" = "#E97132")) +
  scale_x_discrete(labels = c("stress" = "Exam Stress", "no_stress" = "No Stress")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 14, family = "serif", face = "bold"),  
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
  )+
  labs(x = "", y = "") 


#threshold a
a_mean_stress <- mean(threshold_a$stress)
a_sd_stress <- sd(threshold_a$stress)

a_mean_no_stress <- mean(threshold_a$no_stress)
a_sd_no_stress <- sd(threshold_a$no_stress)

a_ttest <- t.test(threshold_a$stress, threshold_a$no_stress, paired = TRUE)

a_ttest #p < .001

a_long <- threshold_a %>%
  pivot_longer(cols = c("stress", "no_stress"), names_to = "Condition", values_to = "a") %>%
  mutate(Participant = rep(1:nrow(threshold_a), each = 2))

a_long$Condition <- factor(a_long$Condition, levels = c("stress", "no_stress"))

ggplot(a_long, aes(x = Condition, y = a, color = Condition)) +
  geom_boxplot(fill = NA, width = 0.5, size = 1.0, fatten = 1.5) +
  geom_point(size = 2) +
  geom_line(aes(group = Participant), color = "black", linetype = "33", alpha = 0.4) +
  scale_color_manual(values = c("stress" = "#002060", "no_stress" = "#E97132")) +
  scale_x_discrete(labels = c("stress" = "Exam Stress", "no_stress" = "No Stress")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 14, family = "serif", face = "bold"),  
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
  )+
  labs(x = "", y = "") 

#drift_rate_v

drift_mean_stress <- mean(drift_rate_v$stress)
drift_sd_stress <- sd(drift_rate_v$stress)

drift_mean_no_stress <- mean(drift_rate_v$no_stress)
drift_sd_no_stress <- sd(drift_rate_v$no_stress)

drift_ttest <- t.test(drift_rate_v$stress, drift_rate_v$no_stress, paired = TRUE)

drift_ttest #p < .5558

drift_long <- drift_rate_v %>%
  pivot_longer(cols = c("stress", "no_stress"), names_to = "Condition", values_to = "Drift") %>%
  mutate(Participant = rep(1:nrow(drift_rate_v), each = 2))

drift_long$Condition <- factor(drift_long$Condition, levels = c("stress", "no_stress"))

ggplot(drift_long, aes(x = Condition, y = Drift, color = Condition)) +
  geom_boxplot(fill = NA, width = 0.5, size = 1.0, fatten = 1.5) +
  geom_point(size = 2) +
  geom_line(aes(group = Participant), color = "black", linetype = "33", alpha = 0.4) +
  scale_color_manual(values = c("stress" = "#002060", "no_stress" = "#E97132")) +
  scale_x_discrete(labels = c("stress" = "Exam Stress", "no_stress" = "No Stress")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 14, family = "serif", face = "bold"),  
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
  )+
  labs(x = "", y = "") 
