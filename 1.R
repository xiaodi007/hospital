library(dplyr)
library(readr)
library(ggplot2)

patient_index_data <- read_csv("./hospital.csv")


###NBDBJGBZ CysC MTP24H
patient_data <- patient_index_data %>% 
  mutate(ageEffect = if_else(gender == '男', 1, 0.742)) %>% 
  mutate(GFR =  30849 * (CRE^(-1.154)) * (age^(-0.203)) * ageEffect) %>% 
  mutate(GFR_CLASS = case_when(
    GFR >= 90 ~ "正常",
    GFR >= 60 & GFR <90 ~ "轻度下降",
    GFR >= 30 & GFR <60 ~ "中度下降",
    GFR >= 15 & GFR <30 ~ "重度度下降",
    GFR <15             ~ "肾衰竭",
    TRUE                ~ "未知"
    
  ))



#CysC GFR
CysC_GFR_plot <- patient_data %>% 
  select(CysC, GFR, GFR_CLASS) %>% 
  filter(is.na(CysC) != TRUE, is.na(GFR) != TRUE)

ggplot(CysC_GFR_plot, aes(CysC, GFR, colour = GFR_CLASS)) + 
  geom_point() + 
xlim(0, 20) + ylim(0, 150) +
geom_smooth(aes(CysC, GFR))



#NBDBJGBZ GFR
NBDBJGBZ_GFR_plot <- patient_data %>% 
  select(NBDBJGBZ, GFR) %>% 
  filter(is.na(NBDBJGBZ) != TRUE, is.na(GFR) != TRUE)

ggplot(NBDBJGBZ_GFR_plot, aes(NBDBJGBZ, GFR)) + 
  geom_point() + 
  #xlim(0, 50) + 
    ylim(0, 150)+
  geom_smooth()



#MTP24H GFR
MTP24H_GFR_plot <- patient_data %>% 
  select(MTP24H, GFR) %>% 
  filter(is.na(MTP24H) != TRUE, is.na(GFR) != TRUE)

ggplot(MTP24H_GFR_plot, aes(MTP24H, GFR)) + 
  geom_point() + 
  #xlim(0, 50) + 
  ylim(0, 150)+
  geom_smooth()
