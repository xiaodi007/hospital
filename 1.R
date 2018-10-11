library(dplyr)
library(readr)
library(ggplot2)

patient_index_data <- read_csv("./hospital.csv")


###NBDBJGBZ CysC MTP24H
patient_data <- patient_index_data %>% 
  filter(is.na(age) != T, 
         is.na(gender) != T, 
         is.na(MTP24H) != T, 
         is.na(CysC) != T, 
         is.na(NBDBJGBZ) != T, 
         is.na(CRE) != T,
         CysC < 50,
         CRE > 2 )%>% 
  distinct() %>% 
  mutate(ageEffect = if_else(gender == '男', 1, 0.742)) %>% 
  mutate(GFR =  30849 * (CRE^(-1.154)) * (age^(-0.203)) * ageEffect) %>% 
  mutate(GFR_CLASS = case_when(
    GFR >= 90 ~ "正常",
    GFR >= 60 & GFR <90 ~ "轻度下降",
    GFR >= 30 & GFR <60 ~ "中度下降",
    GFR >= 15 & GFR <30 ~ "重度下降",
    GFR <15             ~ "肾衰竭",
    TRUE                ~ "未知"
    
  ))

patient_data %>% group_by(GFR_CLASS) %>% summarise(n())

#CysC GFR
CysC_GFR_plot <- patient_data %>% 
  #filter(NBDBJGBZ< 3)
  select(CysC, GFR, GFR_CLASS)

ggplot(CysC_GFR_plot, aes(CysC, GFR, colour = GFR_CLASS)) + 
  geom_point()



#NBDBJGBZ GFR
NBDBJGBZ_GFR_plot <- patient_data %>% 
  select(NBDBJGBZ, GFR, GFR_CLASS)

ggplot(NBDBJGBZ_GFR_plot, aes(NBDBJGBZ, GFR, colour = GFR_CLASS)) + 
  geom_point() 


#MTP24H GFR
MTP24H_GFR_plot <- patient_data %>% 
  select(MTP24H, GFR, GFR_CLASS)

ggplot(MTP24H_GFR_plot, aes(MTP24H, GFR, colour = GFR_CLASS)) + 
  geom_point()


#NBDBJGBZ CysC
NBDBJGBZ_CysC_plot <- patient_data %>% 
  select(NBDBJGBZ, CysC, GFR_CLASS)

ggplot(NBDBJGBZ_CysC_plot, aes(NBDBJGBZ, CysC, colour = GFR_CLASS)) + 
  geom_point()

#CysC GFR lm

NG_lm <- lm(GFR~NBDBJGBZ, data = NBDBJGBZ_GFR_plot)
summary(NG_lm)
