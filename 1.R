library(dplyr)
library(readr)
library(ggplot2)
patient_index_data <- read_csv("F:/Hospital/patient_index_data.csv")
patient_data <- patient_index_data %>% 
  select(CysC, MTP24H) %>% 
  filter(is.na(CysC) != TRUE, is.na(MTP24H) != TRUE)

ggplot(patient_data, aes(CysC, MTP24H)) + 
  geom_point() + 
  ylim(0, 22) + xlim(0, 15)#+
  geom_smooth()
