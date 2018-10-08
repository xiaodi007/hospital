library(dplyr)
library(readr)
library(ggplot2)
patient_index_data <- read_csv("F:/Hospital/patient_index_data.csv")

###NBDBJGBZ CysC MTP24H
patient_data <- patient_index_data %>% 
  select(id_card, MTP24H, NBDBJGBZ) %>% 
  filter(is.na(MTP24H) != TRUE, is.na(NBDBJGBZ) != TRUE)

ggplot(patient_data, aes(MTP24H, NBDBJGBZ)) + 
  geom_point() #+ 
ylim(0, 22) + xlim(0, 15)#+
geom_smooth()
