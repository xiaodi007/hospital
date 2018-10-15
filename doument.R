library(dplyr)
library(readr)
library(ggplot2)
library(readr)
paper_search <- read_csv("paper_search.csv", 
                         col_types = cols(id_index = col_character(), 
                                          id_index_1 = col_character()))
# 体重身高有异常值手动修改
Data <- paper_search %>% 
  filter(CysC < 50,
         CRE > 2 ) %>% 
  select(-c(id_index_1)) %>% 
  mutate(bmi = weight / (height ^ 2)) %>% 
  mutate(Y_CLASS = case_when(
    ACR >= 300            ~ "大量白蛋白尿组",
    ACR >= 30 & ACR < 300 ~ "微量白蛋白尿组",
    ACR <  30             ~ "正常白蛋白尿组",
    TRUE                  ~ "未知"
  )) %>% 
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

# 患者共
Data %>% summarise(n())


#其中男 例，女 例
Data %>% group_by(gender) %>% summarise(n())

#平均 岁
xx <- t.test(Data$age)
xx$conf.int
xx$estimate
#病程平均   年
t.test(Data$courseofdiabetes)

#BMI
t.test(Data$bmi)

# 各组有多少人
xx <- Data %>% group_by(Y_CLASS) %>% summarise(n())
xx

# 正常白蛋白尿组
ZC_DATA <- Data %>% 
  filter(Y_CLASS == '正常白蛋白尿组')
Data %>% filter(Y_CLASS == '正常白蛋白尿组') %>% select(CysC) %>% .[,1]
##Cys
t.test(ZC_DATA$CysC)

group_index <- Data %>% 
  group_by(Y_CLASS) %>% 
  summarise(CysC_mean = mean(CysC), CysC_std = 1.96 * sd(CysC), 
            UREA_mean = mean(UREA), UREA_std = 1.96 * sd(UREA),
            CRE_mean = mean(CRE),   CRE_std = 1.96 * sd(CRE),
            GFR_mean = mean(GFR),   GFR_std = 1.96 * sd(GFR)
  ) %>% 
  ungroup()


T.test <- function(Data, Class, Col){
  DATA <- Data %>% filter(Y_CLASS == Class) %>% as.data.frame(.) %>% .[, quo_name(Col)]
  print(t.test(DATA))
  
  data.frame(mean = t.test(DATA)$estimate,
             conf = t.test(DATA)$estimate - as.numeric(t.test(DATA)$conf.int)[1],row.names = NULL)
  
}
Y_CLASS_name <- distinct(Data, Y_CLASS)$Y_CLASS
ww <- T.test(Data = Data, Class = '大量白蛋白尿组', Col = 'CysC')

Y_CLASS_name <- distinct(Data, Y_CLASS) %>% mutate(num = 1L)
select_name <- tibble(name = c('CysC', 'UREA', 'CRE', 'ACR'), num=1L)
Y_CLASS_name %>% left_join(select_name, by = c("num"))
xxx <- Data %>% 
  filter(Y_CLASS == '大量白蛋白尿组')
  
z <- t.test(xxx$GFR, mu=30)
z$null.value
#CysC GFR
CysC_GFR_plot <- Data %>% 
  #filter(NBDBJGBZ< 3)
  select(CysC, GFR, GFR_CLASS)

ggplot(CysC_GFR_plot, aes(CysC, GFR, colour = GFR_CLASS)) + 
  geom_point()
