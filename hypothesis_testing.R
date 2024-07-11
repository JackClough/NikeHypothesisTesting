setwd("C:/Users/hauer/OneDrive - The University of Montana/msba/2021zfall/adv_marketing/hypothesis_testing_hw_20211114")

# import the libraries
library(psych)
library(GGally)
library(corrplot)
library(ggplot2)
library(car)
library(tidyverse)
library(skimr)
library(corrplot)

# import the data
nike_survey <- read_csv("Nike.csv", 
                        show_col_types = FALSE)

colnames(nike_survey)

motivation <- nike_survey %>%
  select("q8a_Changingscenery",                                      
         "q8b_Onewithnature_thegreatoutdoors",                       
         "q8c_Solitudegettingaway",                                  
         "q8d_Adventureadrenaline",                                  
         "q8e_Personalchallenge_thelastfrontier",         
         "q8f_Fitnessgoodworkout",                                   
         "q8g_Meetlikemindedpeople",                                 
         "q9a_Gettinglost",                                          
         "q9b_Possibilityofinjury",                                  
         "q9c_Badchangingweather",                                   
         "q9d_Dangerouswildlifeencounter",                           
         "q9e_Dangerouspeopleencounter") %>% 
  mutate(q8a_Changingscenery = replace_na(q8a_Changingscenery, mean(q8a_Changingscenery, na.rm =TRUE)),                                      
         q8b_Onewithnature_thegreatoutdoors = replace_na(q8b_Onewithnature_thegreatoutdoors, mean(q8b_Onewithnature_thegreatoutdoors, na.rm =TRUE)),                       
         q8c_Solitudegettingaway = replace_na(q8c_Solitudegettingaway, mean(q8c_Solitudegettingaway, na.rm =TRUE)),                                  
         q8d_Adventureadrenaline = replace_na(q8d_Adventureadrenaline, mean(q8d_Adventureadrenaline, na.rm =TRUE)),                                  
         q8e_Personalchallenge_thelastfrontier = replace_na(q8e_Personalchallenge_thelastfrontier, mean(q8e_Personalchallenge_thelastfrontier, na.rm =TRUE)),         
         q8f_Fitnessgoodworkout = replace_na(q8f_Fitnessgoodworkout, mean(q8f_Fitnessgoodworkout, na.rm =TRUE)),                                   
         q8g_Meetlikemindedpeople = replace_na(q8g_Meetlikemindedpeople, mean(q8g_Meetlikemindedpeople, na.rm =TRUE)),                                 
         q9a_Gettinglost = replace_na(q9a_Gettinglost, mean(q9a_Gettinglost, na.rm =TRUE)),                                         
         q9b_Possibilityofinjury = replace_na(q9b_Possibilityofinjury, mean(q9b_Possibilityofinjury, na.rm =TRUE)),                                  
         q9c_Badchangingweather = replace_na(q9c_Badchangingweather, mean(q9c_Badchangingweather, na.rm =TRUE)),                                   
         q9d_Dangerouswildlifeencounter = replace_na(q9d_Dangerouswildlifeencounter, mean(q9d_Dangerouswildlifeencounter, na.rm =TRUE)),                           
         q9e_Dangerouspeopleencounter = replace_na(q9e_Dangerouspeopleencounter, mean(q9e_Dangerouspeopleencounter, na.rm =TRUE))) %>% 
  rowwise() %>% 
  mutate(getting_away = mean(c(q8a_Changingscenery, q8b_Onewithnature_thegreatoutdoors, q8c_Solitudegettingaway)),
         adventure_challenge = mean(c(q8d_Adventureadrenaline, q8e_Personalchallenge_thelastfrontier, q8g_Meetlikemindedpeople)),
         lost_injured_weather = mean(c(q9a_Gettinglost, q9b_Possibilityofinjury, q9c_Badchangingweather)),
         fitness = q8f_Fitnessgoodworkout,
         wildlife = q9d_Dangerouswildlifeencounter,
         people = q9e_Dangerouspeopleencounter) %>% 
  select(getting_away, adventure_challenge, fitness, lost_injured_weather, wildlife, people)


motivation <- tibble('getting_away'= motivation$getting_away, 
                     'adventure_challenge'= motivation$adventure_challenge, 
                     'fitness'=motivation$fitness, 
                     'lost_injured_badweather' = motivation$lost_injured_weather,
                     'dangerous_wildlife' = motivation$wildlife,
                     'dangerous_people' = motivation$people)

skim(motivation)

mot_cor_matrix <- cor(motivation)
mot_cor_matrix

# messing around with the correlation matrix output
GGally::ggcorr(mot_cor_matrix,
               geom = 'blank',
               label = TRUE, 
               hjust = .75)+
  geom_point(size=10,aes(color=coefficient >0, 
                         alpha = abs(coefficient) > 0.5))+
  ggtitle('Correlation Matrix of Motivation')+
  scale_alpha_manual(values = c('TRUE' = 0.25, 'FALSE' = 0))+
  guides(color = 'none', alpha = 'none')


corrplot(mot_cor_matrix, method = 'color', order='FPC', type = 'lower', diag = FALSE)

corrplot.mixed(mot_cor_matrix, lower = 'number', upper = 'pie', order='FPC')

corrplot(mot_cor_matrix, type = 'lower',method = 'pie', order='FPC', addCoef.col = 'black', number.cex = .8)

dis_mot <- tibble(trail_dist = nike_survey$q1_Runtrail,
                  'getting_away'= motivation$getting_away, 
                  'adventure_challenge'= motivation$adventure_challenge, 
                  'fitness'=motivation$fitness, 
                  'lost_injured_badweather' = motivation$lost_injured_badweather,
                  'dangerous_wildlife' = motivation$dangerous_wildlife,
                  'dangerous_people' = motivation$dangerous_people,
                  gender=nike_survey$Gender)

pairs.panels(dis_mot)

male_mot <- dis_mot %>% 
  filter(gender==1) %>% 
  select(trail_dist, getting_away,
         adventure_challenge, fitness,
         lost_injured_badweather, dangerous_wildlife,
         dangerous_people)

pairs.panels(male_mot, main="Male Pairs Panel")

female_mot <- dis_mot %>% 
  filter(gender==2) %>% 
  select(trail_dist, getting_away, adventure_challenge, fitness,
         lost_injured_badweather, dangerous_wildlife, dangerous_people)

pairs.panels(female_mot, main="Female Pairs Panel")

male_lm <- lm(trail_dist ~ adventure_challenge, data = male_mot)
summary(male_lm)

male_fit <- lm(trail_dist ~ fitness, data = male_mot)
summary(male_fit)

female_fit <- lm(trail_dist ~ fitness, data = female_mot)
summary(female_fit)

dis_lm <- lm(trail_dist ~ adventure_challenge + fitness + gender, data=dis_mot)
summary(dis_lm)
plot(effects::allEffects(dis_lm, residual=T), main="MLR model with Adv/Challenge, Fitness and Gender")

dis_mot %>% 
  na.omit() %>% 
  ggplot() +
    geom_jitter(aes(x=adventure_challenge, y=trail_dist, 
                    color = factor(gender)), size = 2) +
    scale_color_manual(values = c('lightblue3', 'indianred1'), breaks = c('male', 'female')) +
    geom_smooth(data = male_lm, aes(x = adventure_challenge, y = trail_dist),
                method = "lm", color = "lightblue3", alpha = .15) +
    geom_smooth(data = female_lm, aes(x = adventure_challenge, y = trail_dist),
                method = "lm", color = "indianred1", alpha = .215) +
    theme_classic() +
    labs(x = "Adventure/Challenge", 
         y = "Trail Running Dist. Avg. per Week (miles)",
         title = "Men and Women's Trail Running to Adv./Challenge Value")
    
 
    