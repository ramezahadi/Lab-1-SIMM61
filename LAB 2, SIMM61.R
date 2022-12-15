#lab 2 SIMM61

#library
library(psych)
library(tidyverse)
library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)

#load data 
datafile_A <- read.csv('https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Home_assignment/surgery_data_1.csv')
datafile_B <- read.csv('https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Home_assignment/surgery_data_2.csv')
summary(datafile_A)

#mutating 
datafile_A <- datafile_A %>% 
  mutate(ID = as.factor(ID),
         sex = factor(recode(sex,
                             "male" = "0",
                             "female" = "1",
                             "woman" = "1")),
         sex = as.numeric(sex))

#random intercept model
random_inter1 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = datafile_A)

summary(random_inter1)

#variance explained
RSS <- sum((datafile_A$pain - predict(random_inter1))^2)
RSS

tss_modmean <- lm(pain ~ 1, data = datafile_A)
TSS <- sum((datafile_A$pain - predict(tss_modmean))^2)
r2 = 1 - (RSS/TSS)

#conditional r2, r2c = 0.46
r.squaredGLMM(random_inter1)

#mutating B 
datafile_B <- datafile_B %>% 
  mutate(ID = as.factor(ID),
         sex = factor(recode(sex,
                             "male" = "0",
                             "female" = "1")),
         sex = as.numeric(sex))

datafile_B <- datafile_B %>%
  mutate(predpain = 4.305 + (age * (-0.06280)) + (sex * (0.22924)) + (STAI_trait * (-0.02375)) + (pain_cat * (0.08389)) + (mindfulness * (-0.22706)) + (cortisol_serum * (0.51030)))


#variance explained, B 

random_inter2 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = datafile_B)
RSSB <- sum((datafile_B$pain - predict(random_inter2))^2)
RSSB

tss_modmeanB <- lm(pain ~ 1, data = datafile_B)

TSSB <- sum((datafile_B$pain - predict(tss_modmeanB))^2)
r2B = 1 - (RSSB/TSSB)


#most influental predictor 
random_slopint = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), data = datafile_A)


#visualization 
datafile_A = datafile_A %>%
  mutate(pred_slopint = predict(random_slopint))

datafile_A %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
                                                       aes(y = pred_slopint, x = cortisol_serum)) + facet_wrap(~hospital,
                                                                                                         ncol = 2)

#create table 
install.packages('modelsummary')
library(modelsummary)

models <- list(random_inter1 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = datafile_A))

modelsummary(random_inter1)




