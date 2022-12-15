#SIMM61
#RAMEZ AHADI
#LAB1 


library(tidyverse)
library(pscl)
library(lmtest)

#loading & exploring data TITANIC 
titanic_data = read.csv("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Home_assignment/Titanic%20-%20training%20set.csv")

titanic_data %>% 
  summary()
  
#set appropriate class, remove missing values from Age
titanic_data2 <- titanic_data %>% 
  mutate(Parch = as.factor(Parch),
         SibSp = as.numeric(SibSp))

#rearrange variables
titanic_data2 <- titanic_data2 %>% 
  mutate(Pclass = factor(recode(Pclass,
                                "1" = "First Class",
                                "2" = "Second Class", 
                                "3" = "Third Class")),
         Parch = fct_collapse(Parch,
                              no_par_child = c("0"),
                              one_par_child = c("1"),
                              two_par_child = c("2"),
                              other = c("3", "4", "5", "6")),
         survival_status = factor(recode(Survived,
                               "1" = "survived",
                               "0" = "not_survived")),
         Embarked = factor(recode(Embarked,
                              "S" = "Southampton",
                              "C" = "Cherbourg",
                              "Q" = "Queenstown")),
         Age = replace(Age,
                       Age == "n/a", NA)) %>% 
  drop_na()

#summary
titanic_data2 %>% 
  summary()

#logistic regression model for survivers, examining relations 
#examining if Age has an effect
titanic_data2 %>% 
  group_by(survival_status) %>% 
  summarize(mean = mean(Age, na.rm = TRUE),
            sd = sd(Age, na.rm = TRUE))

titanic_data2 %>% 
  ggplot () + 
    aes(y = Age, x = survival_status) + 
    geom_violin(aes(fill = survival_status)) +
    geom_boxplot() + 
    geom_jitter(width = 0.1)

#examining if gender has an effect, female tend to survive more
titanic_data2 %>% 
  group_by(Survived, Sex) %>%
  summarize(n = n()) %>% 
  spread(Survived, n)

titanic_data2 %>% 
  ggplot() + 
  aes(x = Sex, fill = survival_status) +
  geom_bar()

#saved to lab1, shows that.. 
titanic_data2 %>% 
  group_by(survival_status, Parch) %>%
  summarize(n = n()) %>% 
  spread(survival_status, n)

titanic_data2 %>% 
  ggplot() + 
  aes(x = Parch, fill = survival_status) +
  geom_bar()

#saved to lab1, shows that 1 class tend to survive more
titanic_data2 %>% 
  ggplot() + 
    aes(x = Pclass, fill = survival_status) +
    geom_bar()

#saved to lab1, number of siblings doesn't add to estimation 
titanic_data2 %>% 
  ggplot() + 
  aes(x = SibSp, fill = survival_status) +
  geom_bar()

#saved to lab1, shows no important distinction 
titanic_data2 %>%
  filter(!is.na(Embarked)) %>% 
    ggplot() + 
    aes(x = Embarked, fill = survival_status) +
    geom_bar()

#Sex & Pclass to numeric, for better fit to model 
titanic_data2 <- titanic_data2 %>%
  mutate(Sex = factor(recode(Sex,
                             "male" = "1",
                             "female" = "2")),
         Sex = as.numeric(Sex), na.rm = TRUE,
         
         Pclass = as.numeric(Pclass))
summary(titanic_data2)

#model 1, regression
mod1 = glm(Survived ~ Age +
             Sex +
             Pclass + 
             Parch + 
             Embarked +
             SibSp, 
           family = binomial(), data = titanic_data2)
summary(mod1)

#model 2, regression 
mod2 = glm(Survived ~ Age +
             Sex +
             Pclass + 
             SibSp, 
           family = binomial(), data = titanic_data2)
summary(mod2)

#McFadden R^2
pR2(mod2)

#-2LL, deviance 
pR2(mod2)["llh"] * - 2

#testing model accuracy 
titanic_data2 = titanic_data2 %>%
  mutate(pred_mod2 = predict(mod2)) %>%
  mutate(pred_mod2 = case_when(pred_mod2 <= 0 ~ "not_survived",
                               pred_mod2 > 0 ~ "survived"))

#coding correct guesses
titanic_data2 = titanic_data2 %>%
  mutate(correct_prediction = case_when(pred_mod2 == survival_status ~ "correct",
                                        pred_mod2 != survival_status ~ "incorrect"))

#categorization rate overall, 80,3 %
titanic_data2 %>%
  group_by(correct_prediction) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))

#null model 
null_mod = glm(Survived ~ 1, family = binomial(), data = titanic_data2)
  summary(null_mod)

  head(predict(null_mod))
  
#odds of the test group are lower than the odds of the reference group

#percentage of survival 
titanic_data2 %>% 
  group_by(survival_status) %>% 
  summarize(count = n()) %>% 
  mutate(freq = count / sum(count))

#crosstab of survival_status and predicted values 
titanic_data2 %>% 
  group_by(survival_status, pred_mod2) %>% 
  summarize(n = n()) %>% 
  spread(survival_status, n)

#categorized as surviving, result 73 % 
titanic_data2 %>% 
  filter(survival_status == "survived") %>%
  group_by(correct_prediction) %>% 
  summarise(count = n()) %>% 
  mutate(freq = count / sum(count))

#categorized as not surviving, 85 % 
titanic_data2 %>% 
  filter(survival_status == "not_survived") %>%
  group_by(correct_prediction) %>% 
  summarise(count = n()) %>% 
  mutate(freq = count / sum(count))

#testing if model is better than null model. It is, and also stat.significant 
lrtest(null_mod, mod2)

#AIC levels show that model with predictors is better as well 
AIC(null_mod, mod2)


#creating tables for report 
install.packages('modelsummary')
library(modelsummary)

models <- list("models 1" <- glm(Survived ~ Age + Sex + Pclass + SibSp,
                             family = binomial(), data = titanic_data2),
               "models 2" <- glm(Survived ~ 1, family = binomial(), data = titanic_data2))
modelsummary(models)


#relative contribution of predictors
  library(dominanceanalysis)
dominance_mod2 <- dominanceAnalysis(mod2)  
  contributionByLevel(dominance_mod2, fit.functions ="r2.m")

  library(reshape2)
plot(dominance_mod2, which.graph = "conditional", fit.function = "r2.m")  

averageContribution(dominance_mod2,fit.functions = "r2.m")
plot(dominance_mod2, which.graph ="general",fit.function = "r2.m") + coord_flip()

