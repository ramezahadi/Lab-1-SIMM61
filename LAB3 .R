#LAB 3

#library
library(car)
library(dplyr)

library(GGally)
library(corrr)
library(ggcorrplot)

library(psych)
library(tidyverse)
library(MVN)
library(ICS)
library(factoextra)
library(paran)
library(FactoMineR)
library(EFAtools)
library(flextable)
library(modelsummary)

install.packages("EFAtools")
install.packages("FactoMineR")
install.packages("flextable")

#loading data 
ars <- read.csv("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Exercise_06%20-%20CFA%20and%20EFA/animalrights.csv")

#exploring basic descriptive statistics 
str(ars)
summary(ars)

ars %>%
  describe()

ars <- lapply(ars, as.numeric)
str(ars)

#removing missing values 
ars <- ars %>%
  drop_na()

#linear regression to control collinearity diagnosis
#maybe? 

#exploring correlations
ars_quest = ars %>%
  select(ar1:ar28)

correl = ars_quest %>%
  cor()

#visualizing corr structure
ggcorr(correl)

ggcorrplot(cor(ars_quest), p.mat = cor_pmat(ars_quest),
           hc.order = TRUE, type = "lower")

cor(ars_quest) %>%
  network_plot(min_cor = 0.6)

#factorability 
ars_mixed <- mixedCor(ars, c = 1:28, p = NULL)

ars_correl = ars_mixed$rho

str(hsq)

#bartlett's test ratio of observations/variables 
# 149/28 = 5,321 . More than 5 = Not Reliable 

#Kaiser-Meyer-Olkin (KMO) test; KMO is higher than 0.6 in all cases; its factorable
KMO(ars_correl)

#factor extraction, p-value = 0 
result <- mvn(ars[, 1:28], mvnTest = "hz")
result$multivariateNormality


#p-value < 0 
mvnorm.kur.test(na.omit(ars[, 1:28]))

#p-value < 0 
#normality assumption violated 
mvnorm.skew.test(na.omit(ars[, 1:28]))


#paf extraction method & communalities 
EFA_mod1 <- fa(ars_correl, nfactors = 3, fm = "pa")
EFA_mod1

#sorting communality 
EFA_mod1_com <- as.data.frame(sort(EFA_mod1$communality, decreasing = TRUE))
EFA_mod1_com

#0.44 (average should be over 0.6)
mean(EFA_mod1$communality)

#0.42 (average should be over 0.6)
mean(EFA_mod3$communality)

#parallel analysis, arsRetained = 2 
ars_retained = paran(ars, graph = TRUE)
ars_retained$Retained

#fa-parallel 
fa.parallel(ars_correl, n.obs = nrow(ars), fa = "fa", fm = "pa")

#nfactors 
nfactors(ars_correl_2, n.obs = nrow(ars))

#scree plot
scree(ars_correl_2)

#seems like 3 factors are most common
EFA_mod2 <- fa(ars_correl, nfactors = 3, fm = "pa")

EFA_mod2_com <- as.data.frame(sort(EFA_mod2$communality, decreasing = TRUE))
EFA_mod2_com

mean(EFA_mod2$communality)
#0.38

#excluding items 
ars_mixed_2 <- mixedCor(ars, c(2, 4:7, 9:15, 17:27), p = NULL)
ars_correl_2 = ars_mixed_2$rho

EFA_mod3 <- fa(ars_correl_2, nfactors = 3, fm = "pa")
EFA_mod3_com <- as.data.frame(sort(EFA_mod3$communality, decreasing = TRUE))
(EFA_mod3_com)
mean(EFA_mod3$communality)

fa.diagram(EFA_mod3)

#factor rotation
EFA_mod3$rotation

#we have theorethical reason to believe the factors are related
EFA_mod_promax <- fa(ars_correl_2, nfactors = 3, fm = "pa", rotate = "promax")
EFA_mod_promax

#interpret factors 
fa.diagram(EFA_mod_promax)

#saving factor scores
efascores = factor.scores(ars[, c(2, 4:7, 9:15, 17:27)], EFA_mod_promax)$scores
ars_factors = cbind(ars, efascores)

#rename factors 
ars_factors = ars_factors %>%
  rename(rightorwrongf = PA1,
         animalresearch = PA2,
         animalrights = PA3)

#regression 
regmodars = lm(formula = liberal ~ rightorwrongf + animalresearch + animalrights, data = ars_factors)
summary(regmodars)

modelsummary(regmodars)


#table
fa_table <- function(x, cut) {
  loadings <- fa.sort(EFA_mod_promax)$loadings %>% round(3)
  loadings[loadings < cut] <- ""
  add_info <- cbind(EFA_mod_promax$communalities,
                    EFA_mod_promax$uniquenesses) %>%
    
    as.data.frame() %>%
    rename("communalities" = V1,
           "uniqueness" = V2) %>%
    
    rownames_to_column("item")
  
  loadings %>%
    unclass() %>%
    as.data.frame() %>% 
    rownames_to_column("item") %>%
    left_join(add_info) %>%
    mutate(across(where(is.numeric), round, 3))
}


fa_table(EFA_mod_promax, .32) %>%
  view()





