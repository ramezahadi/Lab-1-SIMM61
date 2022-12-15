#LAB 4

#library 
install.packages('psychTools')
library(lavaan)
library(semPlot)
library(semptools)
library(CompQuadForm)
library(ICS)
library(psychTools)

my_data = holzinger.swineford

#creating model
model1 <- '
visperc =~ t01_visperc + t02_cubes + t03_frmbord + t04_lozenges
verabi =~ t06_paracomp + t07_sentcomp + t09_wordmean
procspeed =~ t10_addition + t12_countdot + t13_sccaps
'

fit <- sem(model1, data = my_data)

plot1 = semPaths(fit, label.scale=F, nCharNodes = 3,
                sizeMan2=4, sizeMan=8, asize=2, edge.color="blue", residuals = F, fixedStyle = 1)

summary(fit)
summary(fit, fit.measures = T)


#checking assumptions of normality 

mvnorm.kur.test(my_data)
mvnorm.kur.test(my_data[,c("t01_visperc", "t02_cubes", "t03_frmbord", "t04_lozenges", "t06_paracomp", "t07_sentcomp", "t09_wordmean", "t10_addition", "t12_countdot", "t13_sccaps")])

#second model
model2 <- '
visperc =~ t01_visperc + t02_cubes + t03_frmbord + t04_lozenges
verabi =~ t06_paracomp + t07_sentcomp + t09_wordmean
procspeed =~ t10_addition + t12_countdot + t13_sccaps
t10_addition ~~ t12_countdot
'

fit2 <- sem(model2, data = my_data)

plot2 = semPaths(fit2, label.scale=F, nCharNodes = 3,
                sizeMan2=4, sizeMan=8, asize=2, edge.color="blue", residuals = F, fixedStyle = 1)

summary(fit2, fit.measures = T)

plot3 = semPaths(fit2, label.scale=F, nCharNodes = 3,
                sizeMan2=4, sizeMan=8, asize=2, edge.color="blue", residuals = F, fixedStyle = 1, whatLabels = "est")



#model mediation

modelmediate = 

'
t13_sccaps ~ at12_countdot + bt01_visperc
t12_countdot ~ ct01_visperc 
'

medmodel = sem(modelmediate, data = my_data)
summary(medmodel, fit.measures = T)

semPaths(medmodel, fixedStyle = 1, label.scale=F, nCharNodes = 0,
         sizeMan2=5, sizeMan=15, asize=3, edge.label.cex = 1, whatLabels = "est")




