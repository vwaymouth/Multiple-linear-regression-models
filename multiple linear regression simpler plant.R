setwd("~/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/Documents/University/Glasshouse/Stats")
library("here")
here()

library("caTools")# For Linear regression 

library("ggplot2")
library("dplyr")
library("olsrr")
library("GGally")
library("tidyverse")
library("caret")
library("leaps")
library("vegan")
library("dplyr")
library("lme4")
library("car")
library("emmeans")
library("ggplot2")
library("lmerTest")

all<-read.csv("Data for models all.csv")
all$Plant.number=factor(all$Plant.number)
glimpse(all)

#subset
EV<-subset(all, Species=="EV")
KL<-subset(all, Species=="KL")
ME<-subset(all, Species=="ME")

EV3<-subset(EV, Harvest=="H3")
EV2<-subset(EV, Harvest=="H2")
EV1<-subset(EV, Harvest=="H1")

KL3<-subset(KL, Harvest=="H3")
KL2<-subset(KL, Harvest=="H2")
KL1<-subset(KL, Harvest=="H1")

ME3<-subset(ME, Harvest=="H3")
ME2<-subset(ME, Harvest=="H2")
ME1<-subset(ME, Harvest=="H1")

H3<-subset(all, Harvest=="H3")

names(EV)

#correlation analysis
cols_keep<-c("P","Apath", "Fpara","Apara","Ppath","Dsap","Lsap","Ssap","Usap","Wsap","Rsym","ECM","Endo","Rpath")
#cols_keep<-c("Biomass","Doth", "Euro","Leot","Pezi","Sacc","Sord","Agar","Trem","Mort","Uncl_F","Rare")

EVS <- EV[, (colnames(EV) %in% cols_keep)]
cor(EVS)

EV1S <- EV1[, (colnames(EV1) %in% cols_keep)]
cor(EV1S)

KLS <- KL[, (colnames(KL) %in% cols_keep)]
cor(KLS)

MES <- ME[, (colnames(ME) %in% cols_keep)]
cor(MES)

##cris###
library(MuMIn)
library(nlme)

#####function
# complete list Apara+Myco+Ppath+Rpath+Dsap+Lsap+Psap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym
names(ME3)
#EV Apara+Myco+Ppath+Rpath+Lsap+Psap+Rsap+Ssap+Wsap+EcM+REndo+Rsym
#KL Apara+Myco+Ppath+Rpath+Dsap+Psap+Rsap+Ssap+Wsap+EcM+FEndo+REndo+Rsym
#ME Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym

###Tax
#complete list Agar+Arch+Chyt+Cyst+Doth+Euro+Leot+Mort+Pezi+Sord+Trem+Ucl_F+Rare
#EV Agar+Arch+Chyt+Cyst+Doth+Leot+Mort+Pezi+Sord+Trem+Ucl_F+Rare
#KL Agar+Doth+Euro+Leot+Mort+Pezi+Sord+Trem+Ucl_F+Rare
#ME Agar+Arch+Cyst+Doth+Euro+Mort+Pezi+Sord+Trem+Ucl_F+Rare

options(na.action = "na.fail")  #important to run this and lm model every time that you want to run dredge
fm <- lm(Biomass~ Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym,
           data =ME1, na.action = NULL)

coef(fm)
ms<-dredge(fm, beta= "sd", evaluate = TRUE, rank = "AIC",
           m.lim=c(0,3),
           extra=c("adjR^2", "BIC", "Cp",
                   F=function(x){ r.squaredGLMM(x)}), trace=TRUE)
ma <- model.avg(ms, subset = cumsum(weight) <= .95)
summary(ma)
confint(ma)

options(na.action = "na.omit") #important to reset global option for na.action 

#Harvest 2
options(na.action = "na.fail")  #important to run this and lm model every time that you want to run dredge
fm1 <- lm(Biomass~ Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym,
         data =ME2, na.action = NULL)

coef(fm1)
ms1<-dredge(fm1, beta= "sd", evaluate = TRUE, rank = "AIC",
           m.lim=c(0,3),
           extra=c("adjR^2", "BIC", "Cp",
                   F=function(x){ r.squaredGLMM(x)}), trace=TRUE)
ma1 <- model.avg(ms1, subset = cumsum(weight) <= .95)
summary(ma1)
confint(ma1)
options(na.action = "na.omit") #important to reset global option for na.action 

#Harvest 3
options(na.action = "na.fail")  #important to run this and lm model every time that you want to run dredge
fm2 <- lm(Biomass~ Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym,
          data =ME3, na.action = NULL)

coef(fm2)
ms2<-dredge(fm2, beta= "sd", evaluate = TRUE, rank = "AIC",
            m.lim=c(0,3),
            extra=c("adjR^2", "BIC", "Cp",
                    F=function(x){ r.squaredGLMM(x)}), trace=TRUE)

model.avg(ms2)
ma2 <- model.avg(ms2, subset = cumsum(weight) <= .95)
summary(ma2)
confint(ma2)
options(na.action = "na.omit") #important to reset global option for na.action 

options(na.action = "na.fail")  #important to run this and lm model every time that you want to run dredge
fm3 <- lm(Rvol~ Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym,
          data =ME1, na.action = NULL)

coef(fm3)
ms3<-dredge(fm3, beta= "sd", evaluate = TRUE, rank = "AIC",
            m.lim=c(0,3),
            extra=c("adjR^2", "BIC", "Cp",
                    F=function(x){ r.squaredGLMM(x)}), trace=TRUE)
ma3 <- model.avg(ms3, subset = cumsum(weight) <= .95)
summary(ma3)
confint(ma3)
options(na.action = "na.omit") #important to reset global option for na.action 

#Harvest 2
options(na.action = "na.fail")  #important to run this and lm model every time that you want to run dredge
fm4 <- lm(Rvol~ Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym,
          data =ME2, na.action = NULL)

coef(fm4)
ms4<-dredge(fm4, beta= "sd", evaluate = TRUE, rank = "AIC",
            m.lim=c(0,3),
            extra=c("adjR^2", "BIC", "Cp",
                    F=function(x){ r.squaredGLMM(x)}), trace=TRUE)
ma4 <- model.avg(ms4, subset = cumsum(weight) <= .95)
summary(ma4)
confint(ma4)
options(na.action = "na.omit") #important to reset global option for na.action 

#Harvest 3
options(na.action = "na.fail")  #important to run this and lm model every time that you want to run dredge
fm5 <- lm(Rvol~ Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym,
          data =ME3, na.action = NULL)

coef(fm5)
ms5<-dredge(fm5, beta= "sd", evaluate = TRUE, rank = "AIC",
            m.lim=c(0,3),
            extra=c("adjR^2", "BIC", "Cp",
                    F=function(x){ r.squaredGLMM(x)}), trace=TRUE)
ma5 <- model.avg(ms5, subset = cumsum(weight) <= .95)
summary(ma5)
confint(ma5)
options(na.action = "na.omit") #important to reset global option for na.action 

options(na.action = "na.fail")  #important to run this and lm model every time that you want to run dredge
fm6 <- lm(nleaves~ Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym,
          data =ME1, na.action = NULL)

coef(fm6)
ms6<-dredge(fm6, beta= "sd", evaluate = TRUE, rank = "AIC",
            m.lim=c(0,3),
            extra=c("adjR^2", "BIC", "Cp",
                    F=function(x){ r.squaredGLMM(x)}), trace=TRUE)
ma6 <- model.avg(ms6, subset = cumsum(weight) <= .95)
summary(ma6)
confint(ma6)
options(na.action = "na.omit") #important to reset global option for na.action 

#Harvest 2
options(na.action = "na.fail")  #important to run this and lm model every time that you want to run dredge
fm7 <- lm(nleaves~ Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym,
          data =ME2, na.action = NULL)

coef(fm1)
ms7<-dredge(fm7, beta= "sd", evaluate = TRUE, rank = "AIC",
            m.lim=c(0,3),
            extra=c("adjR^2", "BIC", "Cp",
                    F=function(x){ r.squaredGLMM(x)}), trace=TRUE)
ma7 <- model.avg(ms7, subset = cumsum(weight) <= .95)
summary(ma7)
confint(ma7)
options(na.action = "na.omit") #important to reset global option for na.action 

#Harvest 3
options(na.action = "na.fail")  #important to run this and lm model every time that you want to run dredge
fm8 <- lm(nleaves~ Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym,
          data =ME3, na.action = NULL)

coef(fm8)
ms8<-dredge(fm8, beta= "sd", evaluate = TRUE, rank = "AIC",
            m.lim=c(0,3),
            extra=c("adjR^2", "BIC", "Cp",
                    F=function(x){ r.squaredGLMM(x)}), trace=TRUE)
ma8 <- model.avg(ms8, subset = cumsum(weight) <= .95)
summary(ma8)
confint(ma8)
options(na.action = "na.omit") #important to reset global option for na.action 

options(na.action = "na.fail")  #important to run this and lm model every time that you want to run dredge
fm9 <- lm(RGR~ Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym,
          data =ME1, na.action = NULL)

coef(fm9)
ms9<-dredge(fm9, beta= "sd", evaluate = TRUE, rank = "AIC",
            m.lim=c(0,3),
            extra=c("adjR^2", "BIC", "Cp",
                    F=function(x){ r.squaredGLMM(x)}), trace=TRUE)
ma9 <- model.avg(ms9, subset = cumsum(weight) <= .95)
summary(ma9)
confint(ma9)
options(na.action = "na.omit") #important to reset global option for na.action 

#Harvest 2
options(na.action = "na.fail")  #important to run this and lm model every time that you want to run dredge
fm10 <- lm(RGR~ Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym,
           data =ME2, na.action = NULL)

coef(fm10)
ms10<-dredge(fm10, beta= "sd", evaluate = TRUE, rank = "AIC",
             m.lim=c(0,3),
             extra=c("adjR^2", "BIC", "Cp",
                     F=function(x){ r.squaredGLMM(x)}), trace=TRUE)
ma10 <- model.avg(ms10, subset = cumsum(weight) <= .95)
summary(ma10)
confint(ma10)
options(na.action = "na.omit") #important to reset global option for na.action 

#Harvest 3
options(na.action = "na.fail")  #important to run this and lm model every time that you want to run dredge
fm11 <- lm(RGR~ Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym,
           data =ME3, na.action = NULL)

coef(fm11)
ms11<-dredge(fm11, beta= "sd", evaluate = TRUE, rank = "AIC",
             m.lim=c(0,3),
             extra=c("adjR^2", "BIC", "Cp",
                     F=function(x){ r.squaredGLMM(x)}), trace=TRUE)
ma11 <- model.avg(ms11, subset = cumsum(weight) <= .95)
summary(ma11)
confint(ma11)
options(na.action = "na.omit") #important to reset global option for na.action 
#harvest 1
options(na.action = "na.fail")  #important to run this and lm model every time that you want to run dredge
fm12 <- lm(Myc~ Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym,
           data =ME1, na.action = NULL)

coef(fm12)
ms12<-dredge(fm12, beta= "sd", evaluate = TRUE, rank = "AIC",
             m.lim=c(0,3),
             extra=c("adjR^2", "BIC", "Cp",
                     F=function(x){ r.squaredGLMM(x)}), trace=TRUE)
ma12 <- model.avg(ms12, subset = cumsum(weight) <= .95)
summary(ma12)
confint(ma12)
options(na.action = "na.omit") #important to reset global option for na.action 

#Harvest 2
options(na.action = "na.fail")  #important to run this and lm model every time that you want to run dredge
fm13 <- lm(Myc~ Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym,
           data =ME2, na.action = NULL)

coef(fm13)
ms13<-dredge(fm13, beta= "sd", evaluate = TRUE, rank = "AIC",
             m.lim=c(0,3),
             extra=c("adjR^2", "BIC", "Cp",
                     F=function(x){ r.squaredGLMM(x)}), trace=TRUE)
ma13 <- model.avg(ms13, subset = cumsum(weight) <= .95)
summary(ma13)
confint(ma13)
options(na.action = "na.omit") #important to reset global option for na.action 

#Harvest 3
options(na.action = "na.fail")  #important to run this and lm model every time that you want to run dredge
fm14 <- lm(Myc~ Apara+Myco+Ppath+Rpath+Dsap+Lsap+Rsap+Ssap+Usap+Wsap+EcM+FEndo+REndo+Rsym,
           data =ME3, na.action = NULL)

coef(fm14)
ms14<-dredge(fm14, beta= "sd", evaluate = TRUE, rank = "AIC",
             m.lim=c(0,3),
             extra=c("adjR^2", "BIC", "Cp",
                     F=function(x){ r.squaredGLMM(x)}), trace=TRUE)
ma14 <- model.avg(ms14, subset = cumsum(weight) <= .95)
summary(ma14)
confint(ma14)
options(na.action = "na.omit") #important to reset global option for na.action 



# Variable Importance (Sum of weights)can be calculated/extracted from various objects:
sw(ms)   #: call importance. This gives you the importance value for the variables. 

subset(ms, delta <= 2, recalc.weights = F,)  # shows the subset of best models, with difference in AIC (delta)<2 ("equivalent")

# Variable Importance (Sum of weights)can be calculated/extracted from various objects:
sw(ms1)   #: call importance. This gives you the importance value for the variables. 

# Different ways of finding a confidence set of models:
# delta(AIC) cutoff
subset(ms1, delta <= 2, recalc.weights = F,)  # shows the subset of best models, with difference in AIC (delta)<2 ("equivalent")

# Variable Importance (Sum of weights)can be calculated/extracted from various objects:
sw(ms2)   #: call importance. This gives you the importance value for the variables. 

# Different ways of finding a confidence set of models:
# delta(AIC) cutoff
subset(ms2, delta <= 2, recalc.weights = F,)  # shows the subset of best models, with difference in AIC (delta)<2 ("equivalent")

# Variable Importance (Sum of weights)can be calculated/extracted from various objects:
sw(ms3)   #: call importance. This gives you the importance value for the variables. 

subset(ms3, delta <= 2, recalc.weights = F,)  # shows the subset of best models, with difference in AIC (delta)<2 ("equivalent")

# Variable Importance (Sum of weights)can be calculated/extracted from various objects:
sw(ms4)   #: call importance. This gives you the importance value for the variables. 

# Different ways of finding a confidence set of models:
# delta(AIC) cutoff
subset(ms4, delta <= 2, recalc.weights = F,)  # shows the subset of best models, with difference in AIC (delta)<2 ("equivalent")

# Variable Importance (Sum of weights)can be calculated/extracted from various objects:
sw(ms5)   #: call importance. This gives you the importance value for the variables. 

# Different ways of finding a confidence set of models:
# delta(AIC) cutoff
subset(ms5, delta <= 2, recalc.weights = F,)  # shows the subset of best models, with difference in AIC (delta)<2 ("equivalent")

# Variable Importance (Sum of weights)can be calculated/extracted from various objects:
sw(ms6)   #: call importance. This gives you the importance value for the variables. 

subset(ms6, delta <= 2, recalc.weights = F,)  # shows the subset of best models, with difference in AIC (delta)<2 ("equivalent")

# Variable Importance (Sum of weights)can be calculated/extracted from various objects:
sw(ms7)   #: call importance. This gives you the importance value for the variables. 

# Different ways of finding a confidence set of models:
# delta(AIC) cutoff
subset(ms7, delta <= 2, recalc.weights = F,)  # shows the subset of best models, with difference in AIC (delta)<2 ("equivalent")

# Variable Importance (Sum of weights)can be calculated/extracted from various objects:
sw(ms8)   #: call importance. This gives you the importance value for the variables. 

# Different ways of finding a confidence set of models:
# delta(AIC) cutoff
subset(ms8, delta <= 2, recalc.weights = F,)  # shows the subset of best models, with difference in AIC (delta)<2 ("equivalent")

# Variable Importance (Sum of weights)can be calculated/extracted from various objects:
sw(ms9)   #: call importance. This gives you the importance value for the variables. 

subset(ms9, delta <= 2, recalc.weights = F,)  # shows the subset of best models, with difference in AIC (delta)<2 ("equivalent")

# Variable Importance (Sum of weights)can be calculated/extracted from various objects:
sw(ms10)   #: call importance. This gives you the importance value for the variables. 

# Different ways of finding a confidence set of models:
# delta(AIC) cutoff
subset(ms10, delta <= 2, recalc.weights = F,)  # shows the subset of best models, with difference in AIC (delta)<2 ("equivalent")

# Variable Importance (Sum of weights)can be calculated/extracted from various objects:
sw(ms11)   #: call importance. This gives you the importance value for the variables. 

# Different ways of finding a confidence set of models:
# delta(AIC) cutoff
subset(ms11, delta <= 2, recalc.weights = F,)  # shows the subset of best models, with difference in AIC (delta)<2 ("equivalent")

# Variable Importance (Sum of weights)can be calculated/extracted from various objects:
sw(ms12)   #: call importance. This gives you the importance value for the variables. 

subset(ms12, delta <= 2, recalc.weights = F,)  # shows the subset of best models, with difference in AIC (delta)<2 ("equivalent")

# Variable Importance (Sum of weights)can be calculated/extracted from various objects:
sw(ms13)   #: call importance. This gives you the importance value for the variables. 

# Different ways of finding a confidence set of models:
# delta(AIC) cutoff
subset(ms13, delta <= 2, recalc.weights = F,)  # shows the subset of best models, with difference in AIC (delta)<2 ("equivalent")

# Variable Importance (Sum of weights)can be calculated/extracted from various objects:
sw(ms14)   #: call importance. This gives you the importance value for the variables. 

# Different ways of finding a confidence set of models:
# delta(AIC) cutoff
subset(ms14, delta <= 2, recalc.weights = F,)  # shows the subset of best models, with difference in AIC (delta)<2 ("equivalent")

