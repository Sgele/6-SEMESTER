
# logit -------------------------------------------------------------------

# duomenu nuskaitymas -----------------------------------------------------
duomenys_2lab<-read.csv("/Users/simonagelzinyte/Desktop/6 semestras/Regresinė analizė/diabetes.csv")
table(duomenys_2lab$Outcome)

duomenys2<-subset(duomenys_2lab, duomenys_2lab$Glucose !=0 & duomenys_2lab$BMI != 0 & duomenys_2lab$BloodPressure != 0 & duomenys_2lab$Insulin != 0 & duomenys_2lab$SkinThickness != 0) 

sample <- sample(c(TRUE, FALSE), nrow(duomenys2), replace=TRUE, prob=c(0.7,0.3))
train  <- duomenys2[sample, ]
test   <- duomenys2[!sample, ]
library("writexl")
write_xlsx(test ,"C:\\Users\\ugneo\\OneDrive\\Stalinis kompiuteris\\Duomenų mokslas\\6 semestras\\Projektinis darbas\\test_data.xlsx")
write_xlsx(train ,"C:\\Users\\ugneo\\OneDrive\\Stalinis kompiuteris\\Duomenų mokslas\\6 semestras\\Projektinis darbas\\train_data.xlsx")

test <- read_excel("/Users/simonagelzinyte/Desktop/6 semestras/Regresinė analizė/test_data.xlsx")
train <- read_excel("/Users/simonagelzinyte/Desktop/6 semestras/Regresinė analizė/train_data.xlsx")



# prielaidu tikrinimas ----------------------------------------------------
#prielaidos:
#neturi buti multikolinearumo
#neprikausomos paklaidos
#tiesiskumas tolydziuju kintamuju atzvilgiu su outcome nulis vienas. galima naudoti plot arba box-tidwell testa
#neturi buti isskirciu
#imties dydis turi buti pakankamai didelis 
#https://www.statology.org/assumptions-of-logistic-regression/
#http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/

train$Outcome<-as.factor(train$Outcome)#pasidarome faktoriumi

modelis <- glm(formula = Outcome ~ .,
                family = binomial(logit), data = train)#modelis
summary(modelis)
#multikolinearumo tikrinimas
# -------------------------------------------------------------------------

vif(modelis)
#matome visi gauti koeficientai < 4, tai multikolinearumo problemos neturime


#paklaidu nepriklausomumo tyrimas
# -------------------------------------------------------------------------

#patikrinsime, ar nera autokoreliacijos tarp paklaidu
#tikrinama hipoteze H0:nera autokoreliacijos tarp paklaidu, H1: yra
durbinWatsonTest(modelis)
#gavome p-value=0.52 > 0.05=reiksmingumo lygmuo = alfa, tai galime teigti jog negalime atmesti nulines hipotezes 
#musu paklaidos nera autokoreliuotos

#tikrinsime is grafiko neprikalusomuma
dev.off()
model.data <- augment(modelis) %>% 
  mutate(index = 1:n())  #cia kad gautumeme standartizuotas liekanas
library(ggplot2)
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Outcome), alpha = .5) +
  theme_bw()

#matome, kad 0 ir 1 (neserga, serga) paklaidos standartizuotos pasiskirsciusios nepriklausomai


install.packages("arm")
library(arm)
binnedplot(fitted(modelis), 
           residuals(modelis, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")
#The grey lines represent  ±2 SE bands, which we would expect to contain about 95% of the observations
#https://bookdown.org/jefftemplewebb/IS-6489/logistic-regression.html
plot(modelis, 1)#neparodo nepriklausomumo
#kaip ir gerai
dev.off()
par(mfrow=c(2,3))
plot(modelis)

#tiesiskumas tolydziuju kintamuju atzvilgiu su outcome nulis vienas. galima naudoti plot arba box-tidwell testa
# -------------------------------------------------------------------------

library(dplyr)
library(tidyverse)
library(broom)
library(car)
library(ggplot2)

logodds <- modelis$linear.predictors

g1<-ggplot(train, aes(logodds, Pregnancies))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + ggtitle("Pregnancies")+
  theme_bw() #normaliai


g2<-ggplot(train, aes(logodds, Glucose))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + ggtitle("Glucose")+
  theme_bw() #normaliai



g3<-ggplot(train, aes(logodds, BloodPressure))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + ggtitle("Blood pressure")+ ylab("Blood pressure")+
  theme_bw() #normaliai


g4<-ggplot(train, aes(logodds, Insulin))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + ggtitle("Insulin")+
  theme_bw()#normaliai


g5<-ggplot(train, aes(logodds, SkinThickness))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + ggtitle("Skin Thickness")+ylab("Skin thickness")+
  theme_bw() #normaliai


g6<-ggplot(train, aes(logodds, BMI))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + ggtitle("BMI")+
  theme_bw() #normaliai


g7<-ggplot(train, aes(logodds, DiabetesPedigreeFunction))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + ggtitle("Diabetes pedigree function")+ ylab("Diabetes pedigree function")+
  theme_bw()#nelabai gerai, bet nelabai kas padeda


g8<-ggplot(train, aes(logodds, Age))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + ggtitle("Age")+
  theme_bw()#normaliai
gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, nrow = 4)

plot(Outcome~train$Pregnancies, data=train)
plot(Outcome~train$Glucose, data=train)
plot(Outcome~train$BloodPressure, data=train)
plot(Outcome~train$SkinThickness, data=train)
plot(Outcome~train$Insulin, data=train)
plot(Outcome~train$BMI, data=train)
plot(Outcome~train$DiabetesPedigreeFunction, data=train)
plot(Outcome~train$Age, data=train)

summary(modelis)

#neturi buti isskirciu
cook<-cooks.distance(modelis)
outliers <- cook[cook>1]
outliers
plot(modelis, 4)

#imties dydis turi buti pakankamai didelis
(5*8) / 0.20 #pakankamai
# pirmine analize ---------------------------------------------------------
library(tidyverse)
install.packages("ggstatsplot")
library(ggstatsplot)


gg1<-ggplot(train, aes(x=Outcome, y=Pregnancies)) +
  geom_boxplot() + ggtitle("Pregnancies boxplot") +
  xlab("Diabeties") + ylab("Pregnancies number") 

gg2<-ggplot(train, aes(x=Outcome, y=Glucose)) +
  geom_boxplot() + ggtitle("Glucose boxplot") +
  xlab("Diabetes") + ylab("Glucose concentration") 

gg3<-ggplot(train, aes(x=Outcome, y=BloodPressure)) +
  geom_boxplot() + ggtitle("Blood pressure boxplot") +
  xlab("Diabetes") + ylab("Blood pressure") 

gg4<-ggplot(train, aes(x=Outcome, y=SkinThickness)) +
  geom_boxplot() + ggtitle("Skin Thickness boxplot") +
  xlab("Diabetes") + ylab("Skin Thickness mm") 

gg5<-ggplot(train, aes(x=Outcome, y=Insulin)) +
  geom_boxplot() + ggtitle("Insulin boxplot") +
  xlab("Diabetes") + ylab("Insulin mU/ml") 

gg6<-ggplot(train, aes(x=Outcome, y=BMI)) +
  geom_boxplot() + ggtitle("BMI boxplot") +
  xlab("Diabetes") + ylab("BMI") 

gg7<-ggplot(train, aes(x=Outcome, y=DiabetesPedigreeFunction)) +
  geom_boxplot() + ggtitle("Diabetes pedigree function boxplot") +
  xlab("Diabetes") + ylab("Diabetes pedigree function result") 

gg8<-ggplot(train, aes(x=Outcome, y=Age)) +
  geom_boxplot() + ggtitle("Age boxplot") +
  xlab("Diabetes") + ylab("Age") 

gridExtra::grid.arrange(gg1, gg2, gg3, gg4, gg5, gg6, gg7, gg8, nrow = 4)
# 0 ir 1 ------------------------------------------------------------------

table(train$Outcome)#tenkina, kad ne maziau nei 20 proc

# modelis -----------------------------------------------------------------

summary(modelis)#yra reiksmingu kovarianciu
#tiketinumo santykis H0:visos kovariantes =0 H1: bent viena lygybe neteisinga
modelis_rd<-glm(Outcome~1, family=binomial(logit), data=train)
anova(modelis_rd, modelis, test="Chisq")
#p <2.2e-16 tai nuline hipoteze atmetame



# pazingsnine -------------------------------------------------------------

stepAIC(modelis, direction = c("both"))
stepAIC(modelis, direction = c("backward"))
stepAIC(modelis, direction = c("forward"))
#tai paliekam glucose, skinthickness, bmi, diabetes, age 

modelis11<-glm(formula=Outcome~Glucose+BMI+DiabetesPedigreeFunction+Age, family=binomial(logit), data=train)
summary(modelis11)
summary(modelis)
vif(modelis11)

# exp ---------------------------------------------------------------------

exp(coef(modelis11)) 
# Didesnis už 1 galimybių santykis rodo, kiek kartų, palyginti su 
#ankstesne galimybe, labiau tikėtina, kad Y = 1, o ne 𝑌 = 0.

exp(0.05*coef(modelis11))

# slenkstis ---------------------------------------------------------------
library(pROC)

rocobj <- roc(modelis11$y,modelis11$fitted.values)
coords(rocobj, "best", best.method="youden")#tai gavome geriausia slenksti 0.02388142

# klasifikacijos lentele --------------------------------------------------

library(QuantPsyc)
ClassLog(modelis11, train$Outcome)
sensitivity_train<-44/(44+21)
sensitivity_train
specifity_train<-164/(164+36)
specifity_train
precision_train<-44/(44+36)
precision_train
npv_train<-164/(164+21)
npv_train

predictTest <- predict(modelis11, type = "response", newdata = test)
table(test$Outcome,predictTest > 0.280)
sensivity_test<-40/50
sensivity_test
specificity_test<-61/(61+16)
specificity_test
precision_test<-40/56
precision_test
npv_test<-61/71
npv_test
tkslumas<-(61+40)/(61+16+10+40)
tkslumas
f1<-(1+1)*(sensivity_test*precision_test)/(1*precision_test+sensivity_test)
f1

prognoze <- predict(modelis11, test, type = "response")
library(pROC)
test$Outcome<-as.factor(test$Outcome)
ROC_lr <- roc(test$Outcome, prognoze)
ROC_lr_auc <- auc(ROC_lr)

plot(ROC_lr, col = "green", main = "ROC for logistic regression" )
summary(modelis11)

r_kvadrat<-1-modelis11$deviance/modelis11$null.deviance
r_kvadrat
