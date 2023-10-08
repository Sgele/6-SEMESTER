# reikalingos bibliotekos -------------------------------------------------

library(dplyr)
library(tidyverse)
library("writexl")
library("readxl")
library(tidyr)
library(reshape)
library(reshape2)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)
library(psych)
library('corrr')
library("FactoMineR")
library(ggcorrplot)
library("factoextra")
library(ggpubr)
library(Rtsne)
library(ggfortify)

# duomenu nuskaitymas -----------------------------------------------------

duomenys_be_nuliu <- read_excel("/Users/simonagelzinyte/Desktop/6 semestras/Papildomi duomenų vizualizavimo skyriai/be_nuliu.xlsx")

# procentine serganciu lentele -------------------------------------------

table(duomenys_be_nuliu$Outcome)/length(duomenys_be_nuliu$Outcome)

# pirmas vaizdas apie duoemenis -------------------------------------------

summary(duomenys_be_nuliu)

# Duomenu aprasomoji statistika, juos sugrupavus --------------------------

describeBy(duomenys_be_nuliu$Age, 
           group = duomenys_be_nuliu$Outcome, digits = 4)

describeBy(duomenys_be_nuliu$Pregnancies, 
           group = duomenys_be_nuliu$Outcome, digits = 4)

describeBy(duomenys_be_nuliu$Glucose, 
           group = duomenys_be_nuliu$Outcome, digits = 4)

describeBy(duomenys_be_nuliu$BloodPressure, 
           group = duomenys_be_nuliu$Outcome, digits = 4)

describeBy(duomenys_be_nuliu$`SkinThickness`, 
           group = duomenys_be_nuliu$Outcome, digits = 4)

describeBy(duomenys_be_nuliu$Insulin, 
           group = duomenys_be_nuliu$Outcome, digits = 4)

describeBy(duomenys_be_nuliu$BMI, 
           group = duomenys_be_nuliu$Outcome, digits = 4)

describeBy(duomenys_be_nuliu$`DiabetesPedigreeFunction`, 
           group = duomenys_be_nuliu$Outcome, digits = 4)

# Koreliacijos matrica ----------------------------------------------------
duomenys_be_nuliu$Outcome<-as.numeric(duomenys_be_nuliu$Outcome)
colnames(duomenys_be_nuliu)<-c("Pregnancies", "Glucose", "B.P.", "S.T.", "Insulin", "BMI", "D.P.F.", "Age", "Outcome")
gcor<-ggcorr(duomenys_be_nuliu, label = TRUE, hjust = 0.7, size = 4 , layout.exp = 1)+ 
  ggplot2::labs(title = "Variable correlation matrix") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5, size=10))+ 
  labs(caption = c("Abbreviation explanation:\nB.P. means blood pressure, S. T. means skin thickness and D.P.F. means diabetes pedigree function"))

gcor
colnames(duomenys_be_nuliu)<-c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction", "Age", "Outcome")
# apsirasome isskirciu radimo funkcijas -----------------------------------

duomenys_be_nuliu$Outcome<-as.factor(duomenys_be_nuliu$Outcome)

isskirtis <- function(x) {
  return(x <= quantile(x, .25) - 3*IQR(x) | x >= quantile(x, .75) + 3*IQR(x))
}

salygine_isskirtis<-function(x){
  return(((x <= quantile(x, .25) - 1.5*IQR(x))&(x > quantile(x, .25) - 3*IQR(x))) | (x >= quantile(x, .75) + 1.5*IQR(x))&
           (x < quantile(x, .75) + 3*IQR(x)))
}


# sukuriame stulpeli paciento id ------------------------------------------

duomenys_be_nuliu$paciento_id<-seq(from=1, to= length(duomenys_be_nuliu$Outcome), by=1)

# susikuriame naujus stulpelius paziureti, ar duomenys yra isskirtys --------

duomenys_isskirtys<-duomenys_be_nuliu %>% group_by(Outcome)%>%
  mutate(outlier_pregnancies=ifelse(isskirtis(Pregnancies), paciento_id, NA))%>%
  mutate(salygine_outlier_pregnancies=ifelse(salygine_isskirtis(Pregnancies), paciento_id, NA))%>%
  mutate(outlier_glucose=ifelse(isskirtis(Glucose), paciento_id, NA))%>%
  mutate(salygine_outlier_glucose=ifelse(salygine_isskirtis(Glucose), paciento_id, NA))%>%
  mutate(outlier_bloodpressure=ifelse(isskirtis(BloodPressure), paciento_id, NA))%>%
  mutate(salygine_outlier_bloodpressure=ifelse(salygine_isskirtis(BloodPressure), paciento_id, NA))%>%
  mutate(outlier_skinthickness=ifelse(isskirtis(SkinThickness), paciento_id, NA))%>%
  mutate(salygine_outlier_skinthickness=ifelse(salygine_isskirtis(SkinThickness), paciento_id, NA))%>%
  mutate(outlier_insulin=ifelse(isskirtis(Insulin), paciento_id, NA))%>%
  mutate(salygine_outlier_insulin=ifelse(salygine_isskirtis(Insulin), paciento_id, NA))%>%
  mutate(outlier_BMI=ifelse(isskirtis(BMI), paciento_id, NA))%>%
  mutate(salygine_outlier_BMI=ifelse(salygine_isskirtis(BMI), paciento_id, NA))%>%
  mutate(outlier_diabetespedigreefunction=ifelse(isskirtis(DiabetesPedigreeFunction), paciento_id, NA))%>%
  mutate(salygine_outlier_diabetespedigreefunction=ifelse(salygine_isskirtis(DiabetesPedigreeFunction), paciento_id, NA))%>%
  mutate(outlier_age=ifelse(isskirtis(Age), paciento_id, NA))%>%
  mutate(salygine_outlier_age=ifelse(salygine_isskirtis(Age), paciento_id, NA))


# nagrinesi nestumu skaiciu -----------------------------------------------

#paziurime, ar turime isskirciu
mean(is.na(duomenys_isskirtys$outlier_pregnancies))
sum(!is.na(duomenys_isskirtys$outlier_pregnancies)) #is abieju rodikliu matome, jog isskirciu neturime

#paziurime, ar turime salyginiu isskirciu
mean(is.na(duomenys_isskirtys$salygine_outlier_pregnancies))
sum(!is.na(duomenys_isskirtys$salygine_outlier_pregnancies))#turime 15 salygines isskirtis

#pasiziurime boxplotu
g1<-ggplot(duomenys_isskirtys, aes(x=Outcome, y=Pregnancies)) +
  geom_boxplot() + ggtitle("Pregnancies boxplot by outcome")+
  ylab("Pregnancies number") +  
  geom_point(data = subset(duomenys_isskirtys, salygine_outlier_pregnancies != "NA"),
             aes(x=Outcome, y=Pregnancies), size = 2, color = "red")  +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "Colored dots for conditional outliers and labeled quantiles") +
  stat_summary(fun = "quantile", size = 4,
               geom = "text", aes(label = after_stat(y)),
               position = position_nudge(x = 0.5))
g1


#surasome i isskirciu salyginiu lentele
salygines_isskirtys<-data.frame()
salygines_isskirtys<-duomenys_isskirtys %>% filter(!is.na(salygine_outlier_pregnancies))
#ant boxplotu uzdeti, gal geriau pregnancies number, uzdeti kvantilius visus


# nagrinejame glucose -----------------------------------------------------

#paziurime, ar turime isskirciu
mean(is.na(duomenys_isskirtys$outlier_glucose))
sum(!is.na(duomenys_isskirtys$outlier_glucose)) #is abieju rodikliu matome, jog isskirciu neturime

#paziurime, ar turime salyginiu isskirciu
mean(is.na(duomenys_isskirtys$salygine_outlier_glucose))
sum(!is.na(duomenys_isskirtys$salygine_outlier_glucose))#17 salyginiu isskirciu

#pasiziurime boxplot
g2<-ggplot(duomenys_isskirtys, aes(x=Outcome, y=Glucose)) +
  geom_boxplot() + ggtitle("Glucose boxplot by outcome")+
  ylab("Glucose concentration")   +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  geom_point(data = subset(duomenys_isskirtys, salygine_outlier_glucose != "NA"),
             aes(x=Outcome, y=Glucose), size = 2, color = "red")  +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "Colored dots for conditional outliers and labeled quantiles") +
  stat_summary(fun = "quantile", size = 4,
               geom = "text", aes(label = after_stat(y)),
               position = position_nudge(x = 0.5))
g2




#surasome i salyginiu isskirciu lentele
salygines_isskirtys[(nrow(salygines_isskirtys) + 1):(nrow(salygines_isskirtys) + nrow(filter(duomenys_isskirtys, !is.na(salygine_outlier_glucose)))),] <-
  filter(duomenys_isskirtys, !is.na(salygine_outlier_glucose))


# nagrinejame blood pressure ----------------------------------------------

#paziurime, ar turime isskirciu
mean(is.na(duomenys_isskirtys$outlier_bloodpressure))
sum(!is.na(duomenys_isskirtys$outlier_bloodpressure)) #is abieju rodikliu matome, jog isskirciu neturime

#paziurime, ar turime salyginiu isskirciu
mean(is.na(duomenys_isskirtys$salygine_outlier_bloodpressure))
sum(!is.na(duomenys_isskirtys$salygine_outlier_bloodpressure))#15 salyginiu isskirciu


#pasiziurime boxplot
g3<-ggplot(duomenys_isskirtys, aes(x=Outcome, y=BloodPressure)) +
  geom_boxplot() + ggtitle("Blood pressure boxplot by outcome")+
  ylab("Blood pressure (mm Hg)") + 
  geom_point(data = subset(duomenys_isskirtys, salygine_outlier_bloodpressure != "NA"),
             aes(x=Outcome, y=BloodPressure), size = 2, color = "red")  +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(subtitle = "Colored dots for conditional outliers and labeled quantiles") + 
  stat_summary(fun = "quantile", size = 4,
               geom = "text", aes(label = after_stat(y)),
               position = position_nudge(x = 0.5))
g3


#surasome i salyginiu isskirciu lentele
salygines_isskirtys[(nrow(salygines_isskirtys) + 1):(nrow(salygines_isskirtys) + nrow(filter(duomenys_isskirtys, !is.na(salygine_outlier_bloodpressure)))),] <-
  filter(duomenys_isskirtys, !is.na(salygine_outlier_bloodpressure))

# odos storis --------------------------------------------------------------

#paziurime, ar turime isskirciu
mean(is.na(duomenys_isskirtys$outlier_skinthickness))
sum(!is.na(duomenys_isskirtys$outlier_skinthickness)) #is abieju rodikliu matome, jog isskirciu neturime

#paziurime, ar turime salyginiu isskirciu
mean(is.na(duomenys_isskirtys$salygine_outlier_skinthickness))
sum(!is.na(duomenys_isskirtys$salygine_outlier_skinthickness))#1 salygine isskirtis

#pasiziurime boxplot
g4<-ggplot(duomenys_isskirtys, aes(x=Outcome, y=SkinThickness)) +
  geom_boxplot() + ggtitle("Skin thickness boxplot by outcome")+
  ylab("Skin thickness (mm)") + 
  geom_point(data = subset(duomenys_isskirtys, salygine_outlier_skinthickness != "NA"),
             aes(x=Outcome, y=SkinThickness), size = 2, color = "red")  +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(subtitle = "Colored dot for conditional outlier and labeled quantiles")+
  stat_summary(fun = "quantile", size = 4,
               geom = "text", aes(label = after_stat(y)),
               position = position_nudge(x = 0.5))

g4

#surasome i salyginiu isskirciu lentele
salygines_isskirtys[(nrow(salygines_isskirtys) + 1):(nrow(salygines_isskirtys) + nrow(filter(duomenys_isskirtys, !is.na(salygine_outlier_skinthickness)))),] <-
  filter(duomenys_isskirtys, !is.na(salygine_outlier_skinthickness))


# insulin -----------------------------------------------------------------

#paziurime, ar turime isskirciu
mean(is.na(duomenys_isskirtys$outlier_insulin))
sum(!is.na(duomenys_isskirtys$outlier_insulin)) #7 isskirtys

#paziurime, ar turime salyginiu isskirciu
mean(is.na(duomenys_isskirtys$salygine_outlier_insulin))
sum(!is.na(duomenys_isskirtys$salygine_outlier_insulin))#31 salygine isskirtis

#pasiziurime boxplot
g5<-ggplot(duomenys_isskirtys, aes(x=Outcome, y=Insulin)) +
  geom_boxplot() + ggtitle("Insulin boxplot by outcome")+
  ylab("Insulin (mU / ml)") + 
  geom_point(data = subset(duomenys_isskirtys, outlier_insulin != "NA"),
             aes(x=Outcome, y=Insulin), size = 2, color = "red")   +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(subtitle = "Colored dots for outliers and labeled quantiles") +
  stat_summary(fun = "quantile", size = 4,
               geom = "text", aes(label = after_stat(y)),
               position = position_nudge(x = 0.5))
g5

#surasome isskirtis i atskira lentele
isskirtys<-data.frame()
isskirtys<-duomenys_isskirtys %>% filter(!is.na(outlier_insulin))

#surasome salygines isskirtis i atskira lentele
salygines_isskirtys[(nrow(salygines_isskirtys) + 1):(nrow(salygines_isskirtys) + nrow(filter(duomenys_isskirtys, !is.na(salygine_outlier_insulin)))),] <-
  filter(duomenys_isskirtys, !is.na(salygine_outlier_insulin))



# BMI ---------------------------------------------------------------------
#paziurime, ar turime isskirciu
mean(is.na(duomenys_isskirtys$outlier_BMI))
sum(!is.na(duomenys_isskirtys$outlier_BMI)) #1 isskirtiss

#paziurime, ar turime salyginiu isskirciu
mean(is.na(duomenys_isskirtys$salygine_outlier_BMI))
sum(!is.na(duomenys_isskirtys$salygine_outlier_BMI))#7 salygines isskirtis

#pasiziurime boxplot
g6<-ggplot(duomenys_isskirtys, aes(x=Outcome, y=BMI)) +
  geom_boxplot() + ggtitle("BMI boxplot by outcome")+
  ylab("BMI value") + 
  geom_point(data = subset(duomenys_isskirtys, outlier_BMI != "NA"),
             aes(x=Outcome, y=BMI), size = 2, color = "red")  +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(subtitle = "Colored dot for outlier and labeled quantiles")+
  stat_summary(fun = "quantile", size = 4,
               geom = "text", aes(label = after_stat(y)),
               position = position_nudge(x = 0.5))

g6

#surasome isskirtis i atskira lentele
isskirtys[(nrow(isskirtys) + 1):(nrow(isskirtys) + nrow(filter(duomenys_isskirtys, !is.na(outlier_BMI)))),] <-
  filter(duomenys_isskirtys, !is.na(outlier_BMI))

#surasome salygines isskirtis i atskira lentele
salygines_isskirtys[(nrow(salygines_isskirtys) + 1):(nrow(salygines_isskirtys) + nrow(filter(duomenys_isskirtys, !is.na(salygine_outlier_BMI)))),] <-
  filter(duomenys_isskirtys, !is.na(salygine_outlier_BMI))


# diabetes pedigree function ----------------------------------------------
#paziurime, ar turime isskirciu
mean(is.na(duomenys_isskirtys$outlier_diabetespedigreefunction))
sum(!is.na(duomenys_isskirtys$outlier_diabetespedigreefunction)) #8 isskirtys

#paziurime, ar turime salyginiu isskirciu
mean(is.na(duomenys_isskirtys$salygine_outlier_diabetespedigreefunction))
sum(!is.na(duomenys_isskirtys$salygine_outlier_diabetespedigreefunction))#15 salyginiu isskirciu

#pasiziurime boxplot
g7<-ggplot(duomenys_isskirtys, aes(x=Outcome, y=DiabetesPedigreeFunction)) +
  geom_boxplot() + ggtitle("Diabetes pedigree function boxplot by outcome")+
  ylab("Diabetes pedigree functions value") + 
  geom_point(data = subset(duomenys_isskirtys, outlier_diabetespedigreefunction != "NA"),
             aes(x=Outcome, y=DiabetesPedigreeFunction), size = 2, color = "red")   +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(subtitle = "Colored dots for outliers and labeled quantiles")+
  stat_summary(fun = "quantile", size = 4,
               geom = "text", aes(label = round(after_stat(y), 2)),
               position = position_nudge(x = 0.5))

g7

#surasome isskirtis i atskira lentele
isskirtys[(nrow(isskirtys) + 1):(nrow(isskirtys) + nrow(filter(duomenys_isskirtys, !is.na(outlier_diabetespedigreefunction)))),] <-
  filter(duomenys_isskirtys, !is.na(outlier_diabetespedigreefunction))

#surasome salygines isskirtis i atskira lentele
salygines_isskirtys[(nrow(salygines_isskirtys) + 1):(nrow(salygines_isskirtys) + nrow(filter(duomenys_isskirtys, !is.na(salygine_outlier_diabetespedigreefunction)))),] <-
  filter(duomenys_isskirtys, !is.na(salygine_outlier_diabetespedigreefunction))


# Age ---------------------------------------------------------------------
#paziurime, ar turime isskirciu
mean(is.na(duomenys_isskirtys$outlier_age))
sum(!is.na(duomenys_isskirtys$outlier_age)) #1 isskirtys

#paziurime, ar turime salyginiu isskirciu
mean(is.na(duomenys_isskirtys$salygine_outlier_age))
sum(!is.na(duomenys_isskirtys$salygine_outlier_age))#25 salyginiu isskirciu

#pasiziurime boxplot
g8<-ggplot(duomenys_isskirtys, aes(x=Outcome, y=Age)) +
  geom_boxplot() + ggtitle("Age boxplot by outcome")+
  ylab("Age") + 
  geom_point(data = subset(duomenys_isskirtys, outlier_age != "NA"),
             aes(x=Outcome, y=Age), size = 2, color = "red") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(subtitle = "Colored dot for outlier and labeled quantiles")+
  stat_summary(fun = "quantile", size = 4,
               geom = "text", aes(label = after_stat(y)),
               position = position_nudge(x = 0.5))

g8

#surasome isskirtis i atskira lentele
isskirtys[(nrow(isskirtys) + 1):(nrow(isskirtys) + nrow(filter(duomenys_isskirtys, !is.na(outlier_age)))),] <-
  filter(duomenys_isskirtys, !is.na(outlier_age))

#surasome salygines isskirtis i atskira lentele
salygines_isskirtys[(nrow(salygines_isskirtys) + 1):(nrow(salygines_isskirtys) + nrow(filter(duomenys_isskirtys, !is.na(salygine_outlier_age)))),] <-
  filter(duomenys_isskirtys, !is.na(salygine_outlier_age))


# boxplot panele ----------------------------------------------------------

gridExtra::grid.arrange(g1, g2, g3, g5,  nrow=2)
gridExtra::grid.arrange(g4, g6,  nrow=1)
gridExtra::grid.arrange(g7, g8,  nrow=1)
gridExtra::grid.arrange(g4, g6, g7, g8,  nrow=1)

# suskaiciuojame kiek turim isskirciu bei salyginiu -----------------------
isskirtys<-unique(isskirtys)
nrow(isskirtys) #16 isskirciu per visus pozymius

salygines_isskirtys<-unique(salygines_isskirtys)
nrow(salygines_isskirtys) #111 salyginiu isskirciu


# Papildomi grafikai ------------------------------------------------------

#histogramos
gg1<-ggplot(duomenys_be_nuliu, aes(x=Pregnancies, color=Outcome, fill = Outcome)) +
  geom_histogram() + ylab("Count") + ggtitle("Pregnancies histogram by outcome")

gg2<-ggplot(duomenys_be_nuliu, aes(x=Glucose, fill=Outcome))+
  geom_histogram() + ylab("Count") + ggtitle("Glucose histogram by outcome") + xlab("Glucose concentration")

gg3<-ggplot(duomenys_be_nuliu, aes(x=BloodPressure, fill=Outcome))+ 
  geom_histogram() + ylab("Count") + ggtitle("Blood pressure histogram by outcome") + xlab("Blood pressure")

gg4<-ggplot(duomenys_be_nuliu, aes(x=SkinThickness, fill=Outcome))+ 
  geom_histogram() + ylab("Count") + ggtitle("Skin thickness histogram by outcome") + xlab("Skin thickness (mm)")

gg5<-ggplot(duomenys_be_nuliu, aes(x=Insulin, fill=Outcome))+ 
  geom_histogram() + ylab("Count") + ggtitle("Insulin histogram by outcome") + xlab("Insulin concentration")

gg6<-ggplot(duomenys_be_nuliu, aes(x=BMI, fill=Outcome))+ 
  geom_histogram() + ylab("Count") + ggtitle("BMI histogram by outcome") + xlab("BMI value")

gg7<-ggplot(duomenys_be_nuliu, aes(x=DiabetesPedigreeFunction, fill=Outcome))+ 
  geom_histogram() + ylab("Count") + ggtitle("Diabetes pedigree function histogram by outcome") + 
  xlab("Diabetes pedigree functions value")



ggarrange(gg1, gg2, gg3, gg4, gg5, gg6, gg7, ncol=2, nrow = 4, common.legend = TRUE, legend="right")

dev.off()
#scatter plot  
pairs(duomenys_be_nuliu[,1:8], pch = 19,  cex = 0.9, cex.labels=1.2,
      col = duomenys_be_nuliu$Outcome,labels = c("Pregnancies", "Glucose", "Blood pressure", "Skin thickness", "Insulin", "BMI", "Diabetes pedigree function", "Age"),
      lower.panel=NULL)
par(xpd = TRUE)
legend("bottomleft", cex = 0.7,  fill = unique(duomenys_be_nuliu$Outcome), legend = c( levels(duomenys_be_nuliu$Outcome)),
       title = "Outcome")



# Duomenu normavimas ------------------------------------------------------

data_mod1 <- melt(duomenys_be_nuliu, id.vars = "Age",
                  measure.vars = c("Pregnancies", "Glucose", "BloodPressure", 
                                   "SkinThickness", "Insulin", "BMI", 
                                   "DiabetesPedigreeFunction", "Age"))

p1 <- ggplot(data_mod1) +
  geom_boxplot(aes( y=value, x = as.factor(variable))) + 
  ggtitle("Variable boxplots before rationing") +
  xlab("Variable name") + ylab("Value") +
  theme(plot.title = element_text(size = 20, hjust = 0.5), axis.text.x = element_text(size = 17, angle = 45, hjust=1))
p1
#normavimas pagal vidurki ir dispersija
mean_sd_norm <- function(x) {
  (x - mean(x))/ sd(x)
}

mean_sd_norm.data <- duomenys_be_nuliu #sukuriame duomenu aibes kopija
#normavimas pagal vidurki ir dispersija
mean_sd_norm.data$Pregnancies <- mean_sd_norm(mean_sd_norm.data$Pregnancies)
mean_sd_norm.data$Glucose <- mean_sd_norm(mean_sd_norm.data$Glucose)
mean_sd_norm.data$BloodPressure <- mean_sd_norm(mean_sd_norm.data$BloodPressure)
mean_sd_norm.data$SkinThickness <- mean_sd_norm(mean_sd_norm.data$SkinThickness)
mean_sd_norm.data$Insulin <- mean_sd_norm(mean_sd_norm.data$Insulin)
mean_sd_norm.data$BMI <- mean_sd_norm(mean_sd_norm.data$BMI)
mean_sd_norm.data$DiabetesPedigreeFunction <- mean_sd_norm(mean_sd_norm.data$DiabetesPedigreeFunction)
mean_sd_norm.data$Age <- mean_sd_norm(mean_sd_norm.data$Age)

#boxplot sunormuotiems duomenims pagal vidurki ir dispersija
data_mod2 <- melt(mean_sd_norm.data, id.vars = "Age",
                  measure.vars = c("Pregnancies", "Glucose", "BloodPressure", 
                                   "SkinThickness", "Insulin", "BMI", 
                                   "DiabetesPedigreeFunction", "Age"))

p2 <- ggplot(data_mod2) +
  geom_boxplot(aes( y=value, x = as.factor(variable))) + ggtitle("Variable boxplots after standartizing") +
  xlab("Variable name") + ylab("Value") +
  theme(plot.title = element_text(size = 20, hjust = 0.5), axis.text.x = element_text(size = 17, angle = 45, hjust=1))
p2

gridExtra::grid.arrange(p1, p2,  nrow=1)



# PCA ---------------------------------------------------------------------

dim(mean_sd_norm.data) 
str(mean_sd_norm.data) #paziurime kiekvieno atributo tipa
data <- mean_sd_norm.data #imamame sunormuota duomenu rinkini
colSums(is.na(data)) #tikriname ar nera praleistu reiksmiu
data_no_id <- data[,1:9] #duomenu rinkinys be paciento id

# PCA su isskirtimis ------------------------------------------------------
res.pca <- prcomp(data_no_id[,c(1:8)],
                  scale. = TRUE)

summary(res.pca)
eig.val<-get_eigenvalue(res.pca)
fviz_eig(res.pca, col.var="blue", addlabels=TRUE, hjust = -0.3)

autoplot(res.pca, data=data_no_id, colour="Outcome")


# PCA be isskirciu --------------------------------------------------------

data_no_outliers <- duomenys_be_nuliu[!(row.names(duomenys_be_nuliu) %in% c("12","146", '218', '237', '273', 
                                                                            '461', '611', '169','5', '55',
                                                                            '352', '376', '423', '563',
                                                                            '588', '436')),]
data_no_outliers_no_id <- data_no_outliers[,1:9] #duomenu rinkinys be paciento id
#numerical_data_no_outliers <- data_no_outliers[,1:8] #issirenkame tik skaitinius atributus

#duomenu be isskirciu normavimas
data_no_outliers_no_id$Pregnancies <- mean_sd_norm(data_no_outliers_no_id$Pregnancies)
data_no_outliers_no_id$Glucose <- mean_sd_norm(data_no_outliers_no_id$Glucose)
data_no_outliers_no_id$BloodPressure <- mean_sd_norm(data_no_outliers_no_id$BloodPressure)
data_no_outliers_no_id$SkinThickness <- mean_sd_norm(data_no_outliers_no_id$SkinThickness)
data_no_outliers_no_id$Insulin <- mean_sd_norm(data_no_outliers_no_id$Insulin)
data_no_outliers_no_id$BMI <- mean_sd_norm(data_no_outliers_no_id$BMI)
data_no_outliers_no_id$DiabetesPedigreeFunction <- mean_sd_norm(data_no_outliers_no_id$DiabetesPedigreeFunction)
data_no_outliers_no_id$Age <- mean_sd_norm(data_no_outliers_no_id$Age)

#PCA BE ISSKIRCIU
res.pca.no.outliers <- prcomp(data_no_outliers_no_id[,c(1:8)],
                              scale. = TRUE)

summary(res.pca.no.outliers)
eig.val<-get_eigenvalue(res.pca.no.outliers)
fviz_eig(res.pca.no.outliers, col.var="blue", addlabels=TRUE, hjust = -0.3)

autoplot(res.pca.no.outliers, data=data_no_outliers_no_id, colour="Outcome")


# t-SNE ----------------------------------------------------------------

# pagal perplexity
input_data <- duomenys_be_nuliu[,c(1:8,10)]
output_data <- duomenys_be_nuliu[,9]

plot.new()
par(mfrow = c(2,2))

tsne_results_1 <- Rtsne(input_data, perplexity = 10)
plot(tsne_results_1$Y, col = "oldlace", bg = as.numeric(output_data$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Perplexity = 10', cex.main=1)

tsne_results_2 <- Rtsne(input_data, perplexity = 30)
plot(tsne_results_2$Y, col = "oldlace", bg = as.numeric(output_data$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Perplexity = 30', cex.main=1)

tsne_results_3 <- Rtsne(input_data, perplexity = 50)
plot(tsne_results_3$Y, col = "oldlace", bg = as.numeric(output_data$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Perplexity = 50', cex.main=1)

tsne_results_4 <- Rtsne(input_data, perplexity = 100)
plot(tsne_results_4$Y, col = "oldlace", bg = as.numeric(output_data$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Perplexity = 100', cex.main=1)

# duomenys be iskirciu 
input_data_out <- numerical_data_no_outliers
output_data_out <- data_no_outliers[,9]

plot.new()
par(mfrow=c(2,2))

tsne_results_5 <- Rtsne(input_data_out, perplexity = 10)
plot(tsne_results_5$Y, col = "oldlace", bg = as.numeric(output_data_out$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Perplexity = 10')

tsne_results_6 <- Rtsne(input_data_out, perplexity = 30)
plot(tsne_results_6$Y, col = "oldlace", bg = as.numeric(output_data_out$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Perplexity = 30')

tsne_results_7 <- Rtsne(input_data_out, perplexity = 50)
plot(tsne_results_7$Y, col = "oldlace", bg = as.numeric(output_data_out$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Perplexity = 50')

tsne_results_8 <- Rtsne(input_data_out, perplexity = 100)
plot(tsne_results_8$Y, col = "oldlace", bg = as.numeric(output_data_out$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Perplexity = 100')


# keiciant max iteraciju skaiciu
plot.new()
par(mfrow=c(2,2))

tsne_results_9 <- Rtsne(input_data_out, perplexity = 50, max_iter = 50)
plot(tsne_results_9$Y, col = "oldlace", bg = as.numeric(output_data_out$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Max_iter = 50')

tsne_results_10 <- Rtsne(input_data_out, perplexity = 50, max_iter = 100)
plot(tsne_results_10$Y, col = "oldlace", bg = as.numeric(output_data_out$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Max_iter = 100')

tsne_results_11 <- Rtsne(input_data_out, perplexity = 50, max_iter = 500)
plot(tsne_results_11$Y, col = "oldlace", bg = as.numeric(output_data_out$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Max_iter = 500')

tsne_results_12 <- Rtsne(input_data_out, perplexity = 50, max_iter = 1000)
plot(tsne_results_12$Y, col = "oldlace", bg = as.numeric(output_data_out$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Max_iter = 1000')


# keiciant theta 
plot.new()
par(mfrow=c(2,2))

tsne_results_13 <- Rtsne(input_data_out, perplexity = 30, max_iter = 500, theta = 1)
plot(tsne_results_13$Y, col = "oldlace", bg = as.numeric(output_data_out$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Theta = 1')

tsne_results_14 <- Rtsne(input_data_out, perplexity = 30, max_iter = 500, theta = 0.5)
plot(tsne_results_14$Y, col = "oldlace", bg = as.numeric(output_data_out$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Theta = 0.5')

tsne_results_15 <- Rtsne(input_data_out, perplexity = 30, max_iter = 500, theta = 0.25)
plot(tsne_results_15$Y, col = "oldlace", bg = as.numeric(output_data_out$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Theta = 0.25')

tsne_results_16 <- Rtsne(input_data_out, perplexity = 30, max_iter = 500, theta = 0)
plot(tsne_results_16$Y, col = "oldlace", bg = as.numeric(output_data_out$Outcome), pch = 21, cex = 0.8, 
     xlab = '', ylab = '', main = 'Theta = 0')



# MDS metriniai ---------------------------------------------------------------------
dev.off()
#canberra method

dist_canberra_duomenys <- dist(mean_sd_norm.data[, 1:8], method = "canberra")
mds_canberra_duomenys <- cmdscale(dist_canberra_duomenys, eig = TRUE, k = 2)
mds_canberra_duomenys$GOF #It indicates the fraction of the total variance of the data represented in the MDS plot.

mds_canberra_duomenys1<- data.frame(
  MDS1 = mds_canberra_duomenys$points[, 1],
  MDS2 = mds_canberra_duomenys$points[, 2],
  label = duomenys_be_nuliu$paciento_id,
  classification = duomenys_be_nuliu$Outcome,
  classification1 = ifelse(duomenys_be_nuliu$paciento_id %in% isskirtys$paciento_id, -1, ifelse(duomenys_be_nuliu$Outcome == 0, 0, 1))
)

met_can<-ggplot(mds_canberra_duomenys1, mapping=aes(
  x = MDS1, y = MDS2,col = as.factor(classification1))) +
  geom_point() + scale_color_manual(values=c("black", "#E69F00", "#56B4E9"), labels = c("Outlier", "0", "1")) +
  guides(color = guide_legend(title = "Classification"))+
  ggtitle("Metric MDS using canberra method")#turbut sitaip geriausiai
met_can

# euklido -----------------------------------------------------------------


dist_euklido_duomenys <- dist(mean_sd_norm.data[, 1:8], method = "euclidean")
mds_euklido_duomenys <- cmdscale(dist_euklido_duomenys, eig = TRUE, k = 2)
mds_euklido_duomenys$GOF 

mds_euklido_duomenys1<- data.frame(
  MDS1 = mds_euklido_duomenys$points[, 1],
  MDS2 = mds_euklido_duomenys$points[, 2],
  label = duomenys_be_nuliu$paciento_id,
  classification = duomenys_be_nuliu$Outcome,
  classification1 = ifelse(duomenys_be_nuliu$paciento_id %in% isskirtys$paciento_id, -1, ifelse(duomenys_be_nuliu$Outcome == 0, 0, 1))
)

met_euc1<-ggplot(mds_euklido_duomenys1, aes(
  x = MDS1, y = MDS2,col = as.factor(classification1))) +
  geom_point() + scale_color_manual(values=c("black", "#E69F00", "#56B4E9"), labels = c("Outlier", "0", "1")) +
  guides(color = guide_legend(title = "Classification"))+
  ggtitle("Metric MDS using euclidean method ")#turbut sitaip geriausiai #turbut geriausiai, galima pakeitineti MDS2 ir MDS3

met_euc1

# maximum -----------------------------------------------------------------

dist_max_duomenys <- dist(mean_sd_norm.data[, 1:8], method = "maximum")
mds_max_duomenys <- cmdscale(dist_max_duomenys, eig = TRUE, k = 2)
mds_max_duomenys$GOF

mds_max_duomenys1<- data.frame(
  MDS1 = mds_max_duomenys$points[, 1],
  MDS2 = mds_max_duomenys$points[, 2],
  label = duomenys_be_nuliu$paciento_id,
  classification = duomenys_be_nuliu$Outcome,
  classification1 = ifelse(duomenys_be_nuliu$paciento_id %in% isskirtys$paciento_id, -1, ifelse(duomenys_be_nuliu$Outcome == 0, 0, 1))
)


met_max2<-ggplot(mds_max_duomenys1, aes(
  x = MDS1, y = MDS2,col = as.factor(classification1))) +
  geom_point() + scale_color_manual(values=c("black", "#E69F00", "#56B4E9"), labels = c("Outlier", "0", "1")) +
  guides(color = guide_legend(title = "Classification"))+
  ggtitle("Metric MDS using maximum method")#turbut sitaip geriausiai #turbut geriausiai, galima pakeitineti MDS2 ir MDS3


met_max2
# manhatten ---------------------------------------------------------------
dist_manhaten_duomenys <- dist(mean_sd_norm.data[, 1:8], method = "manhattan")
mds_manhatten_duomenys <- cmdscale(dist_manhaten_duomenys, eig = TRUE, k = 2)
mds_manhatten_duomenys$GOF

mds_manhatten_duomenys1<- data.frame(
  MDS1 = mds_manhatten_duomenys$points[, 1],
  MDS2 = mds_manhatten_duomenys$points[, 2],
  label = duomenys_be_nuliu$paciento_id,
  classification = duomenys_be_nuliu$Outcome,
  classification1 = ifelse(duomenys_be_nuliu$paciento_id %in% isskirtys$paciento_id, -1, ifelse(duomenys_be_nuliu$Outcome == 0, 0, 1))
)

met_manh<-ggplot(mds_manhatten_duomenys1, aes(
  x = MDS1, y = MDS2,col = as.factor(classification1))) +
  geom_point() + scale_color_manual(values=c("black", "#E69F00", "#56B4E9"), labels = c("Outlier", "0", "1")) +
  guides(color = guide_legend(title = "Classification"))+
  ggtitle("Metric MDS using manhattan method")
met_manh

# minkowski ---------------------------------------------------------------

dist_minkowski_duomenys <- dist(mean_sd_norm.data[, 1:8], method = "minkowski")
mds_minkowski_duomenys <- cmdscale(dist_minkowski_duomenys, eig = TRUE, k = 2)
mds_minkowski_duomenys$GOF#

mds_minkowski_duomenys1<- data.frame(
  MDS1 = mds_minkowski_duomenys$points[, 1],
  MDS2 = mds_minkowski_duomenys$points[, 2],
  label = duomenys_be_nuliu$paciento_id,
  classification = duomenys_be_nuliu$Outcome,
  classification1 = ifelse(duomenys_be_nuliu$paciento_id %in% isskirtys$paciento_id, -1, ifelse(duomenys_be_nuliu$Outcome == 0, 0, 1))
)

met_min<-ggplot(mds_minkowski_duomenys1, aes(
  x = MDS1, y = MDS2,col = as.factor(classification1))) +
  geom_point() + scale_color_manual(values=c("black", "#E69F00", "#56B4E9"), labels = c("Outlier", "0", "1")) +
  guides(color = guide_legend(title = "Classification"))+
  ggtitle("Metric MDS using minkowski method")#turbut sitaip geriausiai #turbut geriausiai, galima pakeitineti MDS2 ir MDS3

met_min
dev.off()
ggarrange(met_can, met_euc1, met_max2,met_manh,met_min, ncol = 2, nrow=3, common.legend = TRUE, legend="right")
ggarrange(met_can, met_euc1, met_max2,met_manh,met_min, ncol = 3, nrow=2, common.legend = TRUE, legend="right")

mds_canberra_duomenys$GOF
mds_euklido_duomenys$GOF
mds_max_duomenys$GOF 
mds_manhatten_duomenys$GOF
mds_minkowski_duomenys$GOF#
# non metric -------------------------------------------------------------


library(vegan)#man atrodo nonmetric
non_bray <- metaMDS(comm = mean_sd_norm.data[,1:8], distance = "bray", trace = FALSE, autotransform = FALSE, k=2)
MDS_non_bray <- data.frame(non_bray$points)
MDS_non_bray$outcome<-ifelse(duomenys_be_nuliu$paciento_id %in% isskirtys$paciento_id, -1, ifelse(duomenys_be_nuliu$Outcome == 0, 0, 1))

non_bray_metric<-ggplot(MDS_non_bray, aes(MDS1, MDS2, col = as.factor(outcome))) +
  geom_point() + scale_color_manual(values=c("black", "#E69F00", "#56B4E9"), labels = c("Outlier", "0", "1")) +
  guides(color = guide_legend(title = "Classification"))+
  ggtitle("Non - metric MDS using bray method")
non_bray_metric
non_bray$stress


non_euclidean <- metaMDS(comm = mean_sd_norm.data[,1:8], distance = "euclidean", trace = FALSE, autotransform = FALSE, k=2)
MDS_non_euc <- data.frame(non_euclidean$points)
MDS_non_euc$outcome<-ifelse(duomenys_be_nuliu$paciento_id %in% isskirtys$paciento_id, -1, ifelse(duomenys_be_nuliu$Outcome == 0, 0, 1))

non_metric_euc<-ggplot(MDS_non_euc, aes(MDS1, MDS2, col = as.factor(outcome))) +
  geom_point() + scale_color_manual(values=c("black", "#E69F00", "#56B4E9"), labels = c("Outlier", "0", "1")) +
  guides(color = guide_legend(title = "Classification"))+
  ggtitle("Non - metric MDS using euclidean method")
non_euclidean$stress
non_metric_euc



non_man<- metaMDS(comm = mean_sd_norm.data[,1:8], distance = "manhattan", trace = FALSE, autotransform = FALSE, k=2)
MDS_non_man <- data.frame(non_man$points)
MDS_non_man$outcome<-ifelse(duomenys_be_nuliu$paciento_id %in% isskirtys$paciento_id, -1, ifelse(duomenys_be_nuliu$Outcome == 0, 0, 1))

non_metric_man<-ggplot(MDS_non_man, aes(MDS1, MDS2, col = as.factor(outcome))) +
  geom_point() + scale_color_manual(values=c("black", "#E69F00", "#56B4E9"), labels = c("Outlier", "0", "1")) +
  guides(color = guide_legend(title = "Classification"))+
  ggtitle("Non - metric MDS using manhattan method")
non_man$stress
non_metric_man

non_can<- metaMDS(comm = mean_sd_norm.data[,1:8], distance = "canberra", trace = FALSE, autotransform = FALSE, k=2)
MDS_non_can <- data.frame(non_can$points)
MDS_non_can$outcome<-ifelse(duomenys_be_nuliu$paciento_id %in% isskirtys$paciento_id, -1, ifelse(duomenys_be_nuliu$Outcome == 0, 0, 1))

non_metric_can<-ggplot(MDS_non_can, aes(MDS1, MDS2, col = as.factor(outcome))) +
  geom_point() + scale_color_manual(values=c("black", "#E69F00", "#56B4E9"), labels = c("Outlier", "0", "1")) +
  guides(color = guide_legend(title = "Classification"))+
  ggtitle("Non - metric MDS using canberra method")
non_can$stress
non_metric_can

non_kul<- metaMDS(comm = mean_sd_norm.data[,1:8], distance = "kulczynski", trace = FALSE, autotransform = FALSE, k=2)
MDS_non_kul <- data.frame(non_kul$points)
MDS_non_kul$outcome<-ifelse(duomenys_be_nuliu$paciento_id %in% isskirtys$paciento_id, -1, ifelse(duomenys_be_nuliu$Outcome == 0, 0, 1))

non_metric_kul1<-ggplot(MDS_non_kul, aes(MDS1, MDS2, col = as.factor(outcome))) +
  geom_point() + scale_color_manual(values=c("black", "#E69F00", "#56B4E9"), labels = c("Outlier", "0", "1")) +
  guides(color = guide_legend(title = "Classification"))+
  ggtitle("Non - metric MDS using kulczynski method ")

non_kul$stress
non_metric_kul1


dev.off()
ggarrange(non_bray_metric, non_metric_euc, non_metric_man, non_metric_can, non_metric_kul1,
          ncol = 2, nrow=3, common.legend = TRUE, legend="right")
ggarrange(non_bray_metric, non_metric_euc, non_metric_man, non_metric_can, non_metric_kul1,
          ncol = 3, nrow=2, common.legend = TRUE, legend="right")



non_bray$stress
non_kul$stress
non_can$stress
non_man$stress
non_euclidean$stress