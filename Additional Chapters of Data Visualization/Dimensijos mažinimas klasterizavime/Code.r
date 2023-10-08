# reikalingos bibliotekos -------------------------------------------------

library(dplyr)
library(tidyverse)
library("writexl")
library("readxl")
library(tidyr)
library(reshape)
library(reshape2)
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
library(dbscan)
library("fpc")
library("factoextra")
library(cluster)

# duomenu nuskaitymas -----------------------------------------------------

duomenys_be_nuliu <- read_excel("C:/Users/ugneo/OneDrive/Stalinis kompiuteris/Duomenų mokslas/6 semestras/Projektinis darbas/be_nuliu.xlsx")

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


# Isskirtys --------------------------------------------------------------

# nagrinesi nestumu skaiciu -----------------------------------------------

#paziurime, ar turime isskirciu
mean(is.na(duomenys_isskirtys$outlier_pregnancies))
sum(!is.na(duomenys_isskirtys$outlier_pregnancies)) #is abieju rodikliu matome, jog isskirciu neturime

#paziurime, ar turime salyginiu isskirciu
mean(is.na(duomenys_isskirtys$salygine_outlier_pregnancies))
sum(!is.na(duomenys_isskirtys$salygine_outlier_pregnancies))#turime 15 salygines isskirtis

#surasome i isskirciu salyginiu lentele
salygines_isskirtys<-data.frame()
salygines_isskirtys<-duomenys_isskirtys %>% filter(!is.na(salygine_outlier_pregnancies))

# nagrinejame glucose -----------------------------------------------------

#paziurime, ar turime isskirciu
mean(is.na(duomenys_isskirtys$outlier_glucose))
sum(!is.na(duomenys_isskirtys$outlier_glucose)) #is abieju rodikliu matome, jog isskirciu neturime

#paziurime, ar turime salyginiu isskirciu
mean(is.na(duomenys_isskirtys$salygine_outlier_glucose))
sum(!is.na(duomenys_isskirtys$salygine_outlier_glucose))#17 salyginiu isskirciu

#surasome i salyginiu isskirciu lentele
salygines_isskirtys[(nrow(salygines_isskirtys) + 1):(nrow(salygines_isskirtys) + nrow(filter(duomenys_isskirtys, !is.na(salygine_outlier_glucose)))),] <-
  filter(duomenys_isskirtys, !is.na(salygine_outlier_glucose))

# nagrinejame blood pressure ----------------------------------------------

#paziurime, ar turime isskirciu
mean(is.na(duomenys_isskirtys$outlier_bloodpressure))
sum(!is.na(duomenys_isskirtys$outlier_bloodpressure)) #is abieju rodikliu matome, jog isskirciu neturime

#paziurime, ar turime salyginiu isskirciu
mean(is.na(duomenys_isskirtys$salygine_outlier_bloodpressure))
sum(!is.na(duomenys_isskirtys$salygine_outlier_bloodpressure) )# 15 salyginiu isskirciu

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

#surasome isskirtis i atskira lentele
isskirtys[(nrow(isskirtys) + 1):(nrow(isskirtys) + nrow(filter(duomenys_isskirtys, !is.na(outlier_age)))),] <-
  filter(duomenys_isskirtys, !is.na(outlier_age))

#surasome salygines isskirtis i atskira lentele
salygines_isskirtys[(nrow(salygines_isskirtys) + 1):(nrow(salygines_isskirtys) + nrow(filter(duomenys_isskirtys, !is.na(salygine_outlier_age)))),] <-
  filter(duomenys_isskirtys, !is.na(salygine_outlier_age))

# suskaiciuojame kiek turim isskirciu bei salyginiu -----------------------

isskirtys<-unique(isskirtys)
nrow(isskirtys) #16 isskirciu per visus pozymius

salygines_isskirtys<-unique(salygines_isskirtys)
nrow(salygines_isskirtys) #111 salyginiu isskirciu

# Duomenu normavimas ------------------------------------------------------

data_mod1 <- melt(duomenys_be_nuliu, id.vars = "Age",
                  measure.vars = c("Pregnancies", "Glucose", "BloodPressure", 
                                   "SkinThickness", "Insulin", "BMI", 
                                   "DiabetesPedigreeFunction", "Age"))

p1 <- ggplot(data_mod1) +
  geom_boxplot(aes( y=value, x = as.factor(variable))) + 
  ggtitle("Variable boxplots before rationing") +
  xlab("Variable name") + ylab("Value") +
  theme(plot.title = element_text(size = 20, hjust = 0.5), 
        axis.text.x = element_text(size = 17, angle = 45, hjust=1))
p1

#normavimas pagal vidurki ir dispersija ---------------------------------
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
  geom_boxplot(aes( y=value, x = as.factor(variable))) + 
  ggtitle("Variable boxplots after standartizing") +
  xlab("Variable name") + ylab("Value") +
  theme(plot.title = element_text(size = 20, hjust = 0.5), 
        axis.text.x = element_text(size = 17, angle = 45, hjust=1))
p2

# normavimas pagal min max reiksmes -------------------------------------
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

min_max_norm.data <- duomenys_be_nuliu #xsukuriame duomenu aibes kopija

#normavimas min-max
min_max_norm.data$Pregnancies <- min_max_norm(min_max_norm.data$Pregnancies)
min_max_norm.data$Glucose <- min_max_norm(min_max_norm.data$Glucose)
min_max_norm.data$BloodPressure <- min_max_norm(min_max_norm.data$BloodPressure)
min_max_norm.data$SkinThickness <- min_max_norm(min_max_norm.data$SkinThickness)
min_max_norm.data$Insulin <- min_max_norm(min_max_norm.data$Insulin)
min_max_norm.data$BMI <- min_max_norm(min_max_norm.data$BMI)
min_max_norm.data$DiabetesPedigreeFunction <- min_max_norm(min_max_norm.data$DiabetesPedigreeFunction)
min_max_norm.data$Age <- min_max_norm(min_max_norm.data$Age)


#boxplot sunormuotiems duomenims pagal min ir max
data_mod3 <- melt(min_max_norm.data, id.vars = "Age",
                  measure.vars = c("Pregnancies", "Glucose", "BloodPressure", 
                                   "SkinThickness", "Insulin", "BMI", 
                                   "DiabetesPedigreeFunction", "Age"))

p3 <- ggplot(data_mod3) +
  geom_boxplot(aes( y=value, x = as.factor(variable))) + 
  ggtitle("Variable boxplots after standartizing") +
  xlab("Variable name") + ylab("Value") +
  theme(plot.title = element_text(size = 20, hjust = 0.5), 
        axis.text.x = element_text(size = 17, angle = 45, hjust=1))
p3

gridExtra::grid.arrange(p1, p2, p3, nrow=1)

# k-means -----------------------------------------------------------------

# visos kovariantes -------------------------------------------------------

df <- duomenys_be_nuliu

#pasalinam 2 stulpelius - ju klasterizavimui neprireiks
df_1 <- df[,-9:-10]

# Fitting K-Means clustering Model 
set.seed(240) 


k2 <- kmeans(df_1, centers = 2, nstart = 25)
k3 <- kmeans(df_1, centers = 3, nstart = 25)
k4 <- kmeans(df_1, centers = 4, nstart = 25)
k5 <- kmeans(df_1, centers = 5, nstart = 25)

p1 <- fviz_cluster(k2, geom = "point", data = df_1) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df_1) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df_1) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df_1) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)

#Elbow Method
fviz_nbclust(df_1, kmeans, method = "wss") + labs(title= "Optimalus klasterių skaičius") + xlab("Klasterių skaičius") +ylab(" ")


#Average Silhouette Method
fviz_nbclust(df_1, kmeans, method = "silhouette") + labs(title= "Optimalus klasterių skaičius") + xlab("Klasterių skaičius") +ylab("Vidutinis silueto plotis ") 

#pridedame klasteri prie visu duomenu
cluster2 <- k2$cluster
final_data2 <- duomenys_be_nuliu
final_data2$cluster <- cluster2
table(k2$cluster, final_data2$Outcome)

describeBy(final_data2, group = final_data2$cluster)

#3 klasteriai
cluster3 <- k3$cluster
final_data3 <- duomenys_be_nuliu
table(k3$cluster, final_data3$Outcome)
final_data3$cluster <- cluster3

describeBy(final_data3, group = final_data3$cluster)


# reiksmingos kovariantes -------------------------------------------------
#nėštumų sk, gliukozės kiek, KMI ir diabeto kilmės fja

#atrenkam stulpelius
df_2 <- df_1[, -3:-5]
duom <- duomenys_be_nuliu[, -3:-4]
duom <- duom[,-3]
duom <- duom[,-5]
duom <- duom[,-6]
df_2 <- df_2[,-5]


k2_2 <- kmeans(df_2, centers = 2, nstart = 25)
k3_2 <- kmeans(df_2, centers = 3, nstart = 25)
k4_2 <- kmeans(df_2, centers = 4, nstart = 25)
k5_2 <- kmeans(df_2, centers = 5, nstart = 25)

p1_2 <- fviz_cluster(k2_2, geom = "point", data = df_2 ) + ggtitle("k = 2")
p2_2 <- fviz_cluster(k3_2, geom = "point",  data = df_2) + ggtitle("k = 3")
p3_2 <- fviz_cluster(k4_2, geom = "point",  data = df_2) + ggtitle("k = 4")
p4_2 <- fviz_cluster(k5_2, geom = "point",  data = df_2) + ggtitle("k = 5")

grid.arrange(p1_2, p2_2, p3_2, p4_2, nrow = 2)

#Elbow Method
fviz_nbclust(df_2, kmeans, method = "wss") + labs(title= "Optimalus klasterių skaičius") + xlab("Klasterių skaičius") +ylab(" ") 

#Average Silhouette Method
fviz_nbclust(df_2, kmeans, method = "silhouette") + labs(title= "Optimalus klasterių skaičius") + xlab("Klasterių skaičius") +ylab("Vidutinis silueto plotis ") 

#pridedame klasteri prie visu duomenu
cluster2_2 <- k2_2$cluster
final_data2_2 <- duom
final_data2_2$cluster <- cluster2_2
table(k2_2$cluster, final_data2_2$Outcome)

describeBy(final_data2_2, group = final_data2_2$cluster)

#3 klasteriai
cluster3_2 <- k3_2$cluster
final_data3_2 <- duom
final_data3_2$cluster <- cluster3_2
table(k3_2$cluster, final_data3_2$Outcome)

describeBy(final_data3_2, group = final_data3_2$cluster)



# dim ---------------------------------------------------------------------
dev.off()

dist_euklido_duomenys <- dist(min_max_norm.data[, 1:8], method = "euclidean")
mds_euklido_duomenys <- cmdscale(dist_euklido_duomenys, eig = TRUE, k = 2)
mds_euklido_duomenys$GOF 

mds_euklido_duomenys1<- data.frame(
  MDS1 = mds_euklido_duomenys$points[, 1],
  MDS2 = mds_euklido_duomenys$points[, 2],
  label = duomenys_be_nuliu$paciento_id,
  classification = duomenys_be_nuliu$Outcome,
  classification1 = ifelse(duomenys_be_nuliu$paciento_id %in% isskirtys$paciento_id, -1, ifelse(duomenys_be_nuliu$Outcome == 0, 0, 1))
)


df_3 <- mds_euklido_duomenys1[,-3:-5]

k2_3 <- kmeans(df_3, centers = 2, nstart = 25)
k3_3 <- kmeans(df_3, centers = 3, nstart = 25)
k4_3 <- kmeans(df_3, centers = 4, nstart = 25)
k5_3 <- kmeans(df_3, centers = 5, nstart = 25)

p1_3 <- fviz_cluster(k2_3, geom = "point", data = df_3 ) + ggtitle("k = 2")
p2_3 <- fviz_cluster(k3_3, geom = "point",  data = df_3) + ggtitle("k = 3")
p3_3 <- fviz_cluster(k4_3, geom = "point",  data = df_3) + ggtitle("k = 4")
p4_3 <- fviz_cluster(k5_3, geom = "point",  data = df_3) + ggtitle("k = 5")

grid.arrange(p1_3, p2_3, p3_3, p4_3, nrow = 2)

#Elbow Method
fviz_nbclust(df_3, kmeans, method = "wss") + labs(title= "Optimalus klasterių skaičius") + xlab("Klasterių skaičius") +ylab(" ")

#Average Silhouette Method
fviz_nbclust(df_3, kmeans, method = "silhouette") + labs(title= "Optimalus klasterių skaičius") + xlab("Klasterių skaičius") +ylab("Vidutinis silueto plotis ")  


#pridedame klasteri prie visu duomenu
cluster2_3 <- k2_3$cluster
final_data2_3 <- mds_euklido_duomenys1
final_data2_3$cluster <- cluster2_3
table(k2_3$cluster, final_data2_3$classification)

describeBy(final_data2_3, group = final_data2_3$cluster)

#3 klasteriai
cluster3 <- k3_2$cluster
final_data3_2 <- mds_euklido_duomenys1
final_data3_2$cluster <- cluster3_2
table(k3_2$cluster, final_data3_2$classification)

describeBy(final_data3_2, group = final_data3_2$cluster)

# ------------------------------------------------------------------------------------
# Hierarhinis klasterizavimo algoritmas


# ------------ visiems duomenims

data <- min_max_norm.data
data.outcome <- data$Outcome
table(data.outcome)
data.scaled <- scale(data[, -9])
rownames(data.scaled) <- paste(data$Outcome, 1:dim(data)[1], sep = '_')

# euklidinis atstumas

dist.euclid <- dist(data.scaled, method = 'euclidean')

hc.euc_ward <- hclust(dist.euclid, method = 'ward.D')
hc.euc_compl <- hclust(dist.euclid, method = 'complete')
hc.euc_mc <- hclust(dist.euclid, method = 'mcquitty')
dev.off()
par(mfrow=c(1,3))

plot(hc.euc_ward, main = paste('Ward.D 2 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 170, lty = 2, col="red", lwd=3)#2 klasteriai
rect.hclust(hc.euc_ward, k = 2, border = 2:5)

plot(hc.euc_compl, main = paste('Complete 30 klasterių'), xlab = '', ylab = '', hang = -1)
abline(h = 5.75, lty = 2, col="red", lwd=3)#30 klasteriai
rect.hclust(hc.euc_compl, k = 30, border = 2:5)

plot(hc.euc_mc, main = paste('Mcquitty 8 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 5.75, lty = 2, col="red", lwd=3)#8 klasteriai
rect.hclust(hc.euc_mc, k = 8, border = 2:5)

clusters.euc_ward <- cutree(hc.euc_ward, k = 2)
clusters.euc_compl <- cutree(hc.euc_compl, k = 30)
clusters.euc_mc <- cutree(hc.euc_mc, k = 8)

cl1 <- fviz_cluster(list(data = data.scaled, cluster = clusters.euc_ward)) + ggtitle("Ward")
cl2 <- fviz_cluster(list(data = data.scaled, cluster = clusters.euc_compl)) + ggtitle("Complete")
cl3 <- fviz_cluster(list(data = data.scaled, cluster = clusters.euc_mc)) + ggtitle("Mcquitty")

grid.arrange(cl1, cl2, cl3,  nrow = 1)

table(clusters.euc_ward, data.outcome)
table(clusters.euc_compl, data.outcome)
table(clusters.euc_mc, data.outcome)

# manheteno atstumas

dist.man <- dist(data.scaled, method = 'manhattan')

hc.man_ward <- hclust(dist.man, method = 'ward.D')
hc.man_compl <- hclust(dist.man, method = 'complete')
hc.man_mc <- hclust(dist.man, method = 'mcquitty')

par(mfrow=c(1,3))

plot(hc.man_ward, main = paste('Ward.D 3 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 420, lty = 2, col="red", lwd=3)#3 klasteriai
rect.hclust(hc.man_ward, k = 3, border = 2:5)

plot(hc.man_compl, main = paste('Complete 46 klasterių'), xlab = '', ylab = '', hang = -1)
abline(h = 12, lty = 2, col="red", lwd=3)#46 klasteriai
rect.hclust(hc.man_compl, k = 46, border = 2:5)

plot(hc.man_mc, main = paste('Mcquitty 21 klasterių'), xlab = '', ylab = '', hang = -1)
abline(h = 11, lty = 2, col="red", lwd=3)#21 klasteriai
rect.hclust(hc.man_mc, k = 21, border = 2:5)

clusters.man_ward <- cutree(hc.man_ward, k = 3)
clusters.man_compl <- cutree(hc.man_compl, k = 46)
clusters.man_mc <- cutree(hc.man_mc, k = 21)

cl4 <- fviz_cluster(list(data = data.scaled, cluster = clusters.man_ward)) + ggtitle("Ward")
cl5 <- fviz_cluster(list(data = data.scaled, cluster = clusters.man_compl)) + ggtitle("Complete")
cl6 <- fviz_cluster(list(data = data.scaled, cluster = clusters.man_mc)) + ggtitle("Mcquitty")

grid.arrange(cl4, cl5, cl6,  nrow = 1)

table(clusters.man_ward, data.outcome)
table(clusters.man_compl, data.outcome)
table(clusters.man_mc, data.outcome)

# maksimumo atstumas

dist.max <- dist(data.scaled, method = 'maximum')

hc.max_ward <- hclust(dist.max, method = 'ward.D')
hc.max_compl <- hclust(dist.max, method = 'complete')
hc.max_mc <- hclust(dist.max, method = 'mcquitty')

par(mfrow=c(1,3))

plot(hc.max_ward, main = paste('Ward.D 2 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 90, lty = 2, col="red", lwd=3)#2 klasteriai
rect.hclust(hc.max_ward, k = 2, border = 2:5)

plot(hc.max_compl, main = paste('Complete 8 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 5.6, lty = 2, col="red", lwd=3)#8 klasteriai
rect.hclust(hc.max_compl, k = 8, border = 2:5)

plot(hc.max_mc, main = paste('Mcquitty 14 klasterių'), xlab = '', ylab = '', hang = -1)
abline(h = 3, lty = 2, col="red", lwd=3)#14 klasteriai
rect.hclust(hc.max_mc, k = 14, border = 2:5)

clusters.max_ward <- cutree(hc.max_ward, k = 2)
clusters.max_compl <- cutree(hc.max_compl, k = 8)
clusters.max_mc <- cutree(hc.max_mc, k = 14)

cl7 <- fviz_cluster(list(data = data.scaled, cluster = clusters.max_ward)) + ggtitle("Ward")
cl8 <- fviz_cluster(list(data = data.scaled, cluster = clusters.max_compl)) + ggtitle("Complete")
cl9 <- fviz_cluster(list(data = data.scaled, cluster = clusters.max_mc)) + ggtitle("Mcquitty")

grid.arrange(cl7, cl8, cl9,  nrow = 1)

table(clusters.max_ward, data.outcome)
table(clusters.max_compl, data.outcome)
table(clusters.max_mc, data.outcome)

data.scaled <- as.data.frame(data.scaled)
data.scaled <- merge(data.scaled, as.data.frame(clusters.max_ward), by=0)

data.scaled %>% split(.$clusters.max_ward) %>% map(summary)

# ---------------------- reiksmingoms kovariantems

reiksmingos <- min_max_norm.data[,c('Pregnancies', 'Glucose', 'BMI', 'DiabetesPedigreeFunction', 'Outcome')]
reiksmingos.outcome <- reiksmingos$Outcome
table(reiksmingos.outcome)
reiksmingos.scaled <- scale(reiksmingos[, -5])
rownames(reiksmingos.scaled) <- paste(reiksmingos$Outcome, 1:dim(reiksmingos)[1], sep = '_')

# euklidinis atstumas

dist.euclid <- dist(reiksmingos.scaled, method = 'euclidean')

hc.euc_ward <- hclust(dist.euclid, method = 'ward.D')
hc.euc_compl <- hclust(dist.euclid, method = 'complete')
hc.euc_mc <- hclust(dist.euclid, method = 'mcquitty')

par(mfrow=c(1,3))

plot(hc.euc_ward, main = paste('Ward.D 2 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 140, lty = 2, col="red", lwd=3)#2 klasteriai
rect.hclust(hc.euc_ward, k = 2, border = 2:5)

plot(hc.euc_compl, main = paste('Complete 4 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 7, lty = 2, col="red", lwd=3)#4 klasteriai
rect.hclust(hc.euc_compl, k = 4, border = 2:5)

plot(hc.euc_mc, main = paste('Mcquitty 4 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 5, lty = 2, col="red", lwd=3)#4 klasteriai
rect.hclust(hc.euc_mc, k = 4, border = 2:5)

clusters.euc_ward <- cutree(hc.euc_ward, k = 2)
clusters.euc_compl <- cutree(hc.euc_compl, k = 4)
clusters.euc_mc <- cutree(hc.euc_mc, k = 4)

cl11 <- fviz_cluster(list(data = reiksmingos.scaled, cluster = clusters.euc_ward)) + ggtitle("Ward")
cl12 <- fviz_cluster(list(data = reiksmingos.scaled, cluster = clusters.euc_compl)) + ggtitle("Complete")
cl13 <- fviz_cluster(list(data = reiksmingos.scaled, cluster = clusters.euc_mc)) + ggtitle("Mcquitty")

grid.arrange(cl11, cl12, cl13,  nrow = 1)

table(clusters.euc_ward, reiksmingos.outcome)
table(clusters.euc_compl, reiksmingos.outcome)
table(clusters.euc_mc, reiksmingos.outcome)

# manheteno atstumas

dist.man <- dist(reiksmingos.scaled, method = 'manhattan')

hc.man_ward <- hclust(dist.man, method = 'ward.D')
hc.man_compl <- hclust(dist.man, method = 'complete')
hc.man_mc <- hclust(dist.man, method = 'mcquitty')

par(mfrow=c(1,3))

plot(hc.man_ward, main = paste('Ward.D 3 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 225, lty = 2, col="red", lwd=3)#3 klasteriai
rect.hclust(hc.man_ward, k = 3, border = 2:5)

plot(hc.man_compl, main = paste('Complete 4 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 11.5, lty = 2, col="red", lwd=3)#4 klasteriai
rect.hclust(hc.man_compl, k = 4, border = 2:5)

plot(hc.man_mc, main = paste('Mcquitty 2 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 9, lty = 2, col="red", lwd=3)#2 klasteriai
rect.hclust(hc.man_mc, k = 2, border = 2:5)

clusters.man_ward <- cutree(hc.man_ward, k = 3)
clusters.man_compl <- cutree(hc.man_compl, k = 4)
clusters.man_mc <- cutree(hc.man_mc, k = 2)

cl14 <- fviz_cluster(list(data = reiksmingos.scaled, cluster = clusters.man_ward)) + ggtitle("Ward")
cl15 <- fviz_cluster(list(data = reiksmingos.scaled, cluster = clusters.man_compl)) + ggtitle("Complete")
cl16 <- fviz_cluster(list(data = reiksmingos.scaled, cluster = clusters.man_mc)) + ggtitle("Mcquitty")

grid.arrange(cl14, cl15, cl16,  nrow = 1)

table(clusters.man_ward, reiksmingos.outcome)
table(clusters.man_compl, reiksmingos.outcome)
table(clusters.man_mc, reiksmingos.outcome)

# maksimumo atstumas

dist.max <- dist(reiksmingos.scaled, method = 'maximum')

hc.max_ward <- hclust(dist.max, method = 'ward.D')
hc.max_compl <- hclust(dist.max, method = 'complete')
hc.max_mc <- hclust(dist.max, method = 'mcquitty')

par(mfrow=c(1,3))

plot(hc.max_ward, main = paste('Ward.D 3 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 90, lty = 2, col="red", lwd=3)#3 klasteriai
rect.hclust(hc.max_ward, k = 3, border = 2:5)

plot(hc.max_compl, main = paste('Complete 5 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 5, lty = 2, col="red", lwd=3)#5 klasteriai
rect.hclust(hc.max_compl, k = 5, border = 2:5)

plot(hc.max_mc, main = paste('Mcquitty 2 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 4, lty = 2, col="red", lwd=3)#2 klasteriai
rect.hclust(hc.max_mc, k = 2, border = 2:5)

clusters.max_ward <- cutree(hc.max_ward, k = 3)
clusters.max_compl <- cutree(hc.max_compl, k = 5)
clusters.max_mc <- cutree(hc.max_mc, k = 2)

cl17 <- fviz_cluster(list(data = reiksmingos.scaled, cluster = clusters.max_ward)) + ggtitle("Ward")
cl18 <- fviz_cluster(list(data = reiksmingos.scaled, cluster = clusters.max_compl)) + ggtitle("Complete")
cl19 <- fviz_cluster(list(data = reiksmingos.scaled, cluster = clusters.max_mc)) + ggtitle("Mcquitty")

grid.arrange(cl17, cl18, cl19,  nrow = 1)

table(clusters.max_ward, reiksmingos.outcome)
table(clusters.max_compl, reiksmingos.outcome)
table(clusters.max_mc, reiksmingos.outcome)

reiksmingos.scaled <- as.data.frame(reiksmingos.scaled)
reiksmingos.scaled <- merge(reiksmingos.scaled, as.data.frame(clusters.euc_ward), by=0)

reiksmingos.scaled %>% split(.$clusters.euc_ward) %>% map(summary)


# ---------------------- tsne

input <- data[,1:8]
output <- data[,9]

set.seed(123456)
tsne <- Rtsne(input, perplexity = 50, max_iter = 500)
plot(tsne$Y, bg = as.numeric(output$Outcome), pch = 21, cex = 0.8, main = 't-SNE', xlab = 'Component 1', ylab = 'Component 2')

tsne.outcome <- output$Outcome
table(tsne.outcome)

tsne.data <- scale(tsne$Y)
rownames(tsne.data) <- paste(tsne.outcome, 1:dim(tsne$Y)[1], sep = '_')

# euklidinis atstumas

dist.euclid <- dist(tsne.data, method = 'euclidean')

hc.euc_ward <- hclust(dist.euclid, method = 'ward.D')
hc.euc_compl <- hclust(dist.euclid, method = 'complete')
hc.euc_mc <- hclust(dist.euclid, method = 'mcquitty')

par(mfrow=c(1,3))

plot(hc.euc_ward, main = paste('Ward.D 2 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 275, lty = 2, col="red", lwd=3)#2 klasteriai
rect.hclust(hc.euc_ward, k = 2, border = 2:5)

plot(hc.euc_compl, main = paste('Complete 10 klasterių'), xlab = '', ylab = '', hang = -1)
abline(h = 1.6, lty = 2, col="red", lwd=3)#10 klasteriai
rect.hclust(hc.euc_compl, k = 10, border = 2:5)

plot(hc.euc_mc, main = paste('Mcquitty 5 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 1.5, lty = 2, col="red", lwd=3)#5 klasteriai
rect.hclust(hc.euc_mc, k = 5, border = 2:5)

clusters.euc_ward <- cutree(hc.euc_ward, k = 2)
clusters.euc_compl <- cutree(hc.euc_compl, k = 10)
clusters.euc_mc <- cutree(hc.euc_mc, k = 5)

cl21 <- fviz_cluster(list(data = as.data.frame(tsne.data), cluster = clusters.euc_ward)) + ggtitle("Ward")
cl22 <- fviz_cluster(list(data = as.data.frame(tsne.data), cluster = clusters.euc_compl)) + ggtitle("Complete")
cl23 <- fviz_cluster(list(data = as.data.frame(tsne.data), cluster = clusters.euc_mc)) + ggtitle("Mcquitty")

grid.arrange(cl21, cl22, cl23,  nrow = 1)

table(clusters.euc_ward, tsne.outcome)
table(clusters.euc_compl, tsne.outcome)
table(clusters.euc_mc, tsne.outcome)


# manheteno atstumas

dist.man <- dist(tsne.data, method = 'manhattan')

hc.man_ward <- hclust(dist.man, method = 'ward.D')
hc.man_compl <- hclust(dist.man, method = 'complete')
hc.man_mc <- hclust(dist.man, method = 'mcquitty')

par(mfrow=c(1,3))

plot(hc.man_ward, main = paste('Ward.D 2 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 340, lty = 2, col="red", lwd=3)#2 klasteriai
rect.hclust(hc.man_ward, k = 2, border = 2:5)

plot(hc.man_compl, main = paste('Complete 5 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h = 3.3, lty = 2, col="red", lwd=3)#5 klasteriai
rect.hclust(hc.man_compl, k = 5, border = 2:5)

plot(hc.man_mc, main = paste('Mcquitty 4 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h =1.75, lty = 2, col="red", lwd=3)#4 klasteriai
rect.hclust(hc.man_mc, k = 4, border = 2:5)

clusters.man_ward <- cutree(hc.man_ward, k = 2)
clusters.man_compl <- cutree(hc.man_compl, k = 5)
clusters.man_mc <- cutree(hc.man_mc, k = 4)

cl24 <- fviz_cluster(list(data = as.data.frame(tsne.data), cluster = clusters.man_ward)) + ggtitle("Ward")
cl25 <- fviz_cluster(list(data = as.data.frame(tsne.data), cluster = clusters.man_compl)) + ggtitle("Complete")
cl26 <- fviz_cluster(list(data = as.data.frame(tsne.data), cluster = clusters.man_mc)) + ggtitle("Mcquitty")

grid.arrange(cl24, cl25, cl26,  nrow = 1)

table(clusters.man_ward, tsne.outcome)
table(clusters.man_compl, tsne.outcome)
table(clusters.man_mc, tsne.outcome)

# maksimumo atstumas

dist.max <- dist(tsne.data, method = 'maximum')

hc.max_ward <- hclust(dist.max, method = 'ward.D')
hc.max_compl <- hclust(dist.max, method = 'complete')
hc.max_mc <- hclust(dist.max, method = 'mcquitty')

par(mfrow=c(1,3))

plot(hc.max_ward, main = paste('Ward.D 2 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h =240, lty = 2, col="red", lwd=3)#2 klasteriai
rect.hclust(hc.max_ward, k = 2, border = 2:5)

plot(hc.max_compl, main = paste('Complete 4 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h =2.9, lty = 2, col="red", lwd=3)#4 klasteriai
rect.hclust(hc.max_compl, k = 4, border = 2:5)

plot(hc.max_mc, main = paste('Mcquitty 4 klasteriai'), xlab = '', ylab = '', hang = -1)
abline(h =1.3, lty = 2, col="red", lwd=3)#4 klasteriai
rect.hclust(hc.max_mc, k = 4, border = 2:5)

clusters.max_ward <- cutree(hc.max_ward, k = 2)
clusters.max_compl <- cutree(hc.max_compl, k = 4)
clusters.max_mc <- cutree(hc.max_mc, k = 4)

cl27 <- fviz_cluster(list(data = as.data.frame(tsne.data), cluster = clusters.max_ward)) + ggtitle("Ward")
cl28 <- fviz_cluster(list(data = as.data.frame(tsne.data), cluster = clusters.max_compl)) + ggtitle("Complete")
cl29 <- fviz_cluster(list(data = as.data.frame(tsne.data), cluster = clusters.max_mc)) + ggtitle("Mcquitty")

grid.arrange(cl27, cl28, cl29,  nrow = 1)

table(clusters.max_ward, tsne.outcome)
table(clusters.max_compl, tsne.outcome)
table(clusters.max_mc, tsne.outcome)

tsne.data <- as.data.frame(tsne.data)
tsne.data <- merge(tsne.data, as.data.frame(clusters.euc_ward), by=0)

tsne.data %>% split(.$clusters.euc_ward) %>% map(summary)

data.scaled1 <- as.data.frame(data.scaled)
data.scaled1 <- merge(data.scaled1, as.data.frame(clusters.euc_ward), by=0)

data.scaled1 %>% split(.$clusters.euc_ward) %>% map(summary)

# ------------------------------------------------------------------------------------


# dbscan algoritmas -------------------------------------------------------


# visa duomenu imtis ------------------------------------------------------
install.packages("dbscan")
min_max_visa<-min_max_norm.data[,c(-9, -10)]

minPoints<-c(9, seq(20, 200, 20), 16, seq(200, 500, 50))
minPoints<-sort(minPoints)

#susiradome optimalius parametrus 
dev.off()
par(mfrow=c(2,2))
kNNdistplot(min_max_visa, k =  minPoints[1])
title(main = "Optimalaus spindulio paieška, \nkai kaimynų skaičius yra 9")
abline(h = 0.40, lty = 2, col="red", lwd=3)#jei minPts = 9, eps=0.4
text(x=100, y=0.46, 'eps = 0,40')

kNNdistplot(min_max_visa, k =  minPoints[2])
title(main = "Optimalaus spindulio paieška, \nkai kaimynų skaičius yra 16")
abline(h = 0.43, lty = 2, col="red", lwd=3)#jei minPts=16, eps=0.43
text(x=100, y=0.49, 'eps = 0,43')

kNNdistplot(min_max_visa, k =  minPoints[4])
title(main = "Optimalaus spindulio paieška, \nkai kaimynų skaičius yra 40")
abline(h = 0.52, lty = 2, col="red", lwd=3)#jei minPts=40, eps=0.52
text(x=100, y=0.58, 'eps = 0,52')

kNNdistplot(min_max_visa, k =  minPoints[12])
title(main = "Optimalaus spindulio paieška, \nkai kaimynų skaičius yra 200")
abline(h = 0.73, lty = 2, col="red", lwd=3)#jei minPts=200, eps=0.73
text(x=100, y=0.79, 'eps = 0,73')

eps<-c(0.4, 0.43, 0.46, 0.52, 0.55, 0.58, 0.61, 0.64, 0.68, 0.69, 0.7, 0.73, 0.76, 0.8, 0.83, 0.86, 0.89, 0.93)

# klasterizuojame ---------------------------------------------------------


mano_tema<- theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"), 
                  plot.subtitle = element_text(hjust = 0.5, size=15),
                  axis.text = element_text(colour='black', size=13),
                  axis.title=element_text(size=14,face="bold"), 
                  legend.title = element_text(size=14), 
                  legend.text = element_text(size=12))
min_max_norm.data$Pastumtas_outcome<-ifelse(min_max_norm.data$Outcome==0, 1, 2)
set.seed(123)

db1 <- fpc::dbscan(min_max_visa, MinPts = minPoints[1], eps=eps[1])
db1#0 for outlier
mean(min_max_norm.data$Pastumtas_outcome==db1$cluster) #65.49 proc teatitinka
sil1 <- silhouette(db1$cluster, dist(min_max_visa))
summary(sil1)#-0.09 (outlier) ir 0.41
f1<-fviz_cluster(db1, data = min_max_visa, stand = FALSE,
                 ellipse = T, show.clust.cent = FALSE,
                 geom = "point",palette = "jco", outlier.color="red", legend.title= "Klasteriai")+
  ggtitle(label='Klasterizavimo rezultatas, kai minimalus \nkaimynų skaičius 9, o spindulys 0,4')+
  labs(subtitle = "Išskirtys pažymėtos raudonai (n = 12)")+ mano_tema +
  annotate("text", x = -0.7, y=0.6, 
           label = "65,29 % klasterio grupių sutampa su tikromis grupėmis\nKlasterio silueto koeficientas: 0,41",
           size=5)
f1
mean(which(db1$cluster == 0) %in% isskirtys$paciento_id) #58 proc pripazintu outlier yra ir musu pripazinti
sum(which(db1$cluster == 0) %in% isskirtys$paciento_id)#t. y. 7 pacientai
mean(which(db1$cluster == 0) %in% salygines_isskirtys$paciento_id) #75 proc pripazintu outlier yra ir musu pripazinti salyginiai outlier
sum(which(db1$cluster == 0) %in% salygines_isskirtys$paciento_id)#t. y. 9 pacientai is 12


db2 <- fpc::dbscan(min_max_visa, MinPts = minPoints[2], eps=eps[2])
db2
mean(min_max_norm.data$Pastumtas_outcome==db2$cluster) #65.43 proc teatitinka
sil2 <- silhouette(db2$cluster, dist(min_max_visa))
summary(sil2) #0.03 ir 0.439
f2<-fviz_cluster(db2, data = min_max_visa, stand = FALSE,
                 ellipse = T, show.clust.cent = FALSE,
                 geom = "point",palette = "jco",outlier.color="red", legend.title= "Klasteriai") + mano_tema +
  ggtitle(label='Klasterizavimo rezultatas, kai \nminimalus kaimynų skaičius 16, o spindulys 0,43')+ 
  labs(subtitle = "Išskirtys pažymėtos raudonai (n = 9)")+
  annotate("text", x = -0.7, y=0.6, 
           label = "65,43 % klasterio grupių sutampa su tikromis grupėmis\nKlasterio silueto koeficientas: 0,44",
           size=5)

f2
mean(which(db2$cluster == 0) %in% isskirtys$paciento_id) #67 proc pripazintu outlier yra ir musu pripazinti
sum(which(db2$cluster == 0) %in% isskirtys$paciento_id)#t. y. 6 pacientai
mean(which(db2$cluster == 0) %in% salygines_isskirtys$paciento_id) #89 proc pripazintu outlier yra ir musu pripazinti salyginiai outlier
sum(which(db2$cluster == 0) %in% salygines_isskirtys$paciento_id)#t. y. 8 pacientai is 9

db4 <- fpc::dbscan(min_max_visa, MinPts = minPoints[4], eps=eps[4])
db4
mean(min_max_norm.data$Pastumtas_outcome==db4$cluster) #65.43 proc teatitinka
sil4 <- silhouette(db4$cluster, dist(min_max_visa))
summary(sil4)#-1.957072e-05  4.784635e-01 
f4<-fviz_cluster(db4, data = min_max_visa, stand = FALSE,
                 ellipse = T, show.clust.cent = FALSE,
                 geom = "point",palette = "jco",outlier.color="red", legend.title= "Klasteriai") + mano_tema + 
  ggtitle(label='Klasterizavimo rezultatas, kai \nminimalus kaimynų skaičius 40, o spindulys 0,52')+ 
  labs(subtitle = "Išskirtys pažymėtos raudonai (n = 5)")+
  annotate("text", x = -0.7, y=0.6, 
           label = "65,43 % klasterio grupių sutampa su tikromis grupėmis\nKlasterio silueto koeficientas: 0,48",
           size=5)
f4
mean(which(db4$cluster == 0) %in% isskirtys$paciento_id) #80 proc pripazintu outlier yra ir musu pripazinti
sum(which(db4$cluster == 0) %in% isskirtys$paciento_id)#t. y. 4 pacientai
mean(which(db4$cluster == 0) %in% salygines_isskirtys$paciento_id) #80 proc pripazintu outlier yra ir musu pripazinti salyginiai outlier
sum(which(db4$cluster == 0) %in% salygines_isskirtys$paciento_id)#t. y. 4 pacientai is 5

db12<- fpc::dbscan(min_max_visa, MinPts = minPoints[12], eps=eps[12])
db12#1 grupe nuo sito
mean(min_max_norm.data$Pastumtas_outcome==db12$cluster) #65.57 proc teatitinka
f12<-fviz_cluster(db12, data = min_max_visa, stand = FALSE,
                  ellipse = T, show.clust.cent = FALSE,
                  geom = "point",palette = "jco", legend.title = "Klasteriai") + mano_tema + 
  ggtitle(label='Klasterizavimo rezultatas, kai \nminimalus kaimynų skaičius 200, o spindulys 0,73')+
  annotate("text", x = -0.7, y=0.6, 
           label = "65,57 % klasterio grupių sutampa su tikromis grupėmis",
           size=5)
f12

#siek tiek isvadu:
#visur tera tik vienas klasteris isskirtas
#gerai atpazista outlier

#In the table above, column names are cluster number. Cluster 0 corresponds to outliers (black points in the DBSCAN plot).
# paneles -----------------------------------------------------------------

dev.off()
ggarrange(f1, f2, f4,f12, ncol = 2, nrow=2, common.legend = TRUE, legend="right")

# geriausio klasterio aprasomoji statistika su tikra aprasomaja statistika-----------------------------------------

duomenys_be_nuliu_8<-duomenys_be_nuliu
duomenys_be_nuliu_8$klasteris<-db4$cluster
describeBy(duomenys_be_nuliu_8, group = duomenys_be_nuliu_8$klasteris, digits = 4)
summary(isskirtys)
describeBy(duomenys_be_nuliu, group = duomenys_be_nuliu$Outcome, digits = 4)#tikroji statistika

# su 4 dimensijomis --------------------------------------------------------
#issiaiskininame, kuriuos stebejimus paliksime

# parametru paieska -------------------------------------------------------
modelis <- glm(formula = Outcome ~ .,
               family = binomial(logit), data = duomenys_be_nuliu)

summary(modelis)
#paliekame pregnancies, glucose, BMI, ir Diabetes Pedigree Function

#susikuriame atitinkama imti
min_max_4<-min_max_norm.data[, c(-3, -4,-5, -8)]
min_max_4$Pastumtas_outcome<-ifelse(min_max_4$Outcome==0, 1, 2)

minPoints_4<-c(seq(4, 8, 1),16, seq(20, 200, 20), seq(250, 500, 50))
minPoints_4<-sort(minPoints_4)#12 turime kaimynu skaiciu paimta

#susiradome optimalius parametrus 
par(mfrow=c(2,2))
kNNdistplot(min_max_4[, 1:4], k =  minPoints_4[1])
title(main = "Optimalaus spindulio paieška, \nkai kaimynų skaičius yra 4")
abline(h = 0.18, lty = 2, col="red", lwd=3)#jei minPts = 4, eps=0.18
text(x=100, y=0.22, 'eps = 0,18')

kNNdistplot(min_max_4[, 1:4], k =  minPoints_4[5])
title(main = "Optimalaus spindulio paieška, \nkai kaimynų skaičius yra 8")
abline(h = 0.22, lty = 2, col="red", lwd=3)#jei minPts = 8, eps=0.22
text(x=100, y=0.26, 'eps = 0,22')

kNNdistplot(min_max_4[, 1:4], k =  minPoints_4[9])
title(main = "Optimalaus spindulio paieška, \nkai kaimynų skaičius yra 60")
abline(h = 0.36, lty = 2, col="red", lwd=3)#jei minPts = 60, eps=0.36
text(x=100, y=0.42, 'eps = 0,36')

kNNdistplot(min_max_4[, 1:4], k =  minPoints_4[20])
title(main = "Optimalaus spindulio paieška, \nkai kaimynų skaičius yra 400")
abline(h = 0.63, lty = 2, col="red", lwd=3)#jei minPts = 400, eps=0.63
text(x=100, y=0.69, 'eps = 0,63')

eps_4<-c(0.18, 0.19, 0.2, 0.21,0.22, 0.27, 0.28, 0.32, 0.36, 0.4, 0.42, 0.45, 0.47, 0.49, 0.52, 0.54, 0.56, 0.58, 0.6, 0.63, 0.67, 0.69)

# klasterizuojame ---------------------------------------------------------

mano_tema_4<- theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"), 
                    plot.subtitle = element_text(hjust = 0.5, size=15),
                    axis.text = element_text(colour='black', size=13),
                    axis.title=element_text(size=14,face="bold"), 
                    legend.title = element_text(size=14), 
                    legend.text = element_text(size=12))
set.seed(123)

db4_1 <- fpc::dbscan(min_max_4[,1:4], MinPts = minPoints_4[1], eps=eps_4[1])
db4_1#0 for outlier
mean(min_max_norm.data$Pastumtas_outcome==db4_1$cluster) #63.92 proc teatitinka
sil4_1 <- silhouette(db4_1$cluster, dist(min_max_4[,1:4]))
summary(sil4_1)#-0.11 (outlier) ir 0.43
f4_1<-fviz_cluster(db4_1, data = min_max_4[,1:4], stand = FALSE,
                   ellipse = T, show.clust.cent = FALSE,
                   geom = "point",palette = "jco", outlier.color="red", legend.title= "Klasteriai")+
  ggtitle(label='Klasterizavimo rezultatas, kai \nminimalus kaimynų skaičius 4, o spindulys 0,18')+
  labs(subtitle = "Išskirtys pažymėtos raudonai (n = 29)")+ mano_tema +
  annotate("text", x = -0.5, y=0.5, 
           label = "63,92 % klasterio grupių sutampa su tikromis grupėmis\nKlasterio silueto koeficientas: 0,43",
           size=5)+
  scale_x_continuous(breaks=c(seq(-1.5, 0.5, 0.25)))
f4_1
mean(which(db4_1$cluster == 0) %in% isskirtys$paciento_id) #28 proc pripazintu outlier yra ir musu pripazinti
sum(which(db4_1$cluster == 0) %in% isskirtys$paciento_id)#t. y. 8 pacientai
mean(which(db4_1$cluster == 0) %in% salygines_isskirtys$paciento_id) #62 proc pripazintu outlier yra ir musu pripazinti salyginiai outlier
sum(which(db4_1$cluster == 0) %in% salygines_isskirtys$paciento_id)#t. y. 18 pacientai is 29

db4_5 <- fpc::dbscan(min_max_4[,1:4], MinPts = minPoints_4[5], eps=eps_4[5])
db4_5#0 for outlier
mean(min_max_norm.data$Pastumtas_outcome==db4_5$cluster) #64.61 proc teatitinka
sil4_5 <- silhouette(db4_5$cluster, dist(min_max_4[,1:4]))
summary(sil4_5)#-0.10 (outlier) ir 0.47
f4_5<-fviz_cluster(db4_5, data = min_max_4[,1:4], stand = FALSE,
                   ellipse = T, show.clust.cent = FALSE,
                   geom = "point",palette = "jco", outlier.color="red", legend.title= "Klasteriai")+
  ggtitle(label='Klasterizavimo rezultatas, kai \nminimalus kaimynų skaičius 8, o spindulys 0,22')+
  labs(subtitle = "Išskirtys pažymėtos raudonai (n = 17)")+ mano_tema +
  annotate("text", x = -0.5, y=0.5, 
           label = "64,61 % klasterio grupių sutampa su tikromis grupėmis\nKlasterio silueto koeficientas: 0,47",
           size=5)+
  scale_x_continuous(breaks=c(seq(-1.5, 0.5, 0.25)))
f4_5
mean(which(db4_5$cluster == 0) %in% isskirtys$paciento_id) #35 proc pripazintu outlier yra ir musu pripazinti
sum(which(db4_5$cluster == 0) %in% isskirtys$paciento_id)#t. y. 6 pacientai
mean(which(db4_5$cluster == 0) %in% salygines_isskirtys$paciento_id) #82 proc pripazintu outlier yra ir musu pripazinti salyginiai outlier
sum(which(db4_5$cluster == 0) %in% salygines_isskirtys$paciento_id)#t. y. 14 pacientai is 17

db4_9 <- fpc::dbscan(min_max_4[,1:4], MinPts = minPoints_4[9], eps=eps_4[9])
db4_9#0 for outlier
mean(min_max_norm.data$Pastumtas_outcome==db4_9$cluster) #65.43 proc teatitinka
sil4_9 <- silhouette(db4_9$cluster, dist(min_max_4[,1:4]))
summary(sil4_9)#0.10 (outlier) ir 0.52
f4_9<-fviz_cluster(db4_9, data = min_max_4[,1:4], stand = FALSE,
                   ellipse = T, show.clust.cent = FALSE,
                   geom = "point",palette = "jco", outlier.color="red", legend.title= "Klasteriai")+
  ggtitle(label='Klasterizavimo rezultatas, kai \nminimalus kaimynų skaičius 60, o spindulys 0,36')+
  labs(subtitle = "Išskirtys pažymėtos raudonai (n = 6)")+ mano_tema +
  annotate("text", x = -0.5, y=0.5, 
           label = "65,43 % klasterio grupių sutampa su tikromis grupėmis\nKlasterio silueto koeficientas: 0,52",
           size=5)+
  scale_x_continuous(breaks=c(seq(-1.5, 0.5, 0.25)))
f4_9
mean(which(db4_9$cluster == 0) %in% isskirtys$paciento_id) #83 proc pripazintu outlier yra ir musu pripazinti
sum(which(db4_9$cluster == 0) %in% isskirtys$paciento_id)#t. y. 5 pacientai
mean(which(db4_9$cluster == 0) %in% salygines_isskirtys$paciento_id) #100 proc pripazintu outlier yra ir musu pripazinti salyginiai outlier
sum(which(db4_9$cluster == 0) %in% salygines_isskirtys$paciento_id)#t. y. 6 pacientai is 6

db4_20 <- fpc::dbscan(min_max_4[,1:4], MinPts = minPoints_4[20], eps=eps_4[20])
db4_20#0 for outlier
mean(min_max_norm.data$Pastumtas_outcome==db4_20$cluster) #65.57 proc teatitinka
f4_20<-fviz_cluster(db4_20, data = min_max_4[,1:4], stand = FALSE,
                    ellipse = T, show.clust.cent = FALSE,
                    geom = "point",palette = "jco", outlier.color="red", legend.title= "Klasteriai")+
  ggtitle(label='Klasterizavimo rezultatas, kai \nminimalus kaimynų skaičius 400, o spindulys 0,63')+
  mano_tema +
  annotate("text", x = -0.5, y=0.5, 
           label = "65,57 % klasterio grupių sutampa su tikromis grupėmis",
           size=5)+
  scale_x_continuous(breaks=c(seq(-1.5, 0.5, 0.25)))
f4_20

#siek tiek isvadu:
#visur tera tik vienas klasteris isskirtas
#geriausias tikslumas, kai 140 kaimynu sk ir 0.47 eps, 
#toliau didinant nuo 400 kaimynu sk ir spindulio nuo 063 sk nera isskiriama outlier, 
#gerai atpazista outlier
# paneles -----------------------------------------------------------------

dev.off()
ggarrange(f4_1, f4_5, f4_9,f4_20, ncol = 2, nrow=2, common.legend = TRUE, legend="right")

# geriausio klasterio aprasomoji statistika su tikra aprasomaja statistika-----------------------------------------

duomenys_be_nuliu_4<-duomenys_be_nuliu[, c(-3, -4,-5, -8)]
duomenys_be_nuliu_4$klasteris<-db4_9$cluster
describeBy(duomenys_be_nuliu_4, group = duomenys_be_nuliu_4$klasteris, digits = 4)
summary(isskirtys)
describeBy(duomenys_be_nuliu, group = duomenys_be_nuliu$Outcome, digits = 4)#tikroji statistika
# su 2 dimensijomis -------------------------------------------------------

# pritaikome dimensijos mazinima ------------------------------------------

# euklido -----------------------------------------------------------------
dist_euklido_duomenys <- dist(min_max_norm.data[, 1:8], method = "euclidean")
mds_euklido_duomenys <- cmdscale(dist_euklido_duomenys, eig = TRUE, k = 2)
mds_euklido_duomenys$GOF 

mds_euklido_duomenys1<- data.frame(
  MDS1 = mds_euklido_duomenys$points[, 1],
  MDS2 = mds_euklido_duomenys$points[, 2],
  label = duomenys_be_nuliu$paciento_id,
  classification = duomenys_be_nuliu$Outcome,
  classification1 = ifelse(duomenys_be_nuliu$paciento_id %in% isskirtys$paciento_id, -1, ifelse(duomenys_be_nuliu$Outcome == 0, 0, 1)),
  Pastumtas_outcome = ifelse(duomenys_be_nuliu$Outcome==0, 1, 2)
)

met_euc1<-ggplot(mds_euklido_duomenys1, aes(
  x = MDS1, y = MDS2,col = as.factor(classification1))) +
  geom_point() + scale_color_manual(values=c("black", "#E69F00", "#56B4E9"), labels = c("Išskirtys", "0", "1")) +
  guides(color = guide_legend(title = "Klasifikacija"))+
  ggtitle("MDS naudojant Euklidinę metriką")#turbut sitaip geriausiai #turbut geriausiai, galima pakeitineti MDS2 ir MDS3

met_euc1

#veikia
mds_euklido_duomenys2<- data.frame(
  MDS1 = mds_euklido_duomenys$points[, 1],
  MDS2 = mds_euklido_duomenys$points[, 2],
  label = duomenys_be_nuliu$paciento_id,
  classification = duomenys_be_nuliu$Outcome,
  classification1 = as.factor(ifelse((duomenys_be_nuliu$paciento_id %in% isskirtys$paciento_id)&(duomenys_be_nuliu$Outcome==0), -1, 
                                     ifelse((duomenys_be_nuliu$paciento_id %in% isskirtys$paciento_id)&(duomenys_be_nuliu$Outcome==1), 1,
                                            ifelse(duomenys_be_nuliu$Outcome==1, 0,-2))))
)
met_euc2<-ggplot(mds_euklido_duomenys2, aes(
  x = MDS1, y = MDS2,col = as.factor(classification1))) +
  geom_point(aes(size=(classification1==-1|classification1==1))) +
  scale_color_manual(values=c("#E69F00",  "#5e4d26",  "#56B4E9", "darkblue"), 
                     labels=c("0", "0 išskirtys", "1", "1 išskirtys")) +
  guides(color = guide_legend(title = "Klasifikacija"))+
  ggtitle("MDS naudojant Euklidinę metriką")+scale_size_manual(values=c(2,3),  guide='none')
met_euc2

# parametru paieska -------------------------------------------------------

minPoints_2<-c(seq(3, 15, 2), seq(20, 180, 20), seq(200, 500, 100))
minPoints_2<-sort(minPoints_2)#20 turime kaimynu skaiciu paimta

dev.off()
par(mfrow=c(2,2))
kNNdistplot(mds_euklido_duomenys$points, k =  minPoints_2[1])
title(main = "Optimalaus spindulio paieška, \nkai kaimynų skaičius yra 3")
abline(h = 0.08, lty = 2, col="red", lwd=3)#jei minPts = 3, eps=0.08
text(x=100, y=0.14, 'eps = 0,08')

kNNdistplot(mds_euklido_duomenys$points, k =  minPoints_2[2])
title(main = "Optimalaus spindulio paieška, \nkai kaimynų skaičius yra 5")
abline(h = 0.09, lty = 2, col="red", lwd=3)#jei minPts = 5, eps=0.09
text(x=100, y=0.13, 'eps = 0,09')

kNNdistplot(mds_euklido_duomenys$points, k =  minPoints_2[10])
title(main = "Optimalaus spindulio paieška, \nkai kaimynų skaičius yra 60")
abline(h = 0.25, lty = 2, col="red", lwd=3)#jei minPts = 60, eps=0.25
text(x=100, y=0.29, 'eps = 0,25')

kNNdistplot(mds_euklido_duomenys$points, k =  minPoints_2[16])
title(main = "Optimalaus spindulio paieška, \nkai kaimynų skaičius yra 180")
abline(h = 0.45, lty = 2, col="red", lwd=3)#jei minPts = 180, eps=0.45
text(x=100, y=0.49, 'eps = 0,45')

eps_2<-c(0.08, 0.09, 0.11, 0.12, 0.13, 0.13, 0.15, 0.15, 0.21, 0.25, 0.29, 0.32, 
         0.35, 0.4, 0.42, 0.45, 0.48, 0.55, 0.66, 0.73)


# klasterizavimas --------------------------------------------------------

set.seed(123)

db2_1 <- fpc::dbscan(mds_euklido_duomenys$points, MinPts = minPoints_2[1], eps=eps_2[1])
db2_1#0 for outlier
mean(mds_euklido_duomenys1$Pastumtas_outcome==db2_1$cluster) #64.88 proc teatitinka
sil2_1 <- silhouette(db2_1$cluster, dist(mds_euklido_duomenys$points))
summary(sil2_1)#-0.56 (outlier) ir 0.25, 0.92, 0.72
f2_1<-fviz_cluster(db2_1, data = as.data.frame(mds_euklido_duomenys$points), stand = FALSE,
                   ellipse = T, show.clust.cent = FALSE,
                   geom = "point",palette = "jco", outlier.color="red", legend.title= "Klasteriai")+
  ggtitle(label='Klasterizavimo rezultatas, kai \nminimalus kaimynų skaičius 3, o spindulys 0,08')+
  labs(subtitle = "Išskirtys pažymėtos raudonai (n = 9)")+ mano_tema +
  annotate("text", x = 0.42, y=0.7, 
           label = "66,88 % klasterio grupių sutampa su tikromis grupėmis,
           1 klasterio silueto koeficientas: 0,25
           2 klasterio silueto koeficientas: 0,92\n     3 klasterio silueto koeficientas: 0,72",
           size=5)
f2_1
mean(which(db2_1$cluster == 0) %in% isskirtys$paciento_id) #56 proc pripazintu outlier yra ir musu pripazinti
sum(which(db2_1$cluster == 0) %in% isskirtys$paciento_id)#t. y. 5 pacientai
mean(which(db2_1$cluster == 0) %in% salygines_isskirtys$paciento_id) #78 proc pripazintu outlier yra ir musu pripazinti salyginiai outlier
sum(which(db2_1$cluster == 0) %in% salygines_isskirtys$paciento_id)#t. y. 7 pacientai is 9

db2_2<- fpc::dbscan(mds_euklido_duomenys$points, MinPts = minPoints_2[2], eps=eps_2[2])
db2_2#0 for outlier
mean(mds_euklido_duomenys1$Pastumtas_outcome==db2_2$cluster) #64.47 proc teatitinka
sil2_2 <- silhouette(db2_2$cluster, dist(mds_euklido_duomenys$points))
summary(sil2_2)#0.59 (outlier) ir 0.42
f2_2<-fviz_cluster(db2_2, data = as.data.frame(mds_euklido_duomenys$points), stand = FALSE,
                   ellipse = T, show.clust.cent = FALSE,
                   geom = "point",palette = "jco", outlier.color="red", legend.title= "Klasteriai")+
  ggtitle(label='Klasterizavimo rezultatas, kai \nminimalus kaimynų skaičius 5, o spindulys 0,09')+
  labs(subtitle = "Išskirtys pažymėtos raudonai (n = 14)")+ mano_tema +
  annotate("text", x = 0.3, y=0.75, 
           label = "64,47 % klasterio grupių sutampa su tikromis grupėmis\nKlasterio silueto koeficientas: 0,42",
           size=5)
f2_2
mean(which(db2_2$cluster == 0) %in% isskirtys$paciento_id) #50 proc pripazintu outlier yra ir musu pripazinti
sum(which(db2_2$cluster == 0) %in% isskirtys$paciento_id)#t. y. 7 pacientai
mean(which(db2_2$cluster == 0) %in% salygines_isskirtys$paciento_id) #79 proc pripazintu outlier yra ir musu pripazinti salyginiai outlier
sum(which(db2_2$cluster == 0) %in% salygines_isskirtys$paciento_id)#t. y. 11 pacientai is 14

db2_10<- fpc::dbscan(mds_euklido_duomenys$points, MinPts = minPoints_2[10], eps=eps_2[10])
db2_10#0 for outlier
mean(mds_euklido_duomenys1$Pastumtas_outcome==db2_10$cluster) #65.43 proc teatitinka
sil2_10 <- silhouette(db2_10$cluster, dist(mds_euklido_duomenys$points))
summary(sil2_10)#0.80 (outlier) ir 0.5
f2_10<-fviz_cluster(db2_10, data = as.data.frame(mds_euklido_duomenys$points), stand = FALSE,
                    ellipse = T, show.clust.cent = FALSE,
                    geom = "point",palette = "jco", outlier.color="red", legend.title= "Klasteriai")+
  ggtitle(label='Klasterizavimo rezultatas, kai \nminimalus kaimynų skaičius 60, o spindulys 0,25')+
  labs(subtitle = "Išskirtys pažymėtos raudonai (n = 3)")+ mano_tema +
  annotate("text", x = 0.3, y=0.8, 
           label = "65,43 % klasterio grupių sutampa su \ntikromis grupėmis, klasterio silueto koeficientas: 0,5",
           size=5)
f2_10
mean(which(db2_10$cluster == 0) %in% isskirtys$paciento_id) #100 proc pripazintu outlier yra ir musu pripazinti
sum(which(db2_10$cluster == 0) %in% isskirtys$paciento_id)#t. y. 3 pacientai
mean(which(db2_10$cluster == 0) %in% salygines_isskirtys$paciento_id) #100 proc pripazintu outlier yra ir musu pripazinti salyginiai outlier
sum(which(db2_10$cluster == 0) %in% salygines_isskirtys$paciento_id)#t. y. 3 pacientai is 3

db2_16<- fpc::dbscan(mds_euklido_duomenys$points, MinPts = minPoints_2[16], eps=eps_2[16])
db2_16#0 for outlier
mean(mds_euklido_duomenys1$Pastumtas_outcome==db2_16$cluster) #65.57 proc teatitinka
f2_16<-fviz_cluster(db2_16, data = as.data.frame(mds_euklido_duomenys$points), stand = FALSE,
                    ellipse = T, show.clust.cent = FALSE,
                    geom = "point",palette = "jco", outlier.color="red", legend.title= "Klasteriai")+
  ggtitle(label='Klasterizavimo rezultatas, kai \nminimalus kaimynų skaičius 180, o spindulys 0,45')+
  mano_tema +
  annotate("text", x = 0.5, y=0.8, 
           label = "65,57 % klasterio grupių \nsutampa su tikromis grupėmis",
           size=5)
f2_16


# paneles -----------------------------------------------------------------

dev.off()
ggarrange(f2_1, f2_2,f2_10, f2_16, ncol = 2, nrow=2, common.legend = TRUE, legend="right")

# geriausio klasterio aprasomoji statistika su tikra aprasomaja statistika-----------------------------------------
summary(duomenys_be_nuliu[duomenys_be_nuliu$paciento_id %in% which(db2_1$cluster==1), ])#cia tik 2 stebejima atmeta
describeBy(duomenys_be_nuliu, group = duomenys_be_nuliu$Outcome, digits = 4)#tikroji statistika

duomenys_be_nuliu_2<-duomenys_be_nuliu
duomenys_be_nuliu_2$klasteris1<-db2_1$cluster
describeBy(duomenys_be_nuliu_2, group = duomenys_be_nuliu_2$klasteris1, digits = 4)
summary(isskirtys)
describeBy(duomenys_be_nuliu, group = duomenys_be_nuliu$Outcome, digits = 4)#tikroji statistika

sum(db2_1$cluster == 2)
sum(db2_1$cluster == 3)
#------------------------------------------------------
cluster_number<-numeric()
for(i in 1:length(eps_2)){
  db<-fpc::dbscan(mds_euklido_duomenys$points, MinPts = minPoints_2[i], eps=eps_2[i])
  cluster_number[i]<-length(unique(db$cluster))
}

