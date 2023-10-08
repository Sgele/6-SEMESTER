####################################
#duomenu nuskaitymas
##################################

duomenys <- read.csv("C:/Users/ugneo/Downloads/Sample_Code_R/Duomenys/Future-500-5.csv")

###############################
#susipazinimas su duomenimis, numeric ir factor priskyrimas
###############################

#-------------------------
#tarpus pakeiciame i NA
#---------------------------
duomenys <- replace(duomenys, duomenys =='', NA)


#---------------------------
# procentine praleistu reiksmiu dalis
#---------------------------------------
data_na <- duomenys[!complete.cases(duomenys),]
View(data_na)
# eilutciu kuriose nenurodyta valstija arba finansinis rodiklis, kuri galima
# paskaiciuoti pasalinimas
data_na <- data_na[-c(3, 6, 8, 11, 12, 14, 17:20, 22, 24),]
View(data_na)

nrow(data_na)
nrow(data_na)/nrow(duomenys)*100
#suzinome, kiek turime stebejimu su trukstamomis reiksmemis


#---------------------------------
#kategoriju skaiciaus suradimas
#------------------------------------
length(unique(duomenys$Name))
length(unique(duomenys$Industry))-1#del NA
length(unique(duomenys$State))-1#del NA
length(unique(duomenys$City))


#------------------------------------------
#kategoriniams duomenims pridedame kategorijas
#ir skaitinius pasivertciame numeric
#----------------------------------------------

str(duomenys)
duomenys$Industry<-as.factor(duomenys$Industry)
duomenys$State<-as.factor(duomenys$State)
duomenys$City<-as.factor(duomenys$City)

duomenys$Profit <- as.numeric(as.character(duomenys$Profit))
#salinam nereikalingus simbolius
duomenys$Expenses <- gsub(" Dollars","",duomenys$Expenses)
duomenys$Expenses <- gsub(",","",duomenys$Expenses)
#darom numeric
duomenys$Expenses <- as.numeric(as.character(duomenys$Expenses))

#salinam nereikalingus simbolius
duomenys$Revenue <- gsub("\\$","",duomenys$Revenue)
duomenys$Revenue <- gsub(",","",duomenys$Revenue)
#darom numeric
duomenys$Revenue <- as.numeric(as.character(duomenys$Revenue))

#salinam nereikalingus simbolius
duomenys$Growth <- gsub("\\%","",duomenys$Growth)
#darom numeric
duomenys$Growth <- as.numeric(as.character(duomenys$Growth))

str(duomenys)


###############################
#pirmas vaizdas apie duomenis
################################
summary(duomenys)

#---------------------------
#Statistika pagal pramones sakas
#-----------------------------
library(psych)
describeBy(duomenys$Inception, 
           group = duomenys$Industry, digits = 4)

describeBy(duomenys$Employees, 
           group = duomenys$Industry, digits = 4)

describeBy(duomenys$Revenue, 
           group = duomenys$Industry, digits = 4)

describeBy(duomenys$Expenses, 
           group = duomenys$Industry, digits = 4)

describeBy(duomenys$Profit, 
           group = duomenys$Industry, digits = 4)

describeBy(duomenys$Growth, 
           group = duomenys$Industry, digits = 4)


######################################
#praleistos reiksmes
#####################################

#-----------------------------
#pradinis reiksmiu uzpildymas
#-------------------------------
library(dplyr)
# irasai su praleistomis reiksmemis
duomenys[!complete.cases(duomenys),]

# istriname irasus kuriuose nenurodyta industrija
duomenys <- duomenys[!is.na(duomenys$Industry),]
duomenys[!complete.cases(duomenys),]

# praleistu metu iraso tvarkymas - istriname
duomenys <- duomenys[!is.na(duomenys$Inception),]
duomenys[!complete.cases(duomenys),]

# valstiju prasleistu reiksmiu uzpildymas pagal miestus
duomenys[is.na(duomenys$State) &duomenys$City=="New York", "State"] <- "NY"
duomenys[is.na(duomenys$State) &duomenys$City=="Newport Beach", "State"] <- "CA"
duomenys[is.na(duomenys$State) &duomenys$City=="San Francisco", "State"] <- "CA"
duomenys[is.na(duomenys$State) &duomenys$City=="Alpharetta", "State"] <- "GA"
duomenys[is.na(duomenys$State) &duomenys$City=="Chicago", "State"] <- "IL"
duomenys[!complete.cases(duomenys),]


#----------------------
#uzpildymas mediana
#----------------------

data_med <- duomenys

data_med[!complete.cases(data_med),]

# darbuotoju praleistu reiksmiu uzpildymas mediana pagal industrija

data_med$Employees <- replace(data_med$Employees, data_med$Industry == "Retail" & is.na(data_med$Employees) == T,
                              median(filter(data_med, data_med$Industry == "Retail")$Employees, na.rm = T))

data_med$Employees <- replace(data_med$Employees, data_med$Industry == "Construction" & is.na(data_med$Employees) == T,
                              median(filter(data_med, data_med$Industry == "Construction")$Employees, na.rm = T))

data_med$Employees <- replace(data_med$Employees, data_med$Industry == "Software" & is.na(data_med$Employees) == T,
                              median(filter(data_med, data_med$Industry == "Software")$Employees, na.rm = T))

data_med$Employees <- replace(data_med$Employees, data_med$Industry == "Financial Services" & is.na(data_med$Employees) == T,
                              median(filter(data_med, data_med$Industry == "Financial Services")$Employees, na.rm = T))

data_med[!complete.cases(data_med),]


# 8 ir 44 eilutes pasalinamos del bent 3 nenurodytu finansiniu rodikliu

data_med <- data_med[-c(8, 41),]
data_med[!complete.cases(data_med),]


# pajamu uzpildymas sudedant islaidas ir pelna

data_med[is.na(data_med$Revenue), "Revenue"] <- data_med[is.na(data_med$Revenue), "Expenses"] + 
  data_med[is.na(data_med$Revenue), "Profit"]

data_med[!complete.cases(data_med),]


# islaidos uzpildomos is pajamu atemus pelna

data_med[is.na(data_med$Expenses), "Expenses"] <- data_med[is.na(data_med$Expenses), "Revenue"] - 
  data_med[is.na(data_med$Expenses), "Profit"]

data_med[!complete.cases(data_med),]


# augimo praleistos reiksmes 

data_med$Growth <- replace(data_med$Growth, data_med$Industry == "Software" & is.na(data_med$Growth) == T,
                           median(filter(data_med, data_med$Industry == "Software")$Growth, na.rm = T))

data_med[!complete.cases(data_med),]


#----------------------
#reiksmiu uzpildymas vidurkiu
# --------------- 

data_vid <- duomenys

# darbuotoju praleistu reiksmiu uzpildymas vidurkiu pagal industrija

data_vid$Employees <- replace(data_vid$Employees, data_vid$Industry == "Retail" & is.na(data_vid$Employees) == T,
                              mean(filter(data_vid, data_vid$Industry == "Retail")$Employees, na.rm = T))

data_vid$Employees <- replace(data_vid$Employees, data_vid$Industry == "Construction" & is.na(data_vid$Employees) == T,
                              mean(filter(data_vid, data_vid$Industry == "Construction")$Employees, na.rm = T))

data_vid$Employees <- replace(data_vid$Employees, data_vid$Industry == "Software" & is.na(data_vid$Employees) == T,
                              mean(filter(data_vid, data_vid$Industry == "Software")$Employees, na.rm = T))

data_vid$Employees <- replace(data_vid$Employees, data_vid$Industry == "Financial Services" & is.na(data_vid$Employees) == T,
                              mean(filter(data_vid, data_vid$Industry == "Financial Services")$Employees, na.rm = T))

data_vid[!complete.cases(data_vid),]

# 8 ir 44 eilutes pasalinamos del bent 3 nenurodytu finansiniu rodikliu

data_vid <- data_vid[-c(8, 41),]
data_vid[!complete.cases(data_vid),]

# pajamu uzpildymas sudedant islaidas ir pelna

data_vid[is.na(data_vid$Revenue), "Revenue"] <- data_vid[is.na(data_vid$Revenue), "Expenses"] + 
  data_vid[is.na(data_vid$Revenue), "Profit"]

data_vid[!complete.cases(data_vid),]

# islaidos uzpildomos is pajamu atemus pelna

data_vid[is.na(data_vid$Expenses), "Expenses"] <- data_vid[is.na(data_vid$Expenses), "Revenue"] - 
  data_vid[is.na(data_vid$Expenses), "Profit"]

data_vid[!complete.cases(data_vid),]

# augimo praleistos reiksmes uzpildomos vidurkiu pagal industrija

data_vid$Growth <- replace(data_vid$Growth, data_vid$Industry == "Software" & is.na(data_vid$Growth) == T,
                           mean(filter(data_vid, data_vid$Industry == "Software")$Growth, na.rm = T))

data_vid[!complete.cases(data_vid),]


#---------------------
#reiksmiu uzpildymas moda
# --------------- 

mode <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}

data_mod <- duomenys

# darbuotoju praleistu reiksmiu uzpildymas moda pagal industrija
library(dplyr)

data_mod$Employees <- replace(data_mod$Employees, data_mod$Industry == "Retail" & is.na(data_mod$Employees) == T,
                              mode(filter(data_mod, data_mod$Industry == "Retail")$Employees))

data_mod$Employees <- replace(data_mod$Employees, data_mod$Industry == "Construction" & is.na(data_mod$Employees) == T,
                              mode(filter(data_mod, data_mod$Industry == "Construction")$Employees))

data_mod$Employees <- replace(data_mod$Employees, data_mod$Industry == "Software" & is.na(data_mod$Employees) == T,
                              mode(filter(data_mod, data_mod$Industry == "Software")$Employees))

data_mod$Employees <- replace(data_mod$Employees, data_mod$Industry == "Financial Services" & is.na(data_mod$Employees) == T,
                              mode(filter(data_mod, data_mod$Industry == "Financial Services")$Employees))

data_mod[!complete.cases(data_mod),]

# 8 ir 44 eilutes pasalinamos del bent 3 nenurodytu finansiniu rodikliu

data_mod <- data_mod[-c(8, 41),]
data_mod[!complete.cases(data_mod),]

# pajamu uzpildymas sudedant islaidas ir pelna

data_mod[is.na(data_mod$Revenue), "Revenue"] <- data_mod[is.na(data_mod$Revenue), "Expenses"] + 
  data_mod[is.na(data_mod$Revenue), "Profit"]

data_mod[!complete.cases(data_mod),]

# islaidos uzpildomos is pajamu atemus pelna

data_mod[is.na(data_mod$Expenses), "Expenses"] <- data_mod[is.na(data_mod$Expenses), "Revenue"] - 
  data_mod[is.na(data_mod$Expenses), "Profit"]

data_mod[!complete.cases(data_mod),]

# augimo praleistos reiksmes 

data_mod$Growth <- replace(data_mod$Growth, data_mod$Industry == "Software" & is.na(data_mod$Growth) == T,
                           mode(filter(data_mod, data_mod$Industry == "Software")$Growth))

data_mod[!complete.cases(data_mod),]



#-------------------------------
#statistika pagal uzpildymo metodus 
# ------------------ --------- 

med<-summary(data_med)
vid<-summary(data_vid)
mod<-summary(data_mod)
med

med[,c(5,8:11)]
vid[,c(5,8:11)]
mod[,c(5,8:11)]


###########################################
#tiriame isskirtis
##########################################
library(ggplot2)
library(dplyr)
summ<-data.frame(summary(data_med))
duomenys<-data_med
mean(is.na(duomenys))#=>nebera praleistu reiksmiu, dirbame su duomenimis, kur praleistos reiksmes uzpildytos su mediana

#----------------------------------------------------------------
#apsirasome isskirciu ir salyginiu isskirciu skaiciavimo taisykles
#-------------------------------------------------------------------
isskirtis <- function(x) {
  return(x <= quantile(x, .25) - 3*IQR(x) | x >= quantile(x, .75) + 3*IQR(x))
}

salygine_isskirtis<-function(x){
  return(((x <= quantile(x, .25) - 1.5*IQR(x))&(x > quantile(x, .25) - 3*IQR(x))) | (x >= quantile(x, .75) + 1.5*IQR(x))&
           (x < quantile(x, .75) + 3*IQR(x)))
}


#-----------------------------
#susikuriame atskiras duomenu imtis ir susizymime tai isskirtis ar salygine isskirtis
#----------------------------------

duomenys_darbuotojai <- duomenys %>%
  group_by(Industry) %>%
  mutate(outlier = ifelse(isskirtis(Employees), Name, NA)) %>%
  mutate(salygine_outlier = ifelse(salygine_isskirtis(Employees), Name, NA))

duomenys_pajamos <- duomenys %>%
  group_by(Industry) %>%
  mutate(outlier = ifelse(isskirtis(Revenue), Name, NA))%>%
  mutate(salygine_outlier = ifelse(salygine_isskirtis(Revenue), Name, NA))

duomenys_islaidos <- duomenys %>%
  group_by(Industry) %>%
  mutate(outlier = ifelse(isskirtis(Expenses), Name, NA)) %>%
  mutate(salygine_outlier = ifelse(salygine_isskirtis(Expenses), Name, NA))

duomenys_pelnas <- duomenys %>%
  group_by(Industry) %>%
  mutate(outlier = ifelse(isskirtis(Profit), Name, NA))%>%
  mutate(salygine_outlier = ifelse(salygine_isskirtis(Profit), Name, NA))

duomenys_prieaugis <- duomenys %>%
  group_by(Industry) %>%
  mutate(outlier = ifelse(isskirtis(Growth), Name, NA))%>%
  mutate(salygine_outlier = ifelse(salygine_isskirtis(Growth), Name, NA))

#------------------------------
#Nagrinesime darbuotoju skaiciu
#-------------------------------

#pasiziurime, ar turime isskirciu
mean(is.na(duomenys_darbuotojai$outlier)) #yra isskirciu

#suskaiciuojame, kiek ju turime
sum(!is.na(duomenys_darbuotojai$outlier)) #27 isskirtys
sum(!is.na(duomenys_darbuotojai$salygine_outlier)) #23 salygines isskirtys

#nusibraizome staciakampes diagramas paziureti vaizdiskai isskirtis
g1<-ggplot(duomenys_darbuotojai, aes(x=Industry, y=Employees)) +
  geom_boxplot() + ggtitle("Employees number boxplots by industry") +
  xlab("Type of industry") + ylab("Employees number") + ggrepel::geom_text_repel(aes(label = outlier), size = 3.5)  +
  scale_y_continuous(breaks = c(0, seq(0, 8000, 2000))) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + labs(subtitle = "Labeled outliers")
g1


#Matome, kad yra viena didele isskirtis retail sektoriuje, suzinome apie ja
retail<-duomenys_darbuotojai[duomenys_darbuotojai$Industry == "Retail",]
retail_isskirtis<-retail[which.max(retail$Employees),]
retail_isskirtis

#kiek virsijo mediana
retail_isskirtis$Employees-median(duomenys_darbuotojai$Employees)#7069
#kiek procentais
100-((100*median(duomenys_darbuotojai$Employees))/retail_isskirtis$Employees)#99 proc

duomenys_darbuotojai$max_outlier<-NA
duomenys_darbuotojai$max_outlier[duomenys_darbuotojai$Name == retail_isskirtis$Name]<-retail_isskirtis$Name


#nusibraizome staciakampes diagramas paziureti vaizdiskai isskirtis, cia bus pazymeta didziausioji isskirtis
g2<-ggplot(duomenys_darbuotojai, aes(x=Industry, y=Employees)) +
  geom_boxplot()+ geom_point(data = subset(duomenys_darbuotojai, max_outlier != "NA"),
                             aes(x = Industry, y  = Employees), size = 2, color = "red")+
  geom_text(aes(label=max_outlier), hjust=-.2) + ggtitle("Employees number boxplots by industry") +
  xlab("Type of industry") + ylab("Employees number") +
  scale_y_continuous(breaks = c(0, seq(1000, 7000, 1000))) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(subtitle = "Labeled and colored biggest outlier") 

g2

#sudedame isskirtis i atskira duomenu masyva
duomenys_isskirtys<-subset(duomenys_darbuotojai, duomenys_darbuotojai$outlier != "NA")


#issamiau panagrinesime isskirtis
#---------------------------------
duomenys_darbuotoju_isskirtys <- duomenys_isskirtys %>%
  group_by(Industry) %>%
  mutate(outlier = ifelse(isskirtis(Employees), Name, NA)) %>%
  mutate(salygine_outlier = ifelse(salygine_isskirtis(Employees), Name, NA))

sum(!is.na(duomenys_darbuotoju_isskirtys$outlier)) #2 isskirtys
sum(!is.na(duomenys_darbuotoju_isskirtys$salygine_outlier))#salyginiu nera

duomenys_darbuotoju_isskirtys$outlier[duomenys_darbuotoju_isskirtys$Industry == "Construction"]<-NA

ggplot(duomenys_darbuotoju_isskirtys, aes(x=Industry, y=Employees)) +
  geom_boxplot() + geom_point(data = subset(duomenys_darbuotoju_isskirtys, outlier != "NA"),
                              aes(x = Industry, y  = Employees), size = 3, color = "red") +
  ggrepel::geom_text_repel(aes(label = outlier), size = 3.5)  + ggtitle("Employees outliers boxplots by industry") +
  xlab("Type of industry") + ylab("Employees number") +
  scale_y_continuous(breaks = c(0, seq(1000, 7000, 1000))) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(subtitle = "Labeled and colored outlier") 
table(duomenys_darbuotoju_isskirtys$Industry)

summary(duomenys_darbuotoju_isskirtys)

#---------------------------
#pasaliname visas isskirtis
duomenys_darbuotojai<-filter(duomenys_darbuotojai, is.na(outlier)==T)

#pasiziurime staciakampes diagramas
g3<-ggplot(duomenys_darbuotojai, aes(x=Industry, y=Employees)) +
  geom_boxplot() + ggtitle("Employees number boxplots by industry after outliers removal") +
  xlab("Type of industry") + ylab("Employees number") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(subtitle = "Colored conditional outliers")  + 
  geom_point(data = subset(duomenys_darbuotojai, salygine_outlier != "NA"),
             aes(x = Industry, y  = Employees), size = 1.5, color = "red")

g3
#isskirciu nebera, taciau yra salyginiu isskirciu

#panele pries ir po isskirciu pasalinimo
gridExtra::grid.arrange(g2, g3, nrow = 2)

duomenys_darb_2<-duomenys_darbuotojai

#pakeiciame salygines isskirtis i 5 kvantili arba 95
#---------------------------------------------------
caps <- quantile(duomenys_darbuotojai$Employees, probs=c(.05, .95))

duomenys_darbuotojai$Employees[duomenys_darbuotojai$Employees < quantile(duomenys_darbuotojai$Employees, .25) - 1.5*IQR(duomenys_darbuotojai$Employees)] <- caps[1]
duomenys_darbuotojai$Employees[duomenys_darbuotojai$Employees > quantile(duomenys_darbuotojai$Employees, .25) + 1.5*IQR(duomenys_darbuotojai$Employees)] <- caps[2]

sum(isskirtis(duomenys_darbuotojai$Employees))
sum(salygine_isskirtis(duomenys_darbuotojai$Employees))#salyginiu 67

#isskaido pagal industrijas
g4<-ggplot(duomenys_darbuotojai, aes(x=Industry, y=Employees)) +
  geom_boxplot() + ggtitle("Employees number boxplots by industry when 5 and 95 quantiles for conditional outliers") +
  xlab("Type of industry") + ylab("Employees number") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,150)) + labs(subtitle = "Colored conditional outliers")  + 
  geom_point(data = subset(duomenys_darbuotojai, salygine_outlier != "NA"),
             aes(x = Industry, y  = Employees), size = 1.5, color = "red")

g4

#bandom keisti su mediana
#-----------------------------------------
duomenys_darb_2$Employees[duomenys_darb_2$Employees < quantile(duomenys_darb_2$Employees, .25) - 1.5*IQR(duomenys_darb_2$Employees)] <- median(duomenys_darb_2$Employees)
duomenys_darb_2$Employees[duomenys_darb_2$Employees > quantile(duomenys_darb_2$Employees, .25) + 1.5*IQR(duomenys_darb_2$Employees)] <- median(duomenys_darb_2$Employees)



sum(isskirtis(duomenys_darb_2$Employees))
sum(salygine_isskirtis(duomenys_darb_2$Employees))#salyginiu 10
#isskaido pagal industrijas
g5<-ggplot(duomenys_darb_2, aes(x=Industry, y=Employees)) +
  geom_boxplot()+ ggtitle("Employees number boxplots when using median for conditional outliers") +
  xlab("Type of industry") + ylab("Employees number") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,150)) + labs(subtitle = "Colored conditional outliers")  + 
  geom_point(data = subset(duomenys_darb_2, salygine_outlier != "NA"),
             aes(x = Industry, y  = Employees), size = 1.5, color = "red")

g5

gridExtra::grid.arrange(g4, g5, nrow = 1)

#paziurime vaizdiskai, kuris metodas geresnis
#--------------------------------------------
boxplot(duomenys_darb_2$Employees, duomenys_darbuotojai$Employees, ylim=c(0, 150), ylab = "Employees number", main = "Employees number boxplot across all industries", 
        xlab = "Methods of modifying conditional outliers",
        names = c("Using median", "Using 5 and 95 quantiles"))


#pasirenkame medianos metoda, nes sklaida mazesne bei maziau paciu salyginiu isskirciu


#-------------------------------
#pajamos
#-----------------------------
mean(is.na(duomenys_pajamos$outlier)) #=>1=> nera tikruju isskirciu
sum(!is.na(duomenys_pajamos$outlier)) #0
sum(!is.na(duomenys_pajamos$salygine_outlier))#6 salygines isskirtys

#sukuriame nauja stulpeli, kuriame pajamos milijonais
duomenys_pajamos$Revenue1mln<-duomenys_pajamos$Revenue/1000000

g6<-ggplot(duomenys_pajamos, aes(x=Industry, y=Revenue1mln)) +
  geom_boxplot() + ggtitle("Revenue boxplots by industry") +
  xlab("Type of industry") + ylab("Revenue (1 million $)") +
  scale_y_continuous(limits=c(0, 25))  +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label=salygine_outlier), hjust= -0.1)  + labs(subtitle = "Labeled and colored conditional outliers")+ 
  geom_point(data = subset(duomenys_pajamos, salygine_outlier != "NA"),
             aes(x = Industry, y  = Revenue1mln), size = 2, color = "red")

g6

#tvarkysime salygines isskirtis, jas pakeisime i 5 arba 95 kvantili
caps <- quantile(duomenys_pajamos$Revenue, probs=c(.05, .95))

duomenys_pajamos$Revenue[duomenys_pajamos$Revenue < quantile(duomenys_pajamos$Revenue, .25) - 1.5*IQR(duomenys_pajamos$Revenue)] <- caps[1]
duomenys_pajamos$Revenue[duomenys_pajamos$Revenue > quantile(duomenys_pajamos$Revenue, .25) + 1.5*IQR(duomenys_pajamos$Revenue)] <- caps[2]

sum(isskirtis(duomenys_pajamos$Revenue))
sum(salygine_isskirtis(duomenys_pajamos$Revenue))
#nebeliko jokiu isskirciu nei salyginiu

#sukuriame nauja stulpeli, kuriame pajamos milijonais
duomenys_pajamos$Revenue1mln<-duomenys_pajamos$Revenue/1000000

#dar nusibraizome keleta boxplot diagramu pasiziurejimui vizualiam
#cia isskaido pagal industrijas
g7<-ggplot(duomenys_pajamos, aes(x=Industry, y=Revenue1mln)) +
  geom_boxplot()+ggtitle("Revenue boxplots by industry after conditional outliers modification") +
  xlab("Type of industry") + ylab("Revenue (1 million $)")+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks=c(0, 4,8,12,16)) 

g7

#pries ir po salyginiu isskirciu pasalinimo
gridExtra::grid.arrange(g6, g7, nrow = 2)

#---------------------------------------
#islaidos
#--------------------------------------

mean(is.na(duomenys_islaidos$outlier)) #=>1=> nera tikruju isskirciu
sum(!is.na(duomenys_islaidos$salygine_outlier))#4 salyginiu isskirciu

#sukuriame nauja stulpeli, kuriame islaidos milijonais
duomenys_islaidos$Expenses1mln<-duomenys_islaidos$Expenses/1000000

#vizualiai pasiziurime boxplotus
#cia isskaido pagal industrijas
g8<-ggplot(duomenys_islaidos, aes(x=Industry, y=Expenses1mln)) +
  geom_boxplot()+ggtitle("Expenses boxplots by industry") +
  xlab("Type of industry") + ylab("Expenses (1 million $)")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label=salygine_outlier), hjust= -0.1)  + labs(subtitle = "Labeled and colored conditional outliers")+
  geom_point(data = subset(duomenys_islaidos, salygine_outlier != "NA"),
             aes(x = Industry, y  = Expenses1mln), size = 2.5, color = "red")

g8


#tvarkysime salygines isskirtis, jas pakeisime i 5 arba 95 kvantili
caps <- quantile(duomenys_islaidos$Expenses, probs=c(.05, .95))

duomenys_islaidos$Expenses[duomenys_islaidos$Expenses < quantile(duomenys_islaidos$Expenses, .25) - 1.5*IQR(duomenys_islaidos$Expenses)] <- caps[1]
duomenys_islaidos$Expenses[duomenys_islaidos$Expenses > quantile(duomenys_islaidos$Expenses, .25) + 1.5*IQR(duomenys_islaidos$Expenses)] <- caps[2]

sum(isskirtis(duomenys_islaidos$Expenses))
sum(salygine_isskirtis(duomenys_islaidos$Expenses))
#nebeliko jokiu isskirciu nei salyginiu

#sukuriame nauja stulpeli, kuriame islaidos milijonais
duomenys_islaidos$Expenses1mln<-duomenys_islaidos$Expenses/1000000

#pasalinus salygines isskirtis
#vizualiai pasiziurime boxplotus
#cia isskaido pagal industrijas
g8_2<-ggplot(duomenys_islaidos, aes(x=Industry, y=Expenses1mln)) +
  geom_boxplot()+ggtitle("Expenses boxplots by industry after conditional outlier modification") +
  xlab("Type of industry") + ylab("Expenses (1 million $)")+
  theme(plot.title = element_text(hjust = 0.5))

g8_2


gridExtra::grid.arrange(g8, g8_2, nrow = 2)
#-----------------------------------
#pelnas
#-----------------------------------

mean(is.na(duomenys_pelnas$outlier)) #turime isskirciu
sum(!is.na(duomenys_pelnas$outlier)) #1 isskirtis
sum(!is.na(duomenys_pelnas$salygine_outlier)) #3 salygines isskirtys

#sukuriame nauja stulpeli, kuriame pelnas milijonais
duomenys_pelnas$Profit1mln<-duomenys_pelnas$Profit/1000000

#pasibraizome boxplot
g9<-ggplot(duomenys_pelnas, aes(x=Industry, y=Profit1mln)) +
  geom_boxplot() +
  geom_text(aes(label=outlier), hjust = -0.2) + ggtitle("Profit boxplots by industry") +
  xlab("Type of industry") + ylab("Profit (1 million $)")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(subtitle = "Labeled and colored outlier") + scale_y_continuous(limits=c(0, 700)) +  
  geom_point(data = subset(duomenys_pelnas, outlier != "NA"),
             aes(x = Industry, y  = Profit1mln), size = 2.5, color = "red")

g9
#matome, viena labai didele isskirti statybu sektoriuje

#pasitikrinsime, ar sis stebejimas jau yra isskirciu lenteleje
pelno_isskirtis<-duomenys_pelnas[duomenys_pelnas$Industry == "Construction",]
pelno_isskirtis<-pelno_isskirtis[which.max(pelno_isskirtis$Profit),]
pelno_isskirtis<-pelno_isskirtis[,1:13]
pelno_isskirtis$maxoutlier<-NA
pelno_isskirtis
#doleriais
pelno_isskirtis$Revenue-median(duomenys_pelnas$Revenue) #1078448
#procentais kiek virsijo
100-((100*median(duomenys_pelnas$Revenue))/pelno_isskirtis$Revenue)#9 proc

library(plyr)

match_df(duomenys_isskirtys, pelno_isskirtis, on="Name")
#nera tokio dar stebejimo isskirtyse

#sudedame isskirtis i atskira duomenu masyva
duomenys_isskirtys[nrow(duomenys_isskirtys) + 1,] <- pelno_isskirtis

#Matome, kad yra viena didele isskirtis construction imoneje, ja pasaliname
duomenys_pelnas<-subset(duomenys_pelnas, Name != pelno_isskirtis$Name)

duomenys_pelnas<-mutate( duomenys_pelnas, type=ifelse(Industry=="IT Services","Highlighted","Normal"))

g10<-ggplot(duomenys_pelnas, aes(x=Industry, y=Profit1mln, fill = type)) +
  geom_boxplot() +
  geom_text(aes(label=salygine_outlier), hjust = -0.1)+ ggtitle("Profit boxplots by industry after outlier removal") +
  xlab("Type of industry") + ylab("Profit (1 million $)")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust=0.5))+ 
  labs(subtitle = "Labeled conditional outliers") +  
  geom_point(data = subset(duomenys_pelnas, salygine_outlier != "NA"),
             aes(x = Industry, y  = Profit1mln), size = 2.5, color = "red")+
  scale_fill_manual(values=c("red", "white")) +
  theme(legend.position = "none") 

g10
#pazymetos salygines isskirtys

gridExtra::grid.arrange(g9, g10, nrow=2)

#dabar tvarkysime salygines isskirtis, jas pakeisi i 5 ir 95 kvantili

caps <- quantile(duomenys_pelnas$Profit, probs=c(.05, .95))

duomenys_pelnas$Profit[duomenys_pelnas$Profit < quantile(duomenys_pelnas$Profit, .25) - 1.5*IQR(duomenys_pelnas$Profit)] <- caps[1]
duomenys_pelnas$Profit[duomenys_pelnas$Profit > quantile(duomenys_pelnas$Profit, .25) + 1.5*IQR(duomenys_pelnas$Profit)] <- caps[2]

#patikriname, ar liko salyginiu ar tikru isskirciu
sum(isskirtis(duomenys_pelnas$Profit))
sum(salygine_isskirtis(duomenys_pelnas$Profit))
#neliko

#sukuriame nauja stulpeli, kuriame pelnas butu milijonais
duomenys_pelnas$Profit1mln<-duomenys_pelnas$Profit/1000000

#nubreziame boxplot
g11<-ggplot(duomenys_pelnas, aes(x=Industry, y=Profit1mln)) +
  geom_boxplot() + ggtitle("Profit boxplots by industry \nafter conditional outlier modification and outlier removal") +
  xlab("Type of industry") + ylab("Profit (1 million $)")+
  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(limits=c(0, 15)) 

g11
gridExtra::grid.arrange(g9, g10, g11, nrow=3)

#------------------------------------
#prieaugis
#-----------------------------------
mean(is.na(duomenys_prieaugis$outlier)) #=>1=>nera isskirciu
sum(!is.na(duomenys_prieaugis$salygine_outlier))#19 salyginiu

#pasibraizome boxplot
#cia isskaido pagal industrijas
g12<-ggplot(duomenys_prieaugis, aes(x=Industry, y=Growth)) +
  geom_boxplot() + ggtitle("Growth boxplots by industry") +
  xlab("Type of industry") + ylab("Growth (%)")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust=0.5))+ 
  labs(subtitle = "Labeled conditional outliers") +  
  geom_point(data = subset(duomenys_prieaugis, salygine_outlier != "NA"),
             aes(x = Industry, y  = Growth), size = 1.5, color = "red") +
  scale_y_continuous(limits = c(-5, 30))

g12
#dabar tvarkysime salygines isskirtis, jas pakeisi i 5 ir 95 kvantili

caps <- quantile(duomenys_prieaugis$Growth, probs=c(.05, .95))

duomenys_prieaugis$Growth[duomenys_prieaugis$Growth < quantile(duomenys_prieaugis$Growth, .25) - 1.5*IQR(duomenys_prieaugis$Growth)] <- caps[1]
duomenys_prieaugis$Growth[duomenys_prieaugis$Growth > quantile(duomenys_prieaugis$Growth, .25) + 1.5*IQR(duomenys_prieaugis$Growth)] <- caps[2]

#patikriname, ar liko salyginiu ar tikru isskirciu
sum(isskirtis(duomenys_prieaugis$Growth))
sum(salygine_isskirtis(duomenys_prieaugis$Growth))
#neliko


g13<-ggplot(duomenys_prieaugis, aes(x=Industry, y=Growth)) +
  geom_boxplot() + ggtitle("Growth boxplots by industry after conditional outlier modification") +
  xlab("Type of industry") + ylab("Growth (%)")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(-5, 30))

g13
gridExtra::grid.arrange(g12, g13, nrow=2)

#------------------------------
#Isskirtys
#-----------------------------
#pasaliname is visu duomenu rinkiniu isskirtis
duomenys_darb_2<-anti_join(duomenys_darb_2, duomenys_isskirtys, by = "Name")
duomenys_islaidos<-anti_join(duomenys_islaidos, duomenys_isskirtys, by = "Name")
duomenys_pajamos<-anti_join(duomenys_pajamos, duomenys_isskirtys, by = "Name")
duomenys_pelnas<-anti_join(duomenys_pelnas, duomenys_isskirtys, by = "Name")
duomenys_prieaugis<-anti_join(duomenys_prieaugis, duomenys_isskirtys, by = "Name")

#pasaliname isskirtis, kuriu yra 37
duomenys<-anti_join(duomenys, duomenys_isskirtys, by = "Name")

#pakeiciame salyginiu isskirciu reiksmes
duomenys$Employees<-duomenys_darb_2$Employees
duomenys$Revenue<-duomenys_pajamos$Revenue
duomenys$Expenses<-duomenys_islaidos$Expenses
duomenys$Profit<-duomenys_pelnas$Profit
duomenys$Growth<-duomenys_prieaugis$Growth

#toliau dirbame su duomenys rinkiniu

#---------------------------------
#statistika pagal industrijas
#--------------------------------
summary(duomenys)

#---------------------------------
#duomenu normavimas
#--------------------------------
# boxplot -pries normavima
library(reshape)

duomenys$Revenue1mln<-duomenys$Revenue/1000000
duomenys$Expenses1mln<-duomenys$Expenses/1000000
duomenys$Profit1mln<-duomenys$Profit/1000000

data_mod1 <- melt(duomenys, id.vars = "Inception",
                  measure.vars = c('Employees','Revenue', 'Expenses', 'Profit','Growth' ))

p1 <- ggplot(data_mod1) +
  geom_boxplot(aes( y=value, x = as.factor(variable))) + ggtitle("Variable boxplots before rationing") +
  xlab("Variable name") + ylab("Value") +
  theme(plot.title = element_text(hjust = 0.5))

p1

#normavimas pagal min-max
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

min_max_norm.data <- duomenys #xsukuriame duomenu aibes kopija
#normavimas min-max
min_max_norm.data$Growth <-  min_max_norm(min_max_norm.data$Growth)
min_max_norm.data$Employees <-  min_max_norm(min_max_norm.data$Employees)
min_max_norm.data$Revenue <-  min_max_norm(min_max_norm.data$Revenue)
min_max_norm.data$Expenses <-  min_max_norm(min_max_norm.data$Expenses)
min_max_norm.data$Profit <-  min_max_norm(min_max_norm.data$Profit)

#boxplot sunormuotiems duomenims pagal min-max
data_mod2 <- melt(min_max_norm.data, id.vars = "Inception",
                  measure.vars = c('Employees','Revenue', 'Expenses', 'Profit','Growth' ))

p2 <- ggplot(data_mod2) +
  geom_boxplot(aes( y=value, x = as.factor(variable))) + ggtitle("Variable boxplots after min - max rationing") +
  xlab("Variable name") + ylab("Value") +
  theme(plot.title = element_text(hjust = 0.5))
p2

#normavimas pagal vidurki ir dispersija
mean_sd_norm <- function(x) {
  (x - mean(x))/ sd(x)
}

mean_sd_norm.data <- duomenys #xsukuriame duomenu aibes kopija
#normavimas pagal vidurki ir dispersija
mean_sd_norm.data$Growth <- mean_sd_norm(mean_sd_norm.data$Growth)
mean_sd_norm.data$Profit <- mean_sd_norm(mean_sd_norm.data$Profit)
mean_sd_norm.data$Employees <- mean_sd_norm(mean_sd_norm.data$Employees)
mean_sd_norm.data$Revenue <- mean_sd_norm(mean_sd_norm.data$Revenue)
mean_sd_norm.data$Expenses <- mean_sd_norm(mean_sd_norm.data$Expenses)

#boxplot sunormuotiems duomenims pagal vidurki ir dispersija
data_mod3 <- melt(mean_sd_norm.data, id.vars = "Inception",
                  measure.vars = c('Employees','Revenue', 'Expenses', 'Profit','Growth' ))

p3 <- ggplot(data_mod3) +
  geom_boxplot(aes( y=value, x = as.factor(variable))) + ggtitle("Variable boxplots after standartizing") +
  xlab("Variable name") + ylab("Value") +
  theme(plot.title = element_text(hjust = 0.5))
p3

gridExtra::grid.arrange(p1, p2, p3,  nrow=1)

# lyginimas pagal pramones sakas ------------------------------------------

pelnas <- min_max_norm.data %>% select('Industry','Profit')

# pelno pasiskirstymas pagal pramones sakas
cons <- with(pelnas, sum(Profit[Industry == 'Construction']))
fs <- with(pelnas, sum(Profit[Industry == 'Financial Services']))
gs <- with(pelnas, sum(Profit[Industry == 'Government Services']))
he <- with(pelnas, sum(Profit[Industry == 'Health']))
it <- with(pelnas, sum(Profit[Industry == 'IT Services']))
re <- with(pelnas, sum(Profit[Industry == 'Retail']))
sof <- with(pelnas, sum(Profit[Industry == 'Software']))
count(pelnas, "Industry") #suzinome kiek eiluciu kiekviena pramones saka turi

df <- data.frame(Industry = c("Construction","Financial Services","Government Services","Health","IT Services", "Retail", "Software"),
                 Profit = c(cons/46,fs/51,gs/44,he/80,it/142,re/44,sof/59))

sj1 <- ggplot(df, aes(x = Industry, y = Profit, fill = Industry)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Profit distribution among industries") +
  scale_fill_manual(values=c("lightgrey",  "lightgrey", "lightgrey", "lightgrey","darkgrey", "lightgrey", "lightgrey" )) +
  theme(legend.position="none")

sj1

# islaidu pasiskirstymas pagal pramones sakas

islaidos <- min_max_norm.data %>% select('Industry','Expenses')

cons <- with(islaidos, sum(Expenses[Industry == 'Construction']))
fs <- with(islaidos, sum(Expenses[Industry == 'Financial Services']))
gs <- with(islaidos, sum(Expenses[Industry == 'Government Services']))
he <- with(islaidos, sum(Expenses[Industry == 'Health']))
it <- with(islaidos, sum(Expenses[Industry == 'IT Services']))
re <- with(islaidos, sum(Expenses[Industry == 'Retail']))
sof <- with(islaidos, sum(Expenses[Industry == 'Software']))

df <- data.frame(Industry = c("Construction","Financial Services","Government Services","Health","IT Services", "Retail", "Software"),
                 Expenses = c(cons/46,fs/51,gs/44,he/80,it/142,re/44,sof/59))

sj2 <- ggplot(df,                                      # Grouped barplot using ggplot2
             aes(x = Industry,
                 y = Expenses, fill = Industry)) + 
  geom_bar(stat = "identity",
           position = "dodge") + ggtitle("Expenses distribution among industries") + 
  scale_fill_manual(values=c("lightgrey",  "lightgrey", "lightgrey", "darkgrey","lightgrey", "lightgrey", "lightgrey" )) + 
  theme(legend.position="none")

sj2

# darbuotoju pasiskirstymas pagal pramones sakas

darbuotojai <- min_max_norm.data %>% select('Industry','Employees')

cons <- with(darbuotojai, sum(Employees[Industry == 'Construction']))
fs <- with(darbuotojai, sum(Employees[Industry == 'Financial Services']))
gs <- with(darbuotojai, sum(Employees[Industry == 'Government Services']))
he <- with(darbuotojai, sum(Employees[Industry == 'Health']))
it <- with(darbuotojai, sum(Employees[Industry == 'IT Services']))
re <- with(darbuotojai, sum(Employees[Industry == 'Retail']))
sof <- with(darbuotojai, sum(Employees[Industry == 'Software']))

df <- data.frame(Industry = c("Construction","Financial Services","Government Services","Health","IT Services", "Retail", "Software"),
                 Employees = c(cons/46,fs/51,gs/44,he/80,it/142,re/44,sof/59))

sj3 <- ggplot(df,                                      # Grouped barplot using ggplot2
             aes(x = Industry,
                 y = Employees, fill = Industry)) + 
  geom_bar(stat = "identity",
           position = "dodge") + ggtitle("Employees distribution among industries") +
  scale_fill_manual(values=c("lightgrey",  "lightgrey", "darkgrey","lightgrey", "lightgrey", "lightgrey", "lightgrey" )) + 
  theme(legend.position="none") 
  

sj3

library(ggpubr)
ggarrange(sj1, sj2, sj3, ncol=1)

# Kur daugiausia imoniu ----------------------------------------------------

df <- count(duomenys, "State") #suzinome kiek eiluciu kiekviena pramones saka turi
df

df <- head(df[order(df$freq, decreasing = TRUE),c(1,2)], 5) #randame kiek, kiekviena valstija turi imoniu

sg <- ggplot(df, aes(x = State, y = freq, fill = State)) + 
  geom_bar(stat = "identity",position = "dodge") + geom_text(aes(label=freq), vjust=-0.3, size=3.5) + 
  ylab("Number of industries") + 
  scale_fill_brewer(palette="Set1")
sg


# Kuriose pramones sakose -------------------------------------------------
# pirma issirenki valstija ir tada randi max is stulpelio

#--------
#Kokiu pramones saku daugiausiai CA
#--------
CA <- duomenys %>% select('State','Industry')
CA <- with(CA, count(Industry[State == 'CA']))
CA[order(CA$freq, decreasing = TRUE),c(1,2)]

#--------
#Kokiu pramones saku daugiausiai FL
#--------
FL <- duomenys %>% select('State','Industry')
FL <- with(FL, count(Industry[State == 'FL']))
FL[order(FL$freq, decreasing = TRUE),c(1,2)]

#--------
#Kokiu pramones saku daugiausiai NY
#--------
NY <- duomenys %>% select('State','Industry')
NY <- with(NY, count(Industry[State == 'NY']))
NY[order(NY$freq, decreasing = TRUE),c(1,2)]

#--------
#Kokiu pramones saku daugiausiai TX
#--------
TX <- duomenys %>% select('State','Industry')
TX <- with(TX, count(Industry[State == 'TX']))
TX[order(TX$freq, decreasing = TRUE),c(1,2)]

#--------
#Kokiu pramones saku daugiausiai VA
#--------
VA <- duomenys %>% select('State','Industry')
VA <- with(VA, count(Industry[State == 'VA']))
VA[order(VA$freq, decreasing = TRUE),c(1,2)]

# -------------------------------
# koreliacija
# -------------------------------
library(corrplot)

corrplot.mixed(cor(duomenys[,8:11]),
               lower = "number", 
               upper = "ellipse",
               tl.col = "black")




















