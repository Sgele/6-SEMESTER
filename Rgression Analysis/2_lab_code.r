
# reikalingos bibliotekos -------------------------------------------------

library(MASS)
library(readxl)
library(AER)

# duomenu nuskaitymas ir padalinimas --------------------------------------

library(AER)
data("PhDPublications")

#sample <- sample(c(TRUE, FALSE), nrow(PhDPublications), replace=TRUE, prob=c(0.7,0.3))
#train  <- PhDPublications[sample, ]
#test   <- PhDPublications[!sample, ]
#library("writexl")
#write_xlsx(test ,"C:\\Users\\ugneo\\OneDrive\\Stalinis kompiuteris\\Duomenų mokslas\\6 semestras\\Projektinis darbas\\test_data_2_lab.xlsx")
#write_xlsx(train ,"C:\\Users\\ugneo\\OneDrive\\Stalinis kompiuteris\\Duomenų mokslas\\6 semestras\\Projektinis darbas\\train_data_2lab.xlsx")
train<-read_xlsx("C:/Users/ugneo/OneDrive/Stalinis kompiuteris/Duomenų mokslas/6 semestras/Projektinis darbas/train_data_2lab.xlsx")
test<-read_xlsx("C:/Users/ugneo/OneDrive/Stalinis kompiuteris/Duomenų mokslas/6 semestras/Projektinis darbas/test_data_2_lab.xlsx")

train$gender<-as.factor(train$gender)
train$married<-as.factor(train$married)

test$gender<-as.factor(test$gender)
test$married<-as.factor(test$married)

# prielaidu tikrinimas ----------------------------------------------------


# paziurime vizualiai, ar vidurkis lygus dispersijai ----------------------

h<-hist(train$articles, breaks = seq(0, 20, 1), ylim=c(0, 400), xlab = "Articles number", main = "Histogram of articles")

text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
abline(v = mean(train$articles), col='red', lwd = 2,lty='dashed')
abline(v = var(train$articles), col='blue', lwd = 2,lty='dashed')
legend("topright", legend=c("Mean", "Variance"),
       col=c("red", "blue"), lty = 'dashed',cex=0.8)

mean(train$articles)#1.7

var(train$articles)#3.5
#suskaiciave vidurki ir dispersija, matome, kad turime didesne dispersija

# taikome puasono modeli --------------------------------------------------

articles_pois <- glm(articles ~ ., data = train,
               family = poisson)
summary(articles_pois)
#matome, kad turime reiksmingu kovarianciu

articles_pois$deviance / articles_pois$df.residual #1.8, kas yra jau zymiai daugiau uz vieneta, ir galime itarti, kad
#puasono modelis netinka duomenims

#taikysime tiketinumo santykio kriteriju ir tirsime, ar sioje lygybeje 𝐕(𝑦𝑖| 𝐱𝑖)=𝜇𝑖 + 𝛼 ℎ(𝜇𝑖)  alfa yra 0, ar ne
#jei yra nulis, tai turi tikti puasono modelis. Naudosime tiketinumo santykio kriterijus nes nuline hipoteze bus siauresne, kad =0,
#o alternatyva, kad nelygu, tai gali buti daugiau arba maziau
#Tai tikrinsime, tokias hipotezes:
#H0:alfa=0
#H1:alfa > 0
dispersiontest(articles_pois, trafo = 1, alternative = "greater")#alfa =0.765
dispersiontest(articles_pois, trafo = 2)#alfa 0.444

#abiem atvejais gauname, kad nuline hipoteze atmetame, nes p reiksmes <0.05=reiksmingumo lygmuo
#Neigiamo binominio I modelio atveju dispersija gali būti didesnė už vidurkį, bet jų santykis yra toks pats visiems
#stebėjimams ir lygus (1 + 𝛼).
#Neigiamo binominio II modelio atveju dispersija gali būti didesnė už vidurkį, jų santykis didesnis tiems elementams, kurių
#vidurkis didesnis. 

#Kadangi alfa > 0, tai reiktu taikyti neigiama binomi, nes per didele dispersija


# taikome neigiama binomi -------------------------------------------------
library("MASS")

articles_nb<-glm.nb(articles~., data=train)
summary(articles_nb)
#matome, kad turime reiksmingu kovarianciu, bet is pradziu turime patikrinti prielaidas

# prielaidu tikrinimas ----------------------------------------------------


#1) neturi buti isskirciu
#2) neturi buti multikolinearumo

#1) neturi buti isskirciu
plot(articles_nb,4)
#matome is kuko grafiko, jog visos reiksmes < 1, tai isskirciu neturime

#dfbetas paskaiciuojame

df<-as.data.frame(dfbetas(articles_nb))
mean(abs(df$mentor)> 1)

mean(abs(df$prestige)> 1)
mean(abs(df$kids)> 1)
mean(abs(df$marriedno)> 1)
mean(abs(df$gendermale)> 1)
#matome, kad is visu stebejimu neturime didesniu uz slenksti, tai neturime isskirciu

par(mfrow=c(2,2)) 
plot(df$mentor, type='h', main = "DFBETAS for mentor", ylab = "DFBETAS statistics for mentor")

plot(df$prestige, type='h',  main = "DFBETAS for prestige", ylab = "DFBETAS statistics for prestige", ylim = c(-0.2, 0.2))

plot(df$kids, type='h',main = "DFBETAS for kids", ylab = "DFBETAS statistics for kids")

plot(df$marriedno, type='h', main = "DFBETAS for married", ylab = "DFBETAS statistics for married no",  ylim = c(-0.15, 0.15))

plot(df$gendermale, type='h',  main = "DFBETAS for gender", ylab = "DFBETAS statistics for gender male",  ylim = c(-0.15, 0.15))

dev.off()

#standartizuojame liekanas, is ju ziuresime
resid_stand <- rstandard(articles_nb)

sum(abs(resid_stand) > 3)#turime du stebejimus kurie >3 moduliu
which(abs(resid_stand) > 3)
resid_stand[which(abs(resid_stand) > 3)]
#tai yra 911 ir 914 stebejimas is visos imties, o is train imties 647 ir 649

train[647,] 
train[649,]
#tai yra vede vyrai vieni is daugiausiai parase straipsniu

#multikolinearumas
vif(articles_nb)
#is koef matome, kad nera


# pirminis duomenu vaizdas ------------------------------------------------
library(ggplot2)
library(dplyr)
gg1<-ggplot(train, aes(x=gender, y=articles)) +
  geom_boxplot() + ggtitle("Published articles by gender") +
  xlab("Gender") + ylab("Number of articles") 
gg1

gg2<-ggplot(train, aes(x=married, y=articles)) +
  geom_boxplot() + ggtitle("Published articles by marital status") +
  xlab("Marital status") + ylab("Number of articles") 
gg2


train$kids_2<-as.factor(train$kids)
gg3<-ggplot(train, aes(x=kids_2, y=articles)) +
  geom_boxplot() + ggtitle("Published articles by kids number") +
  xlab("Kids number") + ylab("Number of articles") 
gg3


train$prestige_3<-ifelse(train$prestige > 3, "> 3", "<= 3")
gg5<-ggplot(train, aes(x=prestige_3, y=articles)) +
  geom_boxplot() + ggtitle("Published articles by prestige") + 
  xlab("Prestige level") + ylab("Number of articles") + labs(subtitle = "Prestige number was grouped into 2 levels")
gg5

train$mentor_10<-ifelse(train$mentor < 10, "[0; 10)", ifelse(train$mentor < 20, "[10; 20)", ifelse(
  train$mentor < 30, "[20; 30)", ifelse(train$mentor < 40, "[30; 40)", ifelse( train$mentor < 50, "[40; 50)",ifelse(
    train$mentor < 60, "[50; 60)", "[60; 70)"
  )))
)))
gg6<-ggplot(train, aes(x=mentor_10, y=articles)) +
  geom_boxplot() + ggtitle("Published articles by mentors pusblished articles") + 
  xlab("Mentors published articles") + ylab("Number of articles") + labs(subtitle = "Mentors articles was grouped into 7 levels")
gg6

gridExtra::grid.arrange(gg1, gg2, gg3, gg5, gg6, nrow = 3)

train<-train[,1:6]

table(train$gender)
table(train$married)
428/(428+221)
table(train$kids)

table(cut(train$prestige,seq(0,5,0.5)))
table(cut(train$prestige,seq(0,6,3)))
table(cut(train$mentor,seq(0,70,10)))

# pradedame nagrineti modeli ----------------------------------------------


# tikriname, ar yra nors viena reiksminga kovariante ----------------------

#tikrinsime su tiketinumo asntykio kriterijumi, ar yra nors viena reiksminga kovariante
#tikrinsime hipotezes
#H0:visos kovariantes =0
#H1: bent viena lygybe neteisinga, t. y. bent 1 kovariante nelygi nuliui
modelis<-glm.nb(articles~1, data=train)
anova(modelis, articles_nb, test="Chisq")
#Gavome p < 0.05=reiksmingumo lygmuo, tai nuline hipoteze galime atmesti, t.y. modelyje yra bent viena reiksminga kovariante


# paziurime visa modeli gauta ---------------------------------------------

summary(articles_nb)
#matome, kad visos kovariantes isskyrus prestige yra reiksmingos, kai reiksmingumo lygmuo 0.1, taip pat matome tetos iverti 2.449

# pazingsnine regresija ---------------------------------------------------

stepAIC(articles_nb, direction = "both")
stepAIC(articles_nb, direction ="backward")
stepAIC(articles_nb, direction ="forward")

#matome, kad both ir backward pazingsnine regresija gavome geresni AIC rezultata ir ismete tik prestige kovariante


# naujas modelis ----------------------------------------------------------

naujas_nb<-glm.nb(articles~gender+married+kids+mentor, data=train)
summary(naujas_nb)



# modelio tinkamumo ivertinimas -------------------------------------------

naujas_nb$deviance / naujas_nb$df.residual #gavome 1.11, tai labai geras rezultatas
AIC(naujas_nb)
BIC(naujas_nb)
#pseudo r kvadrata skaiciuojame
1 - naujas_nb$deviance/naujas_nb$null.deviance #mazas 0.09

alfa<-1/naujas_nb$theta
alfa #alfa ivertis

#mokymo aibei tiketini ir reali dazniai
rbind(obs = table(train$articles)[1:14], exp = round(
  sapply(0:13, function(x) sum(dnbinom(x, mu=fitted(naujas_nb),size=naujas_nb$theta)))))
#matome, kad tiketini ir esami dazniai atitinka su nedidele paklaida, taip pat neturime nuliu problemos


# modelio interpretacija --------------------------------------------------
summary(naujas_nb)
naujas_nb$coefficients
#didejant mentoriu skaiciui ir esant istekeju/vedus didejant isleistu straipsniu vidurkis
#didejant vaiku skaiciui ir esant moterimi mazeja isleistu straipsniu vidurkis
exp(coef(naujas_nb))  
#taigi matome is koeficientu, kad nevedusiu zmoniu apie 15 procentu sumazeja isleistu straipsniu vidurkis nei nevedusiu
#padidejus mentoriu skaiciui 1 vienetu 3 procentais padideja isleistu straipsniu vidurkis
#padidejus vaiku skaiciui 1 vienetu seimoje apie 12 procentu sumazetu isleistu straipsniu vidurkis
#bunant vyru 31 proc padideja isleistu straipsniu vidurkis nei bunant moterimi



# su saveikom -------------------------------------------------------------
summary(naujas_nb)
saveikos<-glm.nb(articles~.+prestige*mentor, data=train)
summary(saveikos)#atradome viena su saveikomis
vif(saveikos)#multikolinearumo problema
#siam modeliui atliksime pazinsgnine regresija
stepAIC(saveikos, direction = "both")#palieka visus
stepAIC(saveikos, direction = "backward")#visus
stepAIC(saveikos, direction = "forward")#visus
#palieka visus, nors vedybinis gyvenimas yra reiksmingas su reiksmingumo lygmeniu 0.11

# palyginimas modeliu -----------------------------------------------------

AIC(naujas_nb)#2230.652
AIC(saveikos)#2227.419

BIC(naujas_nb)#2257.504
BIC(saveikos)#2263.222

#modeliai neatsiskiria aiskiai, kuris geresnis

naujas_nb$deviance / naujas_nb$df.residual#1.11
saveikos$deviance / saveikos$df.residual#1.11

#pseudo r kvadrata skaiciuojame
1 - naujas_nb$deviance/naujas_nb$null.deviance#0.09
#pseudo r kvadrata skaiciuojame
1 - saveikos$deviance/saveikos$null.deviance#0.10


#mokymo aibei tiketini ir reali dazniai
rbind(obs = table(train$articles)[1:14], exp = round(
  sapply(0:13, function(x) sum(dnbinom(x, mu=fitted(naujas_nb),size=naujas_nb$theta)))))

#mokymo aibei tiketini ir reali dazniai
rbind(obs = table(train$articles)[1:14], exp = round(
  sapply(0:13, function(x) sum(dnbinom(x, mu=fitted(saveikos),size=saveikos$theta)))))


#suklasifikavo beveik identiskai tik su saveikomis tiketini 0 buvo arciau tikruju per 1



# rmse -------------------------------


test$forecast_1<-predict(naujas_nb, test, type="response") 
test$forecast_2<-predict(saveikos, test, type = "response")

install.packages("Metrics")
library(Metrics)
rmse(test$articles, test$forecast_1)#2.005
rmse(test$articles, test$forecast_2)#2.704

#pagal rmse pirmasis modelis yra geresnis be saveikos

#sqrt(mean((data$actual - data$predicted)^2)) rmse skaiciavimo formule


# prognozuotos reiksmes ---------------------------------------------------

rows <- sample(nrow(test))
rows

ismaisytos_prognozuotos<-test[rows,]
keletas_prognozuotu<-ismaisytos_prognozuotos[1:10,]
