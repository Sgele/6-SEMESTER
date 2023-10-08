# bibliotekos -------------------------------------------------------------

library(splitstackshape)
library(openxlsx)
library(dynpred)
library(Epi)
library(rms)
library(MASS)
library(ggplot2)
library(survminer)
library("readxl")
library(survival)
library(readxl)
library(ggplot2)
library(survminer)
library(car)
library(pROC)

# duomenys ----------------------------------------------------------------
library(survival)

data(cancer, package="survival")

duomenys_tiriamasis<-rotterdam

nrow(duomenys_tiriamasis) #2982 eilutes
nrow(duomenys_tiriamasis[!complete.cases(duomenys_tiriamasis),]) #nera praleistu reiksmiu

# praleistos reiksmes -----------------------------------------------------
dim(rotterdam)
sum(is.na(rotterdam))

summary(rotterdam)
colnames(rotterdam)
head(rotterdam)

# dazniu lenteles
table(rotterdam$death)

table(rotterdam$death, rotterdam$meno)
table(rotterdam$death, rotterdam$size)
table(rotterdam$death, rotterdam$grade)
table(rotterdam$death, rotterdam$hormon)
table(rotterdam$death, rotterdam$chemo)

table(rotterdam$death, rotterdam$nodes)

colnames(rotterdam)
# tarpusavio koreliacija

colnames(rotterdam2) <- c("Metai", "Amþius", "Progesterono receptoriai ", "Estrogenø receptoriai ", "Dienos iki mirties")
rotterdam2<-rotterdam[,c(-1, -4,-5,-6, -7, -10,-11,-12, -13, -15, -16 )]
colnames(rotterdam2)
correlation_matrix <- rotterdam2 %>%
  dplyr::select(where(is.numeric)) %>%
  cor()
corrplot(correlation_matrix, order = "original", method = "color", type = "upper", diag = FALSE, tl.col = "black", addCoef.col = "black")
mtext(
  "Koreliacija",
  at = 6,
  line = 1,
  cex = 2
)

#boxplotai---------------------------------------------------------------
rotterdam$death<-as.factor(rotterdam$death)
grupuoti<-rotterdam%>%group_by(death)

mano_tema<-theme(
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text.y = element_text(size = 12),
  title = element_text(size=12),
  plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), 
  axis.text = element_text(colour='black'))

age<-ggplot(grupuoti,
            aes(x=death, y=age))+
  geom_boxplot()+ylim(c(23, 100))+
  ggtitle("Staèiakampës pacientø amþiaus diagramos pagal iðgyvenamumà") +
  labs(subtitle = "Paþymëti kvantiliai")+
  scale_x_discrete(labels=c("0"="Iðgyveno","1"="Mirë")) + mano_tema+
  xlab("Iðgyvenamumo stadija") + ylab("Amþius")+
  stat_summary(fun = "quantile", size = 4,
               geom = "text", aes(label = round(after_stat(y),0)),
               position = position_nudge(x = 0.5))
age

pgr <-ggplot(grupuoti,
             aes(x=death, y=pgr))+
  geom_boxplot()+ylim(c(0, 5006))+
  ggtitle("Staèiakampës pacientø progesterono receptoriø diagramos pagal iðgyvenamumà") +
  labs(subtitle = "Paþymëtos medianos ir maksimalios reikðmës")+
  scale_x_discrete(labels=c("0"="Iðgyveno","1"="Mirë")) + mano_tema+
  xlab("Iðgyvenamumo stadija") + ylab("Progesterono receptoriai (fmol/l)")+
  stat_summary(fun = "median", size = 4,
               geom = "text", aes(label = round(after_stat(y),0)),
               position = position_nudge(x = 0.5))+
  stat_summary(fun = "max", size = 4,
               geom = "text", aes(label = round(after_stat(y),0)),
               position = position_nudge(x = 0.5))

pgr

er <-ggplot(grupuoti,
            aes(x=death, y=er))+
  geom_boxplot()+ylim(c(0, 3276))+
  ggtitle("Staèiakampës pacientø estrogenø receptoriø diagramos pagal iðgyvenamumà") +
  labs(subtitle = "Paþymëtos medianos ir maksimalios reikðmës")+
  scale_x_discrete(labels=c("0"="Iðgyveno","1"="Mirë")) + mano_tema+
  xlab("Iðgyvenamumo stadija") + ylab("Estrogenø receptoriai (fmol/l)")+
  stat_summary(fun = "median", size = 4,
               geom = "text", aes(label = round(after_stat(y),0)),
               position = position_nudge(x = 0.5))+
  stat_summary(fun = "max", size = 4,
               geom = "text", aes(label = round(after_stat(y),0)),
               position = position_nudge(x = 0.5))

er

grid.arrange(age, pgr, er, ncol=2)

# IMCIU PADALINIMAS -------------------------------------------------------

set.seed(42)  # good idea to set the random seed for reproduciprotity
s<-stratified(duomenys_tiriamasis, c('death'), 0.8,
              bothSets = TRUE)#paskirstom ,atzvelgiant kad butu abieju grupiu rezultatu
train_tiriamasis<-s$SAMP1
test_tiriamasis<-s$SAMP2#irasome duomenis

table(train_tiriamasis$death)
table(test_tiriamasis$death)

#iraseme i excel rezultatus

list_of_datasets <- list("train" = train_tiriamasis, "test" = test_tiriamasis)
#write.xlsx(list_of_datasets, file = "C:\\Users\\ugneo\\OneDrive\\Stalinis kompiuteris\\DuomenÅ³ mokslas\\6 semestras\\Projektinis darbas\\padalinta_tiriamasis.xlsx")
#write.csv(train_tiriamasis, "C:\\Users\\ugneo\\OneDrive\\Stalinis kompiuteris\\DuomenÅ³ mokslas\\6 semestras\\Projektinis darbas\\padalinta_train_tiriamasis.csv", row.names=T)
#write.csv(test_tiriamasis, "C:\\Users\\ugneo\\OneDrive\\Stalinis kompiuteris\\DuomenÅ³ mokslas\\6 semestras\\Projektinis darbas\\padalinta_test_tiriamasis.csv", row.names=T)

train_tiriamasis<-read_excel("C:\\Users\\ugneo\\OneDrive\\Stalinis kompiuteris\\DuomenÅ³ mokslas\\6 semestras\\Projektinis darbas\\padalinta_tiriamasis.xlsx",sheet = "train") 
test_tiriamasis<-read_excel("C:\\Users\\ugneo\\OneDrive\\Stalinis kompiuteris\\DuomenÅ³ mokslas\\6 semestras\\Projektinis darbas\\padalinta_tiriamasis.xlsx",sheet = "test")
# . -----------------------------------------------------------------------

train_tiriamasis$meno<-as.factor(train_tiriamasis$meno)
train_tiriamasis$size<-as.factor(train_tiriamasis$size)
train_tiriamasis$grade<-as.factor(train_tiriamasis$grade)
train_tiriamasis$hormon<-as.factor(train_tiriamasis$hormon)
train_tiriamasis$chemo<-as.factor(train_tiriamasis$chemo)
train_tiriamasis$death<-as.factor(train_tiriamasis$death)
str(train_tiriamasis)

test_tiriamasis$meno<-as.factor(test_tiriamasis$meno)
test_tiriamasis$size<-as.factor(test_tiriamasis$size)
test_tiriamasis$grade<-as.factor(test_tiriamasis$grade)
test_tiriamasis$hormon<-as.factor(test_tiriamasis$hormon)
test_tiriamasis$chemo<-as.factor(test_tiriamasis$chemo)
test_tiriamasis$death<-as.factor(test_tiriamasis$death)

train_tiriamasis<-train_tiriamasis[,c(-12,-13)]
test_tiriamasis<-test_tiriamasis[,c(-12,-13)]

# PERSIKODUOJAME ----------------------------------------------------------

train_tiriamasis$nodes.cat<-as.factor(ifelse(train_tiriamasis$nodes >= 5, 1,0))
test_tiriamasis$nodes.cat<-as.factor(ifelse(test_tiriamasis$nodes >= 5, 1,0))

table(limf = train_tiriamasis$nodes.cat, mirtis = train_tiriamasis$death)
table(limf = test_tiriamasis$nodes.cat, mirtis =test_tiriamasis$death)


# UNIV koksas--------------------------------------------------------------------


# MODELIU SUKURIMAS -------------------------------------------------------
cox_univ_metai_tiriamasis<-coxph(Surv(time = dtime ,event = death == 1 )~ year,
                                 data =  train_tiriamasis)#reiksmingas
summary(cox_univ_metai_tiriamasis)
cox.zph(cox_univ_metai_tiriamasis)

cox_univ_age_tiriamasis<-coxph(Surv(time = dtime ,event = death == 1 )~ age,
                               data =  train_tiriamasis)#reiksmingas
summary(cox_univ_age_tiriamasis)
cox.zph(cox_univ_age_tiriamasis)

cox_univ_meno_tiriamasis<-coxph(Surv(time = dtime ,event = death == 1 )~ meno,
                                data =  train_tiriamasis)#reiksmingas
summary(cox_univ_meno_tiriamasis)
cox.zph(cox_univ_meno_tiriamasis)

cox_univ_size_tiriamasis<-coxph(Surv(time = dtime ,event = death == 1 )~ size,
                                data =  train_tiriamasis)#reiksmingas
summary(cox_univ_size_tiriamasis)
cox.zph(cox_univ_size_tiriamasis)

cox_univ_grade_tiriamasis<-coxph(Surv(time = dtime ,event = death == 1 )~ grade,
                                 data =  train_tiriamasis) #reiksmingas
summary(cox_univ_grade_tiriamasis)
cox.zph(cox_univ_grade_tiriamasis)

cox_univ_pgr_tiriamasis<-coxph(Surv(time = dtime ,event = death == 1 )~ pgr,
                               data =  train_tiriamasis) #reiksmingas
summary(cox_univ_pgr_tiriamasis)
cox.zph(cox_univ_pgr_tiriamasis)

cox_univ_er_tiriamasis<-coxph(Surv(time = dtime ,event = death == 1 )~ er,
                              data =  train_tiriamasis)#nereiksmingas

cox_univ_hormon_tiriamasis<-coxph(Surv(time = dtime ,event = death == 1 )~ hormon,
                                  data =  train_tiriamasis)#reiksmingas
summary(cox_univ_hormon_tiriamasis)
cox.zph(cox_univ_hormon_tiriamasis)

cox_univ_chemo_tiriamasis<-coxph(Surv(time = dtime ,event = death == 1 )~ chemo,
                                 data =  train_tiriamasis)#nereiksmingas

cox_univ_nodes_tiriamasis<-coxph(Surv(time = dtime ,event = death == 1 )~ nodes.cat,
                                 data =  train_tiriamasis)#reiksmingas
summary(cox_univ_nodes_tiriamasis)
cox.zph(cox_univ_nodes_tiriamasis)
# dfbetos -----------------------------------------------------------------

hold_tiriamasis<-2/sqrt(nrow(train_tiriamasis))

dfbeta_metai<-residuals(cox_univ_metai_tiriamasis, type="dfbeta")
dfbeta_amzius<-residuals(cox_univ_age_tiriamasis, type="dfbeta")
dfbeta_meno<-residuals(cox_univ_meno_tiriamasis, type="dfbeta")
dfbeta_size<-residuals(cox_univ_size_tiriamasis, type="dfbeta")
dfbeta_grade<-residuals(cox_univ_grade_tiriamasis, type="dfbeta")
dfbeta_pgr<-residuals(cox_univ_pgr_tiriamasis, type="dfbeta")
dfbeta_er<-residuals(cox_univ_er_tiriamasis, type="dfbeta")
dfbeta_hormon<-residuals(cox_univ_hormon_tiriamasis, type="dfbeta")
dfbeta_chemo<-residuals(cox_univ_chemo_tiriamasis, type="dfbeta")
dfbeta_nodes<-residuals(cox_univ_nodes_tiriamasis, type="dfbeta")

vardai<-c("Metai", "AmÅ¾ius", "MenopauzÄ—","Naviko dydis", 
          "Diferenciacijos laipsnis", "Limfmazgiai", 
          "Progesterono receptoriai","EstrogenÅ³ receptoriai",
          "Hormoninis gydymas","Chemoterapija")
dfbeta<-data.frame(dfbeta_metai, dfbeta_amzius, dfbeta_meno, dfbeta_size, dfbeta_grade,
                   dfbeta_nodes, dfbeta_pgr, dfbeta_er,  dfbeta_hormon, dfbeta_chemo)
par(mfrow=c(2, 2))
for (j in 1:10) {
  plot(dfbeta[, j],
       ylab=vardai[j], xlab="")
  title(xlab="Indeksas")
  abline(h=0, lty=2)
  abline(h = hold_tiriamasis, lty = 2, col="red")
  abline(h = -hold_tiriamasis, lty=2, col="red")
}#nera isskirciu

# TIKRINAME TIESISKUMA kiekybiniams----------------------------------------------------

# year ----------------------------------------------------------------

res_univ_metai_tiriamasis <- residuals(cox_univ_metai_tiriamasis, type="martingale")
plot(train_tiriamasis$year, res_univ_metai_tiriamasis, xlab="Metai kada atlikta operacija (m.)",
     ylab="Martingaliosios liekanos")
abline(h=0, lty=2)
lines(lowess(train_tiriamasis$year, res_univ_metai_tiriamasis, iter=0), col="red")
b <- coef(cox_univ_metai_tiriamasis) # regression coefficients
plot(train_tiriamasis$year, b*train_tiriamasis$year + res_univ_metai_tiriamasis, xlab="Metai kada atlikta operacija (m.)",
     ylab="KomponentÄ— + martingaliosios liekanos")
abline(lm(b*train_tiriamasis$year+ res_univ_metai_tiriamasis ~ train_tiriamasis$year), lty=2)
lines(lowess(train_tiriamasis$year, b*train_tiriamasis$year + res_univ_metai_tiriamasis, iter=0), col="red")#tenkina,

cox.zph(cox_univ_metai_tiriamasis)#tenkina ir reiksmingaas


# pgr ---------------------------------------------------------------------


res_univ_pgr_tiriamasis <- residuals(cox_univ_pgr_tiriamasis, type="martingale")
plot(train_tiriamasis$pgr, res_univ_pgr_tiriamasis, xlab="Progesterono receptoriai (fmol/l)",
     ylab="Martingaliosios liekanos")
abline(h=0, lty=2, col="red")
lines(lowess(train_tiriamasis$pgr, res_univ_pgr_tiriamasis, iter=0))
b <- coef(cox_univ_pgr_tiriamasis) # regression coefficients
plot(train_tiriamasis$pgr, b*train_tiriamasis$pgr + res_univ_pgr_tiriamasis, xlab="Progesterono receptoriai (fmol/l)",
     ylab="KomponentÄ— + martingaliosios liekanos")
abline(lm(b*train_tiriamasis$pgr+ res_univ_pgr_tiriamasis ~ train_tiriamasis$pgr), lty=2, col="red")
lines(lowess(train_tiriamasis$pgr, b*train_tiriamasis$pgr + res_univ_pgr_tiriamasis, iter=0))#tenkina,

# er ----------------------------------------------------------------


cox_univ_er_tiriamasis<-coxph(Surv(time = dtime ,event = death == 1 )~ train_tiriamasis$er,
                              data =  train_tiriamasis)
res_univ_er_tiriamasis <- residuals(cox_univ_er_tiriamasis, type="martingale")
plot(train_tiriamasis$er, res_univ_er_tiriamasis, xlab="EstrogenÅ³ receptoriai (fmol/l)",
     ylab="Martingaliosios liekanos")
abline(h=0, lty=2, col="red")
lines(lowess(train_tiriamasis$er, res_univ_er_tiriamasis, iter=0))
b <- coef(cox_univ_er_tiriamasis) # regression coefficients
plot(train_tiriamasis$er, b*train_tiriamasis$er + res_univ_er_tiriamasis, xlab="EstrogenÅ³ receptoriai (fmol/l)",
     ylab="KomponentÄ— + martingaliosios liekanos")
abline(lm(b*train_tiriamasis$er+ res_univ_er_tiriamasis ~ train_tiriamasis$er), lty=2, col="red")
lines(lowess(train_tiriamasis$er, b*train_tiriamasis$er + res_univ_er_tiriamasis, iter=0))#tenkina,

# age ----------------------------------------------------------------

res_univ_age_tiriamasis <- residuals(cox_univ_age_tiriamasis, type="martingale")
plot( (train_tiriamasis$age), res_univ_age_tiriamasis, xlab="AmÅ¾ius kada atlikta operacija (m.)",
      ylab="Martingaliosios liekanos")
abline(h=0, lty=2, col="red")
lines(lowess( (train_tiriamasis$age), res_univ_age_tiriamasis, iter=0))
b <- coef(cox_univ_age_tiriamasis) # regression coefficients
plot( (train_tiriamasis$age), b* (train_tiriamasis$age) + res_univ_age_tiriamasis, xlab="AmÅ¾ius kada atlikta operacija (m.)",
      ylab="KomponentÄ— + martingaliosios liekanos")
abline(lm(b* (train_tiriamasis$age)+ res_univ_age_tiriamasis ~  (train_tiriamasis$age)), lty=2, col="red")
lines(lowess( (train_tiriamasis$age), b* (train_tiriamasis$age) + res_univ_age_tiriamasis, iter=0))
cox.zph(cox_univ_age_tiriamasis)

#dirbam toliau su year meno, grade, hormon, chemo, nes jie tenkina ph, tiesiskumo ir isskirciu prielaidas

# aic ir h konkordacija ---------------------------------------------------

AIC(cox_univ_metai_tiriamasis)
AIC(cox_univ_meno_tiriamasis)
AIC(cox_univ_grade_tiriamasis)
AIC(cox_univ_hormon_tiriamasis)

cindex(Surv(time = dtime ,event = death == 1 )~ year,
       data =  test_tiriamasis)[3]
cindex(Surv(time = dtime ,event = death == 1 )~ meno,
       data =  test_tiriamasis)[3]
cindex(Surv(time = dtime ,event = death == 1 )~ grade,
       data =  test_tiriamasis)[3]
cindex(Surv(time = dtime ,event = death == 1 )~ hormon,
       data =  test_tiriamasis)[3]


# DUOMENU NUSKAITYMAS -----------------------------------------------------


df <- read.table(file = "padalinta_train_tiriamasis.csv", 
                 sep = ",", header=TRUE)

str(df)

df$meno<-as.factor(df$meno)
df$size<-as.factor(df$size)
df$grade<-as.factor(df$grade)
df$hormon<-as.factor(df$hormon)
df$chemo<-as.factor(df$chemo)
df$death<-as.factor(df$death)


# K-M KREIVES -------------------------------------------------------------

mano_tema<-theme(
  axis.title.x = element_text(size = 10),
  axis.text.x = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  axis.text.y = element_text(size = 10),
  title = element_text(size=10),
  plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), 
  axis.text = element_text(colour='black'))

# neskirstome pagal joki pozymi

KM_1 <- survfit(Surv(time = dtime / 365, 
                     event = death == 1 ) ~ 1, 
                data = df)
summary(KM_1)
print(KM_1, print.rmean = T)

g1<-ggsurvplot(KM_1,
               conf.int=TRUE, # add confidence intervals
               risk.table=c( "nrisk_cumcensor"), # show a risk table below the plot
               palette=c("dodgerblue4"),
               title="Kaplan-Meier kreiv? i?gyvenamumui", # add title to plot
               risk.table.height=.2,
               legend="none",
               legend.labs="Visi",
               xlab="Laikas (metais)",
               ylab="I?gyvenamumo tikimyb?",
               legend.title="",
               risk.table.title = "Pacien?i? skai?ius esan?i? rizikos grup?je \n(cenz?ruot? pacien?i? sukauptinis da?nis)",
               ggtheme = mano_tema,surv.median.line = "hv")  
g1

# pagal menopauze

KM_meno <- survfit(Surv(time = dtime /365, 
                        event = death == 1 ) ~ meno, 
                   data = df)
summary(KM_meno)
print(KM_meno, print.rmean = T)

g_meno<-ggsurvplot(KM_meno,
                   conf.int=TRUE, # add confidence intervals
                   risk.table=c( "nrisk_cumcensor"), # show a risk table below the plot
                   palette=c("dodgerblue4", "orchid2"),
                   title="Kaplan-Meier kreiv? i?gyvenamumui pagal menopauz?s status?", # add title to plot
                   risk.table.height=.2,
                   legend.labs=c("Prie? menopauz?", "Po menopauz?s"),
                   xlab="Laikas (metais)",
                   ylab="I?gyvenamumo tikimyb?",
                   legend.title="Menopauz?s statusas",
                   risk.table.title = "Pacien?i? skai?ius esan?i? rizikos grup?je \n(cenz?ruot? pacien?i? sukauptinis da?nis)",
                   ggtheme = mano_tema,surv.median.line = "hv")  
g_meno

logrank<-survdiff(Surv(time = dtime, 
                       event = death == 1 ) ~ meno, 
                  data = df, rho=0)


# pagal dydi

KM_size <- survfit(Surv(time = dtime / 365, 
                        event = death == 1 ) ~ size, 
                   data = df)
summary(KM_size)
print(KM_size, print.rmean = T)

g_size<-ggsurvplot(KM_size,
                   conf.int=TRUE, # add confidence intervals
                   risk.table=c( "nrisk_cumcensor"), # show a risk table below the plot
                   palette=c( "orchid2","dodgerblue4", "lightgreen"),
                   title="Kaplan-Meier kreiv? i?gyvenamumui pagal naviko dyd?", # add title to plot
                   risk.table.height=.2,
                   legend.labs=c("ma?iau lygu u? 20", "daugiau u? 50",
                                 "(20,50]"),
                   xlab="Laikas (metais)",
                   ylab="I?gyvenamumo tikimyb?",
                   legend.title="Naviko dydis",
                   risk.table.title = "Pacien?i? skai?ius esan?i? rizikos grup?je \n(cenz?ruot? pacien?i? sukauptinis da?nis)",
                   ggtheme = mano_tema, surv.median.line = "hv")  
g_size

pw<-pairwise_survdiff(Surv(time = dtime, 
                           event = death == 1 ) ~ size, 
                      data = df, rho=0,
                      p.adjust.method = "bonferroni")
pw #visi statistikai reiksmingai skiriasi

# pagal diferenciacijos laipsni

KM_grade <- survfit(Surv(time = dtime /365, 
                         event = death == 1 ) ~ grade, 
                    data = df)
summary(KM_grade)
print(KM_grade, print.rmean = T)

g_grade<-ggsurvplot(KM_grade,
                    conf.int=TRUE, # add confidence intervals
                    risk.table=c( "nrisk_cumcensor"), # show a risk table below the plot
                    palette=c("dodgerblue4", "orchid2"),
                    title="Kaplan-Meier kreiv? i?gyvenamumui pagal diferenciacijos laipsn?", # add title to plot
                    risk.table.height=.2,
                    legend.labs=c("2", "3"),
                    xlab="Laikas (metais)",
                    ylab="I?gyvenamumo tikimyb?",
                    legend.title="Diferenciacijos laipsnis",
                    risk.table.title = "Pacien?i? skai?ius esan?i? rizikos grup?je \n(cenz?ruot? pacien?i? sukauptinis da?nis)",
                    ggtheme = mano_tema,surv.median.line = "hv")  
g_grade

logrank<-survdiff(Surv(time = dtime, 
                       event = death == 1 ) ~ grade, 
                  data = df, rho=0)

# pagal hormonini gydyma

KM_hormon <- survfit(Surv(time = dtime / 365, 
                          event = death == 1 ) ~ hormon, 
                     data = df)
summary(KM_hormon)
print(KM_hormon, print.rmean = T)

g_hormon<-ggsurvplot(KM_hormon,
                     conf.int=TRUE, # add confidence intervals
                     risk.table=c( "nrisk_cumcensor"), # show a risk table below the plot
                     palette=c("dodgerblue4", "orchid2"),
                     title="Kaplan-Meier kreiv? i?gyvenamumui pagal hormonin? gydym?", # add title to plot
                     risk.table.height=.2,
                     legend.labs=c("N?ra", "Yra"),
                     xlab="Laikas (metais)",
                     ylab="I?gyvenamumo tikimyb?",
                     legend.title="Hormoninis gydymas",
                     risk.table.title = "Pacien?i? skai?ius esan?i? rizikos grup?je \n(cenz?ruot? pacien?i? sukauptinis da?nis)",
                     ggtheme = mano_tema,surv.median.line = "hv")  
g_hormon

logrank<-survdiff(Surv(time = dtime, 
                       event = death == 1 ) ~ hormon, 
                  data = df, rho=0)

# pagal chemoterapija

KM_chemo <- survfit(Surv(time = dtime / 365, 
                         event = death == 1 ) ~ chemo, 
                    data = df)
summary(KM_chemo)
print(KM_chemo, print.rmean = T)

g_chemo<-ggsurvplot(KM_chemo,
                    conf.int=TRUE, # add confidence intervals
                    risk.table=c( "nrisk_cumcensor"), # show a risk table below the plot
                    palette=c("dodgerblue4", "orchid2"),
                    title="Kaplan-Meier kreiv? i?gyvenamumui pagal chemoterapijos taikym?", # add title to plot
                    risk.table.height=.2,
                    legend.labs=c("Netaikyta", "Taikyta"),
                    xlab="Laikas (metais)",
                    ylab="I?gyvenamumo tikimyb?",
                    legend.title="Chemoterapija",
                    risk.table.title = "Pacien?i? skai?ius esan?i? rizikos grup?je \n(cenz?ruot? pacien?i? sukauptinis da?nis)",
                    ggtheme = mano_tema,surv.median.line = "hv")  
g_chemo

# pagal nodes

nodes <- survfit(Surv(time = dtime / 365 ,event = death == 1 ) ~ nodes.cat, 
                 data = train_tiriamasis)
summary(nodes)
print(nodes, print.rmean = T)


nodes_km<-ggsurvplot(nodes,
                     conf.int=TRUE, # add confidence intervals
                     risk.table=c( "nrisk_cumcensor"), # show a risk table below the plot
                     palette=c("dodgerblue4", "orchid2"),
                     title="Kaplan-Meier kreivÄ— iÅ¡gyvenamumui pagal teigiamÅ³ limfmazgiÅ³ skaiÄiÅ³", # add title to plot
                     risk.table.height=.3,
                     legend.labs=c("Iki 5 imtinai", "Daugiau uÅ¾ 5"),
                     xlab="Laikas (metais)",
                     ylab="IÅ¡gyvenamumo tikimybÄ—",
                     legend.title="",
                     risk.table.title = "PacientÅ³ skaiÄius esanÄiÅ³ rizikos grupÄ—je \n(cenzÅ«ruotÅ³ pacientÅ³ sukauptinis daÅ¾nis)",
                     ggtheme = mano_tema,surv.median.line = "hv")  
nodes_km

logrank_nodes<-survdiff(Surv(time = dtime, 
                             event = death == 1 ) ~ nodes.cat, 
                        data = train_tiriamasis, rho=0)


# multi koksas ------------------------------------------------------------


# pirminis modelis --------------------------------------------------------


cox_multi_tiriamasis<-coxph(Surv(time = dtime ,event = death == 1 )~ .- pid-nodes-recur-rtime,
                            data =  train_tiriamasis)

# vif ---------------------------------------------------------------------


vif(cox_multi_reiksmingas_tiriamasis)#nera

# summary -----------------------------------------------------------------

summary(cox_multi_tiriamasis)

# pr prielaida ------------------------------------------------------------

cox.zph(cox_multi_tiriamasis)#netenkinama 

# pazingsnine -------------------------------------------------------------

stepAIC(cox_multi_tiriamasis, method="both")

po_pazingsnines_multi<-coxph(Surv(time = dtime ,event = death == 1 )~ year+age+size+grade+pgr+nodes.cat,
                             data =  train_tiriamasis)


# NETIESISKUMAS -----------------------------------------------------------

par(mfrow=c(2, 2))
res <- residuals(cox_final, type="martingale")
X <- as.matrix(df[, c("year", "age")]) 
for (j in 1:2) {
  plot(X[, j], res, xlab=c("year", "age")[j],
       ylab="residuals")
  abline(h=0, lty=2)
  lines(lowess(X[, j], res, iter=0))
}
b <- coef(cox_final)[c(1,2)] # regression coefficients
for (j in 1:2) { # component-plus-residual plots
  plot(X[, j], b[j]*X[, j] + res, xlab=c("year", "age")[j],
       ylab="component+residual")
  abline(lm(b[j]*X[, j] + res ~ X[, j]), lty=2)
  lines(lowess(X[, j], b[j]*X[, j] + res, iter=0))
}
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
res_prot <- residuals(cox_final, type="martingale")
plot(df$pgr, res_prot, xlab="pgr", ylab="residuals")
abline(h=0, lty=2)
lines(lowess(df$pgr, res_prot, iter=0))
b <- coef(cox_final)[7] # regression coefficients
plot(df$pgr, b*df$pgr + res_prot, xlab="pgr", ylab="component+residual")
abline(lm(b*df$pgr+ res_prot ~ df$pgr), lty=2)
lines(lowess(df$pgr, b*df$pgr + res_prot, iter=0))



# pr po pazingsnines ------------------------------------------------------

cox.zph(po_pazingsnines_multi)#netenkina

po_pazingsnines_multi_size<-coxph(Surv(time = dtime ,event = death == 1 )~ year+age+strata(size)+grade+pgr+nodes.cat,
                                  data =  train_tiriamasis)
cox.zph(po_pazingsnines_multi_size)#netenkina

train_tiriamasis$age.cat <- car::recode(train_tiriamasis$age, " lo:30=1;
31:50=2; 51:70=3; 71:hi=4 ")

table(train_tiriamasis$age.cat)

po_pazingsnines_multi_size_age<-coxph(Surv(time = dtime ,event = death == 1 )~ year+strata(age.cat)+strata(size)+grade+pgr+nodes.cat,
                                      data =  train_tiriamasis)
cox.zph(po_pazingsnines_multi_size_age)#netenkina

table(train_tiriamasis$pgr)
train_tiriamasis$pgr.cat <- car::recode(train_tiriamasis$pgr, " lo:100=1; 101:hi=2 ")
table(train_tiriamasis$pgr.cat)

po_pazingsnines_multi_size_age_pgr<-coxph(Surv(time = dtime ,event = death == 1 )~ year+strata(age.cat)+strata(size)+grade+strata(pgr.cat)+nodes.cat,
                                          data =  train_tiriamasis)
cox.zph(po_pazingsnines_multi_size_age_pgr)#tenkina

po_pazingsnines_multi_size_age_pgr#galutinis


# testinei persikoduojame -------------------------------------------------

test_tiriamasis$age.cat <- car::recode(test_tiriamasis$age,  " lo:30=1;
31:50=2; 51:70=3; 71:hi=4 ")
test_tiriamasis$pgr.cat <- car::recode(test_tiriamasis$pgr, " lo:100=1; 101:hi=2 ")


# iverciai ----------------------------------------------------------------
AIC(po_pazingsnines_multi_size_age_pgr)#8796.672
cindex(Surv(time = dtime ,event = death == 1 )~ year+strata(age.cat)+strata(size)+grade+strata(pgr.cat)+nodes.cat,
       data =  test_tiriamasis)#0.6442


# BINARINIO ATSAKO MODELIS ------------------------------------------------
# DUOMENYS ----------------------------------------------------------------


df_train<-read_xlsx("/Users/simonagelzinyte/Desktop/6 semestras/RegresineÌ‡ analizeÌ‡/train_cancer.xlsx")
df_train<-df_train[,-12:-13]
df_train<-df_train[,-1]
df_train$nodes<-as.factor(ifelse(df_train$nodes >= 5, 0,1))

df_train$meno<-as.factor(df_train$meno)
df_train$size<-as.factor(df_train$size)
df_train$grade<-as.factor(df_train$grade)
df_train$hormon<-as.factor(df_train$hormon)
df_train$chemo<-as.factor(df_train$chemo)
df_train$death<-as.factor(df_train$death)

model <- glm(death ~ ., data = df_train, family = "binomial"(link='logit'))
summary(model)

# PRIELAIDU TIKRINIMAS ----------------------------------------------------
# MULTIKOLINERUMAS --------------------------------------------------------

vif(model)
#nera multikolinerumo problemos

# ISSKIRTYS ---------------------------------------------------------------
#Kuko matas
plot(model,4)
#matome is kuko grafiko, jog visos reiksmes < 1, reiskias isskirciu neturime

#rstudent
res.std <- rstandard(model)  
plot(res.std, ylab="Standardized Residual", ylim=c(-3.5,3.5))
abline(h =c(-3,0,3), lty = 2)
#kurios reiksmes outlier
which(res.std > 3)


# SUBALANSUOTUMAS ---------------------------------------------------------
summary(df_train$death)

#PAZINGSNINE -------------------------------------------------------------

stepAIC(model, direction = c("both"))
stepAIC(model, direction = c("backward"))
stepAIC(model, direction = c("forward"))
#paliekam yearyear + size + grade + nodes + er + dtime

modelis11 <- glm(formula = death ~ year + size + grade + nodes + er + dtime, 
                 family = binomial(link = "logit"), data = df_train)
summary(modelis11)

#EXP ---------------------------------------------------------------------

exp(coef(modelis11)) 

#SLENKSTIS ---------------------------------------------------------------
library(pROC)

rocobj <- roc(modelis11$y,modelis11$fitted.values)
coords(rocobj, "best", best.method="youden") 
#gavome geriausia slenksti 0.3920327

#KLASIFIKAVIMO LENTELE --------------------------------------------------

library(QuantPsyc)
ClassLog(modelis11, df_train$death)
sensitivity_train<-788/(788+130)
sensitivity_train
specifity_train<-1238/(1238+230)
specifity_train
precision_train<-788/(788+230)
precision_train
npv_train<-1238/(1238+130)
npv_train
bendras_train <- 788+1238/(788+1238+230+130)
bendras_train


# TESTAVIMO AIBES NUSKAITYMAS ---------------------------------------------

test <- read_xlsx("/Users/simonagelzinyte/Desktop/6 semestras/RegresineÌ‡ analizeÌ‡/test_cancer.xlsx")
test<-test[,-12:-13]
test<-test[,-1]
test$nodes<-as.factor(ifelse(test$nodes >= 5, 0,1))
test$meno<-as.factor(test$meno)
test$size<-as.factor(test$size)
test$grade<-as.factor(test$grade)
test$hormon<-as.factor(test$hormon)
test$chemo<-as.factor(test$chemo)
test$death<-as.factor(test$death)

# KLASIFIKAVIMO LENTELE ---------------------------------------------------
predictTest <- predict(modelis11, type = "response", newdata = test)
table(test$death,predictTest > 0.392)
sensivity_test<-211/(211+43)
sensivity_test
specificity_test<-295/(295+47)
specificity_test
precision_test<-211/(211+47)
precision_test
npv_test<-295/(295+43)
npv_test
tkslumas<-(211+295)/(211+295+43+47)
tkslumas
f1<-(1+1)*(sensivity_test*precision_test)/(1*precision_test+sensivity_test)
f1

# PROGNOZES ---------------------------------------------------------------
prognoze <- predict(modelis11, test, type = "response")
library(pROC)
test$death<-as.factor(test$death)
ROC_lr <- roc(test$death, prognoze)
ROC_lr_auc <- auc(ROC_lr)

plot(ROC_lr, col = "green", main = "ROC for logistic regression" )
summary(modelis11)

r_kvadrat<-1-modelis11$deviance/modelis11$null.deviance
r_kvadrat




