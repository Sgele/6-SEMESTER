library("survival")
library("survminer")
library(ggfortify)
library(dplyr)
library(car)


df <- survival::veteran
View(df)

df$trt <- as.factor(df$trt)
df$prior <- as.factor(df$prior)

View(df)

#Pritaikome Cox modeli
cox <- coxph(Surv(time, status) ~ trt + celltype + karno + diagtime +
               age + prior, data = df)
summary(cox)

#Prielaidos tikrinimas
cox.zph(cox)

ggcoxdiagnostics(cox, type = "schoenfeld")

#Itraukiame laiko saveika
veteran_long <- survSplit(Surv(time, status)~ trt + celltype + karno + diagtime +
                            age + prior, 
                          data = df, 
                          id = "id",
                          cut = unique(df$time)) %>%
  mutate(stop = log(time + 20))

cph_long <- coxph(formula = Surv(tstart, time, status)~
                    trt + celltype + celltype:stop +prior + karno + karno:stop + diagtime + age, 
                  data = veteran_long)

#isskirtys
ggcoxdiagnostics(cox, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

#netiesiškumas
ggcoxfunctional(Surv(time, status) ~ diagtime + age, data = df)

#naujas modelis
km_trt_fit <- survfit(Surv(tstart, time, status) ~ trt, data=veteran_long)
autoplot(km_trt_fit)

summary(cph_long)
