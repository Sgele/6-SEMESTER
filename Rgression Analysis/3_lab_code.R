library(ggplot2)
library("quantreg")
library(readxl)
library(dplyr)
library(writexl)
library(MASS)
library(readxl)
install.packages('epiDisplay')
library(epiDisplay)


# DUOMENU APDOROJIMAS -----------------------------------------------------

df<-read.csv("/Users/simonagelzinyte/Desktop/6 semestras/Regresinė analizė/babies.csv")
str(df)
summary(df)
df <- na.omit(df)
df <- df[,-1]
df$parity <- as.factor(df$parity)
df$smoke <- as.factor(df$smoke)


# MOKYMO IR TESTAVIMO AIBES -----------------------------------------------

#sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
#train  <- df[sample, ]
#test   <- df[!sample, ]
#write_xlsx(test ,"C:\\Users\\Austejos\\Desktop\\VU\\3 kursas\\6 semestras\\Regresinė analizė\\test_data_3_lab.xlsx")
#write_xlsx(train ,"C://Users//Austejos//Desktop//VU//3 kursas//6 semestras//Regresinė analizė//train_data_3lab.xlsx")

train <- read_excel("/Users/simonagelzinyte/Desktop/6 semestras/Regresinė analizė/train_data_3lab.xlsx")

# PRADINE ANALIZE ---------------------------------------------------------

#Box plotai ir isskirtys

g1 <- ggplot(train) +
  aes(x = "", y = gestation) +
  geom_boxplot() + ggtitle("Gestation in days boxplot") + xlab("")
  
g2 <- ggplot(train) +
  aes(x = "", y = bwt) +
  geom_boxplot() + ggtitle("Birthweight in ounces boxplot") + xlab("") + ylab("birthweight")

g3 <- ggplot(train) +
  aes(x = "", y = age) +
  geom_boxplot() + ggtitle("Age boxplot") + xlab("")

g4 <- ggplot(train) +
  aes(x = "", y = height) +
  geom_boxplot() + ggtitle("Height in inches boxplot") + xlab("")

g5 <- ggplot(train) +
  aes(x = "", y = weight) +
  geom_boxplot() + ggtitle("Weight in pounds boxplot") + xlab("")

gridExtra::grid.arrange(g1, g2, g3, g4, g5)

#dazniu lenteles

tab1(train$parity, main = "First pregnancy or not, 0 = first pregnancy ")

tab1(train$smoke, main = "Whether the mother smokes")

#aprasomoji statistika
summary(train$bwt)
summary(train$gestation)
summary(train$age)
summary(train$height)
summary(train$weight)


# saveika -----------------------------------------------------------------

interaction.plot(
  x.factor = df$smoke,
  trace.factor = df$bwt,
  response = df$gestation,
  fun = median,
  lyt = 1,
)

# MODELIS -----------------------------------------------------------------

# tiesine regresija
olsreg <- lm(bwt ~ ., data=df)
summary(olsreg)

# pradinis modelis, parinktas 0.5 kvantilis
modelis <- rq(bwt ~ ., data=df, tau = 0.5)
summary(modelis)

# tinklelis
qs <- 1:9/10
qr2 <- rq(bwt ~ ., data=df, tau = qs)



# MODELIO PRIELAIDOS ------------------------------------------------------

vif(modelis)

# PAZINGSNINE REGRESIJA ---------------------------------------------------

stepAIC(modelis, direction = "both")
stepAIC(modelis, direction ="backward")
stepAIC(modelis, direction ="forward")




attach(df)
df <- na.omit(df)
parity <- as.factor(parity)
smoke <- as.factor(smoke)
Y <- bwt 
X <- cbind(gestation, parity, age, height, weight, smoke)
ggplot(df, aes(X,Y)) + geom_point() +
  geom_smooth(method="lm")

quantreg.all <- rq(Y ~ X, tau = seq(0.05, 0.95, by = 0.05), data=df)
quantreg.plot <- summary(quantreg.all)
plot(quantreg.plot)

