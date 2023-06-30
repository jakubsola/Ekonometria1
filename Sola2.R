install.packages("AICcmodavg")

library(faraway)
library(lmtest)
library(goftest)
library(MASS)
library(AICcmodavg)
setwd("D:/GitHub/Ekonometria1")
DATA = read.csv("Sola1.csv", sep = ";")
attach(DATA)
summary(DATA)

model = lm(Total~GDP+Pop+PCR+Urban+WiP+Area+Urb_area+Internet+Roads+Unemp)
summary(model)
AIC(model)
plot(model)
hist(model$residuals, breaks = 20)

#liniowosc
raintest(model)
resettest(model)
harvtest(model)

#normalnosc reszt
shapiro.test(model$residuals)
ks.test(model$residuals, "pnorm")
cvm.test(model$residuals, "pnorm")
ad.test(model$residuals)

#wspolliniowosc zmiennych
cor(DATA[,c(seq(2,12,by=1))],use = "pairwise.complete.obs")
vif(model)

#autokorelacja
dwtest(model)
bgtest(model)

#homoskedastyczność
bptest(model)
gqtest(model)
hmctest(model)

plot(model, 4)
plot(model, 5)
plot(model, 6)
DATA = DATA[-39,]
DATA = DATA[-101,]

step(model)
model2 = lm(Total ~ GDP + Pop + Internet + Roads + Unemp)
summary(model2)
vif(model2)
AIC(model)
AIC(model2)

model3 = lm(Total ~ GDP + log(Internet) + Pop + Roads + Unemp)
summary(model3)
vif(model3)
IP = Internet / Pop
model4 = lm(Total ~ GDP + IP + Roads + Unemp)
summary(model4)
vif(model4)

DATA$Total = ifelse(DATA$Total == 0, 0.001, DATA$Total)
bc = boxcox(Total ~ GDP + IP + Roads + Unemp, data = DATA)
lambda = bc$x[which.max(bc$y)]
lambda
new_model = lm((((Total)^lambda-1)/lambda) ~ GDP + IP + Roads + Unemp)
summary(new_model)
plot(new_model)
#liniowosc
raintest(new_model)
resettest(new_model)
harvtest(new_model)

model5 = lm((((Total)^lambda-1)/lambda) ~ GDP + IP + Roads + log(Unemp))
summary(model5)
vif(model5)
plot(model5)

#liniowosc
raintest(model5)
resettest(model5)
harvtest(model5)

#normalnosc reszt
shapiro.test(model5$residuals)
ks.test(model5$residuals, "pnorm")
cvm.test(model5$residuals, "pnorm")
ad.test(model5$residuals)
hist(model5$residuals, breaks = 20)

#homoskedastyczność
bptest(model5)
gqtest(model5)
hmctest(model5)

#autokorelacja
dwtest(model5)
bgtest(model5)