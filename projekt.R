setwd("D:/GitHub/Ekonometria1")
data = read.csv("DATA.csv", sep = ";")
attach(data)
head(data)
summary(data)

hist(GDP)
density(GDP)
cor(Total,GDP)
plot(GDP,Total)
model_GDP = lm(Total~GDP)
abline(model_GDP)

hist(Pop)
density(Pop)
cor(Total,Pop)
plot(Pop, Total)
model_Pop = lm(Total~Pop)
abline(model_Pop)

hist(PCR)
density(PCR)
cor(Total,PCR)
plot(PCR, Total)
model_PCR = lm(Total~PCR)
abline(model_PCR)
summary(model_PCR)

hist(Urban)
density(Urban)
cor(Total, Urban)
plot(Urban, Total)
model_Urban = lm(Total~Urban)
abline(model_Urban)

hist(WiP)
density(WiP)
cor(Total,WiP)
plot(WiP, Total)
model_WiP = lm(Total~WiP)
abline(model_WiP)
summary(model_WiP)

model = lm(Total~GDP+Pop+PCR+Urban+WiP+Area+Urb_area)
plot(model)
coef(model)
summary(model)


X = as.matrix(data.frame(GDP,Pop,PCR,Urban,WiP,Area,Urb_area))
X =cbind(rep(1, nrow(X)), X)
y = Total
b = solve(t(X) %*% X) %*% t(X) %*% y
