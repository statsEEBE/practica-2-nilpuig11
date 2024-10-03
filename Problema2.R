#Codigo para problema 2
mis_dades<-iris
x<-mis_dades$Petal.Length
y<-mis_dades$Sepal.Length
plot(x,y)
x_bar<-mean(x)
y_bar<-mean(y)
m<-sum((x-x_bar)*(y-y_bar))/sum((x-x_bar)^2) #calcul pendent
m
b<-y_bar-m*x_bar #càlcul punt de tall
b
m*1.5+b #predicció x=1.5

x_pred<-x
x_pred
y_pred<-m*x_pred+b
y_pred
lines(x_pred,y_pred) #recta de predicció
Rsq<-sum((y_pred-y_bar)^2)/sum((y-y_bar)^2) #coeficient de determinació
Rsq
cor<-sqrt(Rsq) #coeficient de correlació
cor
mod<-lm(y~x) #per calcular directament el pendent i el punt de tall

mod
summary(mod) #resum de alguna cosa calculada anteriorment(multiple R-squared=coeficient de determinació)
cor.test(x,y) #coeficient de correlació calculat directament
y_pred2<-predict(mod,data.frame(x=1.5)) #prediccions de x=1.5
y_pred2
