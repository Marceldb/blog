library(dplyr)
library(ggplot2)
library(gridExtra)

anscombe
df<-anscombe
df1<-anscombe %>% select(x1,y1)

x<-anscombe[1:4]
y<-anscombe[5:8]


p1<-df %>% ggplot(aes(x=x1, y=y1))+
  geom_point(aes(colour ="red"), size=3)+ geom_smooth(method = "lm")+ theme(legend.position="none")
p1
ggsave("p1.png")

# medie, varianza, dev standard
summarise_all(df,  mean)
summarise_all(df,  var)
summarise_all(df,  sd)

# correlazione x,y
summarise(df,  cor(x1,y1))
summarise(df,  cor(x2,y2))
summarise(df,  cor(x3,y3))
summarise(df,  cor(x4,y4))

#Fitting lines
fit1<-lm(as.matrix(x1)~as.matrix(y1), data=df)
fit2<-lm(as.matrix(x2)~as.matrix(y2), data=df)
fit3<-lm(as.matrix(x3)~as.matrix(y3), data=df)
fit4<-lm(as.matrix(x4)~as.matrix(y4), data=df)

# printing fitted coefficients
summary(fit1)$coefficients[,1]
summary(fit2)$coefficients[,1]
summary(fit3)$coefficients[,1]
summary(fit4)$coefficients[,1]



summary(fit1)

p2<-df %>% 
  ggplot(aes(x=x2, y=y2))+
  geom_point(aes(colour ="red"), size=3)+ geom_smooth(method = "lm")+ theme(legend.position="none")
p2

p3<-df %>% 
  ggplot(aes(x=x3, y=y3))+
  geom_point(aes(colour ="red"), size=3)+ geom_smooth(method = "lm")+ theme(legend.position="none")
p3

p4<-df %>% 
  ggplot(aes(x=x4, y=y4))+
  geom_point(aes(colour ="red"), size=3)+ geom_smooth(method = "lm")+ theme(legend.position="none")
p4

p5<-grid.arrange(p1,p2,p3,p4)
ggsave("p5.png")
