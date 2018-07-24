# Preston Curve Exploratory analysis example
# using mainly boxplot in R
# by Marcello Del Bono
# https://marcellodelbono.it/
# data from gapminder


#library(openintro)
library(tidyverse)
library(magrittr)
library(gapminder)
pfill <- "skyblue4"
#--- Gapminder Data Exploration----


# first look
summary(gapminder)
table(gapminder$year)
# Data is collected every 5 years
# from 1952 to 2007

# 2007 only data
gapminder2007<-gapminder %>% 
  select(lifeExp, gdpPercap, year) %>%
  filter(year==2007)


plot1<-gapminder %>% 
  ggplot(aes(continent, lifeExp))+
  geom_boxplot()
plot1


#checking outliers
test<-gapminder %>% 
  select(lifeExp, gdpPercap, year, continent, country) %>%
  filter(continent=="Europe" & lifeExp<=65)
test
  
plot1_1<-test %>% 
  ggplot(aes(x=0,lifeExp))+
  geom_boxplot()+
  facet_grid(cols = vars(country))
plot1_1
plot1_1+
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
# Europe outliers are mainly Turkey, Albania, Serbia, Bosnia Herzegovina, Montenegro


# checking turkey
test<-gapminder %>% 
  select(lifeExp, gdpPercap, year, continent, country) %>%
  filter(country=="Turkey" )

plot1_1<-test %>% 
  ggplot(aes(x=0,lifeExp))+
  geom_boxplot()+
  facet_grid(cols = vars(year))+
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
plot1_1



# Coloring outliers
plot2<-gapminder %>% 
  ggplot(aes(continent,  gdpPercap))+
  geom_boxplot(outlier.colour="orange")
plot2

# Rotate the box plot
plot3<-plot2 + coord_flip()
plot3

gapminder2007<-gapminder %>% 
  select(lifeExp, gdpPercap, year) %>%
  filter(year==2007)

# Preston Curve
fig_1<-ggplot(gapminder2007,aes(gdpPercap, lifeExp))+
  geom_point(color=pfill, alpha = 0.1)+
  labs(x="income per person (GDP/capita, PPP$, inflation adjusted)")+
  labs(y="life expectancy (years)")+
  geom_smooth(linetype="dashed")+
  theme_light()+
  labs(caption = "Preston Curve 2007")+
  theme(plot.caption = element_text(hjust = 0.5))
fig_1
