#import data
library(gapminder)
library(ggplot2)
library(GGally)

setwd("D:/blog/matrix-scatterplot")

attach(mtcars)
attach(gapminder)
names(gapminder)

# Basic Scatterplot Matrix
pairs(~country+pop+lifeExp+gdpPercap, data=gapminder, main="Simple Scatterplot Matrix")

# Basic Scatterplot Matrix
pairs(~continent+pop+lifeExp+gdpPercap, data=gapminder, main="Simple Scatterplot Matrix")

# Basic Scatterplot Matrix
pairs(~mpg+disp+drat+wt,data=mtcars, 
      main="Simple Scatterplot Matrix")

head(iris)
ggpairs(iris)

ggpairs(gapminder, columns=2,3,4,5, aes(colour = continent, alpha = 0.4))

ggpairs(gapminder, columns=2:5, aes(alpha = 0.4))

# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2



dev.print(png, file = "myplot.png", width = 900, height = 900)
png(filename = "baseRmatrix.png")
plot(gapminder[1,4])

dev.off()
