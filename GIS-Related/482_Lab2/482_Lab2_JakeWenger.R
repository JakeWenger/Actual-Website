library(GISTools)
library(RColorBrewer)
library(tbart)

load("EN.RData")
setwd("Desktop/482_Lab2")
set.seed(12345)

#population density
pop.density <- (en.county@data$pop.den)

shading <- auto.shading(pop.density, n=7)
choropleth(en.county,pop.density,shading)
choro.legend(1350000, 1405900, shading, cex = 0.6, title = "Population Density")

#air pollution
air.pollution <- (en.county@data$pm25.std)

shading <- auto.shading(air.pollution, n=7,)
choropleth(en.county,air.pollution,shading)
choro.legend(1350000, 1405900, shading, cex = 0.6, title = "Air Pollution")

#p-median 1
solution.1 <- allocations(en.county, p=1)
unique(solution.1$allocation)
unique(solution.1$allocdist)

col.index <- match(solution.1$allocation,unique(solution.1$allocation))
col.alloc <- brewer.pal(1,'Paired')[col.index]
plot(solution.1,col=col.alloc)
points(coordinates(solution.1[c(284),]),col=2)

#p-median 2
solution.2 <- allocations(en.county, p=2)
unique(solution.2$allocation)
unique(solution.2$allocdist)

col.index <- match(solution.2$allocation,unique(solution.2$allocation))
col.alloc <- brewer.pal(2,'Paired')[col.index]
plot(solution.2,col=col.alloc)
points(coordinates(solution.2[c(32,253),]),col=2)

#p-median 3
solution.3 <- allocations(en.county, p=3)
unique(solution.3$allocation)
unique(solution.3$allocdist)

col.index <- match(solution.3$allocation,unique(solution.3$allocation))
col.alloc <- brewer.pal(3,'Paired')[col.index]
plot(solution.3,col=col.alloc)
points(coordinates(solution.3[c(40, 283, 178),]),col=2)

#p-median 4
solution.4 <- allocations(en.county, p=4)
unique(solution.4$allocation)
unique(solution.4$allocdist)

col.index <- match(solution.4$allocation,unique(solution.4$allocation))
col.alloc <- brewer.pal(4,'Paired')[col.index]
plot(solution.4,col=col.alloc)
points(coordinates(solution.4[c(18,17,284,178),]),col=2)

#p-median 5
solution.5 <- allocations(en.county, p=5)
unique(solution.5$allocation)
unique(solution.5$allocdist)

col.index <- match(solution.5$allocation,unique(solution.5$allocation))
col.alloc <- brewer.pal(5,'Paired')[col.index]
plot(solution.5,col=col.alloc)
points(coordinates(solution.5[c(18,49,288,245,159),]),col=2)

#p-median 6
solution.6 <- allocations(en.county, p=6)
unique(solution.6$allocation)
unique(solution.6$allocdist)

col.index <- match(solution.6$allocation,unique(solution.6$allocation))
col.alloc <- brewer.pal(6,'Paired')[col.index]
plot(solution.6,col=col.alloc)
points(coordinates(solution.6[c(14,49,226,170,184,159),]),col=2)

#p-median 7
solution.7 <- allocations(en.county, p=7)
unique(solution.7$allocation)
unique(solution.7$allocdist)

col.index <- match(solution.7$allocation,unique(solution.7$allocation))
col.alloc <- brewer.pal(7,'Paired')[col.index]
plot(solution.7,col=col.alloc)
points(coordinates(solution.7[c(14,49,225,275,88,159,124),]),col=2)

#p-median 8
solution.8 <- allocations(en.county, p=8)
unique(solution.8$allocation)
unique(solution.8$allocdist)

col.index <- match(solution.8$allocation,unique(solution.8$allocation))
col.alloc <- brewer.pal(8,'Paired')[col.index]
plot(solution,col=col.alloc)
points(coordinates(solution[c(14,49,231,277,173,159,182,104),]),col=2)

#p-median 9
solution.9 <- allocations(en.county, p=9)
unique(solution.9$allocation)
unique(solution.9$allocdist)

col.index <- match(solution.9$allocation,unique(solution.9$allocation))
col.alloc <- brewer.pal(9,'Paired')[col.index]
plot(solution.9,col=col.alloc)
points(coordinates(solution.9[c(14,49,13,276,173,146,183,130,147),]),col=2)

#p-median 10
solution.10 <- allocations(en.county, p=10)
unique(solution.10$allocation)
unique(solution.10$allocdist)

col.index <- match(solution.10$allocation,unique(solution.10$allocation))
col.alloc <- brewer.pal(10,'Paired')[col.index]
plot(solution.10,col=col.alloc)
points(coordinates(solution.10[c(14,49,272,269,83,178,185,220,195,140),]),col=2)

#p-median 11
solution.11 <- allocations(en.county, p=11)
unique(solution.11$allocation)
unique(solution.11$allocdist)

col.index <- match(solution.11$allocation,unique(solution.11$allocation))
col.alloc <- brewer.pal(11,'Paired')[col.index]
plot(solution.11,col=col.alloc)
points(coordinates(solution.11[c(14,49,13,76,210,173,195,184,281,104,140),]),col=2)

#p-median 12
solution.12 <- allocations(en.county, p=12)
unique(solution.12$allocation)
unique(solution.12$allocdist)

col.index <- match(solution.12$allocation,unique(solution.12$allocation))
col.alloc <- brewer.pal(12,'Paired')[col.index]
plot(solution.12,col=col.alloc)
points(coordinates(solution.12[c(14,49,13,76,211,83,178,184,283,104,195,140),]),col=2)

#p-median 13
solution.13 <- allocations(en.county, p=13)
unique(solution.13$allocation)
unique(solution.13$allocdist)

col.index <- match(solution.13$allocation,unique(solution.13$allocation))
col.alloc <- brewer.pal(13,'Paired')[col.index]
plot(solution.13,col=col.alloc)
points(coordinates(solution.13[c(14,49,13,188,76,210,156,149,125,281,119,195,147),]),col=2)

#p-median 14
solution.14 <- allocations(en.county, p=14)
unique(solution.14$allocation)
unique(solution.14$allocdist)

col.index <- match(solution.14$allocation,unique(solution.14$allocation))
col.alloc <- brewer.pal(14,'Paired')[col.index]
plot(solution.14,col=col.alloc)
points(coordinates(solution.14[c(14,49,13,188,76,210,156,181,125,281,119,195,147,241),]),col=2)

#p-median 15
solution.15 <- allocations(en.county, p=15)
unique(solution.15$allocation)
unique(solution.15$allocdist)

col.index <- match(solution.15$allocation,unique(solution.15$allocation))
col.alloc <- brewer.pal(15,'Paired')[col.index]
plot(solution.15,col=col.alloc)
points(coordinates(solution.15[c(14,49,13,188,76,211,174,260,181,125,287,119,195,147,241),]),col=2)

#Q3

#weighted distance
w1 <- sum(solution.1$allocdist)
w2 <- sum(solution.2$allocdist)
w3 <- sum(solution.3$allocdist)
w4 <- sum(solution.4$allocdist)
w5 <- sum(solution.5$allocdist)
w6 <- sum(solution.6$allocdist)
w7 <- sum(solution.7$allocdist)
w8 <- sum(solution.8$allocdist)
w9 <- sum(solution.9$allocdist)
w10 <- sum(solution.10$allocdist)

#graph
x <- c(1:10)
y <- c(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10)
plot(x, y, xlab="P-Values", ylab="Weighted Distance", main="Weighted Distance vs. P-Values", type="b", lwd=1)

#Making the star diagram
col.index <- match(solution.6$allocation,unique(solution.6$allocation))
col.alloc <- brewer.pal(6,'Paired')[col.index]
plot(solution.6,col=col.alloc)
points(coordinates(solution.6[c(14,49,226,170,184,159),]),col=2)
plot(star.diagram(solution.6),col='blue',lwd=1,add=TRUE)

#Q4
fixed.5 <- allocations(en.county, p=5, force=c(1,100))
unique(fixed.5$allocation)
uniqunue(fixed.5$allocdist)

col.index <- match(fixed.5$allocation,unique(fixed.5$allocation))
col.alloc <- brewer.pal(5,'Paired')[col.index]
plot(fixed.5,col=col.alloc)
points(coordinates(fixed.5[c(1,17,100,284,178),]),col=2)

