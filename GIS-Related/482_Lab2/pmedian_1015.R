# calling libraries 
library(GISTools)
library(RColorBrewer)
library(tbart)

data("georgia")
med.income <- (georgia@data$MedInc)

## mapping
shading <- auto.shading(med.income, n=7)
choropleth(georgia,med.income,shading)
choro.legend(1350000, 1405900, shading, cex = 0.6, title = "Median Income")
# change the color scheme to green
shading.green <- auto.shading(med.income, n=7)
choropleth(georgia,med.income,shading.green)
choro.legend(1350000, 1405900, shading.green, cex = 0.6, title = "Median Income")

## p-median problems
solution.5 <- allocations(georgia, p=5)
unique(solution.5$allocation)
unique(solution.5$allocdist)

col.index <- match(solution.5$allocation,unique(solution.5$allocation))
col.alloc <- brewer.pal(5,'Accent')[col.index]
plot(solution.5,col=col.alloc)
points(coordinates(solution.5[c(54,2,129,66,28),]),col=2)
