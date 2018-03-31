
#cluster analysis

library(readr)
SchoolPerformance <- read_csv("~/Downloads/SchoolPerformance.csv")
View(SchoolPerformance)
set.seed(98)
head(SchoolPerformance, 6)

#Graph A
sc <- cbind(SchoolPerformance$geometry, SchoolPerformance$reading, SchoolPerformance$grammar, SchoolPerformance$drawing, SchoolPerformance$calculus, SchoolPerformance$history, SchoolPerformance$writing, SchoolPerformance$spelling)
summary(sc)
comp <- princomp(sc, scores = T, cor = T)
summary(comp)
plot(comp)


#Graph B
#Drop last two variables because they are significantly lower in Proportion of Variance
sc <- cbind(SchoolPerformance$geometry, SchoolPerformance$reading, SchoolPerformance$grammar, SchoolPerformance$drawing, SchoolPerformance$calculus, SchoolPerformance$history)
summary(sc)
comp <- princomp(sc, scores = T, cor = T)
summary(comp)
plot(comp)


#Graph C
#Drop last three variables because they are significantly lower in Proportion of Variance
sc <- cbind(SchoolPerformance$geometry, SchoolPerformance$reading, SchoolPerformance$grammar)
summary(sc)
comp <- princomp(sc, scores = T, cor = T)
summary(comp)
plot(comp)

#Graph D
#for fun
screeplot(comp, type="line", main = "Scree Plot")


#Graph E
#for fun
results <- kmeans(SchoolPerformance[, 1:8],6)
plot(SchoolPerformance[c("geometry", "reading", "grammar", "drawing", "calculus", "history")], col = results$cluster)
