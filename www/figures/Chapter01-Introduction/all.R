###################################################
### chunk number 1: chemstr
###################################################
data(Chem97, package = "mlmRev")


###################################################
### chunk number 2: 
###################################################
xtabs( ~ score, data = Chem97)


###################################################
### chunk number 3: 
###################################################
library("lattice")
histogram(~ gcsescore | factor(score), data = Chem97)


###################################################
### chunk number 4: Chem97histogram
###################################################
##grid.rect()
plot(trellis.last.object())


###################################################
### chunk number 5: 
###################################################
densityplot(~ gcsescore | factor(score), data = Chem97, 
            plot.points = FALSE, ref = TRUE)


###################################################
### chunk number 6: Chem97densityplot
###################################################
plot(trellis.last.object())


###################################################
### chunk number 7:  eval=FALSE
###################################################
densityplot(~ gcsescore, data = Chem97, groups = score,
            plot.points = FALSE, ref = TRUE,
            auto.key = list(columns = 3))




###################################################
### chunk number 1: initialize
###################################################
data(Chem97, package = "mlmRev")


###################################################
### chunk number 2: obj
###################################################
tp1 <- histogram(~ gcsescore | factor(score), data = Chem97)
tp2 <- 
    densityplot(~ gcsescore, data = Chem97, groups = score,
                plot.points = FALSE,
                auto.key = list(space = "right", title = "score"))


###################################################
### chunk number 3: 
###################################################
class(tp2)
summary(tp1)


###################################################
### chunk number 4:  eval=FALSE
###################################################
## print(tp1)


###################################################
### chunk number 5:  eval=FALSE
###################################################
## plot(tp1)


###################################################
### chunk number 6: comb
###################################################
plot(tp1, split = c(1, 1, 1, 2))
plot(tp2, split = c(1, 2, 1, 2), newpage = FALSE)


