###################################################
### chunk number 1: 
###################################################
densityplot(~ eruptions, data = faithful)


###################################################
### chunk number 2: dens1
###################################################
plot(trellis.last.object())


###################################################
### chunk number 3: 
###################################################
densityplot(~ eruptions, data = faithful, 
            kernel = "rect", bw = 0.2, plot.points = "rug", n = 200)


###################################################
### chunk number 4: dens2
###################################################
plot(trellis.last.object())


###################################################
### chunk number 1: 
###################################################
library("latticeExtra")
data(gvhd10)
densityplot(~log(FSC.H) | Days, data = gvhd10, 
            plot.points = FALSE, ref = TRUE, layout = c(2, 4))


###################################################
### chunk number 2: gvhdDens1
###################################################
plot(trellis.last.object())


###################################################
### chunk number 3: 
###################################################
histogram(~log2(FSC.H) | Days, gvhd10, xlab = "log Forward Scatter",
          type = "density", nint = 50, layout = c(2, 4))


###################################################
### chunk number 4: gvhdHist1
###################################################
plot(trellis.last.object())


###################################################
### chunk number 1: 
###################################################
data(Chem97, package = "mlmRev")


###################################################
### chunk number 2: 
###################################################
qqmath(~ gcsescore | factor(score), data = Chem97, 
       f.value = ppoints(100))


###################################################
### chunk number 3: Chem97qqmath
###################################################
plot(trellis.last.object())


###################################################
### chunk number 4: 
###################################################
qqmath(~ gcsescore | gender, Chem97, groups = score, aspect = "xy", 
       f.value = ppoints(100), auto.key = list(space = "right"),
       xlab = "Standard Normal Quantiles", 
       ylab = "Average GCSE Score")

## qqmath(~ gcsescore | score, data = Chem97, 
##        f.value = function(n) ppoints(100), aspect = "xy",
##        auto.key = list(space = "right", lines = FALSE))

##


###################################################
### chunk number 5: Chem97qqmathGrpBanked
###################################################
plot(trellis.last.object())


###################################################
### chunk number 6: Chem97qqmathGrpColor
###################################################
plot(trellis.last.object(par.settings = canonical.theme("pdf")))


###################################################
### chunk number 7:  eval=FALSE
###################################################
## library("MASS")
## Chem97.pos <- subset(Chem97, gcsescore > 0)
## with(Chem97.pos, 
##      boxcox(gcsescore ~ score * gender, lambda = seq(0, 4, 1/10)))
## ## 


###################################################
### chunk number 8: 
###################################################
Chem97.mod <- transform(Chem97, gcsescore.trans = gcsescore^2.34)
qqmath(~ gcsescore.trans | gender, Chem97.mod, groups = score,
       f.value = ppoints(100), aspect = "xy",
       auto.key = list(space = "right", title = "score"), 
       xlab = "Standard Normal Quantiles", 
       ylab = "Transformed GCSE Score")



###################################################
### chunk number 9: qqboxcox
###################################################
plot(trellis.last.object())


###################################################
### chunk number 10: 
###################################################
library("latticeExtra")
ecdfplot(~ gcsescore | factor(score), data = Chem97, 
         groups = gender, auto.key = list(columns = 2),
         subset = gcsescore > 0, xlab = "Average GCSE Score")



###################################################
### chunk number 11: ecdf
###################################################
plot(trellis.last.object())


###################################################
### chunk number 12: 
###################################################
qqmath(~ gcsescore | factor(score), data = Chem97, groups = gender, 
       auto.key = list(points = FALSE, lines = TRUE, columns = 2),
       subset = gcsescore > 0, type = "l", distribution = qunif, 
       prepanel = prepanel.qqmathline, aspect = "xy",
       xlab = "Standard Normal Quantiles", 
       ylab = "Average GCSE Score")



###################################################
### chunk number 13: qqunif
###################################################
plot(trellis.last.object())


###################################################
### chunk number 1: 
###################################################

load("../data/human.swaI.rda")
swaI.lengths <- as.numeric(unlist(human.swaI))
p1 <- 
    qqmath(~swaI.lengths, 
           distribution = qexp, 
           f.value = ppoints(500),
           type = c('p', 'g'),
           pch = '.')
p2 <- 
    qqmath(~swaI.lengths, 
           distribution = qexp, 
           f.value = function(n) {
               seq(0, 0.95, length = round(sqrt(n)))
           },
           type = c('p', 'g'),
           pch = '.')
est.rate <- diff(qexp(c(0.25, 0.75))) / IQR(swaI.lengths) 
p3 <- 
    qqmath(~pexp(swaI.lengths, rate = est.rate),
           distribution = qunif, 
           f.value = ppoints(500),
           ylab = "Transformed swaI.lengths",
           type = c('p', 'g'),
           pch = '.')

##



###################################################
### chunk number 2: swa1
###################################################
print(p1, split = c(1, 1, 3, 1), more = TRUE)
print(p2, split = c(2, 1, 3, 1), more = TRUE)
print(p3, split = c(3, 1, 3, 1))


###################################################
### chunk number 3: 
###################################################
load("../data/human.swaI.rda")
swaI.lengths <- as.numeric(unlist(human.swaI))
length(swaI.lengths)


###################################################
### chunk number 4:  eval=FALSE
###################################################
## qqmath(~swaI.lengths, 
##        distribution = qexp, 
##        f.value = ppoints(500),
##        type = c('p', 'g'),
##        pch = '.')


###################################################
### chunk number 5:  eval=FALSE
###################################################
## qqmath(~swaI.lengths, 
##        distribution = qexp, 
##        f.value = function(n) {
##            seq(0, 0.95, length = round(sqrt(n)))
##        },
##        type = c('p', 'g'),
##        pch = '.')


###################################################
### chunk number 6:  eval=FALSE
###################################################
## est.rate <- diff(qexp(c(0.25, 0.75))) / IQR(swaI.lengths) 
## p3 <- 
##     qqmath(~pexp(swaI.lengths, rate = est.rate),
##            distribution = qunif, 
##            f.value = ppoints(500),
##            type = c('p', 'g'),
##            pch = '.')


###################################################
### chunk number 1: initialize
###################################################
data(Chem97, package = "mlmRev")


###################################################
### chunk number 2: 
###################################################
qq(gender ~ gcsescore | factor(score), Chem97, 
   f.value = ppoints(100), aspect = 1)


###################################################
### chunk number 3: chemQq1
###################################################
print(trellis.last.object())


###################################################
### chunk number 1: initialize
###################################################
data(Chem97, package = "mlmRev")
data(gvhd10, package = "latticeExtra")


###################################################
### chunk number 2: 
###################################################
bwplot(factor(score) ~ gcsescore | gender, data = Chem97, 
       xlab = "Average GCSE Score")


###################################################
### chunk number 3: bwplot1
###################################################
plot(trellis.last.object())


###################################################
### chunk number 4: 
###################################################
bwplot(gcsescore^2.34 ~ gender | factor(score), Chem97, 
       varwidth = TRUE, layout = c(6, 1),
       ylab = "Transformed GCSE score")


###################################################
### chunk number 5: bwplot2
###################################################
plot(trellis.last.object())


###################################################
### chunk number 6: 
###################################################
bwplot(Days ~ log(FSC.H), data = gvhd10, 
       xlab = "log(Forward Scatter)", ylab = "Days Past Transplant")


###################################################
### chunk number 7: gvhdBwplot
###################################################
plot(trellis.last.object())


###################################################
### chunk number 8: 
###################################################
bwplot(Days ~ log(FSC.H), gvhd10, 
       panel = panel.violin, box.ratio = 3,
       xlab = "log(Forward Scatter)", 
       ylab = "Days Past Transplant")


###################################################
### chunk number 9: gvhdViolin
###################################################
plot(trellis.last.object())


###################################################
### chunk number 1: 
###################################################
stripplot(factor(mag) ~ depth, quakes)


###################################################
### chunk number 2: stripplot1
###################################################
plot(trellis.last.object())


###################################################
### chunk number 3: 
###################################################
stripplot(depth ~ factor(mag), quakes, 
          jitter.data = TRUE, alpha = 0.6,
          xlab = "Magnitude (Richter)", ylab = "Depth (km)")


###################################################
### chunk number 4: stripplot2
###################################################
plot(trellis.last.object())


###################################################
### chunk number 5: 
###################################################
stripplot(sqrt(abs(residuals(lm(yield~variety+year+site)))) ~ site, 
          data = barley, groups = year, jitter.data = TRUE,
          auto.key = list(points = TRUE, lines = TRUE, columns = 2),
          type = c("p", "a"), fun = median,
          ylab = expression(abs("Residual Barley Yield")^{1 / 2}))

## ylab = expression(sqrt(abs("Residual Barley Yield"))))
## ylab = expression("Absolute Residual Barley Yield" * sqrt(bushels / acre)))

##


###################################################
### chunk number 6: barleystrip
###################################################
plot(trellis.last.object())


