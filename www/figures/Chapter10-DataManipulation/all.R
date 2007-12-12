###################################################
### chunk number 1: 
###################################################
boxcox.trans <- function(x, lambda) {
    if (lambda == 0) log(x) else (x^lambda - 1) / lambda
}


###################################################
### chunk number 2:  eval=FALSE
###################################################
## 
## data(Chem97, package = "mlmRev")
## trellis.device(pdf, file = "Chem97BoxCox.pdf", 
##                width = 8, height = 6)
## for (p in seq(0, 3, by = 0.5)) {
##     plot(qqmath(~boxcox.trans(gcsescore, p) | gender, data = Chem97, 
##                 groups = score, f.value = ppoints(100), 
##                 main = as.expression(substitute(lambda == v, 
##                                                 list(v = p)))))
## }
## dev.off()
## 
## ##


###################################################
### chunk number 3:  eval=FALSE
###################################################
## 
## form <- ~ boxcox.trans(gcsescore, p) | gender
## qqboxcox <- function(lambda) {
##     for (p in lambda)
##         plot(qqmath(form, data = Chem97, 
##                     groups = score, f.value = ppoints(100), 
##                     main = as.expression(substitute(lambda == v, 
##                                                     list(v = p)))))
## }
## qqboxcox(lambda = seq(0, 3, by = 0.5))
## 
## ##


###################################################
### chunk number 4: 
###################################################
Titanic1 <- as.data.frame(as.table(Titanic[, , "Adult" ,])) 
Titanic1


###################################################
### chunk number 5: 
###################################################
barchart(Class ~ Freq | Sex, Titanic1, 
         groups = Survived, stack = TRUE, 
         auto.key = list(title = "Survived", columns = 2))


###################################################
### chunk number 6: titanic1
###################################################
plot(trellis.last.object())


###################################################
### chunk number 7: 
###################################################
Titanic2 <- 
    reshape(Titanic1, direction = "wide", v.names = "Freq", 
            idvar = c("Class", "Sex"), timevar = "Survived")
names(Titanic2) <- c("Class", "Sex", "Dead", "Alive")


###################################################
### chunk number 8: 
###################################################
Titanic2


###################################################
### chunk number 9: titanic2
###################################################
barchart(Class ~ Dead + Alive | Sex, 
         Titanic2, 
         stack = TRUE, 
         auto.key = list(columns = 2))
plot(trellis.last.object())


###################################################
### chunk number 10:  eval=FALSE
###################################################
## Titanic2 <- 
##     reshape(Titanic1, direction = "wide", v.names = "Freq", 
##             idvar = c("Class", "Sex"), timevar = "Survived")
## names(Titanic2) <- c("Class", "Sex", "Dead", "Alive")


###################################################
### chunk number 11:  eval=FALSE
###################################################
## barchart(Class ~ Dead + Alive | Sex, Titanic2, stack = TRUE, 
##          auto.key = list(columns = 2))


###################################################
### chunk number 12: 
###################################################
data(Gcsemv, package = "mlmRev")
## str(Gcsemv)


###################################################
### chunk number 13: 
###################################################
xyplot(written ~ course | gender, data = Gcsemv, 
       type = c("g", "p", "smooth"),
       xlab = "Coursework score", ylab = "Written exam score",
       panel = function(x, y, ...) {
           panel.xyplot(x, y, ...)
           panel.rug(x = x[is.na(y)], y = y[is.na(x)])
       })
##


###################################################
### chunk number 14: gcsemvxy
###################################################
print(trellis.last.object())


###################################################
### chunk number 15: 
###################################################
qqmath( ~ written + course, Gcsemv, type = c("p", "g"), 
       outer = TRUE, groups = gender, auto.key = list(columns = 2),
       f.value = ppoints(200), ylab = "Score")
##


###################################################
### chunk number 16: gcsemvqq
###################################################
print(trellis.last.object())


###################################################
### chunk number 17: 
###################################################
set.seed(20051028)


###################################################
### chunk number 18: 
###################################################
x1 <- rexp(2000)
x1 <- x1[x1 > 1]
x2 <- rexp(1000)


###################################################
### chunk number 19:  eval=FALSE
###################################################
## mg1 <- 
##     qqmath(~ data, make.groups(x1, x2), 
##            distribution = qexp, 
##            groups = which, 
##            aspect = "iso", 
##            type = c('p', 'g'))
## y1 <- rnorm(2000)
## y1 <- y1[y1 > -1]
## y2 <- rnorm(1000)
## mg2 <- 
##     qqmath(~ data, make.groups(y1, y2), 
##            groups = which, 
##            aspect = "iso", 
##            type = c('p', 'g'))
## 


###################################################
### chunk number 20:  eval=FALSE
###################################################
## qqmath(~ x1 + x2, distribution = qexp)


###################################################
### chunk number 21:  eval=FALSE
###################################################
## qqmath( ~ c(x1, x2), distribution = qexp, 
##        groups = rep(c('x1', 'x2'), c(length(x1), length(x2))))


###################################################
### chunk number 22: 
###################################################
str(make.groups(x1, x2))


###################################################
### chunk number 23: 
###################################################
qqmath(~ data, make.groups(x1, x2), groups = which, 
       distribution = qexp, aspect = "iso", type = c('p', 'g'))


###################################################
### chunk number 24: makegroups
###################################################
plot(trellis.last.object())

## print(mg1, split = c(1, 1, 2, 1), more = TRUE)
## print(mg2, split = c(2, 1, 2, 1), more = FALSE)


###################################################
### chunk number 25: 
###################################################
str(beaver1)
str(beaver2)


###################################################
### chunk number 26: 
###################################################
beavers <- make.groups(beaver1, beaver2)
str(beavers)


###################################################
### chunk number 27: 
###################################################
beavers$hour <- 
    with(beavers, time %/% 100 + 24*(day - 307) + (time %% 100)/60)


###################################################
### chunk number 28: 
###################################################
xyplot(temp ~ hour | which, data = beavers, groups = activ, 
       auto.key = list(text = c("inactive", "active"), columns = 2),
       xlab = "Time (hours)", ylab = "Body Temperature (C)", 
       scales = list(x = list(relation = "sliced")))


###################################################
### chunk number 29: xyplot1
###################################################
plot(trellis.last.object())


###################################################
### chunk number 30:  eval=FALSE
###################################################
## barchart(Class ~ Freq | Sex, as.data.frame(Titanic),
##          subset = (Age == "Adult"), groups = Survived, stack = TRUE,
##          auto.key = list(title = "Survived", columns = 2))
## ##


###################################################
### chunk number 31: 
###################################################
data(USAge.df, package = "latticeExtra")
head(USAge.df)


###################################################
### chunk number 32: 
###################################################
xyplot(Population ~ Age | factor(Year), USAge.df, 
       groups = Sex, type = c("l", "g"),
       auto.key = list(points = FALSE, lines = TRUE, columns = 2),
       aspect = "xy", ylab = "Population (millions)",
       subset = Year %in% seq(1905, 1975, by = 10))


###################################################
### chunk number 33: uspop1
###################################################
plot(trellis.last.object())


###################################################
### chunk number 34: 
###################################################
xyplot(Population ~ Year | factor(Age), USAge.df, 
       groups = Sex, type = "l", strip = FALSE, strip.left = TRUE, 
       layout = c(1, 3), ylab = "Population (millions)", 
       auto.key = list(lines = TRUE, points = FALSE, columns = 2),
       subset = Age %in% c(0, 10, 20))
##


###################################################
### chunk number 35: uspop2
###################################################
plot(trellis.last.object())


###################################################
### chunk number 36: 
###################################################
xyplot(Population ~ Year | factor(Year - Age), USAge.df, 
       groups = Sex, subset = (Year - Age) %in% 1894:1905,
       type = c("g", "l"), ylab = "Population (millions)", 
       auto.key = list(lines = TRUE, points = FALSE, columns = 2))
##


###################################################
### chunk number 37: uspop3
###################################################
plot(trellis.last.object())


###################################################
### chunk number 38: uspop3Col
###################################################
plot(trellis.last.object(par.settings = canonical.theme("pdf")))


###################################################
### chunk number 1: 
###################################################
xyplot(stations ~ mag, quakes, jitter.x = TRUE, 
       type = c("p", "smooth"),
       xlab = "Magnitude (Richter)", 
       ylab = "Number of stations reporting") 

##        alpha = 0.6,  #pch = ".", cex = 2,
##        col.line = "#000000FF")

## xyplot(stations ~ mag, quakes, jitter.x = TRUE, 
##        type = c("p", "smooth"))

##


###################################################
### chunk number 2: qshingleXy
###################################################
plot(trellis.last.object())


###################################################
### chunk number 3: 
###################################################
quakes$Mag <- equal.count(quakes$mag, number = 10, overlap = 0.2)


###################################################
### chunk number 4: 
###################################################
summary(quakes$Mag)


###################################################
### chunk number 5: 
###################################################
as.character(levels(quakes$Mag))


###################################################
### chunk number 6: 
###################################################
ps.mag <- plot(quakes$Mag, ylab = "Level",
               xlab = "Magnitude (Richter)")


###################################################
### chunk number 7: 
###################################################
bwp.quakes <- 
    bwplot(stations ~ Mag, quakes, xlab = "Magnitude", 
           ylab = "Number of stations reporting")
##


###################################################
### chunk number 8:  eval=FALSE
###################################################
## plot(bwp.quakes, position = c(0, 0, 1, 0.65))
## plot(ps.mag, position = c(0, 0.65, 1, 1), newpage = FALSE)


###################################################
### chunk number 9: qshingleBw
###################################################
plot(bwp.quakes, position = c(0, 0, 1, 0.65))
plot(ps.mag, position = c(0, 0.65, 1, 1), newpage = FALSE)


###################################################
### chunk number 10: 
###################################################
bwplot(sqrt(stations) ~ Mag, quakes, 
       scales = 
       list(x = list(limits = as.character(levels(quakes$Mag)), 
                     rot = 60)),
       xlab = "Magnitude (Richter)",
       ylab = expression(sqrt("Number of stations")))


##


###################################################
### chunk number 11: qshingleBwSqrt
###################################################
plot(trellis.last.object())


###################################################
### chunk number 12: 
###################################################

qqmath(~ sqrt(stations) | Mag, quakes, 
       type = c("p", "g"), pch = ".", cex = 3, 
       prepanel = prepanel.qqmathline, aspect = "xy",
       strip = strip.custom(strip.levels = TRUE, 
                            strip.names = FALSE),
       xlab = "Standard normal quantiles", 
       ylab = expression(sqrt("Number of stations")))

## qqmath(~ stations | Mag, quakes, type = c("p", "g"),
##        pch = ".", cex = 3, prepanel = prepanel.qqmathline, aspect = "xy",
##        strip = strip.custom(strip.levels = TRUE, strip.names = FALSE),
##        ylab = expression(sqrt("Number of stations")))

##


###################################################
### chunk number 13: qshingleQQ
###################################################
plot(trellis.last.object())


###################################################
### chunk number 14:  eval=FALSE
###################################################
## xyplot(stations ~ mag, quakes, 
##        panel = panel.bwplot, horizontal = FALSE)


###################################################
### chunk number 15: 
###################################################
xyplot(sqrt(stations) ~ mag, quakes, cex = 0.6,
       panel = panel.bwplot, horizontal = FALSE, box.ratio = 0.05, 
       xlab = "Magnitude (Richter)", 
       ylab = expression(sqrt("Number of stations")))

##


###################################################
### chunk number 16: qshingleXyBw
###################################################
plot(trellis.last.object())


###################################################
### chunk number 17: 
###################################################
state.density <-
    data.frame(name = state.name,
               area = state.x77[, "Area"],
               population = state.x77[, "Population"],
               region = state.region)
state.density$density <- with(state.density, population / area)


###################################################
### chunk number 18: 
###################################################
dotplot(reorder(name, density) ~ density, state.density,
        xlab = "Population Density (thousands per square mile)")


###################################################
### chunk number 19: StateDens
###################################################
plot(trellis.last.object())


###################################################
### chunk number 20: 
###################################################
state.density$Density <-
    shingle(state.density$density,
            intervals = rbind(c(0, 0.2),
                              c(0.2, 1)))


###################################################
### chunk number 21: 
###################################################

dotplot(reorder(name, density) ~ density | Density, state.density,
        strip = FALSE, layout = c(2, 1), levels.fos = 1:50,
        scales = list(x = "free"), between = list(x = 0.5),
        xlab = "Population Density (thousands per square mile)", 
        par.settings = list(layout.widths = list(panel = c(2, 1))))




###################################################
### chunk number 22: StateDensBreak
###################################################
plot(trellis.last.object())


###################################################
### chunk number 23: 
###################################################

cutAndStack <- 
    function(x, number = 6, overlap = 0.1, type = 'l',
             xlab = "Time", ylab = deparse(substitute(x)), ...) {
    time <- if (is.ts(x)) time(x) else seq_along(x)
    Time <- equal.count(as.numeric(time), 
                        number = number, overlap = overlap)
    xyplot(as.numeric(x) ~ time | Time,
           type = type, xlab = xlab, ylab = ylab,
           default.scales = list(x = list(relation = "free"),
                                 y = list(relation = "free")),
           ...)
}


###################################################
### chunk number 24: 
###################################################

cutAndStack(EuStockMarkets[, "DAX"], aspect = "xy",
            scales = list(x = list(draw = FALSE), 
                          y = list(rot = 0)))

## cutAndStack(EuStockMarkets[, "DAX"], aspect = "xy",
##             scales = list(x = list(draw = TRUE), 
##                           y = list(rot = 0)))[3]

##


###################################################
### chunk number 25: cutStackEustock
###################################################
plot(trellis.last.object())


###################################################
### chunk number 1: 
###################################################
bdp1 <- 
dotplot(as.character(variety) ~ yield | as.character(site), barley,
        groups = year, layout = c(1, 6),
        auto.key = list(space = "top", columns = 2),
        ## strip = FALSE, strip.left = TRUE,
        aspect = "fill")

bdp2 <- 
dotplot(variety ~ yield | site, barley,
        groups = year, layout = c(1, 6),
        auto.key = list(space = "top", columns = 2),
        ## strip = FALSE, strip.left = TRUE)
        aspect = "fill")
##


###################################################
### chunk number 2: barleyUnordered
###################################################
plot(bdp1, split = c(1, 1, 2, 1))
plot(bdp2, split = c(2, 1, 2, 1), newpage = FALSE)
##


###################################################
### chunk number 3: 
###################################################
state.density <-
    data.frame(name = state.name,
               area = state.x77[, "Area"],
               population = state.x77[, "Population"],
               region = state.region)
state.density$density <- with(state.density, population / area)
##


###################################################
### chunk number 4: 
###################################################
dotplot(reorder(name, density) ~ 1000 * density, state.density, 
        scales = list(x = list(log = 10)), 
        xlab = "Density (per square mile)")


###################################################
### chunk number 5: StateDensLogOrdered
###################################################
plot(trellis.last.object())


###################################################
### chunk number 6: 
###################################################
state.density$region <- 
    with(state.density, reorder(region, density, median))


###################################################
### chunk number 7: 
###################################################
state.density$name <- 
    with(state.density, 
         reorder(reorder(name, density), as.numeric(region)))


###################################################
### chunk number 8: 
###################################################
dotplot(name ~ 1000 * density | region, state.density, 
        strip = FALSE, strip.left = TRUE, layout = c(1, 4),
        scales = list(x = list(log = 10),
                      y = list(relation = "free")),
        xlab = "Density (per square mile)")

##


###################################################
### chunk number 9: StateDensByRegion
###################################################
plot(trellis.last.object())


###################################################
### chunk number 10: 
###################################################
library("latticeExtra")
resizePanels()
##


###################################################
### chunk number 11: StateDensResized
###################################################
plot(trellis.last.object())


###################################################
### chunk number 12: 
###################################################
data(USCancerRates, package = "latticeExtra")
xyplot(rate.male ~ rate.female | state, USCancerRates,
       aspect = "iso", pch = ".", cex = 2,
       index.cond = function(x, y) { median(y - x, na.rm = TRUE) },
       scales = list(log = 2, at = c(75, 150, 300, 600)), 
       panel = function(...) {
           panel.grid(h = -1, v = -1)
           panel.abline(0, 1)
           panel.xyplot(...)
       })
##


###################################################
### chunk number 13: CancerRatesOrdered
###################################################
plot(trellis.last.object())


###################################################
### chunk number 1: 
###################################################
strip.style4 <- function(..., style) {
    strip.default(..., style = 4)
}


###################################################
### chunk number 2: 
###################################################
data(Chem97, package = "mlmRev")
qqmath(~gcsescore | factor(score), Chem97, groups = gender, 
       type = c("l", "g"),  aspect = "xy", 
       auto.key = list(points = FALSE, lines = TRUE, columns = 2),
       f.value = ppoints(100), strip = strip.style4,
       xlab = "Standard normal quantiles", 
       ylab = "Average GCSE score")
##


###################################################
### chunk number 3: chemStrip4
###################################################
plot(trellis.last.object())


###################################################
### chunk number 4: 
###################################################
qqmath(~gcsescore | factor(score), Chem97, groups = gender, 
       type = c("l", "g"),  aspect = "xy", 
       auto.key = list(points = FALSE, lines = TRUE, columns = 2),
       f.value = ppoints(100), strip = strip.custom(style = 4),
       xlab = "Standard normal quantiles", 
       ylab = "Average GCSE score")
##


###################################################
### chunk number 5: 
###################################################
strip.combined <- 
    function(which.given, which.panel, factor.levels, ...) {
    if (which.given == 1) {
        panel.rect(0, 0, 1, 1, col = "grey90", border = 1)
        panel.text(x = 0, y = 0.5, pos = 4, 
                   lab = factor.levels[which.panel[which.given]])
    }
    if (which.given == 2) {
        panel.text(x = 1, y = 0.5, pos = 2,
                   lab = factor.levels[which.panel[which.given]])
    }
}

qqmath(~ gcsescore | factor(score) + gender, Chem97, 
       f.value = ppoints(100), type = c("l", "g"), aspect = "xy",
       strip = strip.combined,
       par.strip.text = list(lines = 0.5),
       xlab = "Standard normal quantiles", 
       ylab = "Average GCSE score")

## 


###################################################
### chunk number 6: denovoStrip
###################################################
plot(trellis.last.object())


