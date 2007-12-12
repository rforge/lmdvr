source("../setup.R")
chapter <- 10


boxcox.trans <- function(x, lambda) {
    if (lambda == 0) log(x) else (x^lambda - 1) / lambda
}
Titanic1 <- as.data.frame(as.table(Titanic[, , "Adult" ,])) 
Titanic1

## 10.1
resizeWindow(5)
barchart(Class ~ Freq | Sex, Titanic1, 
         groups = Survived, stack = TRUE, 
         auto.key = list(title = "Survived", columns = 2))
saveImage()


Titanic2 <- 
    reshape(Titanic1, direction = "wide", v.names = "Freq", 
            idvar = c("Class", "Sex"), timevar = "Survived")
names(Titanic2) <- c("Class", "Sex", "Dead", "Alive")
Titanic2

## 10.2
resizeWindow(5)
barchart(Class ~ Dead + Alive | Sex, 
         Titanic2, 
         stack = TRUE, 
         auto.key = list(columns = 2))
saveImage()

data(Gcsemv, package = "mlmRev")

## 10.3
resizeWindow(5.5)
xyplot(written ~ course | gender, data = Gcsemv, 
       type = c("g", "p", "smooth"),
       xlab = "Coursework score", ylab = "Written exam score",
       panel = function(x, y, ...) {
           panel.xyplot(x, y, ...)
           panel.rug(x = x[is.na(y)], y = y[is.na(x)])
       })
saveImage()


## 10.4
resizeWindow(5)
qqmath( ~ written + course, Gcsemv, type = c("p", "g"), 
       outer = TRUE, groups = gender, auto.key = list(columns = 2),
       f.value = ppoints(200), ylab = "Score")
saveImage()

set.seed(20051028)
x1 <- rexp(2000)
x1 <- x1[x1 > 1]
x2 <- rexp(1000)
str(make.groups(x1, x2))

## 10.5
resizeWindow(5)
qqmath(~ data, make.groups(x1, x2), groups = which, 
       distribution = qexp, aspect = "iso", type = c('p', 'g'))
saveImage()

str(beaver1)
str(beaver2)
beavers <- make.groups(beaver1, beaver2)
str(beavers)
beavers$hour <- 
    with(beavers, time %/% 100 + 24*(day - 307) + (time %% 100)/60)

## 10.6
resizeWindow(5)
xyplot(temp ~ hour | which, data = beavers, groups = activ, 
       auto.key = list(text = c("inactive", "active"), columns = 2),
       xlab = "Time (hours)", ylab = "Body Temperature (C)", 
       scales = list(x = list(relation = "sliced")))
saveImage()

data(USAge.df, package = "latticeExtra")
head(USAge.df)

## 10.7
resizeWindow(6.5)
xyplot(Population ~ Age | factor(Year), USAge.df, 
       groups = Sex, type = c("l", "g"),
       auto.key = list(points = FALSE, lines = TRUE, columns = 2),
       aspect = "xy", ylab = "Population (millions)",
       subset = Year %in% seq(1905, 1975, by = 10))
saveImage()


## 10.8
resizeWindow(4.5)
xyplot(Population ~ Year | factor(Age), USAge.df, 
       groups = Sex, type = "l", strip = FALSE, strip.left = TRUE, 
       layout = c(1, 3), ylab = "Population (millions)", 
       auto.key = list(lines = TRUE, points = FALSE, columns = 2),
       subset = Age %in% c(0, 10, 20))
saveImage()


## 10.9
resizeWindow(6)
xyplot(Population ~ Year | factor(Year - Age), USAge.df, 
       groups = Sex, subset = (Year - Age) %in% 1894:1905,
       type = c("g", "l"), ylab = "Population (millions)", 
       auto.key = list(lines = TRUE, points = FALSE, columns = 2))
saveImage()


## 10.10
resizeWindow(7)
xyplot(stations ~ mag, quakes, jitter.x = TRUE, 
       type = c("p", "smooth"),
       xlab = "Magnitude (Richter)", 
       ylab = "Number of stations reporting") 
saveImage()

quakes$Mag <- equal.count(quakes$mag, number = 10, overlap = 0.2)
summary(quakes$Mag)
as.character(levels(quakes$Mag))
ps.mag <- plot(quakes$Mag, ylab = "Level",
               xlab = "Magnitude (Richter)")
bwp.quakes <- 
    bwplot(stations ~ Mag, quakes, xlab = "Magnitude", 
           ylab = "Number of stations reporting")

## 10.11
resizeWindow(11.5)
plot(bwp.quakes, position = c(0, 0, 1, 0.65))
plot(ps.mag, position = c(0, 0.65, 1, 1), newpage = FALSE)
saveImage()


## 10.12
resizeWindow(5.5)
bwplot(sqrt(stations) ~ Mag, quakes, 
       scales = 
       list(x = list(limits = as.character(levels(quakes$Mag)), 
                     rot = 60)),
       xlab = "Magnitude (Richter)",
       ylab = expression(sqrt("Number of stations")))
saveImage()


## 10.13
resizeWindow(5.5)
qqmath(~ sqrt(stations) | Mag, quakes, 
       type = c("p", "g"), pch = ".", cex = 3, 
       prepanel = prepanel.qqmathline, aspect = "xy",
       strip = strip.custom(strip.levels = TRUE, 
                            strip.names = FALSE),
       xlab = "Standard normal quantiles", 
       ylab = expression(sqrt("Number of stations")))
saveImage()


## 10.14
resizeWindow(6)
xyplot(sqrt(stations) ~ mag, quakes, cex = 0.6,
       panel = panel.bwplot, horizontal = FALSE, box.ratio = 0.05, 
       xlab = "Magnitude (Richter)", 
       ylab = expression(sqrt("Number of stations")))
saveImage()

state.density <-
    data.frame(name = state.name,
               area = state.x77[, "Area"],
               population = state.x77[, "Population"],
               region = state.region)
state.density$density <- with(state.density, population / area)

## 10.15
resizeWindow(12)
dotplot(reorder(name, density) ~ density, state.density,
        xlab = "Population Density (thousands per square mile)")
saveImage()

state.density$Density <-
    shingle(state.density$density,
            intervals = rbind(c(0, 0.2),
                              c(0.2, 1)))

## 10.16
resizeWindow(12)
dotplot(reorder(name, density) ~ density | Density, state.density,
        strip = FALSE, layout = c(2, 1), levels.fos = 1:50,
        scales = list(x = "free"), between = list(x = 0.5),
        xlab = "Population Density (thousands per square mile)", 
        par.settings = list(layout.widths = list(panel = c(2, 1))))
saveImage()

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

## 10.17
resizeWindow(12)
cutAndStack(EuStockMarkets[, "DAX"], aspect = "xy",
            scales = list(x = list(draw = FALSE), 
                          y = list(rot = 0)))
saveImage()

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

## 10.18
resizeWindow(12)
plot(bdp1, split = c(1, 1, 2, 1))
plot(bdp2, split = c(2, 1, 2, 1), newpage = FALSE)
saveImage()

state.density <-
    data.frame(name = state.name,
               area = state.x77[, "Area"],
               population = state.x77[, "Population"],
               region = state.region)
state.density$density <- with(state.density, population / area)

## 10.19
resizeWindow(12)
dotplot(reorder(name, density) ~ 1000 * density, state.density, 
        scales = list(x = list(log = 10)), 
        xlab = "Density (per square mile)")
saveImage()

state.density$region <- 
    with(state.density, reorder(region, density, median))
state.density$name <- 
    with(state.density, 
         reorder(reorder(name, density), as.numeric(region)))

## 10.20
resizeWindow(12)
dotplot(name ~ 1000 * density | region, state.density, 
        strip = FALSE, strip.left = TRUE, layout = c(1, 4),
        scales = list(x = list(log = 10),
                      y = list(relation = "free")),
        xlab = "Density (per square mile)")
saveImage()

library("latticeExtra")
resizePanels()

## 10.21
resizeWindow(12)
trellis.last.object()
saveImage()

data(USCancerRates, package = "latticeExtra")

## 10.22
resizeWindow(12)
xyplot(rate.male ~ rate.female | state, USCancerRates,
       aspect = "iso", pch = ".", cex = 2,
       index.cond = function(x, y) { median(y - x, na.rm = TRUE) },
       scales = list(log = 2, at = c(75, 150, 300, 600)), 
       panel = function(...) {
           panel.grid(h = -1, v = -1)
           panel.abline(0, 1)
           panel.xyplot(...)
       })
saveImage()

strip.style4 <- function(..., style) {
    strip.default(..., style = 4)
}
data(Chem97, package = "mlmRev")

## 10.23
resizeWindow(6)
qqmath(~gcsescore | factor(score), Chem97, groups = gender, 
       type = c("l", "g"),  aspect = "xy", 
       auto.key = list(points = FALSE, lines = TRUE, columns = 2),
       f.value = ppoints(100), strip = strip.style4,
       xlab = "Standard normal quantiles", 
       ylab = "Average GCSE score")
saveImage()


## 10.23 (alternative)
qqmath(~gcsescore | factor(score), Chem97, groups = gender, 
       type = c("l", "g"),  aspect = "xy", 
       auto.key = list(points = FALSE, lines = TRUE, columns = 2),
       f.value = ppoints(100), strip = strip.custom(style = 4),
       xlab = "Standard normal quantiles", 
       ylab = "Average GCSE score")

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

## 10.24
resizeWindow(4.5)
qqmath(~ gcsescore | factor(score) + gender, Chem97, 
       f.value = ppoints(100), type = c("l", "g"), aspect = "xy",
       strip = strip.combined,
       par.strip.text = list(lines = 0.5),
       xlab = "Standard normal quantiles", 
       ylab = "Average GCSE score")
saveImage()

morris <- barley$site == "Morris"
barley$year[morris] <-
    ifelse(barley$year[morris] == "1931", "1932", "1931")

## 10.25
resizeWindow(5.5)
stripplot(sqrt(abs(residuals(lm(yield ~ variety+year+site)))) ~ site,
          data = barley, groups = year, jitter.data = TRUE,
          auto.key = list(points = TRUE, lines = TRUE, columns = 2),
          type = c("p", "a"), fun = median,
          ylab = expression(abs("Residual Barley Yield")^{1 / 2}))
saveImage()
