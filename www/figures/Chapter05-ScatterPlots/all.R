###################################################
### chunk number 1: 
###################################################
xyplot(lat ~ long | cut(depth, 2), data = quakes)


###################################################
### chunk number 2: xyplot1
###################################################
plot(trellis.last.object())


###################################################
### chunk number 3: 
###################################################
xyplot(lat ~ long | cut(depth, 3), data = quakes, 
       aspect = "iso", pch = ".", cex = 2, type = c("p", "g"),
       xlab = "Longitude", ylab = "Latitude", 
       strip = strip.custom(strip.names = TRUE, var.name = "Depth"))


###################################################
### chunk number 4: xyplot2
###################################################
plot(trellis.last.object())


###################################################
### chunk number 5: 
###################################################

xyplot(lat ~ long, data = quakes, aspect = "iso",
       groups = cut(depth, breaks = quantile(depth, ppoints(4, 1))), 
       auto.key = list(columns = 3, title = "Depth"), 
       xlab = "Longitude", ylab = "Latitude")



###################################################
### chunk number 6: xyplot3
###################################################
plot(trellis.last.object())


###################################################
### chunk number 7: 
###################################################
depth.col <- gray.colors(100)[cut(quakes$depth, 100, label = FALSE)]
##


###################################################
### chunk number 8: 
###################################################
depth.ord <- rev(order(quakes$depth))

xyplot(lat ~ long, data = quakes[depth.ord, ], 
       aspect = "iso", type = c("p", "g"),
       pch = 21, fill = depth.col[depth.ord], cex = 2,
       xlab = "Longitude", ylab = "Latitude")

##


###################################################
### chunk number 9: xyplot4
###################################################
plot(trellis.last.object())


###################################################
### chunk number 10: 
###################################################
quakes$Magnitude <- equal.count(quakes$mag, 4)
summary(quakes$Magnitude)


###################################################
### chunk number 11: 
###################################################
quakes$color <- depth.col
quakes.ordered <- quakes[depth.ord, ]
xyplot(lat ~ long | Magnitude, data = quakes.ordered,
       aspect = "iso", fill.color = quakes.ordered$color, cex = 2,
       panel = function(x, y, fill.color, ..., subscripts) {
           fill <- fill.color[subscripts]
           panel.grid(h = -1, v = -1)
           panel.xyplot(x, y, pch = 21, fill = fill, ...)
       },
       xlab = "Longitude", ylab = "Latitude")

## quakes$color <- I(depth.col) # suppress conversion to factor

##


###################################################
### chunk number 12: xyplot5
###################################################
plot(trellis.last.object())


###################################################
### chunk number 13: 
###################################################

depth.breaks <- do.breaks(range(quakes.ordered$depth), 50)

quakes.ordered$color <- 
    level.colors(quakes.ordered$depth, at = depth.breaks, 
                 col.regions = gray.colors)

xyplot(lat ~ long | Magnitude, data = quakes.ordered, 
       aspect = "iso", groups = color, cex = 2,
       panel = function(x, y, groups, ..., subscripts) {
           fill <- groups[subscripts]
           panel.grid(h = -1, v = -1)
           panel.xyplot(x, y, pch = 21, fill = fill, ...)
       },
       legend = 
       list(right = 
            list(fun = draw.colorkey,
                 args = list(key = list(col = gray.colors,
                                        at = depth.breaks), 
                             draw = FALSE))),
       xlab = "Longitude", ylab = "Latitude")

## depth.index <- 
##     findInterval(quakes.ordered$depth, depth.breaks, 
##                  rightmost.closed = TRUE)
## color.gradient <- gray.colors(50)

## quakes.ordered$color <- color.gradient[depth.index]




###################################################
### chunk number 14: xyplot6
###################################################
plot(trellis.last.object())


###################################################
### chunk number 15: quakeDepthColor
###################################################
plot(trellis.last.object(par.settings = canonical.theme("pdf")))


###################################################
### chunk number 1: 
###################################################

types.plain <- c("p", "l", "o", "r", "g", "s", "S", "h", "a", "smooth")
types.horiz <- c("s", "S", "h", "a", "smooth")
horiz <- rep(c(FALSE, TRUE), c(length(types.plain), length(types.horiz)))

types <- c(types.plain, types.horiz)

## x <- seq(-10, 10, length = 30)


set.seed(2007041)
x <- sample(seq(-10, 10, length = 15), 30, TRUE)
y <- x + 0.25 * (x + 1)^2 + rnorm(length(x), sd = 5)

xyplot(y ~ x | gl(1, length(types)),
       xlab = "type", 
       ylab = list(c("horizontal=TRUE", "horizontal=FALSE"), y = c(1/6, 4/6)),
       as.table = TRUE, layout = c(5, 3),
       between = list(y = c(0, 1)),
       strip = function(...) {
           panel.fill(trellis.par.get("strip.background")$col[1])
           type <- types[panel.number()]
           grid.text(lab = sprintf('"%s"', type), 
                     x = 0.5, y = 0.5)
           grid.rect()
       },
##        strip.left = function(...) {
##            if (current.column() == 1)
##            {
##                ## panel.fill(trellis.par.get("strip.background")$col[1])
##                horizontal <- horiz[panel.number()]
##                grid.text(lab = sprintf("%s", horizontal), 
##                          x = 0.5, y = 0.5, rot = 90)
##                ## grid.rect()
##            }
##        },
       scales = list(alternating = c(0, 2), tck = c(0, 0.7), draw = FALSE),
       par.settings = 
       list(layout.widths = list(strip.left = c(1, 0, 0, 0, 0))),
       panel = function(...) {
           type <- types[panel.number()]
           horizontal <- horiz[panel.number()]
           panel.xyplot(..., 
                        type = type,
                        horizontal = horizontal)
       })[rep(1, length(types))]

## 


###################################################
### chunk number 2: xytypes
###################################################
plot(trellis.last.object())


###################################################
### chunk number 3: 
###################################################
data(Earthquake, package = "MEMSS")


###################################################
### chunk number 4: 
###################################################
xyplot(accel ~ distance, data = Earthquake,
       panel = function(...) {
           panel.grid(h = -1, v = -1)
           panel.xyplot(...)
           panel.loess(...)
       }, 
       xlab = "Distance From Epicenter (km)",
       ylab = "Maximum Horizontal Acceleration (g)")
##


###################################################
### chunk number 5: quakenotype
###################################################
plot(trellis.last.object())


###################################################
### chunk number 6: 
###################################################
xyplot(accel ~ distance, data = Earthquake,
       type = c("g", "p", "smooth"),
       scales = list(log = 2),
       xlab = "Distance From Epicenter (km)",
       ylab = "Maximum Horizontal Acceleration (g)")
##


###################################################
### chunk number 7: quakewithtype
###################################################
plot(trellis.last.object())


###################################################
### chunk number 8: 
###################################################
library("locfit")
Earthquake$Magnitude <- 
    equal.count(Earthquake$Richter, 3, overlap = 0.1)
coef <- coef(lm(log2(accel) ~ log2(distance), data = Earthquake))
xyplot(accel ~ distance | Magnitude, data = Earthquake,
       scales = list(log = 2), col.line = "grey", lwd = 2,
       panel = function(...) {
           panel.abline(reg = coef)
           panel.locfit(...)
       },
       xlab = "Distance From Epicenter (km)",
       ylab = "Maximum Horizontal Acceleration (g)")
##


###################################################
### chunk number 9: quakelocfit
###################################################
plot(trellis.last.object())


###################################################
### chunk number 1: 
###################################################
data(SeatacWeather, package = "latticeExtra")
##


###################################################
### chunk number 2: 
###################################################
xyplot(min.temp + max.temp + precip ~ day | month,
       ylab = "Temperature and Rainfall", 
       data = SeatacWeather, type = "l", lty = 1, col = "black")
##


###################################################
### chunk number 3: seatac1
###################################################
plot(trellis.last.object())


###################################################
### chunk number 4: 
###################################################
maxp <- max(SeatacWeather$precip, na.rm = TRUE)
xyplot(min.temp + max.temp + I(80 * precip / maxp) ~ day | month,
       data = SeatacWeather, lty = 1, col = "black",
       ylab = "Temperature and Rainfall", 
       type = c("l", "l", "h"), distribute.type = TRUE)

##


###################################################
### chunk number 5: seatac2
###################################################
plot(trellis.last.object())


###################################################
### chunk number 6: 
###################################################
update(trellis.last.object(),
       ylab = "Temperature (Fahrenheit) \n and Rainfall (inches)",
       panel = function(...) {
           panel.xyplot(...)
           if (panel.number() == 2) {
               at <- pretty(c(0, max.precip))
               panel.axis("right", half = FALSE,
                          at = at * 80 / max.precip, labels = at)
           }
       })


## xyplot(min.temp + max.temp + I(80 * precip / max.precip) ~ day | month,
##        data = SeatacWeather,
##        ylab = "Temperature and Rainfall", 
##        layout = c(3, 1),
##        type = c("o", "o", "h"),
##        distribute.type = TRUE,
##        panel = function(...) {
##            panel.grid(h = -1, v = -1)
##            panel.xyplot(...)
##            if (panel.number() == 2) {
##                at <- pretty(c(0, max.precip))
##                panel.axis("right",
##                           at = at * 80 / max.precip,
##                           labels = at,
##                           half = FALSE)
##            }
##        })



###################################################
### chunk number 7: seatac3
###################################################
plot(trellis.last.object())


###################################################
### chunk number 1: 
###################################################

library("hexbin")
data(gvhd10, package = "latticeExtra")

xyplot(asinh(SSC.H) ~ asinh(FL2.H) | Days, gvhd10, aspect = 1, 
       panel = panel.hexbinplot, .aspect.ratio = 1, trans = sqrt)

## 


###################################################
### chunk number 2: gvhdHex
###################################################
plot(trellis.last.object())


###################################################
### chunk number 1: 
###################################################
splom(USArrests)


###################################################
### chunk number 2: splomArrests
###################################################
plot(trellis.last.object())


###################################################
### chunk number 3: 
###################################################
splom(~USArrests[c(3, 1, 2, 4)] | state.region, 
      pscales = 0, type = c("g", "p", "smooth"))
## [c(4, 2, 3, 1)]


###################################################
### chunk number 4: splomArrestsCond
###################################################
plot(trellis.last.object())


###################################################
### chunk number 5: 
###################################################
splom(~data.frame(mpg, disp, hp, drat, wt, qsec),
      data = mtcars, groups = cyl, pscales = 0, 
      varnames = c("Miles\nper\ngallon", "Displacement\n(cu. in.)",
                   "Gross\nhorsepower", "Rear\naxle\nratio", 
                   "Weight", "1/4 mile\ntime"),
      auto.key = list(columns = 3, title = "Number of Cylinders"))

## splom(~mtcars[c(1, 3, 4, 5, 6, 7)], 
##


###################################################
### chunk number 6: splomMtcars
###################################################
plot(trellis.last.object())


###################################################
### chunk number 7: splomMtcarsCol
###################################################
plot(trellis.last.object(par.settings = canonical.theme("pdf")))


###################################################
### chunk number 8:  eval=FALSE
###################################################
## splom(~mtcars[c(1, 3:7)], data = mtcars, groups = cyl)


###################################################
### chunk number 9:  eval=FALSE
###################################################
## splom(mtcars[c(1, 3:7)], groups = mtcars$cyl)


###################################################
### chunk number 1: 
###################################################
parallel(~mtcars[c(1, 3, 4, 5, 6, 7)] | factor(cyl), 
         mtcars, groups = carb, 
         key = simpleKey(levels(factor(mtcars$carb)), points = FALSE, 
                         lines = TRUE, space = "top", columns = 3), 
         layout = c(3, 1))


###################################################
### chunk number 2: parallelMtcars
###################################################
plot(trellis.last.object())


###################################################
### chunk number 3: 
###################################################
data(gvhd10, package = "latticeExtra")


###################################################
### chunk number 4: 
###################################################
parallel(~ asinh(gvhd10[c(3, 2, 4, 1, 5)]), data = gvhd10, 
         subset = Days == "13", alpha = 0.01, lty = 1)


###################################################
### chunk number 5: parallelGvhd
###################################################
plot(trellis.last.object())


