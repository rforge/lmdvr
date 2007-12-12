###################################################
### chunk number 1: 
###################################################

panel.hypotrochoid <- function(r, d, cycles = 10, density = 30)
{
    if (missing(r)) r <- runif(1, 0.25, 0.75)
    if (missing(d)) d <- runif(1, 0.25 * r, r)
    t <- 2 * pi * seq(0, cycles, by = 1/density)
    x <- (1 - r) * cos(t) + d * cos((1 - r) * t / r)
    y <- (1 - r) * sin(t) - d * sin((1 - r) * t / r)
    panel.lines(x, y)
}



###################################################
### chunk number 2: 
###################################################
panel.hypocycloid <- function(x, y, cycles = x, density = 30) {
    panel.hypotrochoid(r = x / y, d = x / y, 
                       cycles = cycles, density = density)
}


###################################################
### chunk number 3: 
###################################################
prepanel.hypocycloid <- function(x, y) {
    list(xlim = c(-1, 1), ylim = c(-1, 1))
}


###################################################
### chunk number 4: 
###################################################

grid <- data.frame(p = 11:30, q = 10)
grid$k <- with(grid, factor(p / q))

xyplot(p ~ q | k, grid, aspect = 1, scales = list(draw = FALSE),
       prepanel = prepanel.hypocycloid, panel = panel.hypocycloid)


##


###################################################
### chunk number 5: hypocycloid
###################################################
plot(trellis.last.object())


###################################################
### chunk number 6: 
###################################################

p <- xyplot(c(-1, 1) ~ c(-1, 1), aspect = 1, cycles = 15,
            scales = list(draw = FALSE), xlab = "", ylab = "",
            panel = panel.hypotrochoid)

p[rep(1, 42)]
##


###################################################
### chunk number 7: hypotrochoid
###################################################
set.seed(20070706)
plot(trellis.last.object())


###################################################
### chunk number 1: 
###################################################

library("logspline")
prepanel.ls <- function(x, n = 50, ...) {
    fit <- logspline(x)
    xx <- do.breaks(range(x), n)
    yy <- dlogspline(xx, fit)
    list(ylim = c(0, max(yy)))
}
panel.ls <- function(x, n = 50, ...) {
    fit <- logspline(x)
    xx <- do.breaks(range(x), n)
    yy <- dlogspline(xx, fit)
    panel.lines(xx, yy, ...)
}



###################################################
### chunk number 2: 
###################################################
faithful$Eruptions <- equal.count(faithful$eruptions, 4)
densityplot(~ waiting | Eruptions, data = faithful, 
            prepanel = prepanel.ls, panel = panel.ls)


###################################################
### chunk number 3: densLogspline
###################################################
print(trellis.last.object())


###################################################
### chunk number 4: 
###################################################

panel.bwtufte <- function(x, y, coef = 1.5, ...) {
    x <- as.numeric(x); y <- as.numeric(y)
    ux <- sort(unique(x))
    blist <- tapply(y, factor(x, levels = ux), boxplot.stats, 
                    coef = coef, do.out = FALSE)
    blist.stats <- t(sapply(blist, "[[", "stats"))
    blist.out <- lapply(blist, "[[", "out")
    panel.points(y = blist.stats[, 3], x = ux, pch = 16, ...)
    panel.segments(x0 = rep(ux, 2),
                   y0 = c(blist.stats[, 1], blist.stats[, 5]),
                   x1 = rep(ux, 2),
                   y1 = c(blist.stats[, 2], blist.stats[, 4]),
                   ...)
}


###################################################
### chunk number 5: 
###################################################
data(Chem97, package = "mlmRev")
bwplot(gcsescore^2.34 ~ gender | factor(score), Chem97, 
       panel = panel.bwtufte, layout = c(6, 1),
       ylab = "Transformed GCSE score")


###################################################
### chunk number 6: bwplot1
###################################################
plot(trellis.last.object())


###################################################
### chunk number 7:  eval=FALSE
###################################################
## 
## data(Chem97, package = "mlmRev")
## 
## panel.bwtufte.general <- 
##     function(x, y, box.ratio = 1, horizontal = TRUE, ..., 
##              pch = 16,
##              levels.fos = if (horizontal) sort(unique(y)) else sort(unique(x)), 
##              stats = boxplot.stats, coef = 1.5, do.out = TRUE) 
## {
##     if (all(is.na(x) | is.na(y))) return()
##     x <- as.numeric(x)
##     y <- as.numeric(y)
##     if (horizontal) 
##     {
##         blist <- 
##             tapply(x, factor(y, levels = levels.fos), 
##                    stats, coef = coef, do.out = do.out)
##         blist.stats <- t(sapply(blist, "[[", "stats"))
##         blist.out <- lapply(blist, "[[", "out")
##         panel.points(x = blist.stats[, 3],
##                      y = levels.fos, 
##                      pch = pch)
##         panel.segments(x0 = blist.stats[, 1],
##                        y0 = levels.fos,
##                        x1 = blist.stats[, 2],
##                        y1 = levels.fos)
##         panel.segments(x0 = blist.stats[, 4],
##                        y0 = levels.fos,
##                        x1 = blist.stats[, 5],
##                        y1 = levels.fos)
##         panel.points(x = unlist(blist.out),
##                      y = rep(levels.fos, sapply(blist.out, length)))
##     }
##     else
##     {
##         blist <- 
##             tapply(y, factor(x, levels = levels.fos), 
##                    stats, coef = coef, do.out = do.out)
##         blist.stats <- t(sapply(blist, "[[", "stats"))
##         blist.out <- lapply(blist, "[[", "out")
##         panel.points(y = blist.stats[, 3],
##                      x = levels.fos, 
##                      pch = pch)
##         panel.segments(y0 = blist.stats[, 1],
##                        x0 = levels.fos,
##                        y1 = blist.stats[, 2],
##                        x1 = levels.fos)
##         panel.segments(y0 = blist.stats[, 4],
##                        x0 = levels.fos,
##                        y1 = blist.stats[, 5],
##                        x1 = levels.fos)
##         panel.points(y = unlist(blist.out),
##                      x = rep(levels.fos, sapply(blist.out, length)))
##     }
## }
## 
## 
## 
## 
## 
## 
## panel.bwtufte <- 
##     function(x, y, coef = 1.5, ...) 
## {
##     if (all(is.na(x) | is.na(y))) return()
##     x <- as.numeric(x)
##     y <- as.numeric(y)
##     ux <- sort(unique(x))
##     blist <- 
##         tapply(y, factor(x, levels = ux), 
##                boxplot.stats, 
##                coef = coef, 
##                do.out = FALSE)
##     blist.stats <- t(sapply(blist, "[[", "stats"))
##     blist.out <- lapply(blist, "[[", "out")
##     panel.points(y = blist.stats[, 3], x = ux, pch = 16, ...)
##     panel.segments(x0 = rep(ux, 2),
##                    y0 = c(blist.stats[, 1], blist.stats[, 5]),
##                    x1 = rep(ux, 2),
##                    y1 = c(blist.stats[, 2], blist.stats[, 4]),
##                    ...)
## }
## 
## 
## 
## 
## 
## ## p1 <- 
## 
## ##     bwplot(factor(score) ~ gcsescore | gender, Chem97,
## ##            panel = panel.bwtufte)
## 
## 
## 
## p2 <- 
## 
##     bwplot(gcsescore^2.38 ~ gender | factor(score), Chem97, 
##            varwidth = TRUE,
##            panel = panel.bwtufte,
##            do.out = FALSE,
##            layout = c(6, 1))
## 
## 
## 


###################################################
### chunk number 1: 
###################################################
data(Cars93, package = "MASS")
cor.Cars93 <- 
    cor(Cars93[, !sapply(Cars93, is.factor)], use = "pair")
ord <- order.dendrogram(as.dendrogram(hclust(dist(cor.Cars93))))


###################################################
### chunk number 2: 
###################################################

panel.corrgram <- 
    function(x, y, z, subscripts, at, 
             level = 0.9, label = FALSE, ...) 
{
    require("ellipse", quietly = TRUE)
    x <- as.numeric(x)[subscripts]
    y <- as.numeric(y)[subscripts]
    z <- as.numeric(z)[subscripts]
    zcol <- level.colors(z, at = at, ...)
    for (i in seq(along = z)) {
        ell <- ellipse(z[i], level = level, npoints = 50,
                       scale = c(.2, .2), centre = c(x[i], y[i]))
        panel.polygon(ell, col = zcol[i], border = zcol[i], ...)
    }
    if (label) 
        panel.text(x = x, y = y, lab = 100 * round(z, 2), cex = 0.8,
                   col = ifelse(z < 0, "white", "black"))
}    



###################################################
### chunk number 3: 
###################################################

levelplot(cor.Cars93[ord, ord], at = do.breaks(c(-1.01, 1.01), 20), 
          xlab = NULL, ylab = NULL, colorkey = list(space = "top"),
          scales = list(x = list(rot = 90)),
          panel = panel.corrgram, label = TRUE)


##


###################################################
### chunk number 4: panelCorr1
###################################################
plot(trellis.last.object())


###################################################
### chunk number 5: 
###################################################


panel.corrgram.2 <- 
    function(x, y, z, subscripts, at = pretty(z), scale = 0.8, ...) 
{
    require("grid", quietly = TRUE)
    x <- as.numeric(x)[subscripts]
    y <- as.numeric(y)[subscripts]
    z <- as.numeric(z)[subscripts]
    zcol <- level.colors(z, at = at, ...)
    for (i in seq(along = z))
    {
        lims <- range(0, z[i])
        tval <- 2 * base::pi * 
            seq(from = lims[1], to = lims[2], by = 0.01)
        grid.polygon(x = x[i] + .5 * scale * c(0, sin(tval)),
                     y = y[i] + .5 * scale * c(0, cos(tval)),
                     default.units = "native", 
                     gp = gpar(fill = zcol[i]))
        grid.circle(x = x[i], y = y[i], r = .5 * scale, 
                    default.units = "native")
    }
}


###################################################
### chunk number 6: 
###################################################

levelplot(cor.Cars93[ord, ord], xlab = NULL, ylab = NULL,
          at = do.breaks(c(-1.01, 1.01), 101),
          panel = panel.corrgram.2, 
          scales = list(x = list(rot = 90)),
          colorkey = list(space = "top"),
          col.regions = colorRampPalette(c("red", "white", "blue")))


##


###################################################
### chunk number 7: panelCorr2Color
###################################################
plot(trellis.last.object())


###################################################
### chunk number 1: 
###################################################

panel.3d.contour <- 
    function(x, y, z, rot.mat, distance, 
             nlevels = 20, zlim.scaled, ...)
{
    add.line <- trellis.par.get("add.line")
    panel.3dwire(x, y, z, rot.mat, distance, 
                 zlim.scaled = zlim.scaled, ...)
    clines <- 
        contourLines(x, y, matrix(z, nrow = length(x), byrow = TRUE),
                     nlevels = nlevels)
    for (ll in clines) {
        m <- ltransform3dto3d(rbind(ll$x, ll$y, zlim.scaled[2]), 
                              rot.mat, distance)
        panel.lines(m[1,], m[2,], col = add.line$col,
                    lty = add.line$lty, lwd = add.line$lwd)
    }
}


###################################################
### chunk number 2: 
###################################################

wireframe(volcano, zlim = c(90, 250), nlevels = 10, 
          aspect = c(61/87, .3), panel.aspect = 0.6,
          panel.3d.wireframe = "panel.3d.contour", shade = TRUE,
          screen = list(z = 20, x = -60))

## par.settings = list(add.line = list(col = "black")))



###################################################
### chunk number 3: wireContour
###################################################
plot(trellis.last.object())


###################################################
### chunk number 1: 
###################################################
library("maps")
county.map <- map("county", plot = FALSE, fill = TRUE)


###################################################
### chunk number 2: 
###################################################
str(county.map)


###################################################
### chunk number 3: 
###################################################
data(ancestry, package = "latticeExtra")

ancestry <- subset(ancestry, !duplicated(county))
rownames(ancestry) <- ancestry$county
## str(ancestry)



###################################################
### chunk number 4: 
###################################################
freq <- table(ancestry$top)
keep <- names(freq)[freq > 10]
ancestry$mode <-
    with(ancestry,
         factor(ifelse(top %in% keep, top, "Other")))
modal.ancestry <- ancestry[county.map$names, "mode"]


###################################################
### chunk number 5: 
###################################################

library("RColorBrewer")
colors <- brewer.pal(n = nlevels(ancestry$mode), name = "Pastel1")
xyplot(y ~ x, county.map, aspect = "iso", 
       scales = list(draw = FALSE), xlab = "", ylab = "",
       par.settings = list(axis.line = list(col = "transparent")),
       col = colors[modal.ancestry], border = NA, 
       panel = panel.polygon,
       key =
       list(text = list(levels(modal.ancestry), adj = 1),
            rectangles = list(col = colors),
            x = 1, y = 0, corner = c(1, 0)))



###################################################
### chunk number 6: ancestry2d
###################################################
plot(trellis.last.object())


###################################################
### chunk number 7: 
###################################################

rad <- function(x) { pi * x / 180 }
county.map$xx <- with(county.map, cos(rad(x)) * cos(rad(y)))
county.map$yy <- with(county.map, sin(rad(x)) * cos(rad(y)))
county.map$zz <- with(county.map, sin(rad(y)))

panel.3dpoly <- function (x, y, z, rot.mat = diag(4), distance, ...) 
{
    m <- ltransform3dto3d(rbind(x, y, z), rot.mat, distance)
    panel.polygon(x = m[1, ], y = m[2, ], ...)
}


###################################################
### chunk number 8: 
###################################################
aspect <-
    with(county.map,
         c(diff(range(yy, na.rm = TRUE)), 
           diff(range(zz, na.rm = TRUE))) /
         diff(range(xx, na.rm = TRUE)))

cloud(zz ~ xx * yy, county.map, par.box = list(col = "grey"),
      aspect = aspect, panel.aspect = 0.6, lwd = 0.01,
      panel.3d.cloud = panel.3dpoly, col = colors[modal.ancestry],
      
      screen = list(z = 10, x = -30),
      key = list(text = list(levels(modal.ancestry), adj = 1),
                 rectangles = list(col = colors), 
                 space = "top", columns = 4),
      scales = list(draw = FALSE), zoom = 1.1, 
      xlab = "", ylab = "", zlab = "")



###################################################
### chunk number 9: ancestry3d
###################################################
plot(trellis.last.object())


###################################################
### chunk number 10:  eval=FALSE
###################################################
## 
## library("maps")
## state.map <- map("state", plot=FALSE, fill = FALSE)
## 
## panel.3dmap <- function(..., rot.mat, distance, xlim, ylim, zlim,
##                         xlim.scaled, ylim.scaled, zlim.scaled) 
## {
##     scale.vals <- function(x, original, scaled) {
##         scaled[1] + (x-original[1]) * diff(scaled) / diff(original)
##     }
##     scaled.map <- rbind(scale.vals(state.map$x, xlim, xlim.scaled),
##                         scale.vals(state.map$y, ylim, ylim.scaled),
##                         zlim.scaled[1])
##     m <- ltransform3dto3d(scaled.map, rot.mat, distance)
##     panel.lines(m[1,], m[2,], col = "grey76")
## }
## 
## ##


###################################################
### chunk number 11:  eval=FALSE
###################################################
## 
## 
## cloud(density ~ long + lat, state.info,
##       subset = !(name %in% c("Alaska", "Hawaii")),
##       panel.3d.cloud = function(...) {
##           panel.3dmap(...)
##           panel.3dscatter(...)
##       }, 
##       type = "h", scales = list(draw = FALSE), zoom = 1.1,
##       xlim = state.map$range[1:2], ylim = state.map$range[3:4],
##       xlab = NULL, ylab = NULL, zlab = NULL,
##       aspect = c(diff(state.map$range[3:4]) / 
##                  diff(state.map$range[1:2]), 0.3),
##       panel.aspect = 0.75, lwd = 2, screen = list(z = 30, x = -60),
##       par.settings = 
##       list(axis.line = list(col = "transparent"),
##            box.3d = list(col = "transparent", alpha = 0)))
## 
## 
## ##


###################################################
### chunk number 12: 
###################################################

library("latticeExtra")
library("mapproj")
data(USCancerRates)

rng <- with(USCancerRates, 
            range(rate.male, rate.female, finite = TRUE))
nbreaks <- 50
breaks <- exp(do.breaks(log(rng), nbreaks))

mapplot(rownames(USCancerRates) ~ rate.male + rate.female,
        data = USCancerRates, breaks = breaks,
        map = map("county", plot = FALSE, fill = TRUE, 
                  projection = "tetra"),
        scales = list(draw = FALSE), xlab = "", 
        main = "Average yearly deaths due to cancer per 100000")


## data(USCancerRates, package = "latticeExtra")

## rng <- with(USCancerRates, range(rate.male, rate.female, finite = TRUE))
## nbreaks <- 50
## breaks <- exp(do.breaks(log(rng), nbreaks))

## male.int <-
##     cut(USCancerRates[county.map$names, "rate.male"],
##         breaks = breaks, labels = FALSE, include.lowest = TRUE)

## female.int <-
##     cut(USCancerRates[county.map$names, "rate.female"],
##         breaks = breaks, labels = FALSE, include.lowest = TRUE)

## panel.rateMap <-
##     function(x, y, colramp = grey.colors, ...)
## {
##     col.regions <- colramp(nbreaks)
##     which <- if (panel.number() == 1) "Male" else "Female"
##     col <-
##         switch(which,
##                Male = col.regions[male.int],
##                Female = col.regions[female.int])
##     panel.polygon(x, y, col = col, ...)
##     panel.text(x = max(x, na.rm = TRUE),
##                y = min(y, na.rm = TRUE),
##                lab = which, adj = c(1, 0))
## }


## library(RColorBrewer)
## colgrad <- colorRampPalette(brewer.pal(n = 11, name = "Spectral"))

## ## colgrad <- function(n) grey.colors(n, gamma = 0.4)

## cancerDeaths <- 
##     xyplot(y ~ x, county.map, aspect = "iso",
##            scales = list(draw = FALSE), xlab = "", ylab = "",
##            ## par.settings = list(axis.line = list(col = "transparent")),
##            colramp = colgrad, panel = panel.rateMap, lwd = 0.01, 
##            legend = 
##            list(right = 
##                 list(fun = draw.colorkey,
##                      args = list(key = list(col = colgrad(nbreaks),
##                                  at = breaks), 
##                      draw = FALSE))))

## cancerDeaths[c(1, 1)]



###################################################
### chunk number 13: cancerDeaths
###################################################
trellis.par.set(standard.theme("pdf"))
plot(trellis.last.object())
## grid::grid.rect()


