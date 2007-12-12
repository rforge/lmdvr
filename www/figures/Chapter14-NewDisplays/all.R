###################################################
### chunk number 1: 
###################################################
library("latticeExtra")
xyplot(sunspot.year, aspect = "xy", 
       strip = FALSE, strip.left = TRUE,
       cut = list(number = 4, overlap = 0.05))


###################################################
### chunk number 2: sunspotXyplot
###################################################
plot(trellis.last.object())
## grid.rect()


###################################################
### chunk number 3: 
###################################################

data(biocAccess, package = "latticeExtra")
ssd <- stl(ts(biocAccess$counts[1:(24 * 30 * 2)], frequency = 24), 
           "periodic")
xyplot(ssd, xlab = "Time (Days)")



###################################################
### chunk number 4: biocStl
###################################################
plot(trellis.last.object())


###################################################
### chunk number 5: 
###################################################
library("flowViz")
data(GvHD, package = "flowCore")
densityplot(Visit ~ `FSC-H` | Patient, data = GvHD)



###################################################
### chunk number 6: flowDens
###################################################
plot(trellis.last.object())
##


###################################################
### chunk number 1: 
###################################################
library("hexbin")
data(NHANES)
hexbinplot(Hemoglobin ~ TIBC | Sex, data = NHANES, aspect = 0.8)

## , type = "g" ?


###################################################
### chunk number 2: hexbin
###################################################
print(trellis.last.object())


###################################################
### chunk number 3:  eval=FALSE
###################################################
## 
## ## xyplot.ts <- 
## ##     function(x, data = NULL, type = 'l', ...)
## ## {
## ##     stopifnot(is.null(data))
## ##     data <- as.data.frame(x)
## ##     data[["Time"]] <- as.vector(time(x))
## ##     nm <- names(data)
## ##     form <- 
## ##         as.formula(paste(paste(nm[-length(nm)], collapse = "+"), 
## ##                          nm[length(nm)], sep = "~"))
## ##     xyplot(form, data, type = type, ...)
## ## }
## 
## 
## xyplot(sunspot.year)
## xyplot(sunspot.year, aspect = "xy")
## 
## xyplot(EuStockMarkets, aspect = "xy")
## 
## xyplot(EuStockMarkets, aspect = "xy",
##        ylab = "Stock index",
##        auto.key = list(columns = 4))
## 
## 
## xyplot(EuStockMarkets, 
##        cut = list(number = 4, overlap = 0),
##        layout = c(1, 4),
##        scales = list(y = list(relation = "free", rot = 0)),
##        ylab = "Stock index",
##        auto.key = list(columns = 4))
## 
## 
## xyplot(EuStockMarkets, outer = TRUE,
##        ## cut = list(number = 4, overlap = 0),
##        aspect = "xy", 
##        ## strip.left = TRUE, strip = FALSE,
##        ## layout = c(1, 4),
##        scales = list(y = list(log = TRUE)),
##        ylab = "Stock index",
##        auto.key = list(columns = 4))
## 
## 
## ## STL
## 
## ssd <- stl(sunspot.month, "periodic")
## 
## data(biocAccess, package = "latticeExtra")
## 
## ssd <- 
##     stl(ts(biocAccess$counts[1:(24 * 30 * 2)], frequency = 24), 
##         "periodic")
## 
## 
## xyplot(ssd$time.series, 
##        outer = TRUE, 
##        layout = c(1, 3), 
##        strip = FALSE, strip.left = TRUE,
##        as.table = TRUE, 
##        scales = list(y = list(relation = "free", rot = 0)))
## 
## 
## xyplot(ssd)
## 
## 


###################################################
### chunk number 4: 
###################################################

panel.piechart <-
    function(x, y, labels = as.character(y),
             edges = 200, radius = 0.8, clockwise = FALSE,
             init.angle = if(clockwise) 90 else 0,
             density = NULL, angle = 45, 
             col = superpose.polygon$col,
             border = superpose.polygon$border,
             lty = superpose.polygon$lty, ...)
{
    stopifnot(require("gridBase"))
    superpose.polygon <- trellis.par.get("superpose.polygon")
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
    if (panel.number() > 1) par(new = TRUE)
    par(fig = gridFIG(), omi = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
    pie(as.numeric(x), labels = labels, edges = edges, radius = radius,
        clockwise = clockwise, init.angle = init.angle, angle = angle,
        density = density, col = col, border  = border, lty = lty)
}



###################################################
### chunk number 5: 
###################################################

piechart <- function(x, data = NULL, panel = "panel.piechart", ...)
{
    ocall <- sys.call(sys.parent())
    ocall[[1]] <- quote(piechart)
    ccall <- match.call()
    ccall$data <- data
    ccall$panel <- panel
    ccall$default.scales <- list(draw = FALSE)
    ccall[[1]] <- quote(lattice::barchart)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}



###################################################
### chunk number 6: 
###################################################
par(new = TRUE)
piechart(VADeaths, groups = FALSE, xlab = "")

## piechart(VADeaths, groups = FALSE, xlab = "",
##          key = simpleKey(rownames(VADeaths), 
##                          points = FALSE, rect = TRUE, columns = 5))

## piechart(t(VADeaths),
##          labels = NA,
##          key =
##          simpleKey(colnames(VADeaths),
##                    points = FALSE, rect = TRUE,
##                    x = 1, y = 0.75, corner = c(1, 0.5)),
##          groups = FALSE)

## 


###################################################
### chunk number 7: piechartVAD
###################################################
plot.new(); par(new = TRUE)
plot(trellis.last.object(), newpage = FALSE)


