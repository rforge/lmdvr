###################################################
### chunk number 1: 
###################################################
data(Cars93, package = "MASS")


###################################################
### chunk number 2: 
###################################################
table(Cars93$Cylinders)


###################################################
### chunk number 3: 
###################################################
sup.sym <- Rows(trellis.par.get("superpose.symbol"), 1:5)
str(sup.sym)


###################################################
### chunk number 4: 
###################################################
xyplot(Price ~ EngineSize | reorder(AirBags, Price), data = Cars93, 
       groups = Cylinders, subset = Cylinders != "rotary", 
       scales = list(y = list(log = 2, tick.number = 3)), 
       xlab = "Engine Size (litres)", 
       ylab = "Average Price (1000 USD)",
       key = list(text = list(levels(Cars93$Cylinders)[1:5]), 
                  points = sup.sym, space = "right"))
##


###################################################
### chunk number 5: keyrows
###################################################
print(trellis.last.object())


###################################################
### chunk number 6:  eval=FALSE
###################################################
## xyplot(Price ~ EngineSize | reorder(AirBags, Price), data = Cars93, 
##        groups = Cylinders, subset = Cylinders != "rotary", 
##        scales = list(y = list(log = 2, tick.number = 3)), 
##        xlab = "Engine Size (litres)", 
##        ylab = "Average Price (1000 USD)",
##        key = simpleKey(text = levels(Cars93$Cylinders)[1:5], 
##                        space = "right", points = TRUE))
## #


###################################################
### chunk number 7: 
###################################################
xyplot(Price ~ EngineSize | reorder(AirBags, Price), data = Cars93, 
       groups = Cylinders, subset = Cylinders != "rotary", 
       scales = list(y = list(log = 2, tick.number = 3)), 
       xlab = "Engine Size (litres)", 
       ylab = "Average Price (1000 USD)",
       auto.key = list(text = levels(Cars93$Cylinders)[1:5], 
                       space = "right", points = TRUE))
#


###################################################
### chunk number 8:  eval=FALSE
###################################################
## xyplot(Price ~ EngineSize | reorder(AirBags, Price), 
##        data = subset(Cars93, Cylinders != "rotary"),
##        groups = Cylinders[, drop = TRUE],
##        scales = list(y = list(log = 2, tick.number = 3)), 
##        xlab = "Engine Size (litres)", 
##        ylab = "Average Price (1000 USD)",
##        auto.key = list(space = "right"))
## #


###################################################
### chunk number 1: initialize
###################################################
data(Cars93, package = "MASS")
#


###################################################
### chunk number 2: 
###################################################
my.pch <- c(21:25, 20)
my.fill <- c("transparent", "grey", "black")

with(Cars93, 
     xyplot(Price ~ EngineSize, 
            scales = list(y = list(log = 2, tick.number = 3)),
            panel = function(x, y, ..., subscripts) {
                pch <- my.pch[Cylinders[subscripts]]
                fill <- my.fill[AirBags[subscripts]]
                panel.xyplot(x, y, pch = pch, 
                             fill = fill, col = "black")
            },
            key = list(space = "right", adj = 1,
                       text = list(levels(Cylinders)), 
                       points = list(pch = my.pch), 
                       text = list(levels(AirBags)), 
                       points = list(pch = 21, fill = my.fill),
                       rep = FALSE)))

#


###################################################
### chunk number 3: complexkey
###################################################
print(trellis.last.object())


###################################################
### chunk number 4: 
###################################################
## alternative using color
my.pch <- c(21:25, 20)
my.fill <- c("orange", "skyblue", "lightgreen")

col.compkey <- 
    with(Cars93, 
         xyplot(Price ~ EngineSize, 
                scales = list(y = list(log = 2, tick.number = 3)),
                panel = function(x, y, ..., subscripts) {
                    pch <- my.pch[Cylinders[subscripts]]
                    fill <- my.fill[AirBags[subscripts]]
                    panel.xyplot(x, y, 
                                 pch = pch, fill = fill, col = fill)
                },
                key = 
                list(space = "right", 
                     text = list(levels(Cylinders)), 
                     points = list(pch = my.pch), 
                     text = list(levels(AirBags)), 
                     points = list(pch = 16, col = my.fill),
                     rep = FALSE)))

pdf("figs/ann2-compkeyCol.pdf", width=9, height=5)
plot(col.compkey)
dev.off()

#


###################################################
### chunk number 1: 
###################################################
hc1 <- hclust(dist(USArrests, method = "canberra"))
hc1 <- as.dendrogram(hc1)


###################################################
### chunk number 2: 
###################################################
ord.hc1 <- order.dendrogram(hc1)
hc2 <- reorder(hc1, state.region[ord.hc1])
ord.hc2 <- order.dendrogram(hc2)


###################################################
### chunk number 3:  eval=FALSE
###################################################
## levelplot(t(scale(USArrests))[, ord.hc2])


###################################################
### chunk number 4: 
###################################################
library(latticeExtra)
region.colors <- trellis.par.get("superpose.polygon")$col

levelplot(t(scale(USArrests))[, ord.hc2], 
          scales = list(x = list(rot = 90)),
          colorkey = FALSE,
          legend =
          list(right =
               list(fun = dendrogramGrob,
                    args =
                    list(x = hc2, ord = ord.hc2,
                         side = "right", size = 10, size.add = 0.5,
                         add = list(rect =
                           list(col = "transparent",
                              fill = region.colors[state.region])),
                         type = "rectangle"))))


##


###################################################
### chunk number 5: heatmap
###################################################
print(trellis.last.object())


