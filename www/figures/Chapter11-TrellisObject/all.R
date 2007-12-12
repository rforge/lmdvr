###################################################
### chunk number 1: 
###################################################
methods(class = "trellis")


###################################################
### chunk number 2:  eval=FALSE
###################################################
## help("dimnames.trellis")


###################################################
### chunk number 3:  eval=FALSE
###################################################
## help("[.trellis")


###################################################
### chunk number 4: 
###################################################
methods(class = "shingle")


###################################################
### chunk number 5: 
###################################################
methods(generic.function = "barchart")


###################################################
### chunk number 6: 
###################################################

dp.uspe <- 
    dotplot(t(USPersonalExpenditure), 
            groups = FALSE, 
            index.cond = function(x, y) median(x),
            layout = c(1, 5), 
            type = c("p", "h"),
            xlab = "Expenditure (billion dollars)")

dp.uspe.log <- 
    dotplot(t(USPersonalExpenditure), 
            groups = FALSE, 
            index.cond = function(x, y) median(x),
            layout = c(1, 5), 
            scales = list(x = list(log = 2)),
            xlab = "Expenditure (billion dollars)")

##


###################################################
### chunk number 7: multiplePlots
###################################################
plot(dp.uspe,     split = c(1, 1, 2, 1), more = TRUE)
plot(dp.uspe.log, split = c(2, 1, 2, 1), more = FALSE)
##


###################################################
### chunk number 8:  eval=FALSE
###################################################
## plot(dp.uspe,     split = c(1, 1, 2, 1), more = TRUE)
## plot(dp.uspe.log, split = c(2, 1, 2, 1), more = FALSE)
## ##


###################################################
### chunk number 9: 
###################################################
state <- data.frame(state.x77, state.region, state.name)
state$state.name <- 
    with(state, reorder(reorder(state.name, Frost), 
                        as.numeric(state.region)))
dpfrost <- 
    dotplot(state.name ~ Frost | reorder(state.region, Frost),
            data = state, layout = c(1, 4),
            scales = list(y = list(relation = "free")))

##


###################################################
### chunk number 10: 
###################################################
summary(dpfrost)


###################################################
### chunk number 11: 
###################################################
plot(dpfrost, 
     panel.height = list(x = c(16, 13, 9, 12), unit = "null"))
##


###################################################
### chunk number 12: varHeight
###################################################
plot(trellis.last.object())


###################################################
### chunk number 13: 
###################################################
update(trellis.last.object(), layout = c(1, 1))[2]
##


###################################################
### chunk number 14: updateAndExtract
###################################################
plot(trellis.last.object())


###################################################
### chunk number 15: 
###################################################

npanel <- 12
rot <- list(z = seq(0, 30, length = npanel), 
            x = seq(0, -80, length = npanel))

quakeLocs <-
    cloud(depth ~ long + lat, quakes, pch = ".", cex = 1.5,
          panel = function(..., screen) {
              pn <- panel.number()
              panel.cloud(..., screen = list(z = rot$z[pn], 
                                             x = rot$x[pn]))
          },
          xlab = NULL, ylab = NULL, zlab = NULL, 
          scales = list(draw = FALSE), zlim = c(690, 30), 
          par.settings = list(axis.line = list(col="transparent")))

quakeLocs[rep(1, npanel)]



###################################################
### chunk number 16: manyClouds
###################################################
plot(trellis.last.object())


###################################################
### chunk number 17: 
###################################################
data(Chem97, package="mlmRev")
ChemQQ <- 
    qq(gender ~ gcsescore | factor(score), Chem97, 
       f.value = ppoints(100), strip = strip.custom(style = 5))
tmd(ChemQQ)
##


###################################################
### chunk number 18: tmdQQ
###################################################
plot(trellis.last.object())


###################################################
### chunk number 19: 
###################################################

library("latticeExtra")
data(biocAccess)
baxy <- xyplot(log10(counts) ~ hour | month + weekday, biocAccess,
               type = c("p", "a"), as.table = TRUE,
               pch = ".", cex = 2, col.line = "black")



###################################################
### chunk number 20: 
###################################################
dimnames(baxy)$month
dimnames(baxy)$month <- month.name[1:5]
dimnames(baxy)


###################################################
### chunk number 21: 
###################################################
useOuterStrips(baxy)

## useOuterStrips(baxy, strip = strip.custom(factor.levels = month.name[1:4]))

########

## baxy <- 
##     xyplot(log(counts) ~ hour | weekday + month, biocAccess,
##            groups = day, type = "l", col = "black", lty = 1,
##            subset = !(month %in% c("May", "Jun")))



###################################################
### chunk number 22: outerStrips
###################################################
plot(trellis.last.object())


###################################################
### chunk number 23: outerStripsColor
###################################################
trellis.par.set(canonical.theme("pdf"))
plot(trellis.last.object())


