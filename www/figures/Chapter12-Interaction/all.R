###################################################
### chunk number 1: 
###################################################
state <- data.frame(state.x77, state.region)


###################################################
### chunk number 2:  eval=FALSE
###################################################
## current.vpTree()


###################################################
### chunk number 3: 
###################################################
trellis.vpname("xlab", prefix = "plot1")
trellis.vpname("strip", column = 2, row = 2, prefix = "plot2")


###################################################
### chunk number 4:  eval=FALSE
###################################################
## trellis.focus("panel", column = 2, row = 1)


###################################################
### chunk number 5:  eval=FALSE
###################################################
## trellis.focus()


###################################################
### chunk number 6:  eval=FALSE
###################################################
## trellis.unfocus()


###################################################
### chunk number 7:  eval=FALSE
###################################################
## data(Chem97, package = "mlmRev")
##      
## qqmath(~ gcsescore | factor(score), Chem97, groups = gender,
##        f.value = function(n) ppoints(100),
##        aspect = "xy", 
##        page = function(n) {
##            cat("Click on plot to place legend", fill = TRUE)
##            ll <- grid.locator(unit = "npc")
##            if (!is.null(ll))
##                draw.key(simpleKey(levels(factor(Chem97$gender))),
##                         vp = viewport(x = ll$x, y = ll$y),
##                         draw = TRUE)
##        })


###################################################
### chunk number 8:  eval=FALSE
###################################################
##   
## data(Chem97, package = "mlmRev")
##      
## qqmath(~ gcsescore | factor(score), Chem97, groups = gender,
##        f.value = function(n) ppoints(100),
##        aspect = "xy", 
##        page = function(n) {
##            cat("Click on plot to place legend", fill = TRUE)
##            ll <- grid.locator(unit = "npc")
##            if (!is.null(ll))
##                draw.key(simpleKey(levels(factor(Chem97$gender))),
##                         vp = viewport(x = ll$x, y = ll$y),
##                         draw = TRUE)
##        })
## 
## 
## 
## dev.print(device = pdf, file = "interactiveLegend.pdf",
##           width = 9, height = 7, paper = "special")
## 
## dev.print(device = postscript, file = "interactiveLegend.eps",
##           width = 9, height = 7, paper = "special", horizontal = FALSE)
##   
## 


###################################################
### chunk number 9:  eval=FALSE
###################################################
## 
## state <- data.frame(state.x77, state.region)
## xyplot(Murder ~ Life.Exp | state.region, data = state, 
##        layout = c(2, 2), type = c("p", "g"), subscripts = TRUE)
## while (!is.null(fp <- trellis.focus())) {
##     if (fp$col > 0 & fp$row > 0)
##         panel.identify(labels = rownames(state))
## }
## 


###################################################
### chunk number 10:  eval=FALSE
###################################################
## 
## height <- 9
## trellis.device(color = FALSE, width = 9, height = height)
## state <- data.frame(state.x77, state.region)
## xyplot(Murder ~ Life.Exp | state.region, data = state, 
##        layout = c(2, 2), type = c("p", "g"), subscripts = TRUE)
## while (!is.null(fp <- trellis.focus())) {
##     if (fp$col > 0 & fp$row > 0)
##         panel.identify(labels = rownames(state))
## }
## 
## dev.print(pdf, file = "panelIdentify.pdf",
##           width = 9, height = height, paper = "special")
## 
## dev.print(device = postscript, file = "panelIdentify.eps",
##           width = 9, height = height, paper = "special", horizontal = FALSE)
## 


###################################################
### chunk number 11:  eval=FALSE
###################################################
## 
## qqmath(~ (1000 * Population / Area), state, 
##        ylab = "Population Density (per square mile)",
##        xlab = "Standard Normal Quantiles",
##        scales = list(y = list(log = TRUE, at = 10^(0:3))))
## trellis.focus()
## do.call(panel.qqmathline, trellis.panelArgs())
## panel.identify.qqmath(labels = row.names(state))
## trellis.unfocus()
## 


###################################################
### chunk number 12:  eval=FALSE
###################################################
## 
## height <- 11
## trellis.device(color = FALSE, width = 9, height = height)
## state <- data.frame(state.x77, state.region)
## qqmath(~ (1000 * Population / Area), state, 
##        ylab = "Population Density (per square mile)",
##        xlab = "Standard Normal Quantiles",
##        scales = list(y = list(log = TRUE, at = 10^(0:3))))
## trellis.focus()
## do.call(panel.qqmathline, trellis.panelArgs())
## panel.identify.qqmath(labels = row.names(state))
## trellis.unfocus()
## 
## dev.print(pdf, file = "qqmathIdentify.pdf",
##           width = 9, height = height, paper = "special")
## 
## dev.print(device = postscript, file = "qqmathIdentify.eps",
##           width = 9, height = height, paper = "special", horizontal = FALSE)
## 


###################################################
### chunk number 13:  eval=FALSE
###################################################
## env <- environmental
## env$ozone <- env$ozone^(1/3)
## splom(env, pscales = 0, col = "grey")
## trellis.focus("panel", 1, 1, highlight = FALSE)
## panel.link.splom(pch = 16, col = "black")
## trellis.unfocus()


###################################################
### chunk number 14:  eval=FALSE
###################################################
## 
## height <- 9
## trellis.device(color = FALSE, width = 9, height = height)
## 
## env <- environmental
## env$ozone <- env$ozone^(1/3)
## splom(env, pscales = 0, col = "grey")
## trellis.focus("panel", 1, 1, highlight = FALSE)
## panel.link.splom <- panel.brush.splom
## panel.link.splom(pch = 1, col = "black")
## trellis.unfocus()
## 
## dev.print(pdf, file = "splomLink.pdf",
##           width = 9, height = height, paper = "special")
## 
## dev.print(device = postscript, file = "splomLink.eps",
##           width = 9, height = height, paper = "special", horizontal = FALSE)
## 
## 


###################################################
### chunk number 15: 
###################################################

state$name <- with(state, 
                   reorder(reorder(factor(rownames(state)), Frost), 
                           as.numeric(state.region)))

dotplot(name ~ Frost | reorder(state.region, Frost), data = state, 
        layout = c(1, 4), scales = list(y = list(relation="free")))


###################################################
### chunk number 16: varHeightInteractive1
###################################################
plot(trellis.last.object())
##


###################################################
### chunk number 17: 
###################################################
trellis.currentLayout()


###################################################
### chunk number 18: 
###################################################

heights <- 
    sapply(seq_len(nrow(trellis.currentLayout())),
           function(i) {
               trellis.focus("panel", column = 1, row = i, 
                             highlight = FALSE)
               h <- diff(current.panel.limits()$ylim)
               trellis.unfocus()
               h
           })
heights


###################################################
### chunk number 19: 
###################################################
update(trellis.last.object(), 
       par.settings = list(layout.heights = list(panel = heights)))

##


###################################################
### chunk number 20: varHeightInteractive2
###################################################
plot(trellis.last.object())
##


