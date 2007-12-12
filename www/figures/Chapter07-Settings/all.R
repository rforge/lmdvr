###################################################
### chunk number 1: 
###################################################
vad.plot <- 
    dotplot(reorder(Var2, Freq) ~ Freq | Var1,
            data = as.data.frame.table(VADeaths), 
            origin = 0, type = c("p", "h"),
            main = "Death Rates in Virginia - 1940", 
            xlab = "Number of deaths per 100")
vad.plot


###################################################
### chunk number 2: dev1
###################################################
print(trellis.last.object())


###################################################
### chunk number 3: 
###################################################
dot.line.settings <- trellis.par.get("dot.line")
str(dot.line.settings)


###################################################
### chunk number 4: 
###################################################
dot.line.settings$col <- "transparent"
trellis.par.set("dot.line", dot.line.settings)


###################################################
### chunk number 5: 
###################################################
plot.line.settings <- trellis.par.get("plot.line")
str(plot.line.settings)
plot.line.settings$lwd <- 2
trellis.par.set("plot.line", plot.line.settings)


###################################################
### chunk number 6: 
###################################################
vad.plot


###################################################
### chunk number 7: dev2
###################################################
plot(trellis.last.object())


###################################################
### chunk number 8: 
###################################################

panel.dotline <- 
    function(x, y, 
             col = dot.symbol$col, pch = dot.symbol$pch,
             cex = dot.symbol$cex, alpha = dot.symbol$alpha,
             col.line = plot.line$col, lty = plot.line$lty,
             lwd = plot.line$lwd, alpha.line = plot.line$alpha,
             ...)
{
    dot.symbol <- trellis.par.get("dot.symbol")
    plot.line <- trellis.par.get("plot.line")
    panel.segments(0, y, x, y, col = col.line, lty = lty, 
                   lwd = lwd, alpha = alpha.line)
    panel.points(x, y, col = col, pch = pch, cex = cex, alpha = alpha)
}



###################################################
### chunk number 9:  eval=FALSE
###################################################
## update(vad.plot, panel = panel.dotline)


###################################################
### chunk number 10: 
###################################################
trellis.par.set(dot.line = dot.line.settings,
                plot.line = plot.line.settings)


###################################################
### chunk number 11: 
###################################################
trellis.par.set(dot.line = list(col = "transparent"),
                plot.line = list(lwd = 2))


###################################################
### chunk number 12: 
###################################################
trellis.par.set(list(dot.line = list(col = "transparent"),
                     plot.line = list(lwd = 2)))


###################################################
### chunk number 13: 
###################################################
update(vad.plot, 
       par.settings = list(dot.line = list(col = "transparent"),
                           plot.line = list(lwd = 2)))
##


###################################################
### chunk number 1: init
###################################################

## Trying to programmatically create a list of parameters

tp <- trellis.par.get()

## known to be `non-graphical' or too general

unusual <- 
    c("grid.pars", "fontsize", "clip", 
      "axis.components", 
      "layout.heights", "layout.widths")

for (u in unusual) tp[[u]] <- NULL

## remaining names

names.tp <- lapply(tp, names)
unames <- sort(unique(unlist(names.tp)))

ans <- matrix(0, nrow = length(names.tp), ncol = length(unames))
rownames(ans) <- names(names.tp)
colnames(ans) <- unames

for (i in seq(along = names.tp))
    ans[i, ] <- as.numeric(unames %in% names.tp[[i]])
ans <- ans[, order(-colSums(ans))]
ans <- ans[order(rowSums(ans)), ]
ans[ans == 0] <- NA

levelplot(t(ans), colorkey = FALSE, 
          scales = list(x = list(rot = 90)),
          panel = function(x, y, z, ...) {
              panel.abline(v = unique(as.numeric(x)), 
                           h = unique(as.numeric(y)), 
                           col = "darkgrey")
              panel.xyplot(x, y, pch = 16 * z, ...)
          },
          xlab = "Graphical parameters", 
          ylab = "Setting names")


#


###################################################
### chunk number 2:  eval=FALSE
###################################################
## names(trellis.par.get())


###################################################
### chunk number 3: gparams
###################################################
plot(trellis.last.object())


###################################################
### chunk number 4: showSettings
###################################################
invisible(require("grid", quietly = TRUE))
##pushViewport(viewport(angle = 90))
##lattice:::lattice.setStatus(print.more = TRUE)
show.settings()
##upViewport()
##


###################################################
### chunk number 1: 
###################################################
lattice.options(default.args = list(as.table = TRUE))


###################################################
### chunk number 2: 
###################################################
old.prompt <- getOption("prompt")
old.continue <- getOption("continue")
options(prompt = " ", continue = " ")


###################################################
### chunk number 3:  eval=FALSE
###################################################
## lattice.options(default.args = list(as.table = TRUE))
## lattice.options(lattice.theme = standard.theme("pdf"))


###################################################
### chunk number 4:  eval=FALSE
###################################################
##   
## setHook(packageEvent("lattice", "attach"),
##         function(...) {
##             lattice.options(default.args = list(as.table = TRUE))
##             lattice.options(default.theme = 
##                 function() {
##                     switch(EXPR = .Device, 
##                            postscript = ,
##                            pdf = standard.theme(color = FALSE),
##                            standard.theme("pdf", color = TRUE))
##                 })
##         })
## 
## ##


###################################################
### chunk number 5: 
###################################################
options(prompt = old.prompt, continue = old.continue)


