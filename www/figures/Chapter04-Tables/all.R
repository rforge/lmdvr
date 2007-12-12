###################################################
### chunk number 1: 
###################################################
VADeaths


###################################################
### chunk number 2: 
###################################################
class(VADeaths)


###################################################
### chunk number 3: 
###################################################
methods("dotplot")


###################################################
### chunk number 4: 
###################################################
dotplot(VADeaths, groups = FALSE)


###################################################
### chunk number 5: vadeath1
###################################################
plot(trellis.last.object())


###################################################
### chunk number 6: 
###################################################
dotplot(VADeaths, groups = FALSE, 
        layout = c(1, 4), aspect = 0.7, 
        origin = 0, type = c("p", "h"),
        main = "Death Rates in Virginia - 1940", 
        xlab = "Rate (per 1000)")



###################################################
### chunk number 7: vadeath2
###################################################
plot(trellis.last.object())



###################################################
### chunk number 8: 
###################################################
dotplot(VADeaths, type = "o",
        auto.key = list(lines = TRUE, space = "right"),
        main = "Death Rates in Virginia - 1940",
        xlab = "Rate (per 1000)")


###################################################
### chunk number 9: vadeath3
###################################################
plot(trellis.last.object())


###################################################
### chunk number 1: 
###################################################
barchart(VADeaths, groups = FALSE,
         layout = c(1, 4), aspect = 0.7, reference = FALSE, 
         main = "Death Rates in Virginia - 1940",
         xlab = "Rate (per 100)")


###################################################
### chunk number 2: vadeathbar
###################################################
plot(trellis.last.object())


###################################################
### chunk number 3: 
###################################################
data(postdoc, package = "latticeExtra")


###################################################
### chunk number 4:  eval=FALSE
###################################################
## library("xtable")
## print(xtable(unclass(postdoc), 
##              label = "tab:postdoc",
##              digits = rep(0, 6)),
##       table.placement = "tb")


###################################################
### chunk number 5: 
###################################################
barchart(prop.table(postdoc, margin = 1), xlab = "Proportion",
         auto.key = list(adj = 1))


###################################################
### chunk number 6: pdprop
###################################################
plot(trellis.last.object())


###################################################
### chunk number 7: 
###################################################
dotplot(prop.table(postdoc, margin = 1), groups = FALSE, 
        xlab = "Proportion",
        par.strip.text = list(abbreviate = TRUE, minlength = 10))
##


###################################################
### chunk number 8: pddot
###################################################
plot(trellis.last.object())


###################################################
### chunk number 9: 
###################################################
dotplot(prop.table(postdoc, margin = 1), groups = FALSE, 
        index.cond = function(x, y) median(x),
        xlab = "Proportion", layout = c(1, 5), aspect = 0.6,
        scales = list(y = list(relation = "free", rot = 0)),
        prepanel = function(x, y) {
            list(ylim = levels(reorder(y, x)))
        },
        panel = function(x, y, ...) {
            panel.dotplot(x, reorder(y, x), ...)
        })
##


###################################################
### chunk number 10: pdpropOrdered
###################################################
plot(trellis.last.object())


###################################################
### chunk number 11:  eval=FALSE
###################################################
## 
## 
## dotplot(t(prop.table(postdoc, margin = 1)), groups = FALSE, xlab = "Proportion",
##         index.cond = function(x, y) median(x),
##         par.strip.text = list(abbreviate = TRUE, minlength = 10),
##         layout = c(1, 8), aspect = 0.6,
##         scales = list(y = list(relation = "free", rot = 0)),
##         prepanel = function(x, y, ...) {
##             y <- reorder(y, x)
##             list(ylim = levels(y))
##         }, 
##         panel = function(x, y, ...) {
##             y <- reorder(y, x)
##             panel.dotplot(x = x, y = y, ...)
##         })
## 
## 
## barchart(prop.table(postdoc[-8, ], margin = 1), xlab = "Proportion",
##          border = "transparent", auto.key = list(adj = 1))
## 
## 
## ## possible examples
## 
## 
## barchart(prop.table(postdoc, margin = 1), auto.key = list(adj = 1), reference = FALSE)
## ## barchart(prop.table(postdoc, margin = 1), stack = FALSE, auto.key = list(adj = 1))
## 
## barchart(prop.table(postdoc, margin = 1), groups = FALSE)
## 
## dotplot(prop.table(postdoc, margin = 1), groups = FALSE, 
##         par.strip.text = list(abbreviate = TRUE, minlength = 10))
## 
## 
## ## postdoc.df <-
## ##     as.data.frame(postdoc,
## ##                   responseName = "Proportion")
## 
## postdoc.df <-
##     as.data.frame(prop.table(postdoc, margin = 1),
##                   responseName = "Proportion")
## 
## 
## barchart(Field ~ Proportion | Reason, postdoc.df,
##          origin = 0)
## 
## barchart(reorder(Field, Proportion, function(x) x[1]) ~ Proportion | Reason, postdoc.df,
##          origin = 0)
## 
## ## order by other employment not available
## 
## barchart(reorder(Field, Proportion, function(x) x[4]) ~ Proportion | Reason, postdoc.df,
##          origin = 0)
## 
## dotplot(reorder(Field, Proportion, function(x) x[4]) ~ Proportion | Reason, postdoc.df,
##         origin = 0, as.table = TRUE,
##         type = c("p", "h"))
## 
## barchart(Reason ~ Proportion | Field, postdoc.df,
##          origin = 0)
## 
## barchart(Reason ~ Proportion, postdoc.df,
##          groups = Field, auto.key = TRUE,
##          stack = TRUE,
##          origin = 0)
## 
## 


###################################################
### chunk number 12: 
###################################################
data(Chem97, package = "mlmRev")


###################################################
### chunk number 13: 
###################################################
gcsescore.tab <- xtabs(~gcsescore + gender, Chem97)


###################################################
### chunk number 14: 
###################################################
gcsescore.df <- as.data.frame(gcsescore.tab)
gcsescore.df$gcsescore <- 
    as.numeric(as.character(gcsescore.df$gcsescore))


###################################################
### chunk number 15: 
###################################################
xyplot(Freq ~ gcsescore | gender, data = gcsescore.df, 
       type = "h", layout = c(1, 2), xlab = "Average GCSE Score")
##


###################################################
### chunk number 16: discXyplot
###################################################
plot(trellis.last.object())


###################################################
### chunk number 17: 
###################################################
score.tab <- xtabs(~score + gender, Chem97)
score.df <- as.data.frame(score.tab)
barchart(Freq ~ score | gender, score.df, origin = 0)

##


###################################################
### chunk number 18: discBarchart
###################################################
plot(trellis.last.object())


