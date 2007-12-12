###################################################
### chunk number 1: oat
###################################################
data(Oats, package = "MEMSS")
tp1.oats <- 
    xyplot(yield ~ nitro | Variety + Block, data = Oats, type = 'o')
## for (nm in c("Block", "Variety"))
## {
##     Oats[[nm]] <- factor(as.character(Oats[[nm]]))
## }
##


###################################################
### chunk number 2: oats1
###################################################
print(tp1.oats)


###################################################
### chunk number 3: dim1
###################################################
dim(tp1.oats)
dimnames(tp1.oats)


###################################################
### chunk number 4: 
###################################################
xtabs(~Variety + Block, data = Oats)


###################################################
### chunk number 5: 
###################################################
summary(tp1.oats)


###################################################
### chunk number 6: extract1
###################################################
summary(tp1.oats[, 1])


###################################################
### chunk number 7: oats2
###################################################
print(tp1.oats[, 1])


###################################################
### chunk number 8: 
###################################################
update(tp1.oats, 
       aspect="xy")


###################################################
### chunk number 9: oats3
###################################################
print(trellis.last.object())


###################################################
### chunk number 10:  eval=FALSE
###################################################
## t(tp1.oats)


###################################################
### chunk number 11: 
###################################################
update(tp1.oats, aspect = "xy",
       layout = c(0, 18))


###################################################
### chunk number 12: oats4
###################################################
print(trellis.last.object())


###################################################
### chunk number 13: 
###################################################
update(tp1.oats, aspect = "xy", layout = c(0, 18), 
       between = list(x = c(0, 0, 0.5), y = 0.5))


###################################################
### chunk number 14: oats5
###################################################
print(trellis.last.object())


###################################################
### chunk number 1: 
###################################################


###################################################
### chunk number 3:  eval=FALSE
###################################################
dotplot(variety ~ yield | site, barley, 
        layout = c(1, 6), aspect = c(0.7),
        groups = year, auto.key = list(space = 'right'))


###################################################
### chunk number 1: initialize
###################################################

Titanic1 <- as.data.frame(as.table(Titanic[, , "Adult", ]))

Titanic2 <- 
    reshape(Titanic1, 
            direction = "wide",
            v.names = "Freq", 
            idvar = c("Class", "Sex"), 
            timevar = "Survived")

names(Titanic2) <- c("Class", "Sex", "Dead", "Alive")


p2 <- 
    barchart(Class ~ Dead + Alive | Sex, 
             Titanic2, 
             stack = TRUE, 
             auto.key = list(columns = 2))

## options(width = 80)



###################################################
### chunk number 2: 
###################################################
Titanic1 <- as.data.frame(as.table(Titanic[, , "Adult" ,])) 
Titanic1


###################################################
### chunk number 3: 
###################################################
barchart(Class ~ Freq | Sex, Titanic1, 
         groups = Survived, 
         stack = TRUE, 
         auto.key = list(title = "Survived", columns = 2))


###################################################
### chunk number 4: titanic1
###################################################
print(trellis.last.object())


###################################################
### chunk number 5: 
###################################################
Titanic2


###################################################
### chunk number 6: titanic2
###################################################
print(p2)


###################################################
### chunk number 7:  eval=FALSE
###################################################
## barchart(Class ~ Dead + Alive | Sex, 
##          Titanic2, 
##          stack = TRUE, 
##          auto.key = list(columns = 2))


###################################################
### chunk number 8:  eval=FALSE
###################################################
## 
## barchart(Class ~ Freq | Sex, 
##          as.data.frame(Titanic),
##          subset = (Age == "Adult"),
##          groups = Survived, stack = TRUE,
##          auto.key = list(title = "Survived", columns = 2))
## 


###################################################
### chunk number 9:  eval=FALSE
###################################################
## 
## barchart(Class ~ Freq | Sex + Age, 
##          as.data.frame(Titanic),
##          subset = (Age == "Adult"),
##          groups = Survived,
##          stack = TRUE,
##          auto.key = list(title = "Survived", columns = 2))
## 


###################################################
### chunk number 10:  eval=FALSE
###################################################
## 
## barchart(Class ~ Freq | Sex + Age, 
##          as.data.frame(Titanic),
##          subset = (Age == "Adult"),
##          groups = Survived,
##          stack = TRUE,
##          auto.key = list(title = "Survived", columns = 2),
##          drop.unused.levels = FALSE)
## 


###################################################
### chunk number 11: 
###################################################
set.seed(20051028)


###################################################
### chunk number 12: 
###################################################
x1 <- rexp(2000)
x1 <- x1[x1 > 1]
x2 <- rexp(1000)


###################################################
### chunk number 13: 
###################################################
mg1 <- 
    qqmath(~ data, make.groups(x1, x2), 
           distribution = qexp, 
           groups = which, 
           aspect = "iso", 
           type = c('p', 'g'))
y1 <- rnorm(2000)
y1 <- y1[y1 > -1]
y2 <- rnorm(1000)
mg2 <- 
    qqmath(~ data, make.groups(y1, y2), 
           groups = which, 
           aspect = "iso", 
           type = c('p', 'g'))



###################################################
### chunk number 14: makegroups
###################################################
print(mg1, split = c(1, 1, 2, 1), more = TRUE)
print(mg2, split = c(2, 1, 2, 1), more = FALSE)


###################################################
### chunk number 15:  eval=FALSE
###################################################
## qqmath(~ x1 + x2, distribution = qexp)


###################################################
### chunk number 16:  eval=FALSE
###################################################
## qqmath( ~ c(x1, x2), distribution = qexp, 
##        groups = rep(c('x1', 'x2'), c(length(x1), length(x2))))


###################################################
### chunk number 17: 
###################################################
str(make.groups(x1, x2))


###################################################
### chunk number 18:  eval=FALSE
###################################################
## qqmath(~ data, make.groups(x1, x2), 
##        distribution = qexp, 
##        groups = which, 
##        aspect = "iso", 
##        type = c('p', 'g'))


###################################################
### chunk number 1: initialize
###################################################

data(Oats, package = "MEMSS")

## str(simpleKey(levels(Oats$Block)))

key.variety <- 
    list(space = "right", text = list(levels(Oats$Variety)),
         points = list(pch = 1:3, col = "black"))

tp1.oats <- 
xyplot(yield ~ nitro | Block, Oats, aspect = "xy", type = "o", 
       groups = Variety, key = key.variety, lty = 1, pch = 1:3, 
       col.line = "darkgrey", col.symbol = "black",
       xlab = "Nitrogen concentration (cwt/acre)",
       ylab = "Yield (bushels/acre)", 
       main = "Yield of three varieties of oats",
       sub = "A 3 x 4 split plot experiment with 6 blocks")

## 


###################################################
### chunk number 2: oatsGrouped
###################################################
print(tp1.oats)


###################################################
### chunk number 3:  eval=FALSE
###################################################
## key.variety <- 
##     list(space = "right", text = list(levels(Oats$Variety)),
##          points = list(pch = 1:3, col = "black"))
## xyplot(yield ~ nitro | Block, Oats, aspect = "xy", type = "o", 
##        groups = Variety, key = key.variety, lty = 1, pch = 1:3, 
##        col.line = "darkgrey", col.symbol = "black",
##        xlab = "Nitrogen concentration (cwt/acre)",
##        ylab = "Yield (bushels/acre)", 
##        main = "Yield of three varieties of oats",
##        sub = "A 3 x 4 split plot experiment with 6 blocks")
##        


###################################################
### chunk number 4: 
###################################################
barchart(Class ~ Freq | Sex + Age, data = as.data.frame(Titanic), 
         groups = Survived, stack = TRUE, layout = c(4, 1),
         auto.key = list(title = "Survived", columns = 2))


###################################################
### chunk number 5: titanicSame
###################################################
print(trellis.last.object())


###################################################
### chunk number 6: 
###################################################
barchart(Class ~ Freq | Sex + Age, data = as.data.frame(Titanic), 
         groups = Survived, stack = TRUE, layout = c(4, 1), 
         auto.key = list(title = "Survived", columns = 2),
         scales = list(x = "free"))

## 


###################################################
### chunk number 7: titanicFree
###################################################
plot(trellis.last.object())


###################################################
### chunk number 1: 
###################################################
bc.titanic <- 
    barchart(Class ~ Freq | Sex + Age, as.data.frame(Titanic), 
             groups = Survived, stack = TRUE, layout = c(4, 1),
             auto.key = list(title = "Survived", columns = 2),
             scales = list(x = "free"))
## 


###################################################
### chunk number 2:  eval=FALSE
###################################################
## bc.titanic


###################################################
### chunk number 3:  eval=FALSE
###################################################
## update(bc.titanic, panel = panel.barchart)


###################################################
### chunk number 4:  eval=FALSE
###################################################
## update(bc.titanic, 
##        panel = function(...) {
##            panel.barchart(...)
##        })


###################################################
### chunk number 5: 
###################################################
update(bc.titanic, 
       panel = function(...) {
           panel.grid(h = 0, v = -1)
           panel.barchart(...)
       })


###################################################
### chunk number 6: titanicWithGrid
###################################################
plot(trellis.last.object())


###################################################
### chunk number 7: 
###################################################
update(bc.titanic, 
       panel = function(..., border) {
           panel.barchart(..., border = "transparent")
       })


###################################################
### chunk number 8: titanicNoBorder
###################################################
plot(trellis.last.object())


###################################################
### chunk number 9:  eval=FALSE
###################################################
## update(bc.titanic, border = "transparent")


