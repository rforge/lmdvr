source("../setup.R")
chapter <- 2

data(Oats, package = "MEMSS")
tp1.oats <- 
    xyplot(yield ~ nitro | Variety + Block, data = Oats, type = 'o')

## 2.1
resizeWindow(12)
print(tp1.oats)
saveImage()

dim(tp1.oats)
dimnames(tp1.oats)
xtabs(~Variety + Block, data = Oats)
summary(tp1.oats)
summary(tp1.oats[, 1])

## 2.2
resizeWindow(6)
print(tp1.oats[, 1])
saveImage()

## 2.3
resizeWindow(12)
update(tp1.oats, 
       aspect="xy")
saveImage()

## 2.4
resizeWindow(12)
update(tp1.oats, aspect = "xy",
       layout = c(0, 18))
saveImage()

## 2.5
resizeWindow(12)
update(tp1.oats, aspect = "xy", layout = c(0, 18), 
       between = list(x = c(0, 0, 0.5), y = 0.5))
saveImage()


## 2.6
resizeWindow(12.5)
dotplot(variety ~ yield | site, barley, 
        layout = c(1, 6), aspect = c(0.7),
        groups = year, auto.key = list(space = 'right'))
saveImage()

key.variety <- 
    list(space = "right", text = list(levels(Oats$Variety)),
         points = list(pch = 1:3, col = "black"))

## 2.7
resizeWindow(4.4)
xyplot(yield ~ nitro | Block, Oats, aspect = "xy", type = "o", 
       groups = Variety, key = key.variety, lty = 1, pch = 1:3, 
       col.line = "darkgrey", col.symbol = "black",
       xlab = "Nitrogen concentration (cwt/acre)",
       ylab = "Yield (bushels/acre)", 
       main = "Yield of three varieties of oats",
       sub = "A 3 x 4 split plot experiment with 6 blocks")
saveImage()

## 2.8
resizeWindow(5)
barchart(Class ~ Freq | Sex + Age, data = as.data.frame(Titanic), 
         groups = Survived, stack = TRUE, layout = c(4, 1),
         auto.key = list(title = "Survived", columns = 2))
saveImage()

## 2.9
resizeWindow(4.4)
barchart(Class ~ Freq | Sex + Age, data = as.data.frame(Titanic), 
         groups = Survived, stack = TRUE, layout = c(4, 1), 
         auto.key = list(title = "Survived", columns = 2),
         scales = list(x = "free"))
saveImage()

bc.titanic <- 
    barchart(Class ~ Freq | Sex + Age, as.data.frame(Titanic), 
             groups = Survived, stack = TRUE, layout = c(4, 1),
             auto.key = list(title = "Survived", columns = 2),
             scales = list(x = "free"))


## 2.10
resizeWindow(4.4)
update(bc.titanic, 
       panel = function(...) {
           panel.grid(h = 0, v = -1)
           panel.barchart(...)
       })
saveImage()

## 2.11
resizeWindow(4.4)
update(bc.titanic, 
       panel = function(..., border) {
           panel.barchart(..., border = "transparent")
       })
saveImage()


