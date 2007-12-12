source("../setup.R")
chapter <- 4


VADeaths
class(VADeaths)
methods("dotplot")



## 4.1
resizeWindow(6)
dotplot(VADeaths, groups = FALSE)
saveImage()



## 4.2
resizeWindow(12)
dotplot(VADeaths, groups = FALSE, 
        layout = c(1, 4), aspect = 0.7, 
        origin = 0, type = c("p", "h"),
        main = "Death Rates in Virginia - 1940", 
        xlab = "Rate (per 1000)")
saveImage()



## 4.3
resizeWindow(6)
dotplot(VADeaths, type = "o",
        auto.key = list(lines = TRUE, space = "right"),
        main = "Death Rates in Virginia - 1940",
        xlab = "Rate (per 1000)")
saveImage()



## 4.4
resizeWindow(12)
barchart(VADeaths, groups = FALSE,
         layout = c(1, 4), aspect = 0.7, reference = FALSE, 
         main = "Death Rates in Virginia - 1940",
         xlab = "Rate (per 100)")
saveImage()


data(postdoc, package = "latticeExtra")

## 4.5
resizeWindow(5.5)
barchart(prop.table(postdoc, margin = 1), xlab = "Proportion",
         auto.key = list(adj = 1))
saveImage()



## 4.6
resizeWindow(5)
dotplot(prop.table(postdoc, margin = 1), groups = FALSE, 
        xlab = "Proportion",
        par.strip.text = list(abbreviate = TRUE, minlength = 10))
saveImage()



## 4.7
resizeWindow(11.5)
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
saveImage()


data(Chem97, package = "mlmRev")
gcsescore.tab <- xtabs(~gcsescore + gender, Chem97)
gcsescore.df <- as.data.frame(gcsescore.tab)
gcsescore.df$gcsescore <- 
    as.numeric(as.character(gcsescore.df$gcsescore))

## 4.8
resizeWindow(5.5)
xyplot(Freq ~ gcsescore | gender, data = gcsescore.df, 
       type = "h", layout = c(1, 2), xlab = "Average GCSE Score")
saveImage()


score.tab <- xtabs(~score + gender, Chem97)
score.df <- as.data.frame(score.tab)

## 4.9
resizeWindow(4)
barchart(Freq ~ score | gender, score.df, origin = 0)
saveImage()


