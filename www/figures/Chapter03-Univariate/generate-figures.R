source("../setup.R")
chapter <- 3


## 3.1
resizeWindow(4)
densityplot(~ eruptions, data = faithful)
saveImage()


## 3.2
resizeWindow(4)
densityplot(~ eruptions, data = faithful, 
            kernel = "rect", bw = 0.2, plot.points = "rug", n = 200)
saveImage()


library("latticeExtra")
data(gvhd10)

## 3.3
resizeWindow(7)
densityplot(~log(FSC.H) | Days, data = gvhd10, 
            plot.points = FALSE, ref = TRUE, layout = c(2, 4))
saveImage()



## 3.4
resizeWindow(7)
histogram(~log2(FSC.H) | Days, gvhd10, xlab = "log Forward Scatter",
          type = "density", nint = 50, layout = c(2, 4))
saveImage()


data(Chem97, package = "mlmRev")

## 3.5
resizeWindow(6)
qqmath(~ gcsescore | factor(score), data = Chem97, 
       f.value = ppoints(100))
saveImage()



## 3.6
resizeWindow(5)
qqmath(~ gcsescore | gender, Chem97, groups = score, aspect = "xy", 
       f.value = ppoints(100), auto.key = list(space = "right"),
       xlab = "Standard Normal Quantiles", 
       ylab = "Average GCSE Score")
saveImage()


Chem97.mod <- transform(Chem97, gcsescore.trans = gcsescore^2.34)

## 3.7
resizeWindow(5)
qqmath(~ gcsescore.trans | gender, Chem97.mod, groups = score,
       f.value = ppoints(100), aspect = "xy",
       auto.key = list(space = "right", title = "score"), 
       xlab = "Standard Normal Quantiles", 
       ylab = "Transformed GCSE Score")
saveImage()


library("latticeExtra")

## 3.8
resizeWindow(5)
ecdfplot(~ gcsescore | factor(score), data = Chem97, 
         groups = gender, auto.key = list(columns = 2),
         subset = gcsescore > 0, xlab = "Average GCSE Score")
saveImage()



## 3.9
resizeWindow(6)
qqmath(~ gcsescore | factor(score), data = Chem97, groups = gender, 
       auto.key = list(points = FALSE, lines = TRUE, columns = 2),
       subset = gcsescore > 0, type = "l", distribution = qunif, 
       prepanel = prepanel.qqmathline, aspect = "xy",
       xlab = "Standard Normal Quantiles", 
       ylab = "Average GCSE Score")
saveImage()



## 3.10
resizeWindow(7)
qq(gender ~ gcsescore | factor(score), Chem97, 
   f.value = ppoints(100), aspect = 1)
saveImage()



## 3.11
resizeWindow(4.5)
bwplot(factor(score) ~ gcsescore | gender, data = Chem97, 
       xlab = "Average GCSE Score")
saveImage()



## 3.12
resizeWindow(5.5)
bwplot(gcsescore^2.34 ~ gender | factor(score), Chem97, 
       varwidth = TRUE, layout = c(6, 1),
       ylab = "Transformed GCSE score")
saveImage()



## 3.13
resizeWindow(6)
bwplot(Days ~ log(FSC.H), data = gvhd10, 
       xlab = "log(Forward Scatter)", ylab = "Days Past Transplant")
saveImage()



## 3.14
resizeWindow(6)
bwplot(Days ~ log(FSC.H), gvhd10, 
       panel = panel.violin, box.ratio = 3,
       xlab = "log(Forward Scatter)", 
       ylab = "Days Past Transplant")
saveImage()



## 3.15
resizeWindow(5.5)
stripplot(factor(mag) ~ depth, quakes)
saveImage()



## 3.16
resizeWindow(5.5)
stripplot(depth ~ factor(mag), quakes, 
          jitter.data = TRUE, alpha = 0.6,
          xlab = "Magnitude (Richter)", ylab = "Depth (km)")
saveImage()



## 3.17
resizeWindow(5.5)
stripplot(sqrt(abs(residuals(lm(yield~variety+year+site)))) ~ site, 
          data = barley, groups = year, jitter.data = TRUE,
          auto.key = list(points = TRUE, lines = TRUE, columns = 2),
          type = c("p", "a"), fun = median,
          ylab = expression(abs("Residual Barley Yield")^{1 / 2}))
saveImage()



