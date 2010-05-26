
Figure_3.1 <- function() {
    densityplot(~ eruptions, data = faithful)
}

Figure_3.2 <- function() {
    densityplot(~ eruptions, data = faithful, 
                kernel = "rect", bw = 0.2, plot.points = "rug", n = 200)
}

Figure_3.3 <- function() {
    library("latticeExtra")
    data(gvhd10)
    densityplot(~log(FSC.H) | Days, data = gvhd10, 
                plot.points = FALSE, ref = TRUE, layout = c(2, 4))
}

Figure_3.4 <- function() {
    library("latticeExtra")
    data(gvhd10)
    histogram(~log2(FSC.H) | Days, gvhd10, xlab = "log Forward Scatter",
              type = "density", nint = 50, layout = c(2, 4))
}

Figure_3.5 <- function() {
    data(Chem97, package = "mlmRev")
    qqmath(~ gcsescore | factor(score), data = Chem97, 
           f.value = ppoints(100))
}

Figure_3.6 <- function() {
    data(Chem97, package = "mlmRev")
    qqmath(~ gcsescore | gender, Chem97, groups = score, aspect = "xy", 
           f.value = ppoints(100), auto.key = list(space = "right"),
           xlab = "Standard Normal Quantiles", 
           ylab = "Average GCSE Score")
}

Figure_3.7 <- function() {
    data(Chem97, package = "mlmRev")
    Chem97.mod <- transform(Chem97, gcsescore.trans = gcsescore^2.34)
    qqmath(~ gcsescore.trans | gender, Chem97.mod, groups = score,
           f.value = ppoints(100), aspect = "xy",
           auto.key = list(space = "right", title = "score"), 
           xlab = "Standard Normal Quantiles", 
           ylab = "Transformed GCSE Score")
}

Figure_3.8 <- function() {
    library("latticeExtra")
    data(Chem97, package = "mlmRev")
    ecdfplot(~ gcsescore | factor(score), data = Chem97, 
             groups = gender, auto.key = list(columns = 2),
             subset = gcsescore > 0, xlab = "Average GCSE Score")
}

Figure_3.9 <- function() {
    data(Chem97, package = "mlmRev")
    qqmath(~ gcsescore | factor(score), data = Chem97, groups = gender, 
           auto.key = list(points = FALSE, lines = TRUE, columns = 2),
           subset = gcsescore > 0, type = "l", distribution = qunif, 
           prepanel = prepanel.qqmathline, aspect = "xy",
           xlab = "Standard Normal Quantiles", 
           ylab = "Average GCSE Score")
}

Figure_3.10 <- function() {
    data(Chem97, package = "mlmRev")
    qq(gender ~ gcsescore | factor(score), Chem97, 
       f.value = ppoints(100), aspect = 1)
}

Figure_3.11 <- function() {
    data(Chem97, package = "mlmRev")
    bwplot(factor(score) ~ gcsescore | gender, data = Chem97, 
           xlab = "Average GCSE Score")
}

Figure_3.12 <- function() {
    data(Chem97, package = "mlmRev")
    bwplot(gcsescore^2.34 ~ gender | factor(score), Chem97, 
           varwidth = TRUE, layout = c(6, 1),
           ylab = "Transformed GCSE score")
}

Figure_3.13 <- function() {
    library("latticeExtra")
    data(gvhd10)
    bwplot(Days ~ log(FSC.H), data = gvhd10, 
           xlab = "log(Forward Scatter)", ylab = "Days Past Transplant")
}

Figure_3.14 <- function() {
    library("latticeExtra")
    data(gvhd10)
    bwplot(Days ~ log(FSC.H), gvhd10, 
           panel = panel.violin, box.ratio = 3,
           xlab = "log(Forward Scatter)", 
           ylab = "Days Past Transplant")
}

Figure_3.15 <- function() {
    stripplot(factor(mag) ~ depth, quakes)
}

Figure_3.16 <- function() {
    stripplot(depth ~ factor(mag), quakes, 
              jitter.data = TRUE, alpha = 0.6,
              xlab = "Magnitude (Richter)", ylab = "Depth (km)")
}

Figure_3.17 <- function() {
    stripplot(sqrt(abs(residuals(lm(yield~variety+year+site)))) ~ site, 
              data = barley, groups = year, jitter.data = TRUE,
              auto.key = list(points = TRUE, lines = TRUE, columns = 2),
              type = c("p", "a"), fun = median,
              ylab = expression(abs("Residual Barley Yield")^{1 / 2}))
}
