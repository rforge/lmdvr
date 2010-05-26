
Figure_2.1 <- function() {
    data(Oats, package = "MEMSS")
    tp1.oats <- 
        xyplot(yield ~ nitro | Variety + Block, data = Oats, type = 'o')
    print(tp1.oats)
}

Figure_2.2 <- function() {
    data(Oats, package = "MEMSS")
    tp1.oats <- 
        xyplot(yield ~ nitro | Variety + Block, data = Oats, type = 'o')
    print(tp1.oats[, 1])
}

Figure_2.3 <- function() {
    data(Oats, package = "MEMSS")
    tp1.oats <- 
        xyplot(yield ~ nitro | Variety + Block, data = Oats, type = 'o')
    update(tp1.oats, 
           aspect="xy")
}

Figure_2.4 <- function() {
    data(Oats, package = "MEMSS")
    tp1.oats <- 
        xyplot(yield ~ nitro | Variety + Block, data = Oats, type = 'o')
    update(tp1.oats, aspect = "xy",
           layout = c(0, 18))
}

Figure_2.5 <- function() {
    data(Oats, package = "MEMSS")
    tp1.oats <- 
        xyplot(yield ~ nitro | Variety + Block, data = Oats, type = 'o')
    update(tp1.oats, aspect = "xy", layout = c(0, 18), 
           between = list(x = c(0, 0, 0.5), y = 0.5))
}

Figure_2.6 <- function() {
    dotplot(variety ~ yield | site, barley, 
            layout = c(1, 6), aspect = c(0.7),
            groups = year, auto.key = list(space = 'right'))
}

Figure_2.7 <- function() {
    data(Oats, package = "MEMSS")
    key.variety <- 
        list(space = "right", text = list(levels(Oats$Variety)),
             points = list(pch = 1:3, col = "black"))
    xyplot(yield ~ nitro | Block, Oats, aspect = "xy", type = "o", 
           groups = Variety, key = key.variety, lty = 1, pch = 1:3, 
           col.line = "darkgrey", col.symbol = "black",
           xlab = "Nitrogen concentration (cwt/acre)",
           ylab = "Yield (bushels/acre)", 
           main = "Yield of three varieties of oats",
           sub = "A 3 x 4 split plot experiment with 6 blocks")
}

Figure_2.8 <- function() {
    barchart(Class ~ Freq | Sex + Age, data = as.data.frame(Titanic), 
             groups = Survived, stack = TRUE, layout = c(4, 1),
             auto.key = list(title = "Survived", columns = 2))
}

Figure_2.9 <- function() {
    barchart(Class ~ Freq | Sex + Age, data = as.data.frame(Titanic), 
             groups = Survived, stack = TRUE, layout = c(4, 1), 
             auto.key = list(title = "Survived", columns = 2),
             scales = list(x = "free"))
}

Figure_2.10 <- function() {
    bc.titanic <- 
        barchart(Class ~ Freq | Sex + Age, as.data.frame(Titanic), 
                 groups = Survived, stack = TRUE, layout = c(4, 1),
                 auto.key = list(title = "Survived", columns = 2),
                 scales = list(x = "free"))
    update(bc.titanic, 
           panel = function(...) {
               panel.grid(h = 0, v = -1)
               panel.barchart(...)
           })
}

Figure_2.11 <- function() {
    bc.titanic <- 
        barchart(Class ~ Freq | Sex + Age, as.data.frame(Titanic), 
                 groups = Survived, stack = TRUE, layout = c(4, 1),
                 auto.key = list(title = "Survived", columns = 2),
                 scales = list(x = "free"))
    update(bc.titanic, 
           panel = function(..., border) {
               panel.barchart(..., border = "transparent")
           })
}
