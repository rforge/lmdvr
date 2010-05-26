
Figure_4.1 <- function() {
    dotplot(VADeaths, groups = FALSE)
}

Figure_4.2 <- function() {
    dotplot(VADeaths, groups = FALSE, 
            layout = c(1, 4), aspect = 0.7, 
            origin = 0, type = c("p", "h"),
            main = "Death Rates in Virginia - 1940", 
            xlab = "Rate (per 1000)")
}

Figure_4.3 <- function() {
    dotplot(VADeaths, type = "o",
            auto.key = list(lines = TRUE, space = "right"),
            main = "Death Rates in Virginia - 1940",
            xlab = "Rate (per 1000)")
}

Figure_4.4 <- function() {
    barchart(VADeaths, groups = FALSE,
             layout = c(1, 4), aspect = 0.7, reference = FALSE, 
             main = "Death Rates in Virginia - 1940",
             xlab = "Rate (per 100)")
}

Figure_4.5 <- function() {
    data(postdoc, package = "latticeExtra")
    barchart(prop.table(postdoc, margin = 1), xlab = "Proportion",
             auto.key = list(adj = 1))
}

Figure_4.6 <- function() {
    data(postdoc, package = "latticeExtra")
    dotplot(prop.table(postdoc, margin = 1), groups = FALSE, 
            xlab = "Proportion",
            par.strip.text = list(abbreviate = TRUE, minlength = 10))
}

Figure_4.7 <- function() {
    data(postdoc, package = "latticeExtra")
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
}

Figure_4.8 <- function() {
    data(Chem97, package = "mlmRev")
    gcsescore.tab <- xtabs(~gcsescore + gender, Chem97)
    gcsescore.df <- as.data.frame(gcsescore.tab)
    gcsescore.df$gcsescore <- 
        as.numeric(as.character(gcsescore.df$gcsescore))
    xyplot(Freq ~ gcsescore | gender, data = gcsescore.df, 
           type = "h", layout = c(1, 2), xlab = "Average GCSE Score")
}

Figure_4.9 <- function() {
    data(Chem97, package = "mlmRev")
    score.tab <- xtabs(~score + gender, Chem97)
    score.df <- as.data.frame(score.tab)
    barchart(Freq ~ score | gender, score.df, origin = 0)
}

