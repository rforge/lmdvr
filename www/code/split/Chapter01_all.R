
Figure_1.1 <- function()
{
    data(Chem97, package = "mlmRev")
    histogram(~ gcsescore | factor(score), data = Chem97)
}


Figure_1.2 <- function()
{
    data(Chem97, package = "mlmRev")
    densityplot(~ gcsescore | factor(score), data = Chem97, 
                plot.points = FALSE, ref = TRUE)
}

Figure_1.3 <- function()
{
    data(Chem97, package = "mlmRev")
    densityplot(~ gcsescore, data = Chem97, groups = score,
                plot.points = FALSE, ref = TRUE,
                auto.key = list(columns = 3))
}


Figure_1.4 <- function()
{
    data(Chem97, package = "mlmRev")
    tp1 <- histogram(~ gcsescore | factor(score), data = Chem97)
    tp2 <- 
        densityplot(~ gcsescore, data = Chem97, groups = score,
                    plot.points = FALSE,
                    auto.key = list(space = "right", title = "score"))
    plot(tp1, split = c(1, 1, 1, 2))
    plot(tp2, split = c(1, 2, 1, 2), newpage = FALSE)
}

