source("../setup.R")
chapter <- 1

data(Chem97, package = "mlmRev")
xtabs( ~ score, data = Chem97)

## 1.1
resizeWindow(5)
histogram(~ gcsescore | factor(score), data = Chem97)
saveImage()

## 1.2
resizeWindow(5)
densityplot(~ gcsescore | factor(score), data = Chem97, 
            plot.points = FALSE, ref = TRUE)
saveImage()

## 1.3
resizeWindow(5)
densityplot(~ gcsescore, data = Chem97, groups = score,
            plot.points = FALSE, ref = TRUE,
            auto.key = list(columns = 3))
saveImage()


tp1 <- histogram(~ gcsescore | factor(score), data = Chem97)
tp2 <- 
    densityplot(~ gcsescore, data = Chem97, groups = score,
                plot.points = FALSE,
                auto.key = list(space = "right", title = "score"))
class(tp2)
summary(tp1)

## 1.4
resizeWindow(12.5)
plot(tp1, split = c(1, 1, 1, 2))
plot(tp2, split = c(1, 2, 1, 2), newpage = FALSE)
saveImage()


