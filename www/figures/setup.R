library("grid")
library("lattice")
library("qtutils")

theme <- c("stdBW", "stdColor", "stdClassic")[2]

factor <- 0.65
figure <- 1
lattice.options(default.theme =
                switch(theme,
                       stdBW = standard.theme(color = FALSE),
                       stdColor = standard.theme("pdf"),
                       stdClassic = standard.theme("x11")))
resizeWindow <- function(height)
{
    ## this redisplays every time after resize
    ## qt.dev.resize(width = factor * 9, height = factor * height)
    graphics.off()
    trellis.device(QT,
                   width = factor * 9,
                   height = factor * height)
    trellis.par.set(fontsize =
                    list(text = factor * 12,
                         points = factor * 8))
}
saveImage <- function(chap, fig = figure, ext = "png")
{
    if (missing(chap)) chap <- chapter
    if (missing(fig)) {
        fig <- figure
        figure <<- figure + 1
    }
    figname <- sprintf("../images/Figure_%02g_%02g_%s.%s",
                       chap, fig, theme, ext)
    pushViewport(viewport(xscale = c(0, 1),
                          yscale = c(0, 1)))
    panel.text(x = 0.01, y = 1,
               lab = sprintf("Figure %g.%g", chap, fig),
               adj = c(0, 1.5))
    upViewport()
    qt.dev.save(figname)
}

