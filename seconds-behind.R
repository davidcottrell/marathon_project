library(dplyr)

dta <- read.csv (file = "data/times.csv", header = TRUE)

pdf (file = "plots/seconds-behind.pdf", width = 5, height = 5)

plot (0,0, cex = 0, xlim = c(5, 42.195), ylim = c(0, 2000),
      xlab = "Split", ylab = "Seconds behind",
      xaxt = "n",
      cex.axis = 0.75)

dists <- c(5, 10, 15, 20, 42.195/2, 25, 30, 35, 40, 42.195)

for (col2use in cols2use <- grep("SPLIT|FINAL", colnames(dta))) {
    dta2use <- dta[!is.na(dta[,col2use]), ]

    ## set colors and shapes; have to do this for each split since the
    ## rows for the matrix being printed change per split when runners
    ## drop out
    cex2use <- rep(0.15, times = nrow(dta))
    cex2use[dta2use$BIB == 748 | dta2use$BIB == 749] <- .7
    cex2use[dta2use$BIB == 1137 | dta2use$BIB == 1138] <- .7
    cex2use[dta2use$BIB == 633 | dta2use$BIB == 634 | dta2use$BIB == 635] <- .7
    
    pch2use <- rep(19, times = nrow(dta2use))
    pch2use[dta2use$BIB == 748 | dta2use$BIB == 749] <- 1
    pch2use[dta2use$BIB == 1137 | dta2use$BIB == 1138] <- 1
    pch2use[dta2use$BIB == 633  | dta2use$BIB == 634 | dta2use$BIB == 635] <- 1

    color2use <- rep("grey50", times = nrow(dta2use))
    color2use[dta2use$BIB == 748 | dta2use$BIB == 749] <- "red"
    color2use[dta2use$BIB == 1137 | dta2use$BIB == 1138] <- "orange"
    color2use[dta2use$BIB == 633 | dta2use$BIB == 634 | dta2use$BIB == 635] <- "blue"

    points (x = rep(dists[match(col2use, cols2use)], times = nrow(dta2use)), y = dta2use[, col2use] - min(dta2use[, col2use]),
           col = color2use,
           cex = cex2use,
           pch = pch2use            
           )
}

axis (side = 1, at = dists,
      labels = c("5K", "10K", "15K", "", "", "25K", "30K", "35K", "", ""),
      cex.axis = 0.75,
      padj = 0)

axis (side = 1, at = dists[c(4,9)] + c(dists[c(5,10)] - dists[c(4,9)])/2 ,
      labels = c("20K Half", "40K Final"),
      cex.axis = 0.75,
      padj = 0,
      tick = F)

legend (x = 5, y = 2000,
        col = c("blue", "red", "orange"),
        pch = 1,
        legend = c("Luik triplets", "Hahner twins", "Kim twins"),
        cex = 0.75,
        bty = "n"
        )


dta2use4anna <- filter(dta, BIB == 748)
dta2use4lisa <- filter(dta, BIB == 749)


lines (x = dists[c(1, 1, 3, 3)], y = c(590, 620, 620, 590), col = "black", lwd = 0.8)
text (10, 770, "Lisa ahead", pos = 1, cex = 0.75)

lines (x = dists[c(4, 4, 9, 9)], y = c(830, 860, 1910, 1880), col = "black", lwd = 0.8)
text (29, 1490, "Anna ahead", pos = 1, srt = 38.2, cex = 0.75)

dev.off()
