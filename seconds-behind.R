library(dplyr)

dta <- read.csv (file = "data/times.csv", header = TRUE)

pdf (file = "plots/seconds-behind.pdf")

plot (0,0, cex = 0, xlim = c(1, 10), ylim = c(0, 2000),
      xlab = "Split", ylab = "Seconds behind",
      xaxt = "n")

for (col2use in cols2use <- grep("SPLIT|FINAL", colnames(dta))) {
    dta2use <- dta[!is.na(dta[,col2use]), ]

    ## set colors and shapes; have to do this for each split since the
    ## rows for the matrix being printed change per split when runners
    ## drop out
    cex2use <- rep(0.12, times = nrow(dta))
    cex2use[dta2use$BIB == 748 | dta2use$BIB == 749] <- 1
    cex2use[dta2use$BIB == 1137 | dta2use$BIB == 1138] <- 1
    cex2use[dta2use$BIB == 633 | dta2use$BIB == 634 | dta2use$BIB == 635] <- 1
    
    pch2use <- rep(19, times = nrow(dta2use))
    pch2use[dta2use$BIB == 748 | dta2use$BIB == 749] <- 19
    pch2use[dta2use$BIB == 1137 | dta2use$BIB == 1138] <- 19
    pch2use[dta2use$BIB == 633  | dta2use$BIB == 634 | dta2use$BIB == 635] <- 19

    color2use <- rep("grey50", times = nrow(dta2use))
    color2use[dta2use$BIB == 748 | dta2use$BIB == 749] <- "orange"
    color2use[dta2use$BIB == 1137 | dta2use$BIB == 1138] <- "red"
    color2use[dta2use$BIB == 633 | dta2use$BIB == 634 | dta2use$BIB == 635] <- "blue"

    points (x = rep (match(col2use, cols2use), times = nrow(dta2use)), y = dta2use[, col2use] - min(dta2use[, col2use]),
           col = color2use,
           cex = cex2use,
           pch = pch2use            
           )
}

axis (side = 1, at = c(1:10),
      labels = c("5K", "10K", "15K", "20K", "Half", "25K", "30K", "35K", "40K", "Final"))

legend (x = 1, y = 2000,
        col = c("blue", "orange", "red", "grey50"),
        pch = 19,
        legend = c("Estonian triplets", "German twins", "North Korean twins", "Others")
        )


dta2use4anna <- filter(dta, BIB == 748)
dta2use4lisa <- filter(dta, BIB == 749)


lines (x = c(1, 1, 3, 3), y = c(590, 620, 620, 590), col = "black", lwd = 0.8)
text (2, 740, "Lisa ahead", pos = 1)

lines (x = c(4, 4, 9, 9), y = c(830, 860, 1910, 1880), col = "black", lwd = 0.8)
text (6, 1430, "Anna ahead", pos = 1, srt = 42)

dev.off()
