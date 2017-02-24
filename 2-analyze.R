library(MASS)
library(tidyverse)
library(lubridate)
library(stringr)
df <- read.csv("data/times.csv", stringsAsFactors = FALSE) %>% tbl_df()

## set color for twins/triplets
color_kim <- "red"
color_hahner <- "black"
color_luik <- "blue"

# Remove Irvette Van Zyl, since she did not start the race due to
# injury.  Her bib number is 1172.
df <- df[df$BIB != 1172,]

# Remove slowest PBs with the assumption that these are different
# df <- df[df$PB < 10000,]

## add indicator for finishing at least 19 minutes slower than PB
df <- df %>% mutate(slower19 = (FINAL - PB) / 60 >= 19)

## print basic regression to check for p-value of quadratic term
fit <- lm (FINAL ~ PB + I(PB^2), data = df)
print (summary(fit))

## print basic regression with age
fit <- lm (FINAL ~ PB + years2marathon, data = df)
print (summary(fit))


## make final versus pb scatter plots, with and without quadratic terms
pdf("plots/scatter_plot.pdf", height = 5,  width = 5)
ggplot(aes(y=FINAL, x=PB), data = df) + 
    geom_point(size = 1.25, colour = "gray", aes(shape = slower19)) +
    geom_point(data = df[!is.na(df$TWINS),], aes(col = TWINS)) +
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), colour = "black", alpha = .5, size = .5, linetype = "dashed", fill = "black", se = FALSE) + 
    geom_abline(intercept = 0, slope = 1) +
    geom_rug(data = df[is.na(df$FINAL),], aes(x = PB), colour = "gray") +
    geom_rug(data = df[is.na(df$FINAL) & df$TWINS == "Luik triplets",], aes(x = PB), colour = color_luik) +
    xlim(8000,12500) +
    ylim(8000, 12500) +
    ylab("Olympic finishing time, in seconds\n") + 
    xlab("\nSeconds") + 
    ##  ggtitle("Distribution of Y | X") +
    scale_color_manual(values = c(color_hahner, color_kim, color_luik, "gray"), name = "") +
    scale_shape_manual(values = c(19, 15), guide = FALSE) +
    theme_bw() +
    theme(legend.position = c(.8,.2))
dev.off()

pdf("plots/scatter_plot_no_quadratic.pdf", height = 5,  width = 5)
ggplot(aes(y=FINAL, x=PB), data = df) + 
    geom_point(size = 1.25, colour = "gray", aes(shape = slower19)) +
    geom_point(data = df[!is.na(df$TWINS),], aes(col = TWINS)) +
    geom_smooth(method = "lm", formula = y ~ x, colour = "black", alpha = .5, size = .5, linetype = "dashed", fill = "black", se = FALSE) + 
    geom_abline(intercept = 0, slope = 1) +
    geom_rug(data = df[is.na(df$FINAL),], aes(x = PB), colour = "gray") +
    geom_rug(data = df[is.na(df$FINAL) & df$TWINS == "Luik triplets",], aes(x = PB), colour = color_luik) +
    xlim(8000, 12500) +
    ylim(8000, 12500) +
    ylab("Olympic finishing time, in seconds\n") + 
    xlab("\nSeconds") + 
    ##  ggtitle("Distribution of Y | X") +
    scale_color_manual(values = c(color_hahner, color_kim, color_luik, "gray"), name = "") +
    scale_shape_manual(values = c(19, 15), guide = FALSE) +
    theme_bw() +
    theme(legend.position = c(.8,.2))
dev.off()

## make final versus half marathon split scatter plots, with and without quadratic terms
pdf("plots/scatter_plot_half.pdf", height = 5,  width = 5)
df2use <- filter(df, !is.na(SPLIT_HALF))
ggplot(aes(y = FINAL, x = SPLIT_HALF), data = df2use) +
    geom_point(size = 1.25, colour = "gray") + 
    geom_point(data = df2use[!is.na(df2use$TWINS),], aes( col = TWINS)) +
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), colour = "black", alpha = .5, size = .5, linetype = "dashed", fill = "black", se = FALSE) + 
    ## geom_abline(intercept = 0, slope = 1) +
    geom_rug(data = df2use[is.na(df2use$FINAL),], aes(x = SPLIT_HALF), colour = "gray") +
    geom_rug(data = df2use[is.na(df2use$FINAL) & df2use$TWINS == "Luik triplets",], aes(x = PB), colour = color_luik) +
    ##    coord_fixed(xlim = c(4000, 6000), ylim = c(8500, 12500)) +
    ylab("Olympic finishing time, in seconds\n") + 
    xlab("\nSeconds") + 
    ##  ggtitle("Distribution of Y | X") +
    scale_color_manual(values = c(color_hahner, color_kim, color_luik, "gray"), name = "") +
    theme_bw() +
    theme(legend.position = c(0.8, 0.2))
dev.off()

pdf("plots/scatter_plot_half_no_quadratic.pdf", height = 5,  width = 5)
df2use <- filter(df, !is.na(SPLIT_HALF))
ggplot(aes(y = FINAL, x = SPLIT_HALF), data = df2use) +
    geom_point(size = 1.25, colour = "gray") + 
    geom_point(data = df2use[!is.na(df2use$TWINS),], aes( col = TWINS)) +
    geom_smooth(method = "lm", formula = y ~ x, colour = "black", alpha = .5, size = .5, linetype = "dashed", fill = "black", se = FALSE) + 
    ## geom_abline(intercept = 0, slope = 1) +
    geom_rug(data = df2use[is.na(df2use$FINAL),], aes(x = SPLIT_HALF), colour = "gray") +
    geom_rug(data = df2use[is.na(df2use$FINAL) & df2use$TWINS == "Luik triplets",], aes(x = PB), colour = color_luik) +
    ##    coord_fixed(xlim = c(4000, 6000), ylim = c(8500, 12500)) +
    ylab("Olympic finishing time, in seconds\n") + 
    xlab("\nSeconds") + 
    ##  ggtitle("Distribution of Y | X") +
    scale_color_manual(values = c(color_hahner, color_kim, color_luik, "gray"), name = "") +
    theme_bw() +
    theme(legend.position = c(0.8, 0.2))
dev.off()

# Plot Studentized Residuals, WITHOUT QUADRATIC TERM
print_pct <- function(x) {paste0(formatC(round(x,1), format = "f", flag = "0", digits = 1 ), "%")}
##fit <- lm(FINAL ~ PB + I(PB^2), data = df)
fit <- lm(FINAL ~ PB, data = df)
dta <- data.frame( df[!is.na(df$FINAL), c("ATHLETE", "TWINS")], STUD_RESID =  rstudent(fit) )
dta <- arrange(dta, STUD_RESID)
dta <- dta %>% mutate(PERCENTILE = cume_dist(STUD_RESID)*100 )

pdf("plots/studentized_residuals_no_quadratic.pdf", height = 5, width = 5)
ggplot(dta, aes(STUD_RESID, PERCENTILE)) + 
  geom_point(size = 1, colour = "gray") + 
  geom_point(data = na.omit(dta), aes( col = TWINS)) + 
  geom_text(data = dta[dta$TWINS == "Hahner twins",], aes(label = print_pct(PERCENTILE)), nudge_x = -.4, colour = color_hahner) +
  geom_text(data = dta[dta$TWINS == "Kim twins",], aes(label = print_pct(PERCENTILE)), nudge_x = .4, colour = color_kim) +
  geom_text(data = dta[dta$TWINS == "Luik triplets",], aes(label = print_pct(PERCENTILE)), nudge_x = .4, colour = color_luik) +
  scale_x_continuous(breaks = -10:10) +
  ##  ggtitle("Distribution of Studentized Residuals") +
  ylab("Percentile\n") +
  xlab("\nStudentized residual") + 
  scale_color_manual(values = c(color_hahner, color_kim, color_luik, "gray"), name = "") +
  theme_bw() +
  theme(legend.position = c(.8,.2))
dev.off()

##fit <- lm(FINAL ~ SPLIT_HALF + I(SPLIT_HALF^2), data = df)
fit <- lm(FINAL ~ SPLIT_HALF, data = df)
dta <- data.frame( df[!is.na(df$FINAL), c("ATHLETE", "TWINS")], STUD_RESID =  rstudent(fit) )
dta <- arrange(dta, STUD_RESID)
dta <- dta %>% mutate(PERCENTILE = cume_dist(STUD_RESID)*100 )

pdf("plots/studentized_residuals_half_no_quadratic.pdf", height = 5, width = 5)
ggplot(dta, aes(STUD_RESID, PERCENTILE)) + 
  geom_point(size = 1, colour = "gray") + 
  geom_point(data = na.omit(dta), aes( col = TWINS)) + 
  geom_text(data = dta[dta$TWINS == "Hahner twins",], aes(label = print_pct(PERCENTILE)), nudge_x = .5, colour = color_hahner) +
  #geom_text(data = dta[dta$TWINS == "Kim twins",], aes(label = print_pct(PERCENTILE)), nudge_x = -.5, colour = color_hahner) +
  geom_text(data = dta[dta$TWINS == "Kim twins" & !is.na(dta$TWINS),][1,], aes(label = print_pct(PERCENTILE)), nudge_x = -.5, colour = color_kim) +
  geom_text(data = dta[dta$TWINS == "Kim twins" & !is.na(dta$TWINS),][2,], aes(label = print_pct(PERCENTILE)), nudge_x = .5, colour = color_kim) +
  geom_text(data = dta[dta$TWINS == "Luik triplets" & !is.na(dta$TWINS),][1,], aes(label = print_pct(PERCENTILE)), nudge_x = -.5, colour = color_luik) +
  geom_text(data = dta[dta$TWINS == "Luik triplets" & !is.na(dta$TWINS),][2,], aes(label = print_pct(PERCENTILE)), nudge_x = .5, colour = color_luik) +
  ##  ggtitle("Distribution of Studentized Residuals") +
  ylab("Percentile\n") +
  xlab("\nStudentized residual") + 
  scale_color_manual(values = c(color_hahner, color_kim, color_luik, "gray"), name = "") +
  theme_bw() +
  theme(legend.position = c(.8,.2))
dev.off()

nms <- df$ATHLETE
pb <- df$PB
hf <- df$SPLIT_HALF
result <- df$FINAL
rank <- min_rank(df$FINAL)
name_i <- vector()
name_j <- vector()
pb_diff <- vector()
hf_diff <- vector()
result_diff <- vector()
consecutive <- vector()
k = 1
a = 1
for(i in 1:length(pb)){
  for(j in a:length(result)){
    name_i[k] = nms[i]
    name_j[k] = nms[j]
    pb_diff[k] = pb[i] - pb[j]
    hf_diff[k] = hf[i] - hf[j]
    result_diff[k] = result[i] - result[j]
    consecutive[k] = ifelse(abs(rank[i] - rank[j]) == 1, T, F) 
    k = k + 1
  }
  a = a + 1 
}


df2 <- data_frame(name_i, name_j, pb_diff, hf_diff, result_diff, diff_in_diff = abs(result_diff) - abs(pb_diff), consecutive)

df2$twins <- ifelse(df2$name_i == "Anna Hahner" & df2$name_j == "Lisa Hahner", "Hahner twins",
                    ifelse(df2$name_i == "Hye-Gyong Kim" & df2$name_j == "Hye-Song Kim", "Kim twins",
                           ifelse(df2$name_i == "Leila Luik" & df2$name_j == "Lily Luik", "Luik triplets", NA)))


df2b <- df2 %>% filter(name_i != name_j, !is.na(diff_in_diff)) %>% arrange(diff_in_diff)

pdf("plots/diff_in_diff_scatter_plot.pdf", height = 5,  width = 5)
ggplot(aes(y=abs(result_diff), x=abs(pb_diff)), data = df2b) + 
    geom_point(size = .5, colour = "gray", alpha = .5) + 
    geom_point(data = df2b[!is.na(df2b$twins),], aes( col = twins)) +
    ## geom_smooth( colour = "black", alpha = .5, size = .5, linetype = "dashed", fill = "black") +  # removed smoother because points are not independent
    geom_abline(intercept = 0, slope =1) +
    ## geom_rug(data = df[is.na(df$FINAL),], aes(x = PB), colour = "gray") +
    ## geom_rug(data = df[is.na(df$FINAL) & df$TWINS == "Luik triplets",], aes(x = PB), colour = color_luik) +
    coord_fixed(xlim = c(0,3500), ylim = c(0,3500)) +
    ylab("Absolute difference in Olympic finishing times, in seconds\n") + 
    xlab("\nAbsolute differences, in seconds") + 
    ## ggtitle("Among all dyadic combinations") +
    scale_color_manual(values = c(color_hahner, color_kim, color_luik, "gray"), name = "") +
    theme_bw() +
    theme(legend.position = c(.8,.2))
dev.off()


pdf("plots/diff_in_diff_scatter_plot_half.pdf", height = 5,  width = 5)
ggplot(aes(y=abs(result_diff), x=abs(hf_diff)), data = df2b) + 
  geom_point(size = .5, colour = "gray", alpha = .5) + 
  geom_point(data = df2b[!is.na(df2b$twins),], aes( col = twins)) +
  ## geom_smooth( colour = "black", alpha = .5, size = .5, linetype = "dashed", fill = "black") +  # removed smoother because points are not independent
  geom_abline(intercept = 0, slope =1) +
  ## geom_rug(data = df[is.na(df$FINAL),], aes(x = PB), colour = "gray") +
  ## geom_rug(data = df[is.na(df$FINAL) & df$TWINS == "Luik triplets",], aes(x = PB), colour = color_luik) +
  coord_fixed(xlim = c(0,3500), ylim = c(0,3500)) +
  ylab("Absolute difference in Olympic finishing times, in seconds\n") + 
  xlab("\nAbsolute differences, in seconds") + 
  ## ggtitle("Among all dyadic combinations") +
  scale_color_manual(values = c(color_hahner, color_kim, color_luik, "gray"), name = "") +
  theme_bw() +
  theme(legend.position = c(.8,.2))
dev.off()


pdf("plots/diff_in_diff_1.pdf", height = 5, width = 5)
df2b <- df2b %>% mutate(PERCENTILE = cume_dist(diff_in_diff)*100 )
ggplot(df2b, aes(PERCENTILE,diff_in_diff)) + 
  geom_point(size = 1, colour = "gray") + 
  geom_point(data = na.omit(df2b), aes( col = twins)) + 
  geom_text(data = df2b[df2b$twins == "Hahner twins",], aes(label =  paste0(formatC(round(PERCENTILE,1), format = "f", flag = "0", digits = 1 ), "%")), nudge_y = 100, colour = color_hahner) +
  geom_text(data = df2b[df2b$twins == "Kim twins",], aes(label = paste0(formatC(round(PERCENTILE,1), format = "f", flag = "0", digits = 1 ), "%")), nudge_y = 200, colour = color_kim) +
  ggtitle("Distribution of Difference in Differences\n(All Dyads)") +
  ylab("Difference in Difference (sec)\n") +
  xlab("\nPercentile") + 
  scale_color_manual(values = c(color_hahner, color_kim, color_luik), name = "") +
  theme_bw() +
  theme(legend.position = c(.85,.15))
dev.off()

pdf("plots/diff_in_diff_2.pdf", height = 5, width = 5)
h_diff <- df2b$pb_diff[df2b$twins == "Hahner twins" & !is.na(df2b$twins)]
df2c <- df2b %>% filter(abs(pb_diff) <= abs(h_diff)) %>% mutate(PERCENTILE = cume_dist(diff_in_diff)*100 )
ggplot(df2c, aes(PERCENTILE,diff_in_diff)) + 
  geom_point(size = 1, colour = "gray") + 
  geom_point(data = na.omit(df2c), aes( col = twins)) + 
  geom_text(data = df2c[df2c$twins == "Hahner twins",], aes(label = paste(round(PERCENTILE,1),"%")), nudge_y = 100, colour = color_hahner) +
  geom_text(data = df2c[df2c$twins == "Kim twins",], aes(label = paste(round(PERCENTILE,1),"%")), nudge_y = 100, colour = color_kim) +
  ggtitle("Distribution of Difference in Differences\n(Diads where difference in PB < Hahner's)") +
  ylab("Difference in Difference (sec)\n") +
  xlab("\nPercentile") + 
  scale_color_manual(values = c(color_hahner, color_kim, color_luik, "gray"), name = "") +
  theme_bw() +
  theme(legend.position = c(.85,.15))
dev.off()

stop()

