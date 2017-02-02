library(MASS)
library(tidyverse)
library(lubridate)
library(stringr)
df <- read.csv("data/times.csv", stringsAsFactors = FALSE) %>% tbl_df()

# Remove Irvette Van Zyl, since she did not start the race due to
# injury.  Her bib number is 1172.
df <- df[df$BIB != 1172,]

# Remove slowest PBs with the assumption that these are different
# df <- df[df$PB < 10000,]

## add indicator for finishing at least 19 minutes slower than PB
df <- df %>% mutate(slower19 = (FINAL - PB) / 60 >= 19)

pdf("plots/scatter_plot.pdf", height = 5,  width = 5)
ggplot(aes(y=FINAL, x=PB), data = df) + 
    geom_point(size = 1.25, colour = "gray", aes(shape = slower19)) +
    geom_point(data = df[!is.na(df$TWINS),], aes(col = TWINS)) +
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), colour = "black", alpha = .5, size = .5, linetype = "dashed", fill = "black", se = FALSE) + 
    geom_abline(intercept = 0, slope =1) +
    geom_rug(data = df[is.na(df$FINAL),], aes(x = PB), colour = "gray") +
    geom_rug(data = df[is.na(df$FINAL) & df$TWINS == "Luik Triplets",], aes(x = PB), colour = "blue") +
    coord_fixed(xlim = c(8000,12500), ylim = c(8000, 12500)) +
    ylab("Olympic result, in seconds\n") + 
    xlab("\nPersonal best, in seconds") + 
    ##  ggtitle("Distribution of Y | X") +
    scale_color_manual(values = c("orange", "red", "blue", "gray"), name = "") +
    scale_shape_manual(values = c(19, 15), guide = FALSE) +
    theme_bw() +
    theme(legend.position = c(.8,.2))
dev.off()




## make scatter plot for half marathon split
pdf("plots/scatter_plot_half.pdf", height = 5,  width = 5)
df2use <- filter(df, !is.na(SPLIT_HALF))
ggplot(aes(y = FINAL, x = SPLIT_HALF), data = df2use) +
    geom_point(size = 1.25, colour = "gray") + 
    geom_point(data = df2use[!is.na(df2use$TWINS),], aes( col = TWINS)) +
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), colour = "black", alpha = .5, size = .5, linetype = "dashed", fill = "black", se = FALSE) + 
    ## geom_abline(intercept = 0, slope = 1) +
    geom_rug(data = df2use[is.na(df2use$FINAL),], aes(x = SPLIT_HALF), colour = "gray") +
    geom_rug(data = df2use[is.na(df2use$FINAL) & df2use$TWINS == "Luik Triplets",], aes(x = PB), colour = "blue") +
    ##    coord_fixed(xlim = c(4000, 6000), ylim = c(8500, 12500)) +
    ylab("Olympic result, in seconds\n") + 
    xlab("\nHalf marathon split, in seconds") + 
    ##  ggtitle("Distribution of Y | X") +
    scale_color_manual(values = c("orange", "red", "blue", "gray"), name = "") +
    theme_bw() +
    theme(legend.position = c(0.8, 0.2))
dev.off()



nms <- df$ATHLETE
pb <- df$PB
result <- df$FINAL
rank <- min_rank(df$FINAL)
name_i <- vector()
name_j <- vector()
pb_diff <- vector()
result_diff <- vector()
consecutive <- vector()
k = 1
a = 1
for(i in 1:length(pb)){
  for(j in a:length(result)){
    name_i[k] = nms[i]
    name_j[k] = nms[j]
    pb_diff[k] = pb[i] - pb[j]
    result_diff[k] = result[i] - result[j]
    consecutive[k] = ifelse(abs(rank[i] - rank[j]) == 1, T, F) 
    k = k + 1
  }
  a = a + 1 
}


df2 <- data_frame(name_i, name_j, pb_diff, result_diff, diff_in_diff = abs(result_diff) - abs(pb_diff), consecutive)

df2$twins <- ifelse(df2$name_i == "Anna Hahner" & df2$name_j == "Lisa Hahner", "Hahner Twins",
                    ifelse(df2$name_i == "Hye-Gyong Kim" & df2$name_j == "Hye-Song Kim", "Kim Twins",
                           ifelse(df2$name_i == "Leila Luik" & df2$name_j == "Lily Luik", "Luik Triplets", NA)))


df2b <- df2 %>% filter(name_i != name_j, !is.na(diff_in_diff)) %>% arrange(diff_in_diff)

pdf("plots/diff_in_diff_scatter_plot.pdf", height = 5,  width = 5)
ggplot(aes(y=abs(result_diff), x=abs(pb_diff)), data = df2b) + 
  geom_point(size = .5, colour = "gray", alpha = .5) + 
  geom_point(data = df2b[!is.na(df2b$twins),], aes( col = twins)) +
  geom_smooth( colour = "black", alpha = .5, size = .5, linetype = "dashed", fill = "black") + 
  geom_abline(intercept = 0, slope =1) +
  #geom_rug(data = df[is.na(df$FINAL),], aes(x = PB), colour = "gray") +
  #geom_rug(data = df[is.na(df$FINAL) & df$TWINS == "Luik Triplets",], aes(x = PB), colour = "blue") +
  coord_fixed(xlim = c(0,3500), ylim = c(0,3500)) +
  ylab("Absolute difference Olympic result, in seconds\n") + 
  xlab("\nAbsolute difference in personal best, in seconds") + 
  #ggtitle("Among all dyadic combinations") +
  scale_color_manual(values = c("orange", "red", "blue", "gray"), name = "") +
  theme_bw() +
  theme(legend.position = c(.8,.2))
dev.off()


pdf("plots/diff_in_diff_1.pdf", height = 5, width = 5)
df2b <- df2b %>% mutate(PERCENTILE = cume_dist(diff_in_diff)*100 )
ggplot(df2b, aes(PERCENTILE,diff_in_diff)) + 
  geom_point(size = 1, colour = "gray") + 
  geom_point(data = na.omit(df2b), aes( col = twins)) + 
  geom_text(data = df2b[df2b$twins == "Hahner Twins",], aes(label =  paste0(formatC(round(PERCENTILE,1), format = "f", flag = "0", digits = 1 ), "%")), nudge_y = 100, colour = "orange") +
  geom_text(data = df2b[df2b$twins == "Kim Twins",], aes(label = paste0(formatC(round(PERCENTILE,1), format = "f", flag = "0", digits = 1 ), "%")), nudge_y = 200, colour = "red") +
  ggtitle("Distribution of Difference in Differences\n(All Dyads)") +
  ylab("Difference in Difference (sec)\n") +
  xlab("\nPrecentile") + 
  scale_color_manual(values = c("orange", "red", "blue", "gray"), name = "") +
  theme_bw() +
  theme(legend.position = c(.85,.15))
dev.off()

pdf("plots/diff_in_diff_2.pdf", height = 5, width = 5)
h_diff <- df2b$pb_diff[df2b$twins == "Hahner Twins" & !is.na(df2b$twins)]
df2c <- df2b %>% filter(abs(pb_diff) <= abs(h_diff)) %>% mutate(PERCENTILE = cume_dist(diff_in_diff)*100 )
ggplot(df2c, aes(PERCENTILE,diff_in_diff)) + 
  geom_point(size = 1, colour = "gray") + 
  geom_point(data = na.omit(df2c), aes( col = twins)) + 
  geom_text(data = df2c[df2c$twins == "Hahner Twins",], aes(label = paste(round(PERCENTILE,1),"%")), nudge_y = 100, colour = "orange") +
  geom_text(data = df2c[df2c$twins == "Kim Twins",], aes(label = paste(round(PERCENTILE,1),"%")), nudge_y = 100, colour = "red") +
  ggtitle("Distribution of Difference in Differences\n(Diads where difference in PB < Hahner's)") +
  ylab("Difference in Difference (sec)\n") +
  xlab("\nPrecentile") + 
  scale_color_manual(values = c("orange", "red", "blue", "gray"), name = "") +
  theme_bw() +
  theme(legend.position = c(.85,.15))
dev.off()

# Plot Studentized Residuals

fit <- lm(FINAL ~ PB + I(PB^2), data = df)
dta <- data.frame( df[!is.na(df$FINAL), c("ATHLETE", "TWINS")], STUD_RESID =  rstudent(fit) )
dta <- arrange(dta, STUD_RESID)
dta <- dta %>% mutate(PERCENTILE = cume_dist(STUD_RESID)*100 )

pdf("plots/studentized_residuals.pdf", height = 5, width = 5)
ggplot(dta, aes(PERCENTILE,STUD_RESID)) + 
  geom_point(size = 1, colour = "gray") + 
  geom_point(data = na.omit(dta), aes( col = TWINS)) + 
  geom_text(data = dta[dta$TWINS == "Hahner Twins",], aes(label = paste(round(PERCENTILE,1),"%")), nudge_x = -10, colour = "orange") +
  geom_text(data = dta[dta$TWINS == "Kim Twins",], aes(label = paste(round(PERCENTILE,1),"%")), nudge_x = 10, colour = "red") +
##  ggtitle("Distribution of Studentized Residuals") +
  ylab("Studentized residual\n") +
  xlab("\nPrecentile") + 
  scale_color_manual(values = c("orange", "red", "blue", "gray"), name = "") +
  theme_bw() +
  theme(legend.position = c(.8,.2))
dev.off()

# Calculate the percent of runners who did not finish.
# (includes women who did not start)
dnf_rate <- sum(is.na(df$FINAL))/length(df$FINAL)

# Estimate linear relationship between personal best and marathon result, excluding twins.
m <- lm(FINAL ~ PB + I(PB^2), data = df[is.na(df$TWINS),])

# Store the total number of runners, N
N <- length(df$PB)

# Save the vector of personal bests
x <- df$PB

# Create data frame to capture simulation output
sim_df <- data_frame(ATHLETE = df$ATHLETE)

# Set seed
set.seed(14252345)

# Set total number of sims
total_sims <- 10000

# Begin simulation
for (i in 1:total_sims){
  
  # Determine whether each runner finishes given the dnf rate
  dnf <- sample(x = c(F,T), size = N, replace = T, prob = c(1-dnf_rate, dnf_rate) )
  
  # Draw each runner's result from a multivariate normal distribution with 
  # mean Beta and sigma equal to the variance-covariance of Beta
  # Draw an error term from a normal distibution with standard deviation equal 
  # to the standard deviation of the model residuals.
  B <- mvrnorm(n = 1, mu = coef(m), Sigma =  vcov(m))
  e <- rnorm(n = N, mean = 0, sd = summary(m)$sigma)
  
  # Remove the results of those that did not finish
  y_hat <- ifelse(dnf, NA, B[1] + x*B[2] + x^2*B[3] +  e )
  

  # Collect the results
  sim_df[[paste0(i)]] <- y_hat
  
}

sim_df <- sim_df %>% gather(SIM, YHAT, -ATHLETE, convert = T) %>% group_by(SIM) %>% mutate(RANK = min_rank(YHAT))


# Compare Anna to Lisa
#
anna <- df %>% filter(ATHLETE == "Anna Hahner")
anna_sim <- sim_df %>% filter(ATHLETE == "Anna Hahner")

lisa <- df %>% filter(ATHLETE == "Lisa Hahner")
lisa_sim <- sim_df %>% filter(ATHLETE == "Lisa Hahner")

hahner_dist <- abs(anna_sim$YHAT - lisa_sim$YHAT) <= abs(anna$FINAL - lisa$FINAL)
hahner_dist <- ifelse(is.na(hahner_dist), FALSE, hahner_dist)

hahner_rank <- abs(anna_sim$RANK - lisa_sim$RANK) <= 1
hahner_rank <- ifelse(is.na(hahner_rank), FALSE, hahner_rank)

pct_less_than_hahner <- sum(hahner_dist)/length(hahner_dist)
pct_consecutive_hahner <- sum(hahner_rank)/length(hahner_rank)

hahner_df <- data_frame(time_diff = abs(anna_sim$YHAT - lisa_sim$YHAT), rank_diff =  abs(anna_sim$RANK - lisa_sim$RANK))

pdf("plots/simulated_time.pdf", height = 5, width = 5)
ggplot(hahner_df, aes(time_diff)) + 
  geom_histogram(binwidth = 30, colour = "black", fill = NA) +
  xlab("\nDifference in seconds") +
  ylab("Count\n") +
#  ggtitle("\nSimulated final time") +
  theme_bw()
dev.off()

pdf("plots/simulated_rank.pdf", height = 5, width = 5)
ggplot(hahner_df, aes(rank_diff)) + 
  geom_histogram(binwidth = 1, colour = "black", fill = NA) +
  xlab("\nDifference in rank") +
  ylab("Count\n") +
#  ggtitle("\nSimulated final rank") +
  theme_bw()
dev.off()


# Compare Gyong to Song
#
gyong_sim <- sim_df %>% filter(ATHLETE == "Hye-Gyong Kim")
gyong <- df %>% filter(ATHLETE == "Hye-Gyong Kim")

song_sim <-  sim_df %>% filter(ATHLETE == "Hye-Song Kim")
song <-  df %>% filter(ATHLETE == "Hye-Song Kim")

kim_dist <- abs(gyong_sim$YHAT - song_sim$YHAT) <= abs(gyong$FINAL - song$FINAL)
kim_dist <- ifelse(is.na(kim_dist), FALSE, kim_dist)

kim_rank <- abs(gyong_sim$RANK - song_sim$RANK) <= 1
kim_rank <- ifelse(is.na(kim_rank), FALSE, kim_rank)

pct_less_than_kim <- sum(kim_rank)/length(kim_rank)
pct_consecutive_kim <- sum(kim_rank)/length(kim_rank)

pct_consecutive_hahner_and_kim <- sum(hahner_rank & kim_rank)/length(hahner_rank & kim_rank)

# Calculate the percent of the races where the twins placed in consecutive order.

cat("The race was simulated", total_sims, "times.")
cat("The Hahner twins place consecutively in ", round(pct_consecutive_hahner*100, 2), "% of the simulations.", sep = "")
cat("The Kim twins place consecutively in ", round(pct_consecutive_kim*100, 2), "% of the simulations.", sep = "")
cat("Both the Hahner and Kim twins place consecutively in ", round(pct_consecutive_hahner_and_kim*100, 2), "% of the simulations.", sep = "")




# time <- c("SPLIT_5K", "SPLIT_10K","SPLIT_15K", "SPLIT_20K", "SPLIT_HALF", "SPLIT_25K", "SPLIT_30K", "SPLIT_35K", "SPLIT_40K", "FINAL")
# dist <- c(5, 10, 15, 20, 42.195/2, 25, 30, 35, 40, 42.195)
# td <- data.frame(time, dist, stringsAsFactors = F)
# df2 <- df %>% tbl_df %>% gather(time, value, SB:FINAL) %>% filter(str_detect(time, "SPLIT")) %>% left_join(td, by = "time")
# df2 <- df2 %>% group_by(time) %>% mutate(std_value = (value - mean(value, na.rm = T))/sd(value, na.rm = T) )
# ggplot(df2 %>% filter(!is.na(TWINS)), aes(dist, std_value, group = ATHLETE, colour = TWINS)) + geom_point()

