library(MASS)
library(tidyverse)
library(lubridate)
library(stringr)

df <- read.csv("data/times.csv", stringsAsFactors = FALSE) %>% tbl_df()

# Remove Irvette Van Zyl, since she did not start the race due to
# injury.  Her bib number is 1172.
df <- df[df$BIB != 1172,]


max_x_t <- 2300
max_x_r <- 124

max_y_t <- 325
max_y_r <- 200

max_y_t_half <- 500
max_y_r_half <- 300

text_y <- .70
text_x <- .60 


source("sim.r")
# The race was simulated 10000 times.
# The Hahner twins place consecutively in 1.54% of simulations
# The average simulated time between them was 8.07 minutes
# 95% simulated interval for Anna Hahner finishing time in seconds: 8486 10133 
# 95% simulated interval for Lisa Hahner finishing time in seconds: 8604 10268 
source("simhalf.r")
# The race was simulated 10000 times.
# The Hahner twins place consecutively in 2.71% of simulations
# The average simulated time between them was 5.8 minutes
# 95% simulated interval for Anna Hahner finishing time in seconds: 9468 10670 
# 95% simulated interval for Lisa Hahner finishing time in seconds: 9387 10586 
source("simwithage.r")
# The race was simulated 10000 times.
# The Hahner twins place consecutively in 1.82% of simulations
# The average simulated time between them was 7.87 minutes
# 95% simulated interval for Anna Hahner finishing time in seconds: 8369 9999 
# 95% simulated interval for Lisa Hahner finishing time in seconds: 8507 10143 
source("simhalfwithage.r")
# The race was simulated 10000 times.
# The Hahner twins place consecutively in 2.59% of simulations
# The average simulated time between them was 5.85 minutes
# 95% simulated interval for Anna Hahner finishing time in seconds: 9493 10717 
# 95% simulated interval for Lisa Hahner finishing time in seconds: 9443 10637 


# Save simulation results
save(hahner_df_sim, 
     hahner_df_simhalf, 
     hahner_df_simwithage, 
     hahner_df_simhalfwithage, 
     file = "sim_results.RData")


mean(hahner_df_simwithage$anna_final, na.rm = T)
mean(hahner_df_simwithage$lisa_final, na.rm = T)
mean(hahner_df_simwithage$anna_rank, na.rm = T)
mean(hahner_df_simwithage$lisa_rank, na.rm = T)
sum(hahner_df_simwithage$rank_diff == 1, na.rm = T)