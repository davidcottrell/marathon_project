library(tidyverse)
library(rvest)
library(lubridate)

from_web = FALSE

if (from_web){
  path_root <- "https://www.iaaf.org/results/olympic-games/2016/the-xxxi-olympic-games-5771/women/marathon/final/"
  ext <- ""
} else {
  path_root <- "data/"
  ext <- ".html"
}

path_start <- paste0(path_root, "startlist", ext)
path_result <- paste0(path_root, "result", ext)
path_split <- paste0(path_root, "split", ext)

parsed_start <- read_html(path_start)
parsed_result <- read_html(path_result)
parsed_split <- read_html(path_split)

start <- parsed_start %>% html_table() %>% .[[3]]
start <- start %>%
  rename(LANE = `Order / Lane`, 
         BIB = Bib, 
         SB =`SB 2016`) %>% 
  mutate(PB = replace(PB, ATHLETE == "Sarah Attar",  "3:11:27"), 
         RK_PB = min_rank(PB),
         SB = replace(SB, SB == "", NA))

result <- parsed_result %>% html_table() %>% .[[3]]
result <- result %>%
  rename(RK_FINAL = POS,
         FINAL = MARK) %>% 
  select(c(1,3,5))

lst <- parsed_split %>% html_table()

distances <- c("5K", "10K", "15K", "20K", "HALF", "25K", "30K", "35K", "40K")
splits <- data_frame()
for (i in 1:length(distances)){
  temp <- lst[[i+2]] %>% mutate(dist = paste0("SPLIT_", distances[i]) , rank = paste0("RK_", distances[i]))
  splits <- bind_rows(splits, temp)
}

splits <- splits %>% 
  separate(MARK, into = c("h","m","s"), sep = ":", fill = "left") %>% 
  replace_na(list(h = 0)) %>% 
  unite(MARK, h, m, s, sep = ":")

splits_dist <- splits %>% select(-POS, -COUNTRY, -rank) %>% spread(dist, MARK)
splits_rank <- splits %>% select(-MARK, -COUNTRY, -dist) %>% spread(rank, POS)
splits <- full_join(splits_dist,splits_rank, by = "ATHLETE")

df <- start %>% full_join(splits, by = "ATHLETE") %>% full_join(result, by = "ATHLETE")

df <- df %>% mutate(TWINS = ifelse(BIB %in% c(748, 749), "Hahner twins",
                                   ifelse(BIB %in% c(1137, 1138), "Kim twins",
                                          ifelse(BIB %in% c(633:635), "Luik triplets", NA))))

nms <- c("LANE","BIB","ATHLETE","COUNTRY","TWINS","SB","PB","SPLIT_5K", "SPLIT_10K","SPLIT_15K", "SPLIT_20K", "SPLIT_HALF", "SPLIT_25K", "SPLIT_30K", "SPLIT_35K", "SPLIT_40K", "FINAL", "RK_PB", "RK_5K", "RK_10K","RK_15K","RK_20K","RK_HALF","RK_25K","RK_30K","RK_35K","RK_40K","RK_FINAL") 

df <- df[,nms]

df <- df %>% 
  select(-(RK_PB:RK_FINAL)) %>%
  mutate_each(funs(replace(., . %in% c("0:0:00", "DNS", "DNF"),  NA)), SB:FINAL) %>%
  mutate_each(funs(period_to_seconds(hms(.))), SB:FINAL)

## merge in birthdate data
bdays <- read.csv (file = "data/birthday-data.txt", header = TRUE, sep = "\t")
bdays <- bdays %>% mutate (years2marathon = as.numeric((as.Date("8/14/2016", format = "%m/%d/%Y") - as.Date(paste(birthmonth, "/", birthday, "/", birthyear, sep = ""), format = "%m/%d/%Y")) / 365))
df <- left_join (df, bdays, by = c("BIB" = "bib"))

write.csv(df, "data/times.csv", row.names = FALSE)

