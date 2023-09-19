library(baseballr)
library(ggspraychart)
library(tidyverse)
library(lubridate)


# Define your date range
start_date <- ymd("2023-09-05")
end_date <- ymd("2023-09-06")

# Create a list to store the PKs for each day
game_pks_list <- list()

# Iterate through the date range and fetch game PKs for each day
for (date in seq(ymd('2023-03-30'), ymd('2023-06-12'), by = 'days')) {
  date <- as.Date(date, origin = "1970-01-01")
  
  game_pks <- get_game_pks_mlb(date, level_ids = c(1))
  
  game_pks_list[[as.character(date)]] <- game_pks
  
  print(date)
}


# every single MLB game this year 
combined_dataframe <- bind_rows(game_pks_list) 
# select_if(~ all(!is.na(.)))

combined_dataframe %>% 
  filter(is.na(status.reason)) %>% 
  View()

#
combined_dataframe 



# 
orioles_games <- 
  combined_dataframe %>% 
  filter(str_detect(teams.away.team.name, "Orioles") | 
           str_detect(teams.home.team.name, "Orioles"))

os_pks <- orioles_games %>% 
  .$game_pk
## all games 

# #slice 1
# all_pks <- combined_dataframe %>% 
#   .$game_pk
# 
# #slice 2 
# all_pks_2 <- 
#   combined_dataframe_2 %>% 
#   .$game_pk

combined_dataframe %>% 
  group_by(game_pk) %>% 
  filter(n() > 1) 

pk_slice_2 <- combined_dataframe %>% 
  filter(status.codedGameState != "D") %>% 
  filter(gameDate < "2023-06-25") %>% 
  group_by(gameDate) %>% 
  count(sort = F) %>% 
  
  combined_dataframe %>% 
  glimpse()

combined

pk_slice_2 <- combined_dataframe %>% 
  filter(is.na(status.reason)) %>%
  filter(officialDate < "2023-06-25") %>% 
  .$game_pk

all_pbp %>% 
  summarise(min_date = min(game_date),
            max_date = max(game_date))

test <- all_pbp_3 %>% 
  group_by(game_date) %>% 
  count(sort = T) 



all_games_list <- list()
# get pbp for all orioles games 
#create empty list
o_games_list <- list()
all_games_list <- list()
all_games_list_2 <- list()

for (pk in os_pks) {
  
  obj <- get_pbp_mlb(pk)
  
  o_games_list[[as.character(pk)]] <- obj
}

all_pbp %>% 
  group_by(game_pk) %>% 
  summarise(n = n())


for (pk in pk_slice_2) {
  
  obj <- get_pbp_mlb(pk)
  
  all_games_list_2[[as.character(pk)]] <- obj
}


combined_dataframe_2 <- bind_rows(all_games_list_2)


combined_dataframe_2 %>% 
  group_by(game_date, game_pk) %>% 
  count() %>% 
  View()

# all pitches this year
# messed up around June 21. Need to re-query for everything past there 
all_pbp <- bind_rows(all_games_list)

all_pbp %>% 
  group_by(game_date) %>% 
  count() %>% 
  View()

combined_os_pbp %>% 
  group_by(game_date) %>% 
  count() %>% 
  View()

col_names <- all_pbp %>% 
  colnames() %>% 
  as.data.frame()

# all o's pitches this year 
combined_os_pbp <- bind_rows(o_games_list) 


# most fouls in 2 strike counts 
combined_os_pbp %>% 
  filter(game_date <= "2023-06-21") %>% 
  filter(count.strikes.start == 2) %>% 
  group_by(details.description) %>% 
  count(sort = T)

all_pbp %>% 
  filter(str_detect(batting_team, "Orioles")) %>% 
  filter(game_date <= "2023-06-21") %>% 
  filter(count.strikes.start == 2) %>% 
  group_by(details.description) %>% 
  count(sort = T)

all_pbp %>% 
  filter(game_date <= "2023-06-21",
         isPitch == TRUE) %>% 
  group_by(batting_team) %>% 
  summarise(n = n(),
            fouls = sum(details.description == "Foul"),
            in_play_outs = sum(details.description == "In play, out(s)"),
            singles
  )

all_pbp %>% 
  filter(isPitch == TRUE) %>% 
  mutate(team = ifelse(str_detect(batting_team, "Orioles"),"o's", "other")) %>% 
  group_by(team, details.description) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(team) %>% 
  mutate(sum = sum(n),
         pct = n / sum * 100)  %>% 
  select(pct, details.description) %>% 
  pivot_wider(names_from = details.description, 
              values_from = pct) 

all_pbp %>% 
  filter(isPitch == TRUE) %>% 
  mutate(team = ifelse(str_detect(batting_team, "Orioles"),"o's", "other")) %>%
  # filter(details.description == "Swinging Strike") %>% 
  group_by(team, details.description) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(team) %>% 
  mutate(sum = sum(n),
         pct = n / sum * 100)  %>% 
  select(pct, details.description) %>% 
  pivot_wider(names_from = details.description, 
              values_from = pct)  %>% 
  View()

count() %>% 
  ungroup() %>% 
  group_by(batting_team) %>% 
  mutate(sum = sum(n),
         pct = n / sum * 100)  %>% 
  select(pct, details.description) %>% 
  pivot_wider(names_from = details.description, 
              values_from = pct)

combined_os_pbp %>% 
  glimpse()

#spray chart 
all_pbp %>% 
  filter(isPitch == TRUE) %>% 
  filter(about.inning == 7) %>% 
  filter(!is.na(hitData.coordinates.coordX) & !is.na(hitData.coordinates.coordX)) %>% 
  mutate(team = ifelse(str_detect(batting_team, "Orioles"),"o's", "other")) %>%
  # filter(details.description == "Swinging Strike") %>% 
  group_by(team, type = case_when(result.eventType == "single" ~ "single",
                                  result.eventType == "double" ~ "double",
                                  result.eventType == "triple" ~ "triple",
                                  result.eventType == "home_run" ~ "home_run",
                                  str_detect(result.eventType, "error") ~ "error",
                                  TRUE ~ "out"
  )) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(team) %>% 
  mutate(sum = sum(n),
         pct = n / sum * 100)  %>% 
  select(pct, type) %>% 
  pivot_wider(names_from = type, 
              values_from = pct) 

all_pbp %>% 
  group_by(about.inning) %>% 
  count()







