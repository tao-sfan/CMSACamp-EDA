library(tidyverse)
library(gt)
# read in data----
wta_2018_2021_matches <-
  map_dfr(c(2018:2021),
          function(year) {
            read_csv(paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_",
                            year, ".csv")) %>%
              mutate(winner_seed = as.character(winner_seed),
                     loser_seed = as.character(loser_seed))
          })
wta = wta_2018_2021_matches


# matches between different hand player ----
wta %>%
  filter(winner_hand %in% c("L", "R"), loser_hand %in% c("L", "R")) %>%
  group_by(winner_hand, loser_hand) %>%
  summarise(n())


# age difference between players ----
wta %>%
  mutate(age_difference = winner_age - loser_age) %>%
#  select(age_difference) %>%
  ggplot(aes(x='', y=age_difference)) +
  geom_violin() + 
  geom_boxplot(width=.2) +
  coord_flip() +
  theme_bw()


# bo5 check ----
table(wta$best_of)

# 1st: match length by rounds ----
wta %>% 
  group_by(round) %>%
  summarise(avg_length = sum(minutes, na.rm = T)/n()) %>%
  ggplot(aes(x=round, y=avg_length)) +
  geom_point() +
  theme_bw()

wta %>%
  filter(minutes<500) %>% 
  ggplot(aes(x=minutes)) + 
  geom_density() +
  facet_wrap(~ round, scales = "free_y") +
  geom_rug(alpha=0.3) + 
  theme_bw()


# avg double fault and ace by level ----
wta %>% 
  mutate(total_ace = w_ace+l_ace,
         total_df = w_df+l_df,) %>% 
  group_by(tourney_level) %>%
  summarise(avg_ace_by_division = sum(total_ace, na.rm=T)/n(),
            avg_df_by_division = sum(total_df, na.rm=T)/n()) %>%
  pivot_longer(cols = avg_ace_by_division:avg_df_by_division, 
               names_to = "ace_df", values_to = "value") %>%
  ggplot(aes(x=tourney_level, fill = ace_df)) +
  geom_bar(aes(y=value), stat='identity', position='dodge') +
  theme_bw()

#'G' = Grand Slams, 'F' = Tour finals and other season-ending events
#'P' = Premier, 'PM' = Premier Mandatory, 'I' = International.
#'D' is used for Federation/Fed/Billie Jean King Cup, 
#'and also for Wightman Cup and Bonne Bell Cup

# player win rates on surface type ----
games_win = wta %>% 
  group_by(winner_name, surface) %>%
  count() %>%
  rename(player = winner_name)

games_lose = wta %>%
  group_by(loser_name, surface) %>%
  count() %>%
  rename(player = loser_name)

full_join(games_lose, games_win, by=c('player', 'surface')) %>%
  rename(wins = "n.y", losses = "n.x") %>% 
  replace_na(list(losses = 0, wins=0)) %>%
  mutate(total = wins+losses, winrate = wins/total) %>%
  arrange(surface, desc(winrate)) %>%
  filter(total >= 10)


# 2nd: aces on surface type
wta %>% 
  mutate(total_ace = w_ace+l_ace) %>% 
  filter(total_ace <50) %>%
  ggplot(aes(y=total_ace, fill=surface)) +
  geom_boxplot(aes(x='')) +
  coord_flip() + 
  theme_bw()


# first serve winrate
wta %>%
  mutate(w_1strate = w_1stWon/w_1stIn, l_1strate = l_1stWon/l_1stIn)
