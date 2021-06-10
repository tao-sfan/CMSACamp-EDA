library(tidyverse)
wta_2018_2021_matches <-
  map_dfr(c(2018:2021),
          function(year) {
            read_csv(paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_",
                            year, ".csv")) %>%
              mutate(winner_seed = as.character(winner_seed),
                     loser_seed = as.character(loser_seed))
          })

head(wta_2018_2021_matches)

# minutes for each round ----
table(wta_2018_2021_matches$round)

wta_2018_2021_matches %>%
  ggplot(aes(x = round)) +
  geom_point(aes(y = minutes))

# right and left hand ----
wta_2018_2021_matches %>%
  filter(winner_hand %in% c("R", "L"), loser_hand %in% c("R", "L")) %>%
  group_by(winner_hand, loser_hand) %>%
  summarise(n())

# prop of bp saved for winners v losers ----
wta_2018_2021_matches %>%
  count(w_bpSaved/w_bpFaced, l_bpSaved/l_bpFaced)

# number of wins each player has
wta_2018_2021_matches %>%
  count(winner_id) %>%
  arrange(desc(n()))


# The higher the tournament level the more aces made--------------------------------------------------------

wta_2018_2021_matches %>%
  mutate(total_aces <- w_ace + l_ace) %>%
  group_by(tourney_level) 

wta_2018_2021_matches %>% 
  mutate(total_ace = w_ace+l_ace) %>% 
  group_by(tourney_level) %>%
  summarise(avg_ace_by_division = sum(total_ace, na.rm=T)/n()) %>%
  pivot_longer(cols = avg_ace_by_division, 
               names_to = "ace", values_to = "value") %>%
  ggplot(aes(x=tourney_level, fill = ace)) +
  geom_bar(aes(y=value), stat='identity', position='dodge') +
  theme_bw()


# surface and aces --------------------------------------------------------

wta_2018_2021_matches %>%
  mutate(total_ace = w_ace+l_ace) %>%
  group_by(surface) %>%
  summarise(all_aces <- sum(total_ace, na.rm = T))









