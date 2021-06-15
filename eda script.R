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


# Tournament level and aces made--------------------------------------------------------

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


# Hypothesis 1: surface and aces --------------------------------------------------------

wta_2018_2021_matches %>%
  mutate(total_ace = w_ace+l_ace) %>%
  group_by(surface) %>%
  summarise(all_aces <- sum(total_ace, na.rm = T))

wta_2018_2021_matches %>%
  mutate(total_ace = w_ace + l_ace) %>%
  filter(total_ace < 50) %>%
  ggplot(aes(y = total_ace, fill = surface)) +
  geom_boxplot(aes(x= '')) +
  coord_flip() +
  theme_bw()


# Hypothesis 2: 1st serve win rate ------------------------------------------------------

wta_2018_2021_matches %>%
  mutate(w_1stRate = w_1stWon / w_1stIn, l_1stRate = l_1stWon / l_1stIn) %>%
  summarise(w_1stRate, l_1stRate) 

# Hypothesis 3: Match length by round ----

wta_2018_2021_matches %>%
  group_by(round) %>%
  summarise(avg_length = sum(minutes, na.rm=TRUE)/n())


wta_2018_2021_matches %>%
  filter(minutes < 500) %>%
  ggplot(aes(x = minutes)) +
  geom_density() +
  facet_wrap(~ round) +
  geom_rug(alpha = .3) +
  theme_bw()
  

#Clustering ------

w_df_ace <-
  wta_2018_2021_matches %>%
  group_by(winner_name) %>%
  summarise(total_ace_w = sum(w_ace, na.rm = TRUE),
            total_df_w = sum(w_df, na.rm = TRUE),
            n_game_w = n()) %>%
  rename(name=winner_name)

l_df_ace <-
  wta_2018_2021_matches %>%
  group_by(loser_name) %>%
  summarise(total_ace_l = sum(l_ace, na.rm = TRUE),
            total_df_l = sum(l_df, na.rm = TRUE),
            n_game_l = n()) %>%
  rename(name = loser_name)

all_df_ace <-
  full_join(w_df_ace, l_df_ace, by = 'name') %>%
  replace_na(list(total_ace_w = 0, total_df_w = 0,
                  n_game_w = 0, total_ace_l = 0,
                  total_df_l = 0, n_game_l = 0)) %>%
  mutate(avg_ace = (total_ace_w + total_ace_l)/(n_game_w + n_game_l),
         avg_df = (total_df_w +total_df_l)/(n_game_w + n_game_l))


player_dist <- 
  dist(dplyr::select(all_df_ace, avg_ace, avg_df))

wta_hclust<-
  hclust(player_dist, method = "complete")

all_df_ace %>%
  mutate(player_clusters =
           as.factor(cutree(wta_hclust, k = 3))) %>%
  ggplot(aes(x = avg_ace, y = avg_df,
             color = player_clusters)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "bottom")

ggdendrogram(wta_hclust, theme_dendro = FALSE,
             labels = FALSE, leaf_labels = FALSE) +
  labs(y = "Dissimilarity between clusters") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

library(ggdendro)
