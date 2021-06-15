library(tidyverse)

library(gt)
library(ggdendro)
library(seriation)
library(flexclust)
library(protoclust)
# read in data----
=======
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


wta %>%
  filter(minutes<300) %>%
  mutate(fct_relevel(round, 
                     "F", "SF", "QF", "R16", "R32", "R64", "R128", "RR")) %>%
  ggplot(aes(x=minutes)) + 
  geom_density() +
  facet_wrap(~ round) +
  geom_rug(alpha=0.3) + 
  theme_bw()


  # avg double fault and ace by level ----
wta %>% 
  mutate(total_ace = w_ace+l_ace,
         total_df = w_df+l_df,) %>% 
=======
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


full_join(games_lose, games_win, by=c('player', 'surface')) %>%
  rename(wins = "n.y", losses = "n.x") %>% 
  replace_na(list(losses = 0, wins=0)) %>%
  mutate(total = wins+losses, winrate = wins/total) %>%
  arrange(surface, desc(winrate)) %>%
  filter(total >= 10) %>% 
  ungroup() %>%
  group_by(surface) %>%
  slice(1:10) %>%
  ggplot(aes(x=player, y=winrate)) +
  geom_bar(stat='identity') + 
  facet_wrap(~surface, ncol=1) +
  theme_bw()
=======
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
  summarise(total_ace_lose = sum(l_ace, na.rm = T),
            total_df_lose = sum(l_ace, na.rm = T),
            n_game_lose = n()) %>%
  rename(name=loser_name)

all_ace_df = full_join(winner_ace_df, loser_ace_df, by='name') %>%
  replace_na(list(total_ace_win = 0, total_df_win = 0, n_game_win = 0,
                  total_ace_lose = 0, total_df_lose = 0, n_game_lose = 0)) %>%
  mutate(avg_ace = (total_ace_win+total_ace_lose)/(n_game_win+n_game_lose),
         avg_df = (total_df_win+total_df_lose)/(n_game_win+n_game_lose)) %>%
  filter(n_game_win+n_game_lose>10)

player_dist = dist(select(all_ace_df, avg_ace, avg_df))

#heat map
player_dist_matrix = as.matrix(player_dist)
rownames(player_dist_matrix) = all_ace_df$name
colnames(player_dist_matrix) = all_ace_df$name

long_dis_matrix = as_tibble(player_dist_matrix) %>%
  mutate(player1 = rownames(player_dist_matrix)) %>%
  pivot_longer(cols = -player1, names_to = "player2", 
               values_to = "distance")

player_dist_seriate = seriate(player_dist)
player_order = get_order(player_dist_seriate)
player_name_order = all_ace_df$name[player_order]

long_dis_matrix %>%
  mutate(player1 = fct_relevel(player1, player_name_order),
         player2 = fct_relevel(player2, player_name_order)) %>%
  ggplot(aes(x=player1, y=player2, fill=distance)) + 
  geom_tile() + 
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  scale_fill_gradient(low = "darkorange", high = "darkblue")

# hierarchical cluster
ace_df_hclust = hclust(player_dist, method = "complete")  

all_ace_df %>% 
  mutate(player_clusters = as.factor(cutree(ace_df_hclust, k=3))) %>%
  ggplot(aes(x=avg_ace, y=avg_df, color=player_clusters)) + 
  geom_point() + 
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
        panel.grid = element_blank())# +
  #geom_hline(yintercept = 6, linetype = "dashed", color = "darkred")


# kmeans++ cluster
wta_kmeanspp = kcca(select(all_ace_df, avg_ace, avg_df), k=3, 
                control = list(initcent = "kmeanspp"))
all_ace_df %>%
  mutate(player_clusters = as.factor(wta_kmeanspp@cluster)) %>%
  ggplot(aes(x=avg_ace, y=avg_df, color=player_clusters)) + 
  geom_point() +
  theme_bw() +
  theme(legend.position = "bottom")

# minimax linkage
wta_minimax = protoclust(player_dist)

ggdendrogram(wta_minimax, theme_dendro = F, labels = F, leaf_labels = F) +
  theme_bw() + 
  labs(y = "Dissimilarity between clusters") +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())# +
#geom_hline(yintercept = 6, linetype = "dashed", color = "darkred")

all_ace_df %>% 
  mutate(player_clusters = as.factor(protocut(wta_minimax, k=3)$cl)) %>%
  ggplot(aes(x=avg_ace, y=avg_df, color=player_clusters)) + 
  geom_point() + 
  theme_bw() +
  theme(legend.position = "bottom")
=======
library(ggdendro)

