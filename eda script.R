library(tidyverse)
library(gt)
library(ggdendro)
library(seriation)
library(flexclust)
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
  facet_wrap(~ round) +
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

# 3rd: player win rates on surface type ----
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


# cluster avg aces df----
winner_ace_df = wta %>%
  group_by(winner_name) %>%
  summarise(total_ace_win = sum(w_ace, na.rm = T),
          total_df_win = sum(w_df, na.rm = T),
          n_game_win = n()) %>%
  rename(name=winner_name)

loser_ace_df = wta %>%
  group_by(loser_name) %>%
  summarise(total_ace_lose = sum(l_ace, na.rm = T),
            total_df_lose = sum(l_ace, na.rm = T),
            n_game_lose = n()) %>%
  rename(name=loser_name)

all_ace_df = full_join(winner_ace_df, loser_ace_df, by='name') %>%
  replace_na(list(total_ace_win = 0, total_df_win = 0, n_game_win = 0,
                  total_ace_lose = 0, total_df_lose = 0, n_game_lose = 0)) %>%
  mutate(avg_ace = (total_ace_win+total_ace_lose)/(n_game_win+n_game_lose),
         avg_df = (total_df_win+total_df_lose)/(n_game_win+n_game_lose))

#heat map
player_dist = dist(select(all_ace_df, avg_ace, avg_df))

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
  theme_bw() +
  theme(legend.position = "bottom")

ggdendrogram(ace_df_hclust, theme_dendro = F, labels = F, leaf_labels = F) +
  theme_bw() + 
  labs(y = "Dissimilarity between clusters") +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

# kmeans++ cluster
wta_kmeanspp = kcca(select(all_ace_df, avg_ace, avg_df), k=3, 
                control = list(initcent = "kmeanspp"))
all_ace_df %>%
  mutate(player_clusters = as.factor(wta_kmeanspp@cluster)) %>%
  ggplot(aes(x=avg_ace, y=avg_df, color=player_clusters)) + 
  geom_point() +
  theme_bw() +
  theme(legend.position = "bottom")
