## plot xG
## Helmi M. Rizal
## 24 Maret 2022

# load package yang dibutuhkan
pacman::p_load(worldfootballR, dplyr, ggplot2, 
               plotly, magick)

# extract data yang diperlukan
# lalu intip data tsb
barca_shots <- understat_team_season_shots(team_url = "https://understat.com/team/Barcelona/2021")

# buat dataframe xG sesuai kolom yang dibutuhkan
xG_per_game <- barca_shots %>%
  select(match_id, xG) %>%
  group_by(match_id) %>%
  summarize(xG = round(sum(xG), digits = 2)) %>%
  mutate(game = row_number())
  
# buat kategori baru berdasarkan pelatih FC Barcelona
xG_per_game$Coach <- as.factor(ifelse(xG_per_game$game<13, 'pre-Xavi', 'Xavi'))

# mulai plotting  
xG_plot <- ggplot(xG_per_game, aes(x = game, y = xG, fill = Coach)) +
  geom_bar(stat='identity', alpha=.95) +
  theme(plot.caption = element_text(size = 8)) +
  labs(title='FC Barcelona: Expected Goals (xG) per game (La Liga 2021/2022)',
       subtitle ='data dari Understat.com (sampai game ke-28, 21 Maret 2022)',
       x='Game ke-',
       y='xG') +
  theme_bw() +
  theme(legend.position = 'top')

xG_plot

# plotting boxplot distribusi xG
# dibedakan berdasarkan pelatih FC Barcelona
xG_boxplot <- ggplot(xG_per_game, aes(x = Coach, y = xG)) +
  geom_boxplot(aes(fill = Coach)) +
  theme(plot.caption = element_text(size = 8)) +
  labs(title='FC Barcelona: Distribusi Expected Goals (xG) per game (La Liga 2021/2022)',
       x='Coach',
       y='xG') +
  theme_bw() +
  theme(legend.position = 'top')

xG_boxplot

# interactive xG plot
ggplotly(xG_plot)

# interactive xG boxplot
ggplotly(xG_boxplot) %>%
  layout(showlegend=FALSE)

