## plot shot creating actions
## Helmi M Rizal
## 24 Maret 2022

# load package yang dibutuhkan
pacman::p_load(worldfootballR, dplyr, ggplot2,
               readr, magick, plotly)

# extract data yang diperlukan
# data di-export dari link di bawah ini
# https://fbref.com/en/squads/206d90db/2021-2022/matchlogs/s11174/gca/Barcelona-Match-Logs-La-Liga
barca_creation <- read.csv("./barca_creation_fbref.csv")

# buat dataframe shot creating action sesuai kolom yang dibutuhkan
sca_per_game <- barca_creation %>%
  select(Opponent, SCA) %>%
  mutate(game = row_number())

# buat kategori baru berdasarkan pelatih FC Barcelona
sca_per_game$Coach <- as.factor(ifelse(sca_per_game$game<13, 'pre-Xavi', 'Xavi'))

# mulai plotting
SCA_plot <- ggplot(sca_per_game, aes(x = game, 
                                     y = SCA, 
                                     label = Opponent,
                                     fill= Coach)) +
  geom_bar(stat='identity', alpha=.95) +
  theme(plot.caption = element_text(size = 8)) +
  labs(title='FC Barcelona: Shot Creating Actions (SCA) per game (La Liga 2021/2022)',
       x='Game ke-',
       y='SCA') +
  theme_bw() +
  theme(legend.position = 'top')

SCA_plot

# plotting boxplot distribusi xG
# dibedakan berdasarkan pelatih FC Barcelona
SCA_boxplot <- ggplot(sca_per_game, aes(x = Coach, y = SCA)) +
  geom_boxplot(aes(fill = Coach)) +
  theme(plot.caption = element_text(size = 8)) +
  labs(title='FC Barcelona: Distribusi Shot Creating Actions (SCA) per game (La Liga 2021/2022)',
       x='Coach',
       y='SCA') +
  theme_bw() +
  theme(legend.position = 'top')

SCA_boxplot

# buat interactive plot
ggplotly(SCA_plot) 

# interactive boxplot
ggplotly(SCA_boxplot) %>%
  layout(showlegend=FALSE)
  
