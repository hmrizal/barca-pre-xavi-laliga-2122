## plot recovery
## dan plot aerial duels won
## Helmi M Rizal
## 24 Maret 2022

# load package yang dibutuhkan
pacman::p_load(worldfootballR, dplyr, ggplot2,
               readr, magick, plotly, tidyr)

# extract data yang diperlukan
# data di-export dari link di bawah ini
# https://fbref.com/en/squads/206d90db/2021-2022/matchlogs/s11174/misc/Barcelona-Match-Logs-La-Liga
barca_duel <- read.csv("./barca_duel_fbref.csv")

# buat dataframe recovery
# dan aerial duels won
# sesuai kolom yang dibutuhkan
recov_adw_per_game <- barca_duel %>%
  select(Opponent, Recov, Won.) %>%
  mutate(game = row_number())

# buat kategori baru berdasarkan pelatih FC Barcelona
recov_adw_per_game$Coach <- as.factor(ifelse(recov_adw_per_game$game<13, 'pre-Xavi', 'Xavi'))

# mulai plotting: 1. recovery
recov_plot <- ggplot(recov_adw_per_game, aes(x = game, 
                                             y = Recov, 
                                             label = Opponent,
                                             fill= Coach)) +
  geom_bar(stat='identity', alpha=.95) +
  theme(plot.caption = element_text(size = 8)) +
  labs(title='FC Barcelona: Recovery per game (La Liga 2021/2022)',
       subtitle ='data dari fbref.com (sampai game ke-28, 21 Maret 2022)',
       x='Game ke-',
       y='Recovery') +
  theme_bw() +
  theme(legend.position = 'top')

recov_plot

# mulai plotting: 2. aerial duels won
adw_plot <- ggplot(recov_adw_per_game, aes(x = game, 
                                             y = Won., 
                                             label = Opponent,
                                             fill= Coach)) +
  geom_bar(stat='identity', alpha=.95) +
  theme(plot.caption = element_text(size = 8)) +
  labs(title='FC Barcelona: Aerial Duels Won (ADW) per game (La Liga 2021/2022)',
       x='Game ke-',
       y='ADW (dalam %)') +
  theme_bw() +
  theme(legend.position = 'top')

adw_plot

# pivot dataframe untuk plot distribusi
recov_adw_pivot <- recov_adw_per_game %>%
  select(-Opponent) %>%
  pivot_longer(recov_adw_per_game,
               cols = c('Recov', 'Won.'),
               names_to = 'bwa',
               values_to = 'values')
  
# plotting boxplot distribusi xG
# dibedakan berdasarkan pelatih FC Barcelona
recov_adw_boxplot <- ggplot(recov_adw_pivot, aes(x = bwa, 
                                                 y = values,
                                                 fill = Coach)) +
  geom_boxplot() +
  theme(plot.caption = element_text(size = 14)) +
  labs(title='FC Barcelona: Distribusi Ball Winning Action (BWA) per game (La Liga 2021/2022)',
       x = '',
       y = '') +
  theme_bw() +
  theme(legend.position = 'top') +
  facet_wrap(~bwa,
             scales = "free_y",
             strip.position = "top", 
             labeller = as_labeller(c(Recov = "Recovery", Won. = "ADW (dalam %)")))

recov_adw_boxplot

# buat interactive plot: 1. recov
ggplotly(recov_plot)

# buat interactive plot: 2. adw
ggplotly(adw_plot)

# interactive boxplot
ggplotly(recov_adw_boxplot) %>%
  layout(legend = list(orientation = "h",
                       xanchor = "center",
                       x = 0.5)) %>%
  layout(boxmode='group')
