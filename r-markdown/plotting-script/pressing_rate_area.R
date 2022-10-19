## plot pressing rate area
## Helmi M Rizal
## 24 Maret 2022

# load package yang dibutuhkan
pacman::p_load(worldfootballR, dplyr, ggplot2,
               readr, magick, plotly, tidyr)

# ambil dataset yang diperlukan
barca_defense <- read.csv("./barca_defense_fbref.csv")

# hitung dan buat dataframe pressing rate
pressing_rate <- barca_defense %>%
  select(Opponent, Press, Def.3rd.1, Mid.3rd.1, Att.3rd.1) %>%
  mutate(def_pct = (Def.3rd.1/Press) %>% round(digits = 3) * 100,
         mid_pct = (Mid.3rd.1/Press) %>% round(digits = 3) * 100,
         att_pct = (Att.3rd.1/Press) %>% round(digits = 3) * 100,
         game = row_number())
         
# bagi dalam 2 periode kepelatihan
pressing_rate$Coach = as.factor(ifelse(pressing_rate$game<13, 'pre-Xavi', 'Xavi'))

# pivot dataframe untuk plotting
pra_pivot <- pressing_rate %>%
  select(-c(1:5)) %>%
  pivot_longer(pressing_rate_area,
               cols = ends_with('pct'),
               names_to = 'area',
               values_to = 'pct')

# klasifikasi area pressing
pra_pivot$area <- factor(pra_pivot$area, levels = c('def_pct', 'mid_pct', 'att_pct'))

# plotting pressing rate area
pra_boxplot <- ggplot(pra_pivot, aes(x = area, y = pct, fill = Coach)) +
  geom_boxplot() +
  theme(plot.caption = element_text(size = 14)) +
  labs(title='FC Barcelona: Distribusi Pressing Rate Area (PRA) per game (La Liga 2021/2022)',
       x = '',
       y = '') +
  theme_bw() +
  theme(legend.position = 'top') +
  facet_wrap(~area,
             strip.position = "top", 
             labeller = as_labeller(c(def_pct = "Defensive Third (dalam %)", 
                                      mid_pct = "Middle Third (dalam %)",
                                      att_pct = "Attacking Third (dalam %)")))

pra_boxplot

# interactive boxplot
## judul kurang naik
ggplotly(pra_boxplot) %>%
  layout(legend = list(orientation = "h",
                       xanchor = "center",
                       x = 0.5)) %>%
  layout(boxmode='group')  
