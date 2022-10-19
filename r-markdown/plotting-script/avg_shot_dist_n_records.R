## plot average shot distance
## dan plot record
## Helmi M Rizal
## 24 Maret 2022

# load package yang dibutuhkan
pacman::p_load(worldfootballR, dplyr, ggplot2,
               readr, magick, plotly, tidyr)

# extract dataset
barca_shooting <- read_csv("./barca_shooting_fbref.csv")

## 1. avg shot distance

# ambil kolom yang dibutuhkan
shot_location <- barca_shooting %>%
  select(Dist) %>%
  mutate(game = row_number())

# buat kategorisasi pelatih
shot_location$Coach = as.factor(ifelse(shot_location$game<13, 'pre-Xavi', 'Xavi'))

# pisahkan data untuk pelatih Xavi
xavi_shot_location <- shot_location %>%
  filter(Coach == 'Xavi')

# kelompokkan jarak tendangan dari gawang
xsloc <- data.frame(xavi = c(length(which(xavi_shot_location$Dist < 15)),
                             length(which(xavi_shot_location$Dist >= 15 & xavi_shot_location$Dist <= 20)),
                             length(which(xavi_shot_location$Dist > 20))))

# pisahkan data untuk pelatih pre - Xavi
pre_xavi_shot_location <- shot_location %>%
  filter(Coach == 'pre-Xavi')

# kelompokkan jarak tendangan dari gawang
pxsloc <- data.frame(pre_xavi = c(length(which(pre_xavi_shot_location$Dist < 15)),
                                  length(which(pre_xavi_shot_location$Dist >= 15 & pre_xavi_shot_location$Dist <= 20)),
                                  length(which(pre_xavi_shot_location$Dist > 20))))

# buat kelompok jarak
location <- data.frame(area = c("< 15 yard", "15 s/d 20 yard", "> 20 yard"))

# gabungkan pemisahan dan kelompok jarak
shot_loc_comp <- cbind(xsloc, pxsloc, location)

# plotting avg shot distance
slc_plot <- plot_ly()

slc_plot <- slc_plot %>% add_pie(data = shot_loc_comp, labels = ~area, values = ~xavi,
                                 name = "Xavi", domain = list(row = 0, column = 0))

slc_plot <- slc_plot %>% add_pie(data = shot_loc_comp, labels = ~area, values = ~pre_xavi,
                                 name = "pre-Xavi", domain = list(row = 0, column = 1))

slc_plot <- slc_plot %>% layout(title = "FC Barcelona: Average Shot Distance dari Gawang (La Liga 2021/2022)", showlegend = T,
                                grid=list(rows=1, columns=2),
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# tampilkan plot
slc_plot

## 2. result
# ambil kolom yang dibutuhkan
record <- barca_shooting %>%
  select(Result) %>%
  mutate(game = row_number())

# buat kategorisasi pelatih
record$Coach = as.factor(ifelse(record$game<13, 'pre-Xavi', 'Xavi'))

# pisahkan data untuk pelatih Xavi
xavi_record <- record %>%
  filter(Coach == 'Xavi')

# kelompokkan hasil per game
xrec <- data.frame(xavi = c(length(which(xavi_record$Result == 'W')),
                            length(which(xavi_record$Result == 'D')),
                            length(which(xavi_record$Result == 'L'))))

# pisahkan data untuk pelatih pre-Xavi
pre_xavi_record <- record %>%
  filter(Coach == 'pre-Xavi')

# kelompokkan hasil per game
pxrec <- data.frame(pre_xavi = c(length(which(pre_xavi_record$Result == 'W')),
                                 length(which(pre_xavi_record$Result == 'D')),
                                 length(which(pre_xavi_record$Result == 'L'))))

# gabungkan pemisahan dan kelompok hasil
rec <- data.frame(hasil = c("Menang", "Seri", "Kalah"))

# plotting record
record_comp <- cbind(xrec, pxrec, rec)

record_plot <- plot_ly()

record_plot <- record_plot %>% add_pie(data = record_comp, labels = ~hasil, values = ~xavi,
                                 name = "Xavi", domain = list(row = 0, column = 0))

record_plot <- record_plot %>% add_pie(data = record_comp, labels = ~hasil, values = ~pre_xavi,
                                 name = "pre-Xavi", domain = list(row = 0, column = 1))

record_plot <- record_plot %>% layout(title = "FC Barcelona: Records (La Liga 2021/2022)", showlegend = T,
                                grid=list(rows=1, columns=2),
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# tampilkan plot
record_plot 