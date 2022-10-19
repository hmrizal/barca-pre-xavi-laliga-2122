## age vs utility plot
## Helmi M Rizal
## 24 Maret 2022

# load package yang dibutuhkan
pacman::p_load(worldfootballR, dplyr, ggplot2,
               tidyverse, polite, plotly,
               rvest, readr, tidyr)

# scraping data umur pemain di url
session <- bow("https://www.transfermarkt.com/fc-barcelona/leistungsdaten/verein/131/reldata/ES1%262021/plus/1")
print(session)

# mengambil nama-nama kolom
result_name <- scrape(session) %>% 
  html_nodes("#yw1 .bilderrahmen-fixed") %>% 
  html_attr("title") 
# mengambil umur pemain
result_age <- scrape(session) %>% 
  html_nodes(".posrela+ .zentriert") %>% 
  html_text()
# mengambil jumlah menit bermain 
result_mins <- scrape(session) %>% 
  html_nodes("td.rechts") %>% 
  html_text()

# membuat list dari hasil scraping
resultados <- list(result_name, result_age, result_mins)
col_name <- c("name", "age", "minutes")

# lalu reduce(cbind) untuk menggabungkan list, ubah names menjadi cols 
resultados %>% 
  reduce(cbind) %>% 
  as_tibble() %>% 
  set_names(col_name) -> results_comb

glimpse(results_comb)

# transformasi data, simpan ke dataframe barca_minutes
barca_minutes <- results_comb %>% 
  # bandingkan total menit bermain dengan jumlah semua game (28)
  mutate(age = as.numeric(age),
         minutes = minutes %>% 
           str_replace("\\.", "") %>% 
           str_replace("'", "") %>% 
           as.numeric(),
         min_perc = (minutes / 2520) %>% round(digits = 3)) %>% 
  # saring pemain yang belum pernah bermain sama sekali
  filter(!is.na(minutes)) %>% 
  # pisah nama depan dan nama belakang pemain
  # untuk ditampilkan ke grafik
  separate(name, into = c("first_name", "last_name"), sep = " ") %>% 
  
  # ubah penamaan pemain yang tidak sesuai kriteria
  mutate(
    last_name = case_when(                        
      first_name == "Marc-André" ~ "ter Stegen",
      first_name == "Neto" ~ "Neto",
      first_name == "Pedri" ~ "Pedri",
      first_name == "Frenkie" ~ "F. de Jong",
      first_name == "Gavi" ~ "Gavi",
      first_name == "Lucas" ~ "de Vega",
      first_name == "Pierre-Emerick" ~ "Aubameyang",
      first_name == "Luuk" ~ "L. de Jong",
      TRUE ~ last_name)) %>% 
  arrange(desc(min_perc))

barca_minutes <- barca_minutes %>%
  mutate(utility = min_perc * 100) %>%
  mutate(puncak = ifelse(age < 25, "Belum", 
                         ifelse(age >= 25 & age <= 30, 'Ya', 'Sudah Lewat')))

# plot age vs utility
barca_minutes_plot <- plot_ly(barca_minutes, x = ~age, y = ~utility, type = 'scatter', 
                              mode = 'markers',
                              symbol = ~puncak, symbols = c('circle','x','o'),
                              hoverinfo = 'text',
                              text = ~paste('</br> Nama: ', last_name,
                                            '</br> Usia: ', age, ' tahun',
                                            '</br> % Menit Bermain: ', utility))

barca_minutes_plot <- barca_minutes_plot %>%
  layout(yaxis = list(ticksuffix = "%")) %>%
  layout(title = "FC Barcelona: Age vs Utility (La Liga 2021/2022)") %>%
  layout(legend = list(title=list(text=' Usia Puncak? ')))

