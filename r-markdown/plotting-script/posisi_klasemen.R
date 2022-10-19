## plot pergerakan klasemen
## Helmi M Rizal
## 24 Maret 2022

# load package yang dibutuhkan
pacman::p_load(worldfootballR, dplyr, ggplot2,
               readr, magick, plotly, tidyr, data.table)

# extract dataset
df <- read_csv("SP1.csv")

# Tambah kolom untuk home dan away point
df <- df %>% mutate(HomePoints = ifelse(FTR == "H", 3, ifelse(FTR == "D", 1, 0)),
                  AwayPoints = ifelse(FTR == "H", 0, ifelse(FTR == "D", 1, 3)))

# Klasemen untuk hasil home
HomeTable <- df %>% 
  group_by(HomeTeam) %>% 
  summarise(HomeGames = n(), 
            HomeWins = sum(FTR == "H"),
            HomeDraws = sum(FTR == "D"),
            HomeLosses = sum(FTR == "A"),
            HomeGoals = sum(FTHG),
            HomeConcede = sum(FTAG),
            HomeGD = HomeGoals - HomeConcede,
            HomePoints = sum(HomePoints)) %>% 
  rename("Team" = "HomeTeam")

# Klasemen untuk hasil away
AwayTable <- df %>% 
  group_by(AwayTeam) %>% 
  summarise(AwayGames = n(), 
            AwayWins = sum(FTR == "A"), 
            AwayDraws = sum(FTR == "D"), 
            AwayLosses = sum(FTR == "H"), 
            AwayGoals = sum(FTAG), 
            AwayConcede = sum(FTHG), 
            AwayGD = AwayGoals - AwayConcede, 
            AwayPoints = sum(AwayPoints)) %>% 
  rename("Team" = "AwayTeam")

# Gabungkan klasemen home dan away
TotalTable <- merge(HomeTable, AwayTable, by = "Team") %>% 
  mutate(Played = HomeGames + AwayGames, 
         Wins = HomeWins + AwayWins, 
         Draws = HomeDraws + AwayDraws, 
         Losses = HomeLosses + AwayLosses, 
         GF = HomeGoals + AwayGoals, 
         GA = HomeConcede + AwayConcede, 
         GD = HomeGD + AwayGD, 
         Points = HomePoints + AwayPoints)

#Keeping only the total standings
LeagueTable <- TotalTable[,c(1,18:25)]
LeagueTable <- LeagueTable[order(-LeagueTable$Points, -LeagueTable$GD),]
LeagueTable

# Buat list kosong untuk menyimpan ranking tiap team per week
listofranks <- list()

# Buat for loop 
# untuk menyimpan klasemen dalam list kosong per week
for(i in 1:ceiling(nrow(df)/10)) 
    {
      z <- df %>% 
        head(n = i*10) %>% 
        mutate(HomePoints = ifelse(FTR == "H", 3, ifelse(FTR == "D", 1, 0)), 
               AwayPoints = ifelse(FTR == "H", 0, ifelse(FTR == "D", 1, 3)))
      home_standing <- z %>% 
        group_by(HomeTeam) %>% 
        summarise(HomeGames = n(), 
                  HomeWins = sum(FTR == "H"), 
                  HomeDraws = sum(FTR == "D"), 
                  HomeLosses = sum(FTR == "A"), 
                  HomeGoals = sum(FTHG), 
                  HomeConcede = sum(FTAG), 
                  HomeGD = HomeGoals - HomeConcede, 
                  HomePoints = sum(HomePoints)) %>% 
        rename("Team" = "HomeTeam")
      away_standing <- z %>% 
        group_by(AwayTeam) %>% 
        summarise(AwayGames = n(), 
                  AwayWins = sum(FTR == "A"), 
                  AwayDraws = sum(FTR == "D"), 
                  AwayLosses = sum(FTR == "H"), 
                  AwayGoals = sum(FTAG), 
                  AwayConcede = sum(FTHG), 
                  AwayGD = AwayGoals - AwayConcede, 
                  AwayPoints = sum(AwayPoints)) %>% 
        rename("Team" = "AwayTeam")
      
      # Merge menjadi 'all = TRUE' 
      # sehingga klasemen dapat terbentuk setelah week pertama, 
      # dengan hanya 10 team will di home standing dan sisanya di away standing
      total_standing <- merge(home_standing, away_standing, by = "Team", all = TRUE)
      
      # NA dari merge sebelumnya diubah menjadi 0
      total_standing[is.na(total_standing)] <- 0
      
      total_standing <- total_standing %>% 
        mutate(Played = HomeGames + AwayGames, 
               Wins = HomeWins + AwayWins, 
               Draws = HomeDraws + AwayDraws, 
               Losses = HomeLosses + AwayLosses, 
               GF = HomeGoals + AwayGoals, 
               GA = HomeConcede + AwayConcede, 
               GD = HomeGD + AwayGD, 
               Points = HomePoints + AwayPoints)
      
      league_standing <- total_standing[,c(1,18:25)]
      league_standing <- league_standing[order(-league_standing$Points, 
                                               -league_standing$GD, 
                                               -league_standing$GF),] %>% 
        mutate(Rank = 1:20, Week = i)
      
      listofranks[[i]] <- league_standing[,c(1,8:11)]
}

# Gunakan rbindlist dari package data.table 
# untuk menggabungkan list dataset sebelumnya
weekly_standing <- rbindlist(listofranks)
head(weekly_standing)
tail(weekly_standing)

# filter klasemen mingguan untuk Barcelona
barca_weekly_standing <- weekly_standing %>%
  filter(Team == "Barcelona")

# buat kategori baru berdasarkan pelatih FC Barcelona
barca_weekly_standing$Coach <- as.factor(ifelse(barca_weekly_standing$Week<13, 'pre-Xavi', 'Xavi'))

# plotting posisi klasemen Barca
barca_movement_plot <- ggplot(barca_weekly_standing, aes(x = Week, y = Rank)) +
  geom_point(stat='identity', alpha=.95) +
  theme(plot.caption = element_text(size = 8)) +
  labs(title='FC Barcelona: Posisi Klasemen per game (La Liga 2021/2022)',
       subtitle ='data dari football-data.co.uk (sampai game ke-28, 21 Maret 2022)',
       x='Game ke-',
       y='Posisi ke-') +
  geom_path(aes(x = Week, y = Rank, color = Coach)) +
  scale_y_reverse() +
  theme_bw() +
  theme(legend.position = 'top')

ggplotly(barca_movement_plot) %>%
  layout(legend = list(orientation = "h",
                       xanchor = "center",
                       x = 0.5)) 
