
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, gganimate,jsonlite,httr,ggimage, showtext)

font_add_google(name = "Montserrat",
                family = "Montserrat")


url_inicio <- "https://footballapi.pulselive.com/football/standings?compSeasons=489&altIds=true&detail=2&FOOTBALL_COMPETITION=1&gameweekNumbers=1-"
url_final <- "&live=true"

urls <- c()
for (fechas in 1:38) {
  urls <- c(urls, paste0(url_inicio, fechas, url_final))
}

headers <- c(
  'Origin' = 'https://www.premierleague.com',
  'Referer' = 'https://www.premierleague.com',
  'user-agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36'
)

# #Obtener datos

team_names <- list()
positions <- list()
gameweeks <- list()  
for (i in 1:length(urls)) {
  response <- GET(urls[i], add_headers(.headers=headers))
  # Datos en formato JSON
  data <- content(response, as="parsed", encoding="UTF-8")
  
  for (j in 1:20) {
    team_names <- c(team_names, data[["tables"]][[1]][["entries"]][[j]][["team"]][["name"]])
    positions <- c(positions, data[["tables"]][[1]][["entries"]][[j]][["position"]])
    gameweeks <- c(gameweeks, data[["tables"]][[1]][["entries"]][[j]][["form"]][[1]][["gameweek"]][["gameweek"]])
  }
}
#Construir dataframe
df <- data.frame(team = unlist(team_names), position = unlist(positions), gameweek = unlist(gameweeks))
#Dejar solo Arsenal y Manchester City
df<-df%>%
  filter(team=="Arsenal" | team=="Manchester City")

#Obtener los logos del Arsenal y del Manchester City
url_logo_arsenal <- "https://github.com/claudiodanielpc/football/blob/master/premierleague/team_logos/arsenal.png?raw=true"
url_logo_mancity <- "https://github.com/claudiodanielpc/football/blob/master/premierleague/team_logos/mancity.png?raw=true"

#Pegar los logos en el dataframe
df<-df%>%
  mutate(logo=ifelse(team=="Arsenal",url_logo_arsenal,url_logo_mancity),
        team_color=ifelse(team=="Arsenal","red","blue"))



tabla<-ggplot(df,
              aes(gameweek,position,group=team))+
  geom_line(aes(color = team))+
  geom_segment(aes(xend = max(gameweek)+2,
                   yend = position), linetype = 2)+
  geom_text(aes(x = max(gameweek) + 2, 
                label = team), 
            size = 4, hjust = 0)+
  geom_image(aes(image=logo),size=0.05)+
  coord_cartesian(clip = 'off') +
  scale_x_continuous(limits = c(0, max(df$gameweek) + 5 ),
                     breaks = c(0, seq(5, max(df$gameweek) + 5, 5)))+
  scale_y_reverse()+
  scale_color_manual(values = df$team_color)+
  
  
  theme_minimal()+
  labs(title = "Premier League 2022/2023",
       subtitle = "Positions",
       y = "Position",
       x = "Gameweek",
       caption = "@claudiodanielpc using Premier League data.")+
  theme(legend.position = "none",
        text=element_text(family="Montserrat"),
      
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0, size=20,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=17, face="italic"),
        plot.caption = element_text(hjust = 0,size=16))+transition_reveal(gameweek)


##Animation
animate(tabla, nframes = 300, fps = 10, 
        start_pause = 30, end_pause = 40,
        height = 800, width = 1000 )

##Save
anim_save("D:/github/football/premierleague/premierleague_22_23.gif")


