##Premier League Positions
##libraries
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, gganimate,
               gifski, dplyr,lubridate, scales,ggimage)

##Read the dataframe with Premier League positions
pl<-read.csv("https://raw.githubusercontent.com/claudiodanielpc/football/master/premierleague/premierleague.csv",
             encoding="latin",header=TRUE,check.names=FALSE)


###Generate table with teams, colors and logos
teams<-pl %>%
###all 20 teams  
distinct(team)%>%
  #Sort
  arrange(team)%>%
  ##Colours for team lines
  mutate(color = c( "#ef0107", "#ed1c24",
             "#005daa", "#80bfff",
             "#0000ff", "#034694",
             "#0a4af5", "#0d00e9",
             "grey50", "#192552",
             "#0053a0", "#dd0000",
             "#97c1e7", "#e80909",
             "#000000", "#ff0000",
             "#132257", "#fbee23",
             "#7f0000", "#fdb913"))%>%
  ##You can download the logos from my Github :) :
  #https://github.com/claudiodanielpc/football/tree/master/premierleague/team_logos
  mutate(logos=c("C:/Users/personal/Desktop/equipos/afc.png",
                 "C:/Users/personal/Desktop/equipos/arsenal.png",
                 "C:/Users/personal/Desktop/equipos/aston villa.png",
                 "C:/Users/personal/Desktop/equipos/brighton.png",
                 "C:/Users/personal/Desktop/equipos/burnley.png",
                 "C:/Users/personal/Desktop/equipos/chelsea.png",
                 "C:/Users/personal/Desktop/equipos/crystal.png",
                 "C:/Users/personal/Desktop/equipos/everton.png",
                 "C:/Users/personal/Desktop/equipos/leicester.png",
                 "C:/Users/personal/Desktop/equipos/liverpool.png",
                 "C:/Users/personal/Desktop/equipos/mancity.png",
                 "C:/Users/personal/Desktop/equipos/manutd.png",
                 "C:/Users/personal/Desktop/equipos/newcastle.png",
                 "C:/Users/personal/Desktop/equipos/norwich.png",
                 "C:/Users/personal/Desktop/equipos/sheffield.png",
                 "C:/Users/personal/Desktop/equipos/southampton.png",
                 "C:/Users/personal/Desktop/equipos/spurs.png",
                 "C:/Users/personal/Desktop/equipos/watford.png",
                 "C:/Users/personal/Desktop/equipos/westham.png",
                 "C:/Users/personal/Desktop/equipos/wolves.png"
                 ))%>%as.tibble()


##join table with dataframe
pl<-pl%>%
  inner_join(teams,"team")

##Plot
  p=ggplot(pl,
           aes(matchweek,rev(position),group=team))+
  geom_line(aes(color = team))+
  geom_segment(aes(xend = max(matchweek)+2,
                   yend = rev(position)), linetype = 2)+
    geom_image(aes(image=logos))+
  geom_text(aes(x = max(matchweek) + 2, 
                label = team), 
            size = 4, hjust = 0)+
    transition_reveal(matchweek)+
    coord_cartesian(clip = 'off') +
    theme_minimal()+
  scale_y_continuous(breaks = c(21 - 1, 21 - 4, 21 - 7, 21 - 10, 21 - 18, 21 - 20), 
                     labels = c("1","4","7","10","18","20"))+ 
    scale_x_continuous(limits = c(0, max(pl$matchweek) + 5 ),
                       breaks = c(0,5,10,15,20,25,29,31)) +
    scale_color_manual(values = pl$color)+
  labs(title = "Premier League 2019/2020",
       subtitle = "Positions",
       y = "Position",
       x = "Matchweek",
       caption = "@claudiodanielpc")+
  theme(legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0, size=20,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=17, face="italic"),
        plot.caption = element_text(hjust = 0,size=16))+
    ease_aes('cubic-in-out')
  
  ##Animation
  animate(p, nframes = 300, fps = 15, 
          start_pause = 30, end_pause = 40,
          height = 800, width = 800 )
  
  ##Save
  anim_save("premierleague.gif")
  