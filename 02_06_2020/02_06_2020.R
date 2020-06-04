library(tidyverse)
library(glue)
library(scales)
library(lubridate)
library(magrittr)

marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

head(marbles)

srgb <- function(...){
  rgb(...,maxColorValue = 255)
}


team_col <- c("Rojo Rollers" = srgb(221,92,95),
              "Thunderbolts" = srgb(53,77,122),
              "Balls of Chaos" = srgb(254,151,2),
              "Team Momo" = srgb(91, 154, 75),
              "Savage Speeders" = srgb(137,23,6),
              "Snowballs" = srgb(149,187,234),
              "Green Ducks" = srgb(135,154,75),
              "Hornets" = srgb(3,3,5),
              "Mellow Yellow" = srgb(245,213,0),
              "Team Galactic" = srgb(200,74,223),
              "O'rangers" = srgb(255,119,11),
              "Raspberry Racers" = srgb(223,35,73),
              "Hazers" = srgb(175,187,195),
              "Limers" = srgb(77,99,12),
              "Midnight Wisps" = srgb(110,209,246),
              "Team Primary" = srgb(223,181,39))

backgr_col <- srgb(204,226,239)

# f <- function(x) {
#   x <- na.omit(x)
#   if (length(x) > 0) paste(x,collapse='-') else NA
# }
# https://stackoverflow.com/questions/39920870/merging-rows-with-shared-information

marbles %>% 
  select(- source, -date, -host,- notes) %>% 
  mutate(RaceMode = stringr::str_sub(race, 3, 3)) %>%
  mutate(RaceNmbr = stringr::str_sub(race, 4, 4)) %>% 
  mutate(avg_speed = case_when(RaceMode == "Q" ~ number_laps*track_length_m/time_s,
                               RaceMode =="R" ~ number_laps*track_length_m/time_s)) %>% 
  group_by(team_name, RaceNmbr) %>% 
  summarise(qual_pos = as.numeric(stringr::str_sub(pole[1],2,3)),
            race_points = points[2],
            time_pos = time_s[2]) %>%
  ungroup() %>% 
  arrange(RaceNmbr, time_pos) %>% 
  group_by(RaceNmbr) %>% 
  mutate(race_pos = row_number()) %>% 
  ungroup() %>% 
  select(race_pos, qual_pos) %>% 
  
  lm(formula = race_pos ~ qual_pos, data = ., method = "qr") %>% 
  summary(.)  %>% 
  .$coefficients %>% 
  as_tibble() %>% 
  select(Estimate) %>% 
  mutate(xpos = 15.5, ypos = c(1,2), 
         lab = c("Intercept", "Slope")) -> lm_vars


marbles %>% 
  select(- source, -date, -host,- notes) %>% 
  mutate(RaceMode = stringr::str_sub(race, 3, 3)) %>%
  mutate(RaceNmbr = stringr::str_sub(race, 4, 4)) %>% 
  mutate(avg_speed = case_when(RaceMode == "Q" ~ number_laps*track_length_m/time_s,
                               RaceMode =="R" ~ number_laps*track_length_m/time_s)) %>% 
  group_by(team_name, RaceNmbr) %>% 
  summarise(qual_pos = as.numeric(stringr::str_sub(pole[1],2,3)),
            race_points = points[2],
            time_pos = time_s[2]) %>%
  ungroup() %>% 
  arrange(RaceNmbr, time_pos) %>% 
  group_by(RaceNmbr) %>% 
  mutate(race_pos = row_number()) -> data_race_qual

arrows_df <- data.frame("x" = 2,
                        "y" = 14,
                        "xend" = c(1.2, 1.2),
                        "yend" = c(15.8, 14))

text_df   <- data.frame("lab" = "Starting first isnt\n always an advantage!",
                        "x" = 2,
                        "y" = 14,
                        "hjust" = 1)

data_race_qual %>% 
  {
    ggplot(data = ., mapping = aes(x = qual_pos, y = race_pos)) +
      geom_count() +
      geom_smooth(stat = "smooth", se = TRUE, method = "lm") + 
      scale_x_reverse() +
      scale_y_reverse() + 
      theme_minimal() +
      geom_text(data = lm_vars, aes(x = xpos, y = ypos, label = glue("{lab}: {round(Estimate,2)}")), hjust = 0) +
      geom_segment(data = arrows_df, aes(x = x, xend = xend, y = y, yend = yend), arrow = arrow(length = unit(2, "mm"), type = "closed")) + 
      geom_text(data = text_df, aes(x=x, y=y, label=lab, hjust=hjust))+
      labs(
        title = "Correlation between Qualifying and Race Position",
        subtitle = "Simple Linear Model",
        x = "Qualification Position",
        y = "Race Position",
        caption = "TidyTuesday 02 June 2020"
        
      )+
      theme(
        panel.background = element_rect(fill = backgr_col,
                                        colour = backgr_col,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        legend.position = "none"
      )
    # geom_line(data= lm_cqr) 
  }

ggsave('Corr_Q_R_Linear.png', width = 10, height = 6)

text_df_loess   <- data.frame("lab" =c("Starting in the lower 6 positions\n will most likely result in\n no points!",
                                       "While Starting in the upper 6 positions\n will likely result a good result!"),
                              "x" = c(15,2),
                              "y" = c(2,14),
                              "hjust" = c(0,1),
                              "vjust"= c(1,0.5))

arrows_df_loess <- data.frame("x" = c(14,4),
                              "y" = c(4,13),
                              "xend" = c(14, 3),
                              "yend" = c(11, 5.5))


data_race_qual  %>% 
  
  {
    ggplot(data = ., mapping = aes(x = qual_pos, y = race_pos)) +
      geom_count() +
      geom_smooth(stat = "smooth", se = TRUE, method = "loess") + 
      scale_x_reverse() +
      scale_y_reverse() + 
      theme_minimal() +
      geom_segment(data = arrows_df_loess, aes(x = x, xend = xend, y = y, yend = yend), arrow = arrow(length = unit(2, "mm"), type = "closed")) + 
      geom_text(data = text_df_loess, aes(x=x, y=y, label=lab, hjust=hjust, vjust=vjust))+
      labs(
        title = "Correlation between Qualifying and Race Position",
        subtitle = "Local Regression Model",
        x = "Qualification Position",
        y = "Race Position",
        caption = "TidyTuesday 02 June 2020"
        
      ) +
      theme(
        panel.background = element_rect(fill = backgr_col,
                                        colour = backgr_col,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        legend.position = "none"
      )
    # geom_line(data= lm_cqr) 
  }

ggsave('Corr_Q_R_Loess.png', width = 10, height = 6)

marbles %>% 
  select(- source, -date, -host,- notes) %>% 
  mutate(RaceMode = stringr::str_sub(race, 3, 3),
         RaceNmbr = stringr::str_sub(race, 4, 4),
         avg_speed = case_when(RaceMode == "Q" ~ number_laps*track_length_m/time_s,
                               RaceMode =="R" ~ number_laps*track_length_m/time_s)) %>%
  group_by(team_name) %>% 
  mutate(final_points = sum(points, na.rm=T)) %>% 
  select(RaceMode, RaceNmbr, avg_speed, team_name, final_points) %>% 
  {
    ggplot(data = ., mapping = aes(x= reorder(team_name, avg_speed, FUN = median), 
                                   y = avg_speed, fill = team_name)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      scale_fill_manual(values = rep(c(team_col),2)) +
      geom_text(aes(y= 0.46, x = team_name, label = final_points, color = team_name)) +
      
      scale_colour_manual(values=rep("white",16))+
      theme(
        panel.background = element_rect(fill = backgr_col,
                                        colour = backgr_col,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        legend.position = "none"
      ) +
      labs(
        title = "Speed and Points of all Teams",
        subtitle = "Including both Race and Qualy, Points in White inside the boxes",
        x = "",
        y = "Speed in [m/s]",
        caption = "TidyTuesday 02 June 2020"
        
      ) 
    
  }

ggsave('Speeds_and_points.png', width = 10, height = 6)


marbles %>% 
  select(- source, -date, -host,- notes) %>% 
  mutate(RaceMode = stringr::str_sub(race, 3, 3),
         RaceNmbr = stringr::str_sub(race, 4, 4),
         avg_speed = case_when(RaceMode == "Q" ~ number_laps*track_length_m/time_s,
                               RaceMode =="R" ~ number_laps*track_length_m/time_s)) %>%
  group_by(team_name) %>% 
  mutate(final_points = sum(points, na.rm=T)) %>% 
  select(RaceMode, RaceNmbr, avg_speed, team_name, final_points) %>% 
  {
    ggplot(data = ., mapping = aes(x= reorder(team_name, final_points, FUN = min), 
                                   y = avg_speed, fill = team_name)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      scale_fill_manual(values = rep(c(team_col),2)) +
      geom_text(aes(y= 0.46, x = team_name, label = final_points, color = team_name)) +
      
      scale_colour_manual(values=rep("white",16))+
      theme(
        panel.background = element_rect(fill = backgr_col,
                                        colour = backgr_col,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        legend.position = "none"
      ) +
      labs(
        title = "Speed and Points of all Teams",
        subtitle = "Including both Race and Qualy, Points in White inside the boxes",
        x = "",
        y = "Speed in [m/s]",
        caption = "TidyTuesday 02 June 2020"
        
      ) 
    
  }

ggsave('Speeds_and_points_by_points.png', width = 10, height = 6)



marbles %>% 
  select(- source, -date, -host,- notes) %>% 
  mutate(RaceMode = stringr::str_sub(race, 3, 3),
         RaceNmbr = stringr::str_sub(race, 4, 4),
         avg_speed = case_when(RaceMode == "Q" ~ number_laps*track_length_m/time_s,
                               RaceMode =="R" ~ number_laps*track_length_m/time_s)) %>%
  group_by(team_name) %>% 
  mutate(final_points = sum(points, na.rm=T)) %>% 
  select(RaceMode, RaceNmbr, avg_speed, team_name, final_points) %>% 
  {
    ggplot(data = ., mapping = aes(x= reorder(RaceMode, avg_speed, FUN = median), 
                                   y = avg_speed, fill = team_name)) +
      geom_boxplot() +
      facet_grid(~ team_name, scales = "free") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      
      #scale_fill_manual(values = rep(c(team_col),2)) +
      # geom_text(aes(y= 0.46, x = team_name, label = final_points, color = team_name)) +
      #  aes(fill=rep(team_col,16)) +
      #scale_colour_manual(values = team_col) +
      scale_fill_manual(values = team_col) +
      #scale_fill_brewer(palette = "Set1"
      #                  , name = "team_name") +
      theme(
        panel.background = element_rect(fill = backgr_col,
                                        colour = backgr_col,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        legend.position = "none"
      ) +
      scale_x_discrete(labels=c("Q" = "Qualy", "R" = "Race")) +
      labs(
        title = "Speed during Race and Qualification",
        subtitle = "For all teams",
        x = "",
        y = "Speed in [m/s]",
        caption = "TidyTuesday 02 June 2020"
        
      ) 
    
  }
