library(SBpitch)
library(ggrepel)
getDangerMap(leic_hull, "Hull City", "Leicester v Hull (02/09/2023)")

getDangerMap <- function(game, teamName, gameName) {
  passes <- game %>% filter(type == "Pass")
  passes$location.x <- sapply(passes$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
  passes$location.y <- sapply(passes$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))
  passes$end_location.x <- sapply(passes$pass_end_location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
  passes$end_location.y <- sapply(passes$pass_end_location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))
  
  teamPasses <- passes %>% subset(team == teamName)
  dangerousPasses <- teamPasses %>% filter(end_location.x >= 102 & (end_location.y >= 30 & end_location.y <=50)) %>% 
    select(player, type, location.x, location.y, end_location.x, end_location.y, obv_for_net)
  
  carries <- game %>% filter(type == "Carry")
  carries$location.x <- sapply(carries$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
  carries$location.y <- sapply(carries$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))
  carries$end_location.x <- sapply(carries$carry_end_location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
  carries$end_location.y <- sapply(carries$carry_end_location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))
  
  teamCarries <- carries %>% subset(team == teamName)
  dangerCarries <- teamCarries %>% filter(location.x >= 60 & (end_location.x - location.x) > 10) %>% 
    select(player, type, location.x, location.y, end_location.x, end_location.y, obv_for_net)
  
  dangerActions <- rbind(dangerousPasses, dangerCarries)
  
  create_Pitch(grass_colour = "grey", line_colour = "#8F8F8F",
               background_colour = "grey", goal_colour = "#000000",
               goaltype = "line", middlethird = FALSE, BasicFeatures = FALSE,
               JdeP = FALSE, padding = 5) +
    geom_segment(data = dangerActions, aes(x = location.x, y = location.y,
                                    xend = end_location.x, yend = end_location.y,
                                    colour = type), 
    lineend = "round",
    size = 0.5,
    arrow = arrow(length = unit(0.07, "inches"), 
                  ends = "last", type = "open")) +
    scale_color_manual(values = c("Pass" = "blue", "Carry" = "skyblue")) +  # Set arrow colors based on actionType
    labs(title = paste(teamName, "Passes into Danger Zone"), subtitle = gameName) +
    scale_y_reverse() +
    coord_fixed(ratio = 105/100) +
    theme(axis.text.y = element_blank(),
          plot.title = element_text(margin = margin(r = 5, b = 5, t = 10), face="bold",size = 32.5, 
                                    colour = "black", hjust = 0.5),
          plot.subtitle = element_text(margin = margin(r = 5, b = 5),size = 16, 
                                       colour = "black", hjust = 0.5)) 
  
  
}


