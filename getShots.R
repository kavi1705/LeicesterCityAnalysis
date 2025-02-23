getShotMap(leic_hull, "Leicester City", "Leicester v Hull (02/09/2023)")

getShotMap <- function(game, teamName, gameName){
  shots <- game %>% filter(type == "Shot")
  shots$location.x <- sapply(shots$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
  shots$location.y <- sapply(shots$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))
  shots$end_location.x <- sapply(shots$shot_end_location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
  shots$end_location.y <- sapply(shots$shot_end_location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))
  
  teamShots <- shots %>% subset(team == teamName)
  
  shotmapxgcolors <- c("#192780", "#2a5d9f", "#7F0000", "#5F0000")
  # Define the danger zone
  danger_zone <- teamShots %>%
    filter(location.x >= 102 & (location.y < 50 & location.y > 30))
  danger_zone_shots <- nrow(danger_zone)
  
  ggplot() +
    annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) + 
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) + 
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) + 
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) + 
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) + 
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) + 
    annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) + 
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) + 
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+ 
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+ 
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
    theme(rect = element_blank(),
          line = element_blank()) +
    # Danger Zone - Rectangle
    annotate("rect", xmin = 102, xmax= 120, ymin = 30, ymax = 50, fill = "yellow", alpha = 0.2) + 
    
    # add penalty spot right
    annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) + annotate("path", colour = "black", size = 0.6,
                                                                                  x=60+10*cos(seq(0,2*pi,length.out=2000)),
                                                                                  y=40+10*sin(seq(0,2*pi,length.out=2000)))+
    # add centre spot
    annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) + annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
                                                                                 y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") + 
    annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6, y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    geom_point(data = teamShots, aes(x = location.x, y = location.y, fill = shot_statsbomb_xg, shape = shot_body_part, size = shot_statsbomb_xg),
               alpha = 0.8) +
    theme(axis.text.x=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.caption=element_text(size=13, hjust=0.5, vjust=0.5), plot.subtitle = element_text(size = 18, hjust = 0.5), 
          axis.text.y=element_blank(),
          plot.title = element_text(margin = margin(r = 5, b = 5), face="bold",size = 32.5, 
                                    colour = "black", hjust = 0.5),
          plot.margin = margin(t = 50, r = 0, b = 50, l = 0, unit = "pt"),
          #legend.direction = "horizontal",
          axis.ticks=element_blank(),
          aspect.ratio = c(65/100),
          plot.background = element_rect(fill = "darkgrey"),
          strip.text.x = element_text(size=13)) +
    labs(title = gameName, subtitle = paste(teamName, "Shots in Danger Zone: ", danger_zone_shots)) + 
    guides(fill = guide_colourbar(title.position = "top"),
           shape = guide_legend(override.aes = list(size = 7, fill = "black"))) + #7 
    coord_flip(xlim = c(85, 125)) +
    scale_shape_manual(values = c("Head" = 21, "Right Foot" = 23, "Left Foot" = 24), name ="")
}
