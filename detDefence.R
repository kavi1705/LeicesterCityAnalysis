library(tidyverse)
hull <- "Leiecster City v Hull City"
cov <- "Leicester City v Coventry City"
game_name <- hull
game <- def_hull

getDefenseMap(leic_hull, "Leicester City", "Leicester v Hull (02/09/2023)")

getDefenseMap <- function(game, teamName, gameName){
  
  game <- game %>% filter(type =="Pressure" | type =="Ball Recovery" | type == "Foul Committed" | type == "Block" | type == "Interception")
  game$location.x <- sapply(game$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
  game$location.y <- sapply(game$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))
  
  heatmap <- game %>%
    subset(team == teamName) %>% 
    mutate(
      location.x = pmin(120, pmax(0, location.x)),
      location.y = pmin(80, pmax(0, location.y)),
      xbin = cut(location.x, breaks = seq(from=0, to=120, by = 20),include.lowest=TRUE),
      ybin = cut(location.y, breaks = seq(from=0, to=80, by = 20),include.lowest=TRUE)) %>% 
    group_by(xbin, ybin) %>%
    summarise(
      bin_DA = n(),
      location.x = median(location.x),
      location.y = median(location.y)
    )
  
  library(grid)
  defensiveactivitycolors <- c("#dc2429", "#dc2329", "#df272d", "#df3238", "#e14348", "#e44d51", "#e35256", "#e76266", "#e9777b", 
                               "#ec8589", "#ec898d", "#ef9195", "#ef9ea1", "#f0a6a9", "#f2abae", "#f4b9bc", "#f8d1d2", "#f9e0e2",
                               "#f7e1e3", "#f5e2e4", "#d4d5d8", "#d1d3d8", "#cdd2d6", "#c8cdd3", "#c0c7cd", "#b9c0c8", "#b5bcc3",
                               "#909ba5", "#8f9aa5", "#818c98", "#798590", "#697785", "#526173", "#435367", 
                               "#3a4b60", "#2e4257", "#1d3048", "#11263e", "#11273e", "#0d233a", "#020c16")
  
  ggplot(data= heatmap, aes(x = location.x, y = location.y, fill = bin_DA, group = bin_DA)) + 
    geom_bin2d(binwidth = c(20, 80), position = "identity", alpha = 0.9) + #2
    annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) + 
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) + 
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) + 
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) + 
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) + 
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) + 
    annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) + 
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) + 
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "white", size = 0.6)+ 
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
    theme(rect = element_blank(), line = element_blank()) +
    annotate("point", x = 12 , y = 40, colour = "white", size = 1.05) + annotate("point", x = 108 , y = 40, colour = "white", size = 1.05) + 
    annotate("path", colour = "white", size = 0.6, x=60+10*cos(seq(0,2*pi,length.out=2000)), y=40+10*sin(seq(0,2*pi,length.out=2000)))+
    annotate("point", x = 60 , y = 40, colour = "white", size = 1.05) + 
    annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") + 
    annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
    theme(axis.text.x=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.caption=element_text(size=13, hjust=0.5, vjust=0.5), plot.subtitle = element_text(size = 18, hjust = 0.5), 
          axis.text.y=element_blank(),
          legend.title = element_blank(), legend.text=element_text(size=22),
          legend.key.size = unit(1.5, "cm"),
          plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5, colour = "black", hjust = 0.5), legend.direction = "vertical",
          axis.ticks=element_blank(),
          plot.background = element_rect(fill = "darkgrey"),
          strip.text.x = element_text(size=13)) + #4
    scale_y_reverse() + #5
    scale_fill_gradientn(colors = c("#FFFFFF", "#FF0000"), na.value = "grey", values = scales::rescale(c(0, max(heatmap$bin_DA)))) +
    labs(title = paste(teamName, "Defensive Actions Heat Map"), subtitle = gameName) + #7
    coord_fixed(ratio = 95/100) +
    annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="last", length=unit(2.55,"mm")), gp=gpar(col="black", fill=NA, lwd=2.2)),
                      xmin=25, xmax = 95, ymin = -83, ymax = -83) + #9 
    guides(fill = guide_legend(reverse = TRUE))
  
}


