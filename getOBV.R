
getObv <- function(game, teamName, gameName){
  obv <- game %>% subset(team == teamName)
  sum_obv <- obv %>%
    group_by(player) %>%
    summarise(total_obv_for = sum(obv_for_net, na.rm = TRUE))
  
  sum_obv <- sum_obv %>%
    mutate(color = ifelse(total_obv_for >= 0, "Positive", "Negative"))
  
  # Create a horizontal bar graph
  ggplot(sum_obv, aes(x = player, y = total_obv_for, fill = color)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = player, y = 0), 
              position = position_dodge(width = 0.9), color = "white")+
    coord_flip() +  # Flip the coordinates to make it horizontal
    scale_fill_manual(values = c("Positive" = "darkblue", "Negative" = "skyblue")) +
    labs(title = "Individual Player OBV",
         subtitle = gameName,
         y = "Total OBV") +
    theme_minimal() +
    guides(fill = FALSE)+
    theme(axis.text.y = element_blank(),
          panel.background = element_rect('darkgrey'), 
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(margin = margin(r = 5, b = 5), face="bold",size = 32.5, 
                                    colour = "black", hjust = 0.5),
          plot.subtitle = element_text(margin = margin(r = 5, b = 5),size = 16, 
                                    colour = "black", hjust = 0.5))
  
}

getObv(leic_hull, "Leicester City", "Leicester v Hull (02/09/2023)")