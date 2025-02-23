matches <- read.csv("statsbomb_matches.csv") %>% subset(match_id == 3895406 | match_id == 3895362)
comps <- read.csv("statsbomb_competitions.csv")

leic_hull <- read.csv("statsbomb_events_3895406.csv")
leic_cov <- read.csv("statsbomb_events_3895362.csv") 
lineup1 <- read.csv("statsbomb_lineups_3895406.csv")
lineup2 <- read.csv("statsbomb_lineups_3895362.csv")

#shots
shots_hull <- leic_hull %>% filter(type == "Shot")
shots_hull$location.x <- sapply(shots_hull$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
shots_hull$location.y <- sapply(shots_hull$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))
shots_hull$end_location.x <- sapply(shots_hull$shot_end_location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
shots_hull$end_location.y <- sapply(shots_hull$shot_end_location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))

shots_cov <- leic_cov %>% filter(type == "Shot")
shots_cov$location.x <- sapply(shots_cov$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
shots_cov$location.y <- sapply(shots_cov$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))
shots_cov$end_location.x <- sapply(shots_cov$shot_end_location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
shots_cov$end_location.y <- sapply(shots_cov$shot_end_location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))
#passes
passes_hull <- leic_hull %>% filter(type == "Pass")
passes_hull$location.x <- sapply(passes_hull$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
passes_hull$location.y <- sapply(passes_hull$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))
passes_hull$end_location.x <- sapply(passes_hull$pass_end_location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
passes_hull$end_location.y <- sapply(passes_hull$pass_end_location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))
percentile_99 <- quantile(passes_hull$obv_for_net, probs = 0.99, na.rm = TRUE)

passes_cov <- leic_cov %>% filter(type == "Pass")
passes_cov$location.x <- sapply(passes_cov$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
passes_cov$location.y <- sapply(passes_cov$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))
passes_cov$end_location.x <- sapply(passes_cov$pass_end_location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
passes_cov$end_location.y <- sapply(passes_cov$pass_end_location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))

passes_v_cov <- passes_cov %>%  subset(team == "Leicester City")
passes_v_hull <- passes_hull %>% subset(team == "Leicester City")
event_types <- as.data.frame(unique(leic_cov$type))

#obv

players_obv_hull <- leic_hull %>% group_by(player, team) %>% summarise(obv_for = sum(obv_for_net, na.rm = TRUE),
                                                            obv_ag = sum(obv_against_net, na.rm = TRUE),
                                                            obv_net = sum(obv_total_net, na.rm = TRUE))



players_obv_cov <- leic_cov %>% group_by(player, team) %>% summarise(obv_for = sum(obv_for_net, na.rm = TRUE),
                                                                       obv_ag = sum(obv_against_net, na.rm = TRUE))
#def actions

def_hull <- leic_hull %>% filter(type =="Pressure" | type =="Ball Recovery" | type == "Foul Committed" | type == "Block" | type == "Interception")
def_hull$location.x <- sapply(def_hull$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
def_hull$location.y <- sapply(def_hull$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))
def_hull <- def_hull %>% subset(team == "Leicester City")

def_cov <- leic_cov %>% filter(type =="Pressure" | type =="Ball Recovery" | type == "Foul Committed" | type == "Block" | type == "Interception")
def_cov$location.x <- sapply(def_cov$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][1]))
def_cov$location.y <- sapply(def_cov$location, function(x) as.numeric(str_extract_all(x, "\\d+\\.?\\d+")[[1]][2]))
def_cov<- def_cov %>% subset(team == "Leicester City")


#setpieces

sp_hull <- leic_hull %>% filter(pass_type == "Corner" | pass_type == "Free Kick" | pass_type == "Throw-in")


