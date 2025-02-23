

leic_shot_cov <- shots_cov %>%
  filter(team == 'Leicester City') %>%
  select(player, location.x, location.y, end_location.x, end_location.y, shot_statsbomb_xg)


cov_shot <- shots_cov %>%
  filter(team == 'Coventry City') %>%
  select(player, location.x, location.y, end_location.x, end_location.y, shot_statsbomb_xg)

ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour='white', fill='#021e3f') +
  geom_point(data=leic_shot_cov, aes(x=location.x, y=location.y, size=shot_statsbomb_xg), color="blue") +
  geom_point(data=cov_shot, aes(x=120-location.x, y=location.y, size=shot_statsbomb_xg), color="light blue") +
  labs(
    title="leic v cov",
    subtitle = "Shots Map ",
    caption="Data Source: StatsBomb"
  ) + 
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(family="Geneva", color='white'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "none"
  )


leic_shot_hull <- shots_hull %>%
  filter(team == 'Leicester City') %>%
  select(player, location.x, location.y, end_location.x, end_location.y, shot_statsbomb_xg)


hull_shot <- shots_hull %>%
  filter(team == 'Hull City') %>%
  select(player, location.x, location.y, end_location.x, end_location.y, shot_statsbomb_xg)

ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour='white', fill='#021e3f') +
  geom_point(data=leic_shot_hull, aes(x=location.x, y=location.y, size=shot_statsbomb_xg),  color="blue") +
  geom_point(data=hull_shot, aes(x=120-location.x, y=location.y, size=shot_statsbomb_xg), color="light blue") +
  labs(
    title="leic v hull",
    subtitle = "Shots Map ",
    caption="Data Source: StatsBomb"
  ) + 
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(family="Geneva", color='white'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "none"
  )
