library(ggplot2)
library(ggsoccer)

ggplot(def_cov) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="v COV",
       subtitle="DATE",
       caption="Data Source: StatsBomb") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='darkgrey'),
    panel.background = element_rect(fill='#021e3f', color='darkgrey'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )


#hull
ggplot(def_hull) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="v Hull",
       subtitle="DATE",
       caption="Data Source: StatsBomb") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

