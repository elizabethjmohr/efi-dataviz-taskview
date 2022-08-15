library(tidyverse)
library(read4cast)
library(lubridate)
library(cowplot)
library(gganimate)
library(magick)
library(av)

# read EFI null temperature forecast for the aquatics challenge
null_forecast <- read4cast::read_forecast(paste0("https://data.ecoforecast.org/forecasts/aquatics/",
                                      "aquatics-2020-09-01-EFInull.csv.gz")) %>%
  mutate(time = as_date(time)) %>%
  filter(variable == "temperature",
         site_id == "BARC")

observed <- neon4cast::combined_scores("aquatics") |>
  filter(time >= lubridate::as_datetime('2020-09-01'), 
         time < lubridate::as_datetime('2020-09-08'), 
         variable == "temperature", 
         !is.na(observed)) |>
  group_by(time) |>
  summarise(observed = median(observed))

HOP <- null_forecast %>% 
  filter(ensemble %in% random_ensembles) %>%
  mutate(ensemble_rank = dense_rank(ensemble)) %>%
  uncount(n_frames, .id="frame") %>%
  filter(ensemble_rank <= frame) %>%
  arrange(frame, ensemble_rank, time) %>%
  group_by(frame) %>%
  mutate(alpha = if_else(ensemble_rank == frame, 1, 0.5)) %>%
  ungroup() %>%
  mutate(
    datetime = as_datetime(paste(time, "00:00:00")),
    xmin = datetime - hours(9), 
    xmax = datetime + hours(9)
  ) %>%
  ggplot() +
  geom_segment(
    aes(x = xmin, xend = xmax, y = predicted, yend = predicted, 
        alpha = alpha, color = 'predicted'),
    size = 2) +
  transition_manual(frame) +
  geom_point(data = observed, aes(x = time, y = observed, color = 'observed')) +
  scale_color_manual(
    name = NULL,
    values = c('observed'  = "#000000", 'predicted' = '#00bfff'),
    labels = c('observed', 'predicted'), 
    guide = guide_legend(override.aes = list(linetype = c(0,1), shape = c(16,NA)))) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%b-%d") +
  ylab("temperature (degrees Celsius)") +
  theme_minimal_hgrid() +
  scale_alpha(guide = 'none') + 
  theme(axis.title.x = element_blank(),
        legend.position = 'right') + 
  ggtitle('Number of ensembles: {frame}')

animation <- animate(HOP, fps = 2.5)
anim_save("./HOP.gif", 
          animation = animation)

gif <- image_read("./HOP.gif")
image_write_video(gif,
                  path = "./HOP.mp4",
                  framerate = 2.5)


