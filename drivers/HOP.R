library(tidyverse)
library(lubridate)
library(cowplot)
library(gganimate)
library(magick)
library(av)

# read EFI null temperature forecast for the aquatics challenge
aquatics_null <- read4cast::read_forecast(paste0("https://data.ecoforecast.org/forecasts/aquatics/",
                                      "aquatics-2020-09-01-EFInull.csv.gz")) %>%
  mutate(time = as_date(time)) %>%
  filter(variable == "temperature",
         site_id == "BARC")

# calculate median and 95% prediction interval bounds for each day
summary <- aquatics_null %>%
  group_by(time) %>%
  summarise(median = median(predicted, na.rm = TRUE),
            lower = quantile(predicted, 0.025),
            upper = quantile(predicted, 0.975))

set.seed(8953762)
n_frames <- 100 # Total number of ensembles in animation
random_ensembles <- sample(unique(aquatics_null$ensemble),
                           size = n_frames)

HOP1 <- aquatics_null %>% 
  filter(ensemble %in% random_ensembles) %>%
  mutate(ensemble_rank = dense_rank(ensemble)) %>%
  ggplot()+
  geom_ribbon(data = summary, 
              aes(x = time, ymin = lower, ymax = upper), 
              fill = "#588B8E",
              alpha = 0.3)+
  geom_line(data = summary, aes(x = time, y = median), 
            color = "#588B8E", size = 1.5)+
  geom_point(aes(x = time, y = predicted))+
  geom_line(aes(x = time, y = predicted, group = ensemble))+
  scale_x_date(date_breaks = "1 day",
               date_labels = "%b-%d") +
  ylab("predicted temperature (degrees Celsius)")+
  theme_minimal_grid()+
  theme(axis.title.x = element_blank(),
        legend.position = 'none')+
  transition_manual(ensemble_rank)

animation1 <- animate(HOP1, fps = 2.5)
anim_save("./HOP1.gif", 
          animation = animation1)

gif1 <- image_read("./HOP1.gif")
image_write_video(gif1,
                  path = "./HOP1.mp4",
                  framerate = 2.5)


HOP2 <- aquatics_null %>% 
  filter(ensemble %in% random_ensembles) %>%
  mutate(ensemble_rank = dense_rank(ensemble)) %>%
  uncount(n_frames, .id="frame") %>%
  filter(ensemble_rank <= frame) %>%
  arrange(frame, ensemble_rank, time) %>%
  group_by(frame) %>%
  mutate(alpha = if_else(ensemble_rank == frame, 1, 0.5)) %>%
  ungroup() %>%
  mutate(datetime = as_datetime(paste(time, "00:00:00")),
         xmin = datetime - hours(9), 
         xmax = datetime + hours(9))%>%
  ggplot()+
  geom_segment(aes(x = xmin, xend = xmax, y = predicted, yend = predicted, alpha = alpha),
               size = 2,
               color = "#00bfff")+
  transition_manual(frame)+
  scale_x_datetime(date_breaks = "1 day",
               date_labels = "%b-%d") +
  ylab("predicted temperature (degrees Celsius)")+
  theme_minimal_hgrid()+
  theme(axis.title.x = element_blank(),
        legend.position = 'none')

animation2 <- animate(HOP2, fps = 2.5)
anim_save("./HOP2.gif", 
          animation = animation2)

gif2 <- image_read("./HOP2.gif")
image_write_video(gif2,
                  path = "./HOP2.mp4",
                  framerate = 2.5)


