library(tidyverse)
library(ggtext)
library(glue)
library(scales)
read_csv("data/GLB.Ts+dSST.csv", skip=1, na = "***") %>% 
  select(year = Year, t_diff = 'J-D') %>% 
  ggplot(aes(x = year, y = t_diff)) +
  geom_line(aes(color = "1"), size = 0.5, show.legend = FALSE) +
  geom_point(fill = "white", aes(color="1"), shape = 21, show.legend = TRUE) +
  geom_smooth(se = F, aes(color = "2"), size = 0.5, span= 0.15, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1880, 2024, 20), expand = c(0,0)) +
  scale_y_continuous(limits = c(-0.5, 1.5), expand = c(0,0)) +
  scale_color_manual(name=NULL,
                     breaks = c(1,2),
                     values = c("gray", "black"),
                     labels =c("Annual mean", "Lowess Smoothing"),
                     guide = guide_legend(override.aes = list(shape=15, size=5))) +
  labs(title = "GLOBAL LAND-OCEAN TEMPERATURE INDEX",
       subtitle = "Data source: NASA's Goddard Institure for Space Studies(GISS). \nGredit: NASA/GISS",
       x = "YEAR",
       y = "Temperature Anomoaly (C)") +
  theme_light() +
  theme(
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(margin = margin(b=10), color = "red", face="bold"),
    plot.subtitle = element_text(size=8, margin = margin(b=10)),
    legend.position = c(0.15, 0.9),
    legend.title = element_text(size = 0), 
    legend.key.height = unit(10, "pt"),
    legend.margin = margin(0,0,0,0))

ggsave("figures/temperature_index_plot.png", width = 6, height = 4)

# Plotting the global temperature index as bars using ggplot2 and NASA GISS data (CC215)

library(tidyverse)
library(ggtext)
library(glue)
library(scales)

t_data <- read_csv("data/GLB.Ts+dSST.csv", skip=1, na = "***") %>% 
  select(year = Year, t_diff = 'J-D') %>% 
  drop_na() 

annotation <- t_data %>% 
  arrange(year) %>% 
  slice(1, n()) %>% 
  mutate(t_diff = 0,
         x = year + c(-5, 5))

max_t_diff <- format(round(max(t_data$t_diff), 1), nsmall = 1)

t_data %>% 
  ggplot(aes(x = year, y = t_diff, fill = t_diff)) +
  geom_col(show.legend = FALSE) +
  geom_text(data = annotation, aes(x = x, label = year), color="white") +
  geom_text(x=1880, y=1.15, hjust=0,  
            label=glue("Global temperatures have increased by over {max_t_diff}\u00B0C since {min(t_data$year)}"),
            color="white") +
  # scale_fill_gradient2(low = "darkblue", mid="white", high = "darkred",
  #                     midpoint=0, limits = c(-0.5, 1.5)) +
  # scale_fill_gradientn(colors = c("darkblue", "white", "darkred"),
  #                      values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
  #                      limits = c(min(t_data$t_diff), max(t_data$t_diff))) +
  scale_fill_stepsn(colors = c("darkblue", "white", "darkred"),
                    values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
                    limits = c(min(t_data$t_diff), max(t_data$t_diff)),
                    n.break=9) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white")
  )

ggsave("figures/temperature_bar_plot.png", width = 7, height = 4)

# Using ggplot2 to recreate iconic warming stripes visualization of climate change (CC216)

t_data <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>% 
  select(year=Year, t_diff = 'J-D') %>% 
  drop_na()
  
t_data %>% 
  ggplot(aes(x = year, y = 1, fill =t_diff)) +
  geom_tile(show.legend = FALSE) +
  scale_fill_stepsn(colors=c("#08306B", "white", "#67000D"), 
                    values = round(rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))), 2),
                    n.breaks = 12) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(breaks = seq(1890, 2020, 30)) +
  labs(title = glue("Global temperature change ({min(t_data$year)}-{max(t_data$year)})")) + 
  theme_void() +
  theme(
    axis.text.x = element_text(color = "white", 
                               margin = margin(t=5, b=10, unit = "pt")),
    plot.title = element_text(color = "white", 
                               margin = margin(b=5, t=10, unit = "pt"),
                              hjust = 0.05),
    plot.background = element_rect(fill = "black")
    )


ggsave("figures/warming_stripes.png", width =8 , height =4.5 )


