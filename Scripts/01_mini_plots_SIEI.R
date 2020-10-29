# Purpose: Create mini strip plots for SIEI Style Guide
# Authors: T. Essam & A. Chafetz
# Date: 2020-10-29
# Notes: This project never ends.


# START IT UP -------------------------------------------------------------
  
  library(ggplot2)
  library(tidyverse)
  library(glitr)
  library(here)
  
  # Where stuff gonna live
  graph <- "Graphics"
  sizing = 0.8



# BAR GRAPH ---------------------------------------------------------------

bar <- tibble::tribble(
  ~partner, ~val,
  "prime 1",  90L,
  "prime 2",  70L,
  "prime 3",  50L,
  "prime 4",  25L
)

ggplot(bar, aes(y = fct_reorder(partner, val, .desc = F), x = val)) +
  geom_col() +
  theme_void()
ggsave(here(graph, "bar_graph.svg"), device = "svg", scale = sizing)


# TIME SERIES -------------------------------------------------------------

library(tsibble)
ts <- UKDriverDeaths %>% as_tsibble()

summary(ts$value)
min <- ts$value %>% min()
max <- ts$value %>% max()

ts %>% slice(1:35) %>% 
  ggplot(aes(x = index, y = value), ymax = value) + 
  geom_ribbon(aes(ymin = min, ymax = value), fill = grey10k) +
  geom_line(aes(y = value)) +
  coord_cartesian(ylim = c(min, max)) +
  theme_void()
ggsave(here(graph, "time_series_graph.svg"), device = "svg", scale = sizing)



# RANKING -----------------------------------------------------------------

rank <- data.frame(
  site = c("A", "A", "B", "B"),
  year = c(2010, 2020, 2010, 2020),
  value = c(90, 55, 60, 50),
  color = c(grey90k, grey90k, grey40k, grey40k)
)  

ggplot(rank, aes(x = year, y = value, group = site, colour = color, fill = color)) +
  geom_line(size = 2) +
  geom_point(size = 6) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void()
ggsave(here(graph, "slope_graph.svg"), device = "svg", scale = sizing)


# DEVIATION ---------------------------------------------------------------

dev <- data.frame(
  group = LETTERS[1:6],
  value = rev(c(-50, 15, -35, 40, -20, 25))
)  

ggplot(dev, aes(x = value, y = group)) + 
  geom_col() +
  theme_void()

ggsave(here(graph, "deviation.svg"), device = "svg", scale = sizing)

# WAFFLE GRAPH ------------------------------------------------------------

library(hrbrthemes)
library(waffle)

parts <- c(22, 14)

waffle(parts, rows = 6, flip = T, colors = c(grey70k, grey10k), legend_pos = "none", size = 1) 
ggsave(here(graph, "waffle_graph.svg"), device = "svg")
