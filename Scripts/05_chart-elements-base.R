## PROJECT: pretty_in_gray40k
## AUTHOR:  A.Chafetz & T.Essam | USAID
## PURPOSE: makeover plots for style guide
## DATE:    2021-02-16
## UPDATED: 2024-03-18
## NOTES:   adapted from glitr

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(scales)
library(glitr)
library(systemfonts)


# CHART ELEMENTS ----------------------------------------------------------


tibble(x = c("FY50Q1", "FY50Q2", "FY50Q3", "FY50Q4"),
       y = seq(0, 1200, length.out = 4)) %>%
  ggplot(aes(x, y)) +
  geom_blank() +
  geom_point(data = tibble(x = "FY50Q2", y = 640),
             shape = 21, 
             size = 12,
             fill = hw_lavender_haze,
             color = hw_lavender_haze, stroke = 1.1,
             alpha = .8) +
  scale_y_continuous(label = comma,
                     limits = c(0, 1000)) +
  labs(x = NULL, y = NULL,
       title = "TITLE",
       subtitle = "Subtitle",
       caption = "data source (date) | reference id") +
  si_style()

si_save("chart_elements.svg",
       path = "Graphics",
       height = 3.7633,
       width = 4.9306)
