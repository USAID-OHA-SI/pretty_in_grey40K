## PROJECT: pretty_in_gray40k
## AUTHOR:  A.Chafetz & T.Essam | USAID
## PURPOSE: makeover plots for style guide
## DATE:    2021-02-16
## UPDATED: 
## NOTES:   adapted from glitr

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(scales)
library(glitr)
library(systemfonts)
library(svglite)


# CHART ELEMENTS ----------------------------------------------------------


tibble(x = c("FY50Q1", "FY50Q2", "FY50Q3", "FY50Q4"),
       y = seq(0, 1200, length.out = 4)) %>%
  ggplot(aes(x, y)) +
  scale_y_continuous(label = comma) +
  labs(x = NULL, y = NULL,
       title = "TITLE",
       subtitle = "caption/description",
       caption = "data source") +
  si_style()

si_save("chart_elements.svg",
       path = "Graphics",
       height = 3.7633,
       width = 4.9306)
