---
layout: post
title: "Deviation Example"
author: Tim Essam \| SI
date: 2021-01-12
categories: [vignette]
tags: [ggplot]
---

```{r}
# Setup knitr defaults and folder paths
  knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, out.width = '100%')
  
  pub_images <- "public_images"

# Set up caption object
  caption <- paste0("Source: Testing data from glitr package | Created on: ", Sys.Date())
  
# Add libraries needed
  library(tidyverse)
  library(scales)
  library(glitr)
  library(here)
  library(ggrepel)
```

### Deviation

Deviation charts excel at showing how near or far a metric is from a fixed point. They are often use to show how a metric deviates from a point, and how a given category of a group compares to other group members. For this next example, we will look at how partner performance in terms of target achievement diverged from the mean achievement overall.

To create this plot, we will first calculate partner achievement as a percent of targets, overall partner achievement, and then take the difference of the two to calculate our deviation metric.

```{r}
hts_dev <- 
  hts %>% 
  filter(indicator == "HTS_TST", period == "FY49", period_type != "results") %>% 
  group_by(primepartner, period_type, indicator) %>% 
  summarise(partner_totals = sum(value)) %>% 
  ungroup()  

# Spread the data to make the acheivement calculations a bit easier
 hts_dev_wide <- 
   hts_dev %>% 
   pivot_wider(names_from = period_type, values_from = partner_totals) %>% 
   mutate(achievement = cumulative / targets) %>% 
   group_by(indicator) %>% 
   mutate(annual_results = sum(cumulative), 
          annual_targets = sum(targets), 
          annual_achievement = annual_results / annual_targets, 
          deviation = achievement - annual_achievement,
          partner_order = fct_reorder(
            paste0(primepartner, " ", "(", comma(cumulative), "/", comma(targets), ")"), deviation)
          ) %>% 
   # Remove dedups
   filter(primepartner != "Dedup")
```

![working data frame after pivot_wider operation](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/hts_dev_wide.png "working data frame after pivot_wider operation")

#### Creating Partner Flags

With the main data munging complete, we turn our attention to creating variables we can use in our deviation plot. First, we generate a simple deviation plot in black and white to get a sense of what the over/under-achievement distribution looks like.

![deviation plot first iteration](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/deviation_plot_1.png "deviation plot first iteration")

The next step is to encode partners falling short of the overall achievement average with a different color. Two vertical lines are added at the zero value of the x-axis. This help anchor the deviating bars. Finally, the x-axis is changed to a percentage.

```{r}
 dev_plot <- 
   hts_dev_wide %>% 
   mutate(bar_color = if_else(deviation <= 0, old_rose, scooter)) %>% 
    ggplot(aes(x = deviation, y = partner_order)) +
    geom_col(aes(fill = bar_color)) +
    geom_vline(xintercept = 0, size = 2, colour = grey10k) +
    geom_vline(xintercept = 0, size = 1, colour = grey90k) +
    scale_fill_identity() +
    si_style_xgrid() +
   scale_x_continuous(labels = percent) +
   labs(x = "Achievement share deviation from overall average (115% above targets)", 
        y = NULL, 
        title = "VIRGO AND SERPENS WERE THE HIGHEST TESTING PARTNERS AS A SHARE OF RESULTS TO TARGETS",
        subtitle = "Green (red) depicts partners above (below) the acheivement average.")
```

![deviation plot](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/deviation_plot_2.png "deviation plot second iteration with colors added")
