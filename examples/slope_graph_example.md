---
layout: post
title: "Ranking (slope charts) Example"
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

### Visualizing Rankings (slope plots)

A slope plot or connected line graph is an effective way to show discrete changes across time for a metric. These types of graphs tend to work best when the change is dramatic and/or categories are easily sorted. In the example below, we will look at how testing results differed from FY49Q1 to FY50Q1. The main question we want to investigate is whether partners were performing better or worse in FY50Q1 compared to Q1 of the previous year.

```{r}
# Munge data to show only annual targets for HTS_TST
# Generate percentage change and a period color variable
  hts_slope <- 
    hts %>% 
    filter(indicator == "HTS_TST", 
           str_detect(period, "Q1"),
           period_type == "results", 
           !primepartner %in% c("Dedup", "Leo")
           ) %>% 
    group_by(primepartner, period) %>%
    summarise(total_tgts = sum(value, na.rm = TRUE)) %>% 
    mutate(percent_change = (total_tgts / lag(total_tgts, order_by = period) - 1),
           percent_change_allpds = percent_change,
           period_color = ifelse(period == "FY49Q1", denim, scooter), 
           partner_label = ifelse(period == "FY50Q1", primepartner, NA)
           ) %>% 
    fill(., percent_change_allpds, .direction = "up") %>% 
    ungroup() 

    
  hts_slope %>% 
    ggplot(aes(x = period, 
               y = total_tgts, 
               group = primepartner)) +
    geom_line() +
    si_style_xline()
```

![*Slope plot first iteration - who dropped the spaghetti?*](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/slope_plot_1.png "slope plot first iteration")

While the graph above shows the decline for most partners, it is hard to see if any partners had better performance in FY50Q1. If we look at the munged data, see that Ophiuchus actually had tested more individuals in FY50Q1. We can use color to help draw this anomaly out.

We accomplish this by filtering the data into two groups using a filter on the data entering the geom. the line `geom_line(data = . %>% filter(percent_change_allpds < 0))` tells ggplot to only use data returned from this filter in drawing the lines. Partners that have a positive percent change are plotted towards the end of the ggplot stack to place them on top.

We finish by expanding the x-scale with the `expand` argument in the `scale_x_discrete()` function. Expand can be used to add padding around the data to ensure they are placed some distance away from the axes.

```{r}
  hts_slope %>% 
    ggplot(aes(x = period, y = total_tgts, group = primepartner,
               color = if_else(percent_change_allpds > 0, genoa, grey20k),
               fill = if_else(percent_change_allpds > 0, genoa, grey20k))) +
    geom_line(data = . %>% filter(percent_change_allpds < 0)) +
    geom_line(data = . %>% filter(percent_change_allpds >= 0)) +
    geom_point(shape = 21, size = 3) +
    scale_fill_identity() +
    scale_color_identity() +
    si_style_nolines() +
    scale_x_discrete(expand = c(0.05, 0.05)) +
    labs(x = NULL, y = NULL,
         title = "OPHINUCHUS WAS THE ONLY PARTNER TO SEE HIGHER TESTING VOLUMES IN FY50Q1",
         caption = caption)
```

![Slope plot second iteration](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/slope_plot_2.png "slope plot second iteration")

This plot could be further polished by adding in some annotation and calling out the partners with the largest percentage changes. Another option would be to group the partners by the degree of annual change in testing. The `cut_interval()` command will create a given number of groups with equal ranges. Below, `n`, the number of groups to be calculated, is set to three to group partners into large, medium and small clusters. We then make use of the `facet_wrap()` option to plot these three groups in a single row.

```{r}
  hts_slope %>% 
    mutate(cut_groups_pct = cut_interval(percent_change_allpds, n = 3),
           cut_groups_pct = factor(cut_groups_pct, 
                                   labels = c("Large", "Medium", "Small"))) %>% 
    ggplot(aes(x = period, y = total_tgts, group = primepartner,
               color = if_else(percent_change_allpds > 0, genoa, grey20k),
               fill = if_else(percent_change_allpds > 0, genoa, grey20k))) +
    geom_line(data = . %>% filter(percent_change_allpds < 0)) +
    geom_line(data = . %>% filter(percent_change_allpds >= 0)) +
    geom_point(shape = 21, size = 3) +
    facet_wrap(~cut_groups_pct) +
    scale_fill_identity() +
    scale_color_identity() +
    si_style_xline() +
    scale_x_discrete(expand = c(0.05, 0.05)) +
    labs(x = NULL, y = NULL,
         title = "OPHINUCHUS WAS THE ONLY PARTNER TO SEE HIGHER TESTING VOLUMES IN FY50Q1",
         subtitle = "Facets organized by degree of change",
         caption = caption)
```

![Slope plot third iteration](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/slope_plot_3.png "slope plot third iteration")

#### Integrated Labels for Clarity

The previous plots are great for highlighting the performance of a single partner. But these plots fail to tell us which partners had the largest or smallest changes. To accomplish this, we can encode these changes with colors and then add integrated (direct) labels to the graphic. We make use of the `ggrepel` package to directly label each point. In doing this, the y-axis can be dropped and label values are added to the points.

```{r}
  # Set SIEI colors that will be used to encode data points and lines
  colors_cat <- c(denim, scooter, burnt_sienna)

  slope_plot <-  hts_slope %>% 
    mutate(cut_groups_pct = cut_interval(percent_change_allpds, n = 3),
           cut_groups_pct = factor(cut_groups_pct, 
                                   labels = c("Large", "Medium", "Small"))) %>% 
    ggplot(aes(x = period, y = total_tgts, group = primepartner,
               color = cut_groups_pct,
               fill = cut_groups_pct)) +
    # Order in which the lines appear - line will fall below point.
    geom_line() +
    geom_point(shape = 21, size = 3) +
    # Add integrated labels to each point
    ggrepel::geom_text_repel(data = . %>% filter(period == "FY49Q1"), 
            aes(label = paste0(primepartner, " - ", scales::comma(total_tgts))), 
            nudge_x = - 0.3,
            size = 3,
            force = 1,
            ) +
    ggrepel::geom_text_repel(data = . %>% filter(period == "FY50Q1"), 
            aes(label = paste0(primepartner, " - ", scales::comma(total_tgts))), 
            nudge_x = 0.3,
            size = 3, 
            force = 1,
            ) +
    scale_fill_manual(values = colors_cat) +
    scale_color_manual(values = colors_cat) +
    scale_y_continuous(expand = c(0.025,0)) +
    facet_wrap(~cut_groups_pct, ncol = 3) +
    si_style_xline() +
    theme(legend.position = "none",
          axis.text.y = element_blank()) + 
    labs(x = NULL, y = NULL,
         title = "CORVUS, AURIGA AND VIRGO HAD THE LARGEST QUARTER 1 PERFORMANCE DROPS ACROSS FISCAL YEARS",
         subtitle = "Facets organized by degree of change. Comparison based on FY49Q1 and FY50Q1 testing results.",
         caption = caption)

```

![Slope plot final iteration with direct labels](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/slope_plot_labeled.png "Slope plot final iteration with direct labels")

As a final step, this plot could benefit from some annotation summarizing why these declines occurred. It may also be useful to add in a note on which partner had the largest decline. This may also help orient the reader on how to interpret the plot.
