---
layout: post
title: "Key Performance Indicators Example"
author: Tim Essam | SI
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
  library(patchwork)
```

### Showing Magnitude (Bar Charts)

Bar graphs are one of the most effective ways of comparing the magnitudes of a metric aross numerous categories. Sorting the bars based on the magnitude (not alphabetically) helps the reader make quick comparisons. Highlighting bars above a given threshold can help distinguish different classes of categories.

When munging the hts data for the bar graph, make use of the `forcats::fct_reorder()` or `tidytext::reorder_within()` functions to help with sorting bars and/or facets. If you try to plot without setting the primepartner as an ordered factor, you will get a plot like the one below.

```{r}

# Munge the hts data down to testing yields for a given year
  hts_bar <- 
    hts %>% 
    filter(period == "FY50", period_type == "cumulative") %>% 
    group_by(indicator, primepartner) %>% 
    summarise(total = sum(value, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(indicator, total) %>% 
    mutate(yield = (HTS_TST_POS / HTS_TST)) 

  hts_bar %>% 
    ggplot(aes(x = primepartner, y = yield)) + 
    coord_flip() + 
    geom_col() +
    si_style_xgrid()
```

![Unsorted bar graph](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/Unsorted_bar.png "Unsorted bar graph")

Sort the partners variable by yield and then plot as a rotated, sorted bar graph. Notice that we are using the `si_style_xgrid()` option from the `glitr` package to call baked-in formatting.

```{r}
  hts_bar_sorted <- 
    hts_bar %>% 
    mutate(partner_ordered = fct_reorder(primepartner, yield)) 
  
  hts_bar_sorted %>% 
    ggplot(aes(x = partner_ordered, 
               y = yield)) + 
    coord_flip() + 
    geom_col() +
    si_style_xgrid()
```

![Sorted bar graph](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/Sorted_bar.png "Sorted bar graph")

This plot can be further polished by i) highlighting all partners that have yields higher than 20% ii) touching up the axes labels and iii) adding in a title and caption. If using the graphic in a presentation, adding additional annotation, such as why these two prime partners were such high performers, would be recommended as well.

To highlight the partners that have achieved beyond a given threshold, we create a new variable called color_flag. If the `yield` value is greater than 20%, then we assign the color to be scooter (`#1e87a5`) for those partners. Otherwise, the value is assigned to be grey20k (`#d1d3d4`). The `scale_fill_identity()` option is used in the `ggplot2` call to reference the fill value passed in the aesthetics portion of the code. Finally, we make use of the `scales::percent_format()` function to format that x-axis to a percentage.

```{r}
# Match axis label colors to top to performers
  label_colors <- if_else(sort(hts_bar_sorted$yield, decreasing = FALSE) > 0.2, 
                          scooter, 
                          color_caption)

  hts_bar_sorted %>% 
    mutate(color_flag = if_else(yield > 0.2, scooter, grey20k)) %>% 
    ggplot(aes(x = partner_ordered, 
               y = yield, 
               fill = color_flag)) + 
    coord_flip() + 
    geom_col() +
    scale_fill_identity() +
    scale_y_continuous(labels = percent_format()) +
    si_style_xgrid() +
    labs(x = NULL, y = NULL, 
         title = "BOOTES AND LEO ACHIEVED THE HIGHEST TESTING YIELDS",
         caption = caption
         ) +
    theme(axis.text.y = element_text(colour = label_colors))
```

![Final bar graph with SIEI colors (scooter) incorporate to highlight high performers.](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/bar_chart_example.png "Adorned bar graph")
