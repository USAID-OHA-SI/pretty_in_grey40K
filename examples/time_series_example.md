---
layout: post
title: "Showing Change Over Time Example"
author: Tim Essam | SI
date: 2021-01-12
categories: [vignette]
tags: [ggplot]
---

### Showing Change Over Time (Time-Series Chart)

Time-series plots are used to show how a metric changes over a fixed period of time - be it in days, months, quarters or years. Annotate major events to help explain large variations or the lack of expected variation. If comparing many sites or categories across time, break the plots into small multiples and use a group average to show over/under performers.

We start by setting the `knitr` options and local paths and loading the required libraries.

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

In the example below, we look at how testing volumes varied by the previous five quarters. We would like to answer the following question: How has testing volume fluctuated across the past 5 fiscal quarters for Virgo?

```{r}
hts_line <- 
  hts %>% 
  filter(str_detect(period, "Q")) %>% 
  group_by(primepartner, indicator, period) %>% 
  summarise(qtr_totals = sum(value, na.rm = TRUE)) %>% 
  ungroup()
  
# Focus just on Virgo to start. Be sure to pass a grouping variable to the 
# aes() to get the plot to show up.
  hts_line %>% 
    filter(primepartner == "Virgo", 
           indicator == "HTS_TST") %>% 
    ggplot(aes(x = period, 
               y = qtr_totals, 
               group = primepartner)) +
    geom_line() +
    si_style_ygrid()
```

![Time-series base plot](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/time_series_1_example.png "Time-series plot")

The chart above is a good start. It is clean and clearly shows the decline in testing for Virgo. Adding in a shaded fill and some points can help this plot pop out a bit more. Let's first fill the area below the line in a light gray and add in some dots to help the quarters stand out.

```{r}
  hts_line %>% 
    filter(primepartner == "Virgo", 
           indicator == "HTS_TST") %>% 
    ggplot(aes(x = period, 
               y = qtr_totals, 
               group = primepartner)) +
    geom_line() +
    geom_area(fill = grey10k, 
              alpha = .75) +
    geom_point(shape = 21, 
               fill = "white", 
               size = 3) +
    si_style_ygrid() +
    labs(x = NULL, 
         y = NULL, 
         title = "TESTING VOLUME DOWN IN FY50Q1 FOR VIRGO",
         caption = caption)
```

![Time-series first iteration](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/time_series_2_example.png "Time-series first iteration")

#### Encoding with Size and Color

If you wanted to take this chart a step further, you could encode each dot with a color and size that is mapped to testing volumes. To accomplish this, we can pass a `fill` and `size` aesthetic to the `geom_point` function. We then use the `scale_fill_si()` function and apply the `genoas` palette. To show a broader range of values, the filter is expanded to two additional prime partners. A `facet_wrap()` call is also included to create small multiples --- one graph for each prime partner placed on fixed scales.

```{r}
  hts_line %>% 
    filter(primepartner %in% c("Virgo", "Ursa Major", "Capricornus"), 
           indicator == "HTS_TST") %>% 
    ggplot(aes(x = period, 
               y = qtr_totals, 
               group = primepartner)) +
    geom_line() +
    geom_area(fill = grey10k, alpha = .75) +
    geom_point(aes(fill = qtr_totals, 
                   size = qtr_totals), 
               shape = 21, 
               color = "white", 
               stroke = 1) +
    scale_fill_si(palette = "genoas", 
                  discrete = FALSE) +
    facet_wrap(~primepartner) +
    si_style_ygrid() +
    scale_size_area() +
    scale_x_discrete(labels = c("Q1\nFY49", "Q2\n", "Q3\n", "Q4\n", "Q1\nFY50"))+
    labs(x = NULL, 
         y = NULL, 
         title = "TESTING VOLUMES DOWN IN FY50Q1",
         caption = caption) +
    theme(legend.position = "none")
```

![Time-series second iteration with small multiples and size + color encoding](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/time_series_3_example-01.png "Time-series iteration 2")

To see a series of charts for all the partners, we can use a series of facets. Calculating the average performance across partners by period yields a valuable comparison metric. We then organize the partners from over to under performing. To highlight data points above/below average, we use a conditional fill on dots placed on the line graph. Negative space is created by shading the stroke around each dot with a white fill. A light grade shaded area shows the magnitude of deviation from the mean. Finally, we clean up the x-axis labels using a manual scale with line breaks to align fiscal quarters.

```{r}
hts_line_order <- 
  hts_line %>% 
  group_by(period, indicator) %>% 
  mutate(period_ave = mean(qtr_totals, na.rm = TRUE),
         deviation = qtr_totals - period_ave) %>% 
  ungroup() %>% 
  mutate(partner_order = fct_reorder(primepartner, deviation, .fun = mean, .desc = TRUE),
         dot_color = if_else(deviation >= 0, scooter, old_rose))

  hts_line_order %>% 
    filter(indicator == "HTS_TST", 
           !primepartner %in% c("Dedup", "Leo")) %>% 
    ggplot(aes(x = period, 
               y = qtr_totals, 
               group = primepartner)) +
    geom_line(aes(y = period_ave), 
              linetype = "dashed", 
              color = grey50k) +
    geom_line(color = grey60k) +
    geom_ribbon(aes(ymin = qtr_totals, 
                    ymax = period_ave), 
                alpha = 0.33, 
                fill = grey20k) +
    geom_point(aes(fill = dot_color), 
               shape = 21, 
               size = 2, 
               colour = "white") +
    scale_fill_identity() +
    facet_wrap(~partner_order) +
    scale_x_discrete(labels = c("Q1\nFY49", "Q2\n", "Q3\n", "Q4\n", "Q1\nFY50"))+
    scale_y_continuous(labels = comma) +
    si_style_ygrid() +
    labs(x = NULL, 
         y = NULL, 
         title = "TESTING VOLUMES ARE TRENDING DOWN",
         subtitle = "Points in red (green) indicator performance below (above) the quarterly mean (dotted line).",
         caption = caption) +
    theme(axis.text = element_text(size = 8))
```

![Time-series final iteration](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/time_series_4_example.png "Time-series final iteration")
