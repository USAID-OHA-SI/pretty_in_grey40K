---
layout: post
title: "Summarizing Mutliple Dimensions with Heatmaps"
author: Tim Essam | SI
date: 2021-01-22
categories: [vignette]
tags: [ggplot]
---

Heatmaps are useful for showing a phenomenon or metric across multiple dimensions. They can be used to show how a metric evolves across time and categories, or across multiple categories. Color and arrangement are the primary visual cues used to help the reader understand the phenomenon.

```{r}
# Setup knitr defaults and folder paths
  knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, out.width = '100%')
  
  pub_images <- "public_images"

# Set up caption object
  caption <- paste0("Source: Testing data from glitr package | Created on: ", Sys.Date())
```

### Heatmaps to summarize value across multiple dimensions

A [heatmap](https://en.wikipedia.org/wiki/Heat_map) is a type of visualization that shows magnitude of a phenomenon as a color in two dimensions. Generally, the x and y dimensions are categorical variables (such as testing modality by implementing partner) and the color is encoded to the quantitative variable being depicted (positivity rate). Hue (shades of color) or color intensity is used to give readers visual cues about how the phenomenon is clustered across the x and y variables. The Wall Street Journal's [The Impact of Vaccines](https://graphics.wsj.com/infectious-diseases-and-vaccines) is an excellent use case of the heatmap.

In this example, we will create a heatmap summarizing the positivity rate by testing modality and implementing partner.

```{r}

# Load required libraries 
  library(tidyverse)
  library(glitr)
  library(scales)
  library(tidytext)

# Munge the data frame to get positivity in a long format
# We want primeparnter, modality, positivity stacked long
  df_heatmap <- 
    hts %>% 
    filter(period_type == "cumulative",
           period == "FY50") %>% 
    pivot_wider(names_from = indicator,
                values_from = value,
                values_fill = 0) %>% 
    group_by(primepartner, modality) %>% 
    summarise_at(vars(starts_with("HTS")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(positivity = HTS_TST_POS / HTS_TST,
           primepartner = fct_reorder(primepartner, HTS_TST, sum, .desc = TRUE),
           modality = fct_reorder(modality, positivity)
           )

# Make a first pass on the heatmap  
  df_heatmap %>% 
    ggplot(aes(x = primepartner, 
                  y = modality, 
                  fill = positivity)) +
    geom_tile() +
    si_style_nolines()
```

![Heatmap first iteration](https://raw.githubusercontent.com/USAID-OHA-SI/pretty_in_grey40K/main/examples/images/heatmap_iteration1.png "Heatmap first iteration"){width="1778"}

This first iteration is OK. There are quite a few things we can do to improve the readability of the graphic. We'll start by i) moving the primepartner names to the top using the `scale_x_discrete()` argument, ii) then we will add a white stroke around each box by passing a color argument to the `geom_tile()` line, iii) finally, we can add a `geom_text()` to each tile to help the reader more easily understand how the color intensity is encoded to the positivity rate.

```{r}
  df_heatmap %>% 
    ggplot(aes(x = primepartner, 
                  y = modality, 
                  fill = positivity)) +
    geom_tile(color = "white", 
              size = 0.9) +
    geom_text(aes(label = percent(positivity, 1)), 
              color = "white", 
              size = 3) +
    scale_x_discrete(position = "top") +
    scale_fill_si(palette = "denims", discrete = FALSE)+
    si_style_nolines() +
    theme(panel.background = element_rect(fill = "#f6f6f6", color ="white"))
```

![Heatmap second iteration](https://raw.githubusercontent.com/USAID-OHA-SI/pretty_in_grey40K/main/examples/images/heatmap_iteration2.png "Heatmap second iteration"){width="1778"}

This looks much better. But, we could make the text a bit more readable by using a darker text color for squares with low positivity rates. We can also add a subtle grayish background to help bound the squares a bit. We can also experiment with the `coord_fixed()`. This will keep our tiles a fixed square shape. This option can be useful when you want to squish rectangles back into squares or vice versa. Finally, to ensure that our x-axis labels do not overlap we can use the `guide_axis()` function in the `scale_x_discrete()` call to make sure x-axis titles are dodged (non-overlapping).

```{r}
  df_heatmap %>% 
    ggplot(aes(x = primepartner, 
                  y = modality, 
                  fill = positivity)) +
    geom_tile(color = "white", 
              size = 0.9) +
    geom_text(aes(label = percent(positivity, 1),
                  color = if_else(positivity <= 0.25, grey90k, "white")),
              size = 3) +
    scale_x_discrete(position = "top", 
                     guide = guide_axis(n.dodge = 2)) +
    scale_fill_si(palette = "denims", discrete = FALSE) +
    scale_color_identity() +
    si_style_nolines() +
    theme(panel.background = element_rect(fill = "#f6f6f6", color ="white"),
          legend.position = "none") +
    labs(x = NULL, 
         y = NULL,
         title = "INDEX TESTING HAS THE HIGHEST POSITIVITY RATES ACROSS ALL MODALITIES",
         caption = caption) +
    coord_fixed(ratio = 1)
```

![Heatmap final product](https://raw.githubusercontent.com/USAID-OHA-SI/pretty_in_grey40K/main/examples/images/heatmap_iteration3.png "Heatmap final product"){width="1775"}

As a finishing feature, we recommend added some basic annotation to provide contextual information about squares with high positivity rates (did these modalities / partners only test a handful of people? etc.)
