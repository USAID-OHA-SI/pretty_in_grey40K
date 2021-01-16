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
```

### How to use `ggplot2` and `glitr` to create clean visualizations

The following tutorial provides a crash course for using the SIEI recommended chart types and color palettes. The examples correspond to those outlined in the [Data Viz Cheat Sheet](https://user-images.githubusercontent.com/5873344/100870163-2661fb80-346c-11eb-8114-bf677f8cc6fb.png). "Data Viz Cheat Sheet"). All plots are created using the `ggplot2`, `glitr` and a few helper packages (`scales`, `ggrepel`, `patchwork` and `sf`). To reproduce results from this tutorial, you will need to download and install all of these packages.

### Wait, what is `glitr`?

`glitr` is a hand-crafted, SI R graphics package that allows a user to create standardized plots with a common look and feel (think Baldwin Brothers). The package includes the SIEI recommended color schemes and chart formatting options to minimize chart junk. Detailed examples on how to use functions included in the package can be found in the `glitr` [cookbook](https://github.com/USAID-OHA-SI/glitr/blob/master).

### Key Performance Indicators (KPIs)

KPIs are useful for summarizing a metric or series of metrics. KPI graphics should be simple and easy to interpret. The inclusion of arrows can help indicate the direction of change.

```{r}
  library(tidyverse)
  library(scales)
  library(glitr)
  library(here)
  library(patchwork)

  # Munge the data to get KPIs in a data frame
  # Add thousands (K) or millions (M) label to number depending on size
    kpi <- 
      hts %>% 
      filter(period == "FY50", period_type == "cumulative") %>% 
      group_by(indicator) %>% 
      summarise(total = round(sum(value, na.rm = TRUE)/1000)) %>% 
      ungroup() %>% 
      spread(indicator, total) %>% 
      mutate(Yield = (HTS_TST_POS / HTS_TST)) %>% 
      pivot_longer(everything(), 
                   values_to = "value", 
                   names_to = "indicator") %>% 
      mutate(row_order = 0,
             label = case_when(
               indicator %in% c("HTS_TST", "HTS_TST_POS") ~ paste0(value, " K"),
               TRUE ~ percent(value, 0.01)
               )
             )
    
  # Now create a geom_text plot with the KPIs. 
  # We use the facet strip position to note the indicator type.
    p <- ggplot(kpi, aes(y = row_order, x = indicator, colour = indicator)) +
      # Toggle chunk below if you want a shape to appear behind the text
      #geom_point(aes(fill = indicator), shape = 22, size = 50, alpha = 0.25) + 
      geom_text(aes(label = label), size = 10) +
      facet_wrap(~indicator, nrow = 1, strip.position = "bottom", scales = "free") +
      si_style_void() +
      scale_color_si(palette = "denim") +
      scale_fill_si(palette = "denim") +
      theme(legend.position = "none",
            strip.text = element_text(size = 12, hjust = 0.5)) 
    
  # When saving, compress the dimensions to only retain the geom and the facet label.
  # Making KPI's may be easier in Google Presentation/PowerPoint.
    si_save(file.path(pub_images, "kpi"), plot = p, width = 8, height = 0.75)
```

![Key Performance Metric Graphic](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/kpi.png "KPI")
