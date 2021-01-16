---
layout: post
title: "Key Performance Indicators Example"
author: Tim Essam \| SI
date: 2021-01-12
categories: [vignette]
tags: [ggplot]
---

### Required packages

You will need a couple new packages to run the code below. The `gisr` and `glitr` packages are SI developed packages to help with gis and data munging related tasks, respectively. You can install the packages with the following chunks of code.

    devtools::install_github("USAID-OHA-SI/glamr")
    devtools::install_github("USAID-OHA-SI/gisr")

```{r}
# Setup knitr defaults and folder paths
  knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, out.width = '100%')
  
  pub_images <- "public_images"
  images <- "Images"

# Set up caption object
  caption <- paste0("Source: Testing data from glitr package | Created on: ", Sys.Date())
  
# Add libraries needed
  library(tidyverse)
  library(sf)
  library(scales)
  library(glitr)
  library(here)
  library(patchwork)
  library(gisr)
  library(glitr)
```

### Spatial

Maps convey geographic patterns of data using coordinates and polygons. Maps are useful for showing spatial correlations or the geographic distribution of a metric. Combining a map with a bar graph can be a powerful way to summarize a metrics distribution by magnitude and geography. In the example below, we walk through how to create a [choropleth](https://en.wikipedia.org/wiki/Choropleth_map "Choropleth map wikipedia page") map and combine it with a bar graph. As before, we use the `hts` and `hts_geo` data available from the `glitr` package.

We will start by calculating the `hts_dev_wide` data frame that is also presented in the `Deviation` tutorial. We make use of the simple features `sf` package to load and manipulate data that has spatial defined spatial properties.

#### How to check what type of data you are wrangling

To check the property of an R object or data frame you can use the `str()` function. This function compactly displays the internal structure of an R object. As we will be working with the `hts_geo` data set, we can check its properties by loading the `glitr` package (done in the chunk above) and then passing the data object to the `str`() function.

`str(hts_geo)`

![Output from of str(hts_geo)](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/hts_geo_str_results.png "Output from of str(hts_geo)")

In the first line, you will notice that the classes of the object are 'sf' and 'data.frame'. The 'sf' portion of line indicates that the object is a collection of simple features that includes attributes and geometries in the form of a data frame. The major advantage of working with an 'sf' object is that it is compatible with the `tidyverse`. Practically, this means that you can use the pipe `%>%` operator and pass the object through `tidyverse` functions. When plotting, `ggplot2` has a dedicated geom `geom_sf()` to visualize simple features.

In the example below, we walk through how to join attribute data from the `hts` data to the `hts_geo` data frame. You may find yourself doing a similar exercise if you have a `.shp` (shapefile) file that is lacking attribution information you would like to visualize or analyze. See [`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html "Read simple features or layers from file or database") for details on how to read a `.shp` as an sf object.

```{r}
# Munge the hts data to create the hts_dev_wide data frame used in other tutorials
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
            deviation = achievement - annual_achievement) %>% 
     # Remove dedups
     filter(primepartner != "Dedup")

# Merge the hts_dev_wide data frame with the hts_geo
   hts_geo_dev <- left_join(hts_geo, hts_dev_wide)
   
# Use the glamr function (https://github.com/USAID-OHA-SI/glamr) to print results   
   glamr::prinf(hts_geo_dev)
```

![Results of joining hts_geo with hts_dev_wide](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/left_join_hts_geo_hts_dev_wide.png "Results of joining hts_geo with hts_dev_wide")

Th output of the join operation shows that the `hts_geo` data merged 13 features (rows) with 11 fields (columns). We can see that partner Leo has NAs for all calculated metrics. Looking at the original `hts` data you can see that Leo in fact only started reporting on testing results in FY50. As the `hts` data is filtered to FY49, it makes sense that Leo has NAs for the calculated metrics. Instead of removing Leo with a filter operation, we leave it in to show how to visualize NA values when creating maps.

#### Plotting the joined data

To plot the joined data we can use `ggplot2` and the `geom_sf()` function. The code chunk for creating a basic map is below.

```{r}
  hts_geo_dev %>% 
    ggplot() +
    geom_sf() 
```

![Map first iteration](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/map_first_iteration.png "Map first iteration")

#### Creating a boundary

The first map lacked quite a few things to make the visual useful. The lack of polygon labels and colors to encode testing achievement puts this map in the category of a poorly done base map. Below, we show how to encode target achievement with colors, resulting in a choropleth map. We also take advantage of `sf`'s [`summarise`](https://r-spatial.github.io/sf/reference/tidyverse.html)`()` command to create an outline of the polygons. This can be useful for helping to define the boundary of the area being mapped. To show how different geom parameters affect the map's look, two maps are presented. The first is based on using default parameters and the second version shows how incorporating SIEI colors and helper functions from `glitr` and `gisr` can improve the product.

```{r}
# Create an admin0 to outline the admin1 shapefile
# We will pass this sf object to ggplot as a new data source
  hts_geo_admin0 <- hts_geo_dev %>% summarise(cumulative)
  
# Original basemap with a fill value passed to asethics
  map_lhs <-  hts_geo_dev %>% 
  ggplot() +
  geom_sf(aes(fill = achievement)) 
  
# Build on our original basemap
  map_rhs <- 
    hts_geo_dev %>% 
    ggplot() +
    geom_sf(aes(fill = achievement), 
            color = "white", 
            stroke = 0.5, 
            alpha = 0.85,
            ) +
    geom_sf(data = hts_geo_admin0, 
            color = grey90k, 
            stroke = 2, 
            fill = NA) +
  scale_fill_si(palette = "denims", 
                discrete = FALSE, 
                label = percent,
                na.value = grey20k) + # this controls the fill on missing values (Leo)
  gisr::si_style_map() 
  
# compare maps
  map_lhs + map_rhs
```

![Map second and third iterations](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/map_second_iterations.png "Map second and third iteration")

The visualization on the right is getting us closer to a presentation ready product. We still need to incorporate polygon labels, annotations and titles. Below, we make use of the `geom_sf_text()` function to add in polygon labels.

```{r}
  # Define a vector with label colors
  label_color <- if_else(hts_geo_dev$achievement > 2, "white", "black")

map_lhs_adorned <- 
  hts_geo_dev %>%
  ggplot() + 
  geom_sf(aes(fill = achievement), 
          color = "white", 
          size = 0.5, 
          alpha = 0.85) + 
  geom_sf_text(aes(label = paste0(primepartner, "\n", percent(achievement, 2))),
                   color = label_color,
                   size = 2.5) + 
  geom_sf(data = hts_geo_admin0, 
          color = grey90k, 
          stroke = 2, 
          fill = NA) +
  scale_fill_si(palette = "denims", 
                discrete = FALSE, 
                label = percent,
                na.value = grey20k) +
  gisr::si_style_map() +
  theme(legend.position = "none") +
  labs(title = "VIRGO LEADS TESTING ACHIEVEMENT AT NEARLY 400%",
       caption = caption)
```

![Map fourth iteration](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/map_fourth_iteration.png "Map fourth iteration")

Much better, but we can still make a few tweaks that will help this product stand on its own. Complementing the map with bar graph can help the reader compare magnitude differences on the metric being presented without having to do too much math (remember, your job is to make the reader understand a product within 5-10 seconds of seeing it).

```{r}
  bar_graph <- 
    hts_dev_wide %>% 
    ggplot(aes(y = fct_reorder(primepartner, achievement), 
               x = achievement, 
               fill = achievement)) +
    geom_col(aes(x = 1), 
             fill = grey10k, 
             alpha = 0.85) +
    geom_col() + 
    geom_vline(xintercept = 1, 
               size = 1.5, 
               color = "white") +
    scale_fill_si(palette = "denims", 
                  discrete = FALSE, 
                  label = percent) +
    scale_x_continuous(labels = percent) +
    labs(x = NULL, 
         y = NULL, 
         caption = caption) +
    si_style_xline() +
    theme(legend.position = "none")
  
  # We can remove the title and caption from the map by passing NULL values to labs()
    bar_graph + labs(caption = NULL) + 
      map_lhs_adorned + 
      labs(title = NULL) + 
      plot_annotation(title = "VIRGO AND SERPENS LEAD TESTING ACHEIVEMENT IN FY49") 
```

![Combined bar graph and map](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/Map_bar_combined-01.png "Combined bar graph and map")

To export this object for additional editing in a vector graphics software, we use the `si_save()` function. This function is a wrapper for ggsave and uses default options to save the plot to a size optimal for presentations (10 x 5.625 inches).

```{r}
  combined_plot <- 
    bar_graph + 
    labs(caption = NULL) + 
    map_lhs_adorned + 
    labs(title = NULL) + 
    plot_annotation(title = "VIRGO AND SERPENS LEAD TESTING ACHEIVEMENT IN FY49") 

si_save(here("images", "HTS_Saturn_bar_map_combined.svg"), plot = combined_plot)
```
