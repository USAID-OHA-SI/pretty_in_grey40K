---
layout: post
title: "Combining Plots To Summarize Achievement"
author: Tim Essam \| SI
date: 2021-01-12
categories: [vignette]
tags: [ggplot]
---

### Target Achievement Summary

Sometimes we want to show whether or not a partner has achieve their targets for a given period of time and indicator. Rather than cram all this information into a single graphic, we can use a combination of text, filled circles and bar graphs to show levels and achievement status in a single graphic.

```{r}

# Spread the data wide for ease of plotting
  hts_dev <- 
    hts %>% 
    filter(indicator == "HTS_TST", 
           period == "FY49", 
           period_type != "results") %>% 
    group_by(primepartner, period_type, indicator) %>% 
    summarise(partner_totals = sum(value)) %>% 
    ungroup()  

# Spread the data to make the acheivement calculations a bit easier
  hts_dev_wide <- 
     hts_dev %>% 
     pivot_wider(names_from = period_type, 
                 values_from = partner_totals) %>% 
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

We start with a bar graph that summarizes achievement. An effective method for showing this type of relationship is to use a percent achievement bar graph with negative space to highlight a threshold. We will make use of the `hts_dev_wide` data.frame to create this plot. In doing so, we will create a base layer of a bar chart that stops at 100%. We then fill this space in with light gray (`grey10k`). Next, we overlay the achievement metric and reinforce the 100% threshold by inserting a white vertical line at 100%. This vertical line is placed on top of the achievement bars to show a clear threshold. The combination of the white negative space the light gray form a natural threshold without being distracting.

```{r}
# Create achievement bar graph with negative space highlighting threshold
# Incorporate text geom to display percent achievement and color bars for partners
# reaching 100% or higher
  hts_ach <- 
    hts_dev_wide %>% 
    ggplot(aes(x = achievement, 
               y = fct_reorder(primepartner, achievement))) +
    geom_col(aes(x = 1), 
             fill = grey10k) +
    geom_col(aes(fill = if_else(achievement > 1, genoa, grey60k))) +
    geom_vline(xintercept = 1, 
               size = 1.5, 
               colour = "white") +
    geom_text(aes(x = -.25, 
                  label = percent(achievement, accuracy = 2), 
              color = if_else(achievement > 1, genoa, "black")), 
              hjust = 0.5) +
    si_style_nolines() +
    scale_x_continuous(position = "top") +
    theme(axis.text.x =  element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(hjust = .1)) +
    #scale_x_continuous(labels = percent) +
    scale_fill_identity() +
    scale_color_identity() +
    labs(y = NULL) 
```

![bar graph for achievement](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/ach_bar_rhs-02.png "bar graph for achievement")

Next, we create a basic geom_point graph that takes a filled value when a partner has reached 100% achievement or higher. A new \`Targets Achieved\` variable is created as a constant. This is a placeholder to align the plot along the x-dimension. We strip all formatting except the x-axis title. This is moved to the top.

```{r}
# Create geom_point plot that shows whether or not a partner has achieved targets
  hts_tgt_achieved <- 
    hts_dev_wide %>% 
    mutate(achieved = if_else(achievement > 1, genoa, grey10k),
            `Targets Achieved` = 1) %>% 
    ggplot(aes(x = `Targets Achieved`, 
               y = fct_reorder(primepartner, achievement), 
               fill = achieved)) +
    geom_point(shape = 21, 
               size = 4) +
    scale_fill_identity() +
    si_style_nolines() + 
    scale_x_discrete(position = "top") +
    theme(axis.text.y =  element_blank(),
          axis.title.y = element_blank())
```

![filled circles to be placed in middle section](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/ach_circle_middle_2.png "filled circles to be placed in middle section")

Next, we create a column summarizing how near or far a prime partner is from their set targets. We use a `geom_text()` call to plot the calculated metric in a single column. The partners are ordered by achievement so the order remains constant across all plots.

```{r}
# Create an object of sorted partner achievement where colors indicate a threshold
# This will be used to fill in the axis labels with different colors

  hts_gap_table <-
        hts_dev_wide %>% 
    mutate(achieved = if_else(achievement > 1, genoa, color_title),
           `Deficit/Surplus` = 1) %>% 
    ggplot(aes(x = `Deficit/Surplus`, 
               y = fct_reorder(primepartner, achievement))) + 
    geom_text(aes(x = `Deficit/Surplus`,
                  label = paste0(comma(cumulative - targets)),
                  color = achieved)
              ) + 
    scale_color_identity() + 
    si_style_nolines() + 
    scale_x_discrete(position = "top") + 
    theme(axis.text.y =  element_blank(), 
          axis.title.y = element_blank()) 
```

![gap summary column](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/hts_gap_middle_1.png "gap summary column")

The final piece of the graphic is a column summarizing the actual cumulative results and targets for each partner. Similar to the plot above, we use a `geom_text()` call to print the results in a column. The `paste0` function is used to glue together the results and targets. In the `theme()` section of the ggplot chunk, we pass the `ach_color` object which allows for conditionally formatting the partner names.

```{r}

  ach_color <- if_else(sort(hts_dev_wide$achievement, decreasing = FALSE) > 1, 
                     genoa, color_title)

  hts_ach_table <-  
    hts_dev_wide %>% 
    mutate(achieved = if_else(achievement > 1, genoa, color_title),
            `Results / Targets` = 1) %>% 
    ggplot(aes(x = `Results / Targets`, 
               y = fct_reorder(primepartner, achievement), 
               fill = grey70k)) +
    geom_text(aes(label = paste0(comma(cumulative), " / ", comma(targets)), 
                  color = achieved)) +
    scale_color_identity() +
    si_style_nolines() +
    scale_x_discrete(position = "top") +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(colour = ach_color, 
                                     family = "Source Sans Pro SemiBold"))
```

![summary table column](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/hts_table_lhs.png "summary table column")

The final step is to append each plot together using the `patchwork` package. The `plot_layout` option allows us to control the width of each plot. To ensure the summary table and bar graph fit, we tinker with the width size until we arrive at a satisfactory value.

```{r}
# Put it all together with patchwork
  ach_table_plot <-
    hts_ach_table + 
    hts_gap_table + 
    hts_tgt_achieved + 
    hts_ach + 
    plot_layout(widths = c(2, 1, 1, 4)) +
    plot_annotation(caption = caption,
         title = "SEVEN OF TWELVE PARTNERS ACHIEVED HTS TARGETS")
```

![Combined achievement plot](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/ach_table_plot.png "Combined achievement plot")
