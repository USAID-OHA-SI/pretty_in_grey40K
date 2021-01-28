---
layout: post
title: "Publication Quality Tables"
author: Tim Essam | SI
date: 2021-01-12
categories: [vignette]
tags: [gt]
---

Sometimes a well formatted table can be as effective as a visualization. In this example, we will go show you how use the `gt` package to generate a high-quality table.

```{r}
# Setup knitr defaults and folder paths
  knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, out.width = '100%')
  
  pub_images <- "public_images"

# Set up caption object
  caption <- paste0("Source: Testing data from glitr package | Created on: ", Sys.Date())
```

### Using `gt` to make publication quality tables

The `gt` [package](s%20all%20about%20making%20it%20simple%20to%20produce%20nice-looking%20display%20tables) is all about making it simple to produce nice-looking display tables. As before, we'll use the `hts` testing data to create our table.

```{r}
  library(tidyverse)
  library(scales)
  library(glitr)
  library(here)
  library(gt)

# Munge hts data into our desired aggregation
# Spread the data wide for ease of plotting
  hts_dev <- 
    hts %>% 
    filter(indicator %in% c("HTS_TST","HTS_TST_POS"),  
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

\`\`\`

![prepped data frame](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/hts_dev_gt.png "prepped data frame")

To get our `gt` table prepared, we pass our data frame to the `gt()` function --- the main entry point into the **gt** API.

```{r}
# Set our data frame to be a gt object
gt_table <- gt(data = hts_dev_wide)

# preview the table
gt_table
```

![gt table first iteration](https://github.com/USAID-OHA-SI/pretty_in_grey40K/raw/main/examples/images/gt_iteration1.png "gt table first iteration")

As you see from the table preview, we have quite a bit of work to do in formatting columns. Interestingly, the `gt` function read the 2nd column of the data frame we passed as a *row group* label.

```{r}
# Set our data frame to be a gt object and declare a groupname_col
# We also hide columns that we are not interested in by using the cols_hide() function
gt_table <- gt(data = hts_dev_wide)

# preview the table
gt_table
```

![Table second iteration](https://raw.githubusercontent.com/USAID-OHA-SI/pretty_in_grey40K/main/examples/images/gt_iteration2.png "Table second iteration")

In the final iteration, we make a few cosmetic changes to the table pop a bit more. We use a splash of color to highlight testing results that surpass 100K. Row headers and footers are increased in size to help separate the two indicators.

```{r}

# Show how to use text transform to flag observations
gt_table %>% 
  tab_header(title = "Partner Performance") %>% 
    tab_options(table.font.names = "Source Sans Pro",
                table_body.hlines.color = "white",
                row_group.border.top.width = px(3),
                row_group.border.top.color = "black",
                row_group.border.bottom.color = "black",
                table_body.border.bottom.width = px(3),
                table_body.border.bottom.color = "black") %>% 
     data_color(
    columns = 2:4,
    colors = scales::col_numeric(
      palette = glitr::si_rampr(  
        pal_name = "denims"
      ) %>% as.character(),
      domain = c(1e5, 4e5), # US the domain to set the bounds on which colors are applied
      na.color = "white"
    )
  )
  
```

[![Final table](https://raw.githubusercontent.com/USAID-OHA-SI/pretty_in_grey40K/main/examples/images/Table%20final%20iteration-01.png "Final table")](https://raw.githubusercontent.com/USAID-OHA-SI/pretty_in_grey40K/main/examples/images/Table%20final%20iteration-01.png)

For a thorough review of tables best practices and loads of gt tables examples, visit [The Mockup Blog](https://themockup.blog/posts/2020-09-04-10-table-rules-in-r/).
