# PROJECT:  pretty_in_grey40K
# PURPOSE:  mock ups applying colors
# AUTHOR:   A.Chafetz | USAID
# REF ID:   7ba931e5 
# LICENSE:  MIT
# DATE:     2024-05-29
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)
  library(glue)
  library(tidytuesdayR)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "7ba931e5"  #a reference to be places in viz captions 
  
  cap <- glue("Source: TTB data via Tidy Tuesday (2020-03-31) | Ref id: {ref_id}")

# PALETTES ----------------------------------------------------------------

  hw_midnight_blue_cat <-   c("#002A6C", "#5BB5D5", "#F9C555", "#419164", "#E14BA1") 
  hw_viking_cat <-          c("#5BB5D5", "#002A6C", "#F36428", "#F9C555", "#3B5BBE")
  hw_electric_indigo_cat <- c("#3B5BBE", "#E14BA1", "#F9C555", "#B97ABF", "#419164")
  hw_orchid_bloom_cat <-    c("#E14BA1", "#002A6C", "#B97ABF", "#419164", "#F9C555")
  hw_sun_kissed_cat <-      c("#F9C555", "#3B5BBE", "#5BB5D5", "#F36428", "#419164")
  hw_hunter_cat <-          c("#419164", "#002A6C", "#E14BA1", "#F9C555", "#3B5BBE")
  hw_lavender_haze_cat <-   c("#B97ABF", "#F9C555", "#3B5BBE", "#002A6C", "#5BB5D5") 
  hw_tango_cat <-           c("#F36428", "#002A6C", "#3B5BBE", "#5BB5D5", "#F9C555")
  
  hw_slate_40 <- "#bfbfbf"
  
  hw_cat <- list(hw_midnight_blue_cat, hw_viking_cat, hw_electric_indigo_cat,
                 hw_orchid_bloom_cat, hw_sun_kissed_cat, hw_hunter_cat,
                 hw_lavender_haze_cat, hw_tango_cat)
  
  names(hw_cat) <- c("hw_midnight_blue_cat", "hw_viking_cat", "hw_electric_indigo_cat",
                     "hw_orchid_bloom_cat", "hw_sun_kissed_cat", "hw_hunter_cat",
                     "hw_lavender_haze_cat", "hw_tango_cat")
   
# IMPORT ------------------------------------------------------------------

  #using brewing data from TidyTuesday
  #github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-31/readme.md
  brew <- tt_load('2020-03-31')
  
  beer_states <- brew$beer_states
  beer_taxed <- brew$beer_taxed
  brewing_materials <- brew$brewing_materials
  brewer_size <- brew$brewer_size
  

# SCATTER PLOT ------------------------------------------------------------

  df_scatter <- brewing_materials %>% 
    filter(str_detect(material_type, "Total", negate = TRUE),
           type != "Other",
           month < 5,
           year == 2016,
           month_current < 1000000,
           month_prior_year < 1000000) 
  
  plot_scatter <- function(pal, name){
    df_scatter %>% 
      ggplot(aes(month_current, month_prior_year, fill = type)) +
      geom_point(color = "white", size = 4, shape = 21) +
      scale_x_log10(label = comma) +
      scale_y_log10(label = comma) +
      scale_fill_manual(values = c({pal}[1:2], rep(hw_slate_40, 4))) +
      si_style_xyline() +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL,
           subtitle = {name}) +
      theme(legend.position = "none",
            axis.text = element_blank())
  }
  

  map2(hw_cat, 
       names(hw_cat) %>% str_remove_all("hw_|_cat") %>% str_replace("_", " ") %>% str_to_title(),
       plot_scatter) %>% 
    reduce(`+`) +
    plot_annotation("SCATTER PLOT",
                    subtitle = "Mock ups to test different color combinations | First two colors of categorical palette with Slate 40% for remaining categories",
                    caption = cap,
                    theme = si_style())
  
  
  # si_preview()
  si_save("mockup_scatter.png", path = "Images")
  

# FACETS AREA PLOTS -------------------------------------------------------


  df_area <- brewing_materials %>% 
    filter(year == 2013,
           !type %in% c("Malt and malt products",
                        "Hops (dry)",
                        "Hops (used as extracts)"),
           str_detect(type, "Total", negate = TRUE)) %>% 
    select(type, month, month_current) %>% 
    spread(month, month_current) %>%
    bind_cols(tibble(type_l = LETTERS[1:6])) %>%
    gather(month, month_current, -type, -type_l) %>%
    mutate(month = as.numeric(month)) %>% 
    filter(type_l %in% LETTERS[1:4]) %>% 
    mutate(type_l = fct_reorder(type_l, month_current, max, .desc = TRUE))
  
  plot_area <- function(pal, name){
    df_area %>% 
      ggplot(aes(month, month_current, group = type_l, color = type_l, fill = type_l)) +
      geom_area(alpha = .2) +
      geom_hline(yintercept = 0, color = "#505050") +
      facet_wrap(~type_l, nrow = 1) +
      expand_limits(y = 0) +
      scale_fill_manual(values = {pal}, aesthetics = c("fill", "color"))+
      labs(color = NULL,
           subtitle = {name}) +
      si_style_void() +
      theme(legend.position = "none",
            strip.text.x = element_blank(),
            panel.spacing = unit(.2, "lines")) 
  }
   
  
  map2(hw_cat, 
       names(hw_cat) %>% str_remove_all("hw_|_cat") %>% str_replace("_", " ") %>% str_to_title(),
       plot_area) %>% 
    reduce(`+`) +
    plot_annotation("AREA PLOT SMALL MULTIPLES",
                    subtitle = "Mock ups to test different color combinations | First four colors of categorical palette (area filled at 20% opacity)",
                    caption = cap,
                    theme = si_style())
  
  
  # si_preview()
  si_save("mockup_area.png", path = "Images")
  


# STACKED BAR CHART -------------------------------------------------------

  df_stacked <- brewing_materials %>% 
    filter(year == 2016,
           type %in% c("Hops (dry)", "Wheat and wheat products")) %>% 
    mutate(quarter = case_when(month <= 3 ~ "Q1",
                               month <= 6 ~ "Q2",
                               month <= 9 ~ "Q3",
                               TRUE ~ "Q4"),
           sex = ifelse(type == "Hops (dry)", "Female", "Male"),
           fill_alpha = ifelse(sex == "Female", 1, .4)
    ) %>%
    group_by(sex, quarter, fill_alpha) %>% 
    summarise(value = sum(month_current, na.rm = TRUE),
              .groups = "drop")
  
  plot_stacked <- function(pal, name, gray = FALSE){

    col2 <- ifelse(gray == FALSE, pal[1], hw_slate_40)
    
    df_stacked %>% 
      ggplot(aes(quarter, value, fill = fct_rev(sex), alpha = fill_alpha)) +
      geom_blank(aes(y = value * 1.05)) +
      geom_col() +
      scale_y_continuous(expand = c(.01, .01), label = comma) +
      scale_alpha_identity() +
      scale_fill_manual(values = c(col2, pal[1])) +
      labs(x = NULL, y = NULL,
           subtitle = {name}) +
      si_style_ygrid() +
      theme(legend.position = "none",
            axis.text = element_blank())  
  }
 

  map2(hw_cat, 
       names(hw_cat) %>% str_remove_all("hw_|_cat") %>% str_replace("_", " ") %>% str_to_title(),
       plot_stacked) %>% 
    reduce(`+`) +
    plot_annotation("STACKED BAR",
                    subtitle = "Mock ups to test different color combinations | Background area filled with color at 40% opacity",
                    caption = cap,
                    theme = si_style())
  
  
  # si_preview()
  si_save("mockup_stacked.png", path = "Images")
  
  
  map2(hw_cat, 
       names(hw_cat) %>% str_remove_all("hw_|_cat") %>% str_replace("_", " ") %>% str_to_title(),
       plot_stacked, gray = TRUE) %>% 
    reduce(`+`) +
    plot_annotation("STACKED BAR",
                    subtitle = glue("Mock ups to test different color combinations | Background area filled with Slate at 40%"),
                    caption = cap,
                    theme = si_style())
  
  # si_preview()
  si_save("mockup_stacked-gray.png", path = "Images")



# SLOPE CHART -------------------------------------------------------------


  df_line <- beer_states %>% 
    filter(state %in% c("NC", "VA", "NY"),
           type == "Bottles and Cans",
           year %in% c(2009, 2019)
    ) 
  
  plot_slope <- function(pal, name){
    df_line %>% 
      ggplot(aes(year, barrels, color = state)) +
      geom_vline(xintercept = c(2009, 2019), color = "gray80") +
      geom_path(size = 1.5) +
      geom_point(size = 4) +
      scale_x_continuous(breaks = c(2009, 2019)) +
      scale_color_manual(values = {pal}) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL,
           subtitle = {name}) +
      si_style_void() +
      theme(legend.position = "none")
  }
  
  map2(hw_cat, 
       names(hw_cat) %>% str_remove_all("hw_|_cat") %>% str_replace("_", " ") %>% str_to_title(),
       plot_slope) %>% 
    reduce(`+`) +
    plot_annotation("SLOPED LINE CHART",
                    subtitle = "Mock ups to test different color combinations | First three colors of categorical palette",
                    caption = cap,
                    theme = si_style())
  
  
  # si_preview()
  si_save("mockup_slope.png", path = "Images")

# JITTER PLOT -------------------------------------------------------------


  df_jitter <- brewing_materials %>%
    filter(str_detect(type, "Total", negate = TRUE),
           type %in% c("Sugar and syrups", "Corn and corn products", 
                       "Rice and rice products", "Hops (dry)"),
           year == 2012) %>% 
    group_by(type) %>% 
    mutate(median = median(month_current)) %>% 
    ungroup() %>% 
    mutate(median = case_when(month == 6 ~ median),
           type = fct_reorder(type, month_current)) 
  
  plot_jitter <- function(pal, name){
    df_jitter %>% 
      ggplot(aes(month_current, type)) +
      geom_errorbar(aes(xmin = median, xmax = median),  na.rm = TRUE,
                    color = "#505050", size = 1.2) +
      geom_point(aes(fill = fct_rev(type)), 
                 position = position_jitter(seed = 42,  height = .2),
                 shape = 21, color = "gray90", stroke = .2
                 ) +
      scale_fill_manual(values = {pal}) +
      labs(x = NULL, y = NULL,
           subtitle = {name}) +
      si_style() +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.text.y = element_blank())
  }
  
  
  map2(hw_cat, 
       names(hw_cat) %>% str_remove_all("hw_|_cat") %>% str_replace("_", " ") %>% str_to_title(),
       plot_jitter) %>% 
    reduce(`+`) +
    plot_annotation("JITTER PLOT",
                    subtitle = "Mock ups to test different color combinations | First four of categorical palette",
                    caption = cap,
                    theme = si_style())
  
  
  # si_preview()
  si_save("mockup_jitter.png", path = "Images")
 
      
  