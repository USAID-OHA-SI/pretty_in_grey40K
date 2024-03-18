# Purpose: Create mini strip plots for SIEI Style Guide
# Authors: T. Essam & A. Chafetz
# Date: 2020-10-29
# Notes: This project never ends.


# START IT UP -------------------------------------------------------------
  
  library(ggplot2)
  library(tidyverse)
  library(glitr)
  library(here)
  library(systemfonts)
  
  # Where stuff gonna live
  graph <- "Graphics"
  sizing = 0.8



# BAR GRAPH ---------------------------------------------------------------

  bar <- tibble::tribble(
    ~partner, ~val,
    "prime 1",  90L,
    "prime 2",  70L,
    "prime 3",  50L,
    "prime 4",  25L
  )
  
  ggplot(bar, aes(y = fct_reorder(partner, val, .desc = F), x = val)) +
    geom_col() +
    theme_void()
  ggsave(here(graph, "bar.svg"), device = "svg", scale = sizing)


# TIME SERIES -------------------------------------------------------------

  library(tsibble)
  ts <- UKDriverDeaths %>% as_tsibble()
  
  summary(ts$value)
  min <- ts$value %>% min()
  max <- ts$value %>% max()
  
  ts %>% slice(1:35) %>% 
    ggplot(aes(x = index, y = value), ymax = value) + 
    geom_ribbon(aes(ymin = min, ymax = value), fill = grey10k) +
    geom_line(aes(y = value)) +
    coord_cartesian(ylim = c(min, max)) +
    theme_void()
  ggsave(here(graph, "time_series.svg"), device = "svg", scale = sizing)



# RANKING -----------------------------------------------------------------

  rank <- data.frame(
    site = c("A", "A", "B", "B"),
    year = c(2010, 2020, 2010, 2020),
    value = c(90, 55, 60, 50),
    color = c(grey90k, grey90k, grey40k, grey40k)
  )  
  
  ggplot(rank, aes(x = year, y = value, group = site, colour = color, fill = color)) +
    geom_line() +
    geom_point(size = 6) +
    scale_fill_identity() +
    scale_color_identity() +
    theme_void()
  ggsave(here(graph, "slope.svg"), device = "svg")


# DEVIATION ---------------------------------------------------------------

  dev <- data.frame(
    group = LETTERS[1:6],
    value = rev(c(-50, 15, -35, 40, -20, 25))
  )  
  
  ggplot(dev, aes(x = value, y = group)) + 
    geom_col() +
    theme_void()
  
  ggsave(here(graph, "deviation.svg"), device = "svg", scale = sizing)
  
  

# CORRELATION -------------------------------------------------------------

  library(MASS)
  
  set.seed(20201030)
  samples = 90
  r = 0.73
  data = mvrnorm(n = samples,  mu = c(4, 5), 
                 Sigma = matrix(c(1, r, r, 1), nrow = 2), 
                 empirical = TRUE,) %>% 
    as_tibble() %>% 
    rename(x = V1, y = V2)
  
  cor(data$x, data$y)
  
  ggplot(data, aes(x = x, y = y)) + 
    geom_point(size = 3, alpha = 0.80, color = grey90k) +
    si_style_xyline() +
    labs(x = NULL, y = NULL) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_line(size = 1.5, colour = grey90k))
  
  ggsave(here(graph, "scatterplot.svg"), device = "svg")
  
    
# WAFFLE GRAPH ------------------------------------------------------------

  library(hrbrthemes)
  library(waffle)
  
  parts <- c(22, 14)
  
  waffle(parts, rows = 6, flip = T, colors = c(grey70k, grey10k), legend_pos = "none", size = 1) 
  ggsave(here(graph, "waffle_graph.svg"), device = "svg")
  


# SPATIAL / MAP -----------------------------------------------------------

  library(sf)
  library(rnaturalearth)  
  
  ne <- ne_states(country = "united states of america", returnclass = c("sf")) %>% 
    filter(name_en %in% c("Nebraska", "Iowa", "Missouri", "Oklahoma", "Kansas",
                          "South Dakota", "Minnesota", "Illinois", "Wisconsin")) 
  
  ne %>% 
    ggplot() +
    geom_sf(aes(fill = factor(woe_id)), color = "white", size = 0.25) +
    scale_fill_brewer(palette = "Greys") +
    theme_void() +
    theme(legend.position = "none")
  
  ggsave(here(graph, "maps.svg"), device = "svg")
  

# SMALL MULTIPLES ---------------------------------------------------------

<<<<<<< HEAD
  anc <- anscombe %>% 
    pivot_longer(cols = everything(),
              names_to = c(".value", "set"),
              names_pattern = "(.)(.)") %>% 
    mutate(set = case_when(
      set == 1 ~ "Country A",
      set == 2 ~ "Country B",
      set == 3 ~ "Country C",
      set == 4 ~ "Country D"
    ))
    
  ggplot(anc, aes(x = x, y = y, group = set)) +
    geom_point(shape = 21, size = 4, fill = grey10k) +
    facet_wrap(~paste0(set, "\n")) +
    geom_smooth(se = F, colour = grey90k, method = "lm") +
    si_style_void()+
    expand_limits(x = 0, y = 0) +
    labs(x = NULL, y = NULL)
    
  ggsave(here(graph, "small_multiples.svg"), device = "svg")  
  

# HEX VALUE CONVERSION TO RGB FOR PALETTE ---------------------------------

  library(viridis)
  library(scales)
  
  colors <- c("#2057a7", "#1e87a5", "#c43d4d", "#f2bc40", "#8980cb", "#287c6f", "#e07653")
  
  purrr::map(colors, ~col2rgb(.))
  
  # Genrate hex values for viridis palette
  show_col(viridis_pal()(10))
  # anc <- anscombe %>% 
  #   pivot_longer(cols = everything(),
  #             names_to = c(".value", "set"),
  #             names_pattern = "(.)(.)") %>% 
  #   mutate(set = case_when(
  #     set == 1 ~ "Country A",
  #     set == 2 ~ "Country B",
  #     set == 3 ~ "Country C",
  #     set == 4 ~ "Country D"
  #   ))
  #   
  # ggplot(anc, aes(x = x, y = y, group = set)) +
  #   geom_point(shape = 21, size = 4, fill = grey10k) +
  #   facet_wrap(~paste0(set, "\n")) +
  #   geom_smooth(se = F, colour = grey90k, method = "lm") +
  #   si_style_void()+
  #   expand_limits(x = 0, y = 0) +
  #   labs(x = NULL, y = NULL)
  #   
  # ggsave(here(graph, "small_multiples.svg"), device = "svg")  
  # 
  df_tx <- cascade %>% 
    filter(indicator == "TX_NEW",
          period_type == "results") 
  
  sel_partner <- df_tx %>% 
    count(primepartner, wt = value) %>% 
    slice_max(n = 3, order_by = n) %>% 
    pull(primepartner)

  df_tx <- df_tx %>% 
    filter(primepartner %in% sel_partner)
  
  df_tx %>% 
    ggplot(aes(period, value)) +
    geom_col() +
    geom_hline(yintercept = 0) +
    facet_grid(primepartner~.) +
    labs(x = NULL, y = NULL) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_blank())

  ggsave(here(graph, "small_multiples.svg"), device = "svg", scale = sizing) 
  
  
