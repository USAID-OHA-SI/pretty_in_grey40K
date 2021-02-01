## PROJECT: pretty_in_gray40k
## AUTHOR:  A.Chafetz & T.Essam | USAID
## PURPOSE: viz for chart principles (not ideal-> good plots)
## DATE:    2021-01-15
## UPDATED: 2021-01-30


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(scales)
library(glitr)
library(ggrepel)
library(extrafont)
library(fs)
library(glue)
library(ggtext)
library(RColorBrewer)
library(svglite)
library(patchwork)
library(tidytuesdayR)
library(TrainingDataset) #remotes::install_github("ICPI/TrainingDataset")
library(plotrix)


# ENSURE FOLDER STRUCTURE -------------------------------------------------

# glamr::folder_setup()

# LOAD DATA ---------------------------------------------------------------

  #using brewing data from TidyTuesday
  #github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-31/readme.md
  brew <- tt_load('2020-03-31')

  beer_states <- brew$beer_states
  brewing_materials <- brew$brewing_materials
  brewer_size <- brew$brewer_size

# Annotations provide clarifying details ----------------------------------


beer_states %>%  
  filter(state == "VA",
         type == "Bottles and Cans",
         between(year, 2012, 2016)) %>% 
  ggplot(aes(year, barrels)) +
  geom_blank(aes(y = barrels * 1.1)) +
  geom_col(fill = denim) +
  expand_limits(y = 0) +
  si_style_void()

ggsave("cp_text_annotate.svg", 
       path = "Graphics",
       width = 2.0119, height = 1)

# Consistent typeface and sizing ------------------------------------------
# 
# set.seed(seeds[4])
# df <- tibble(x = runif(10, 25, 50),
#              y = runif(10, 25, 50),
#              z = runif(10, 80, 100),
#              n = 1:10
#              )
# 
# v1 <- df %>%
#   ggplot(aes(x, y)) +
#   geom_point(size = 4) +
#   si_style_void() +
#   lab(title = "Title")
# 
# v2 <- df %>%
#   filter(n < 6) %>% 
#   ggplot(aes(z, as.character(n))) +
#   geom_col() +
#   si_style_void() +
#   labs(title = "test")
# 
# 
# df %>% 
#   filter(n <= 3) %>% 
#   select(x, y, n) %>%
#   pivot_longer(c(x, y)) %>% 
#   group_by(n) %>% 
#   mutate(row = row_number()) %>% 
#   ggplot(aes(row, value, group = n)) +
#   geom_path() +
#   geom_point() +
#   scale_x_continuous(expand = c(.05, .05)) +
#   si_style_void()



# INTEGRATED LEGEND -------------------------------------------------------

tibble(pd = 1:5,
       mmd_o3 = c(.2, .4, .5, .7, .75),
       mmd_u3 = mmd_o3-1) %>% 
  pivot_longer(-pd,
               names_to = "type") %>%
  mutate(across(c(pd, type), as.factor),
         across(c(pd, type), fct_rev)) %>% 
  ggplot(aes(pd, value)) +
  # geom_col(aes(x = -1), fill = "#ebebeb") +
  # geom_col(aes(x = 1), fill = "#ebebeb") +
  geom_col(aes(fill = type)) +
  geom_hline(aes(yintercept = 0), color = "#505050") +
  coord_flip() +
  scale_fill_manual(values = c(old_rose, scooter)) +
  si_style_void() +
  theme(strip.text = element_blank(),
        legend.position = "none")

ggsave("cp_text_integrate_notideal.svg", 
       path = "Graphics",
       width = 2.0119, height = 1.75)


# Category labels abbreviated ---------------------------------------------

brewing_materials %>% 
  filter(str_detect(material_type, "Total", negate = TRUE),
         type != "Other",
         month < 5,
         year == 2016,
         month_current < 1000000,
         month_prior_year < 1000000) %>% 
  ggplot(aes(month_current, month_prior_year, color = type)) +
  geom_point() +
  scale_x_log10(label = comma) +
  scale_y_log10(label = comma) +
  scale_color_si("siei") +
  si_style_xyline() +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
  
  ggsave("cp_text_abbr.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)


  

# ROUND -------------------------------------------------------------------

  
  
  beer_states %>% 
    filter(type == "Bottles and Cans",
           between(year, 2014, 2016),
           state == "total") %>% 
    mutate(barrels = barrels *.2,
           barrels = ifelse(year == min(year), barrels * .6, barrels)) %>% 
    ggplot(aes(year, barrels)) +
    geom_col(fill = moody_blue) +
    si_style_void()
  
  ggsave("cp_text_cleannum.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  
  

# Void of thick borders and ticks -----------------------------------------
  
  beer_states %>% 
    filter(state %in% c("NC", "VA", "NY"),
           type == "Bottles and Cans",
           year %in% c(2009, 2019)
           ) %>% 
    ggplot(aes(year, barrels, color = state)) +
    geom_blank(aes(y = .6 * barrels)) +
    geom_blank(aes(y = 1.1 * barrels)) +
    geom_path(size = 1.5) +
    geom_point(size = 4) +
    expand_limits(x = c(2008.5, 2019.5)) +
    scale_x_continuous(breaks = c(2009, 2019)) +
    scale_color_si("burnt_sienna") +
    labs(x = NULL, y = NULL, color = NULL) +
    theme(axis.text = element_blank(),
          # panel.background = element_blank(),
          panel.grid = element_line(color = "gray30"),
          axis.ticks = element_line(color = "blue", size = 2),
          # legend.position = "none",
          legend.text = element_blank(),
          panel.border = element_rect(color = "gray20", fill = NA, size = 2)
          )
  
  ggsave("cp_lines_borders_notideal.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  beer_states %>% 
    filter(state %in% c("NC", "VA", "NY"),
           type == "Bottles and Cans",
           year %in% c(2009, 2019)
    ) %>% 
    ggplot(aes(year, barrels, color = state)) +
    geom_vline(xintercept = c(2009, 2019), color = "gray80") +
    geom_path(size = 1.5) +
    geom_point(size = 4) +
    scale_x_continuous(breaks = c(2009, 2019)) +
    scale_color_si("burnt_sienna") +
    labs(x = NULL, y = NULL) +
    si_style_void() +
    theme(legend.position = "none")

  ggsave("cp_lines_borders_better.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
# Avoid dual axis ---------------------------------------------------------

  #bar and points/lines --> mekko plot
  #https://www.washingtonpost.com/graphics/investigations/police-shootings-database/
  
  brewer_size %>% 
    filter(year == max(year),
           brewer_size %in% c("30,001 to 60,000 Barrels", 
                              "60,001 to 100,000 Barrels",
                              "100,001 to 500,000 Barrels",
                              "500,001 to 1,000,000 Barrels")) %>%
    mutate(share = n_of_brewers/sum(n_of_brewers)) %>% 
    ggplot(aes(brewer_size, total_barrels)) +
    geom_col(aes(fill = brewer_size)) +
    geom_point(aes(y = share*9000000), shape = 23, fill = old_rose, size = 3,
               na.rm = TRUE) +
    expand_limits(y = 9000000) +
    scale_fill_si(palette = "scooter") +
    scale_y_continuous(expand = c(.005, .005), name = NULL,
                       sec.axis = sec_axis(~./9000000, name = NULL, labels = percent)) +
    labs(x = NULL) +
    si_style_xyline() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  ggsave("cp_lines_dual_notideal.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  brewer_size %>% 
    filter(year == max(year),
           brewer_size %in% c("30,001 to 60,000 Barrels", 
                              "60,001 to 100,000 Barrels",
                              "100,001 to 500,000 Barrels",
                              "500,001 to 1,000,000 Barrels")) %>%
    mutate(share = n_of_brewers/sum(n_of_brewers)) %>% 
    ggplot(aes(brewer_size, total_barrels, fill = brewer_size, width = n_of_brewers)) +
    geom_col() +
    facet_grid(~brewer_size, scales="free_x", space="free_x") +
    expand_limits(y = 9000000) +
    scale_fill_si(palette = "scooter") +
    si_style_void() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_blank(),
          panel.spacing = unit(.1, "lines"))
  
  ggsave("cp_lines_dual_better.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)

  
  
# Reference lines/area for context ----------------------------------------


    
  
  viz_ref <- brewing_materials %>%
    filter(str_detect(type, "Total", negate = TRUE),
           type %in% c("Sugar and syrups", "Corn and corn products", 
                       "Rice and rice products", "Hops (dry)"),
           year == 2012) %>% 
    group_by(type) %>% 
    mutate(median = median(month_current)) %>% 
    ungroup() %>% 
    mutate(median = case_when(month == 6 ~ median)) %>% 
    ggplot(aes(month_current, fct_reorder(type, month_current))) +
    geom_point(aes(color = type), position = position_jitter(seed = 42,  height = .2)) +
    scale_color_si("genoa", alpha = .8) +
    labs(x = NULL, y = NULL) +
    si_style() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  ggsave("cp_lines_ref_notideal.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  
  viz_ref + 
    geom_errorbar(aes(xmin = median, xmax = median),  na.rm = TRUE,
                  color = "#505050", size = 1.2) 
     
  ggsave("cp_lines_ref_better.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  rm(viz_ref)  
  
# Data ordered intentionally ----------------------------------------------

  n <- 8
  set.seed(42)
  df <- tibble(x = runif(n),
               y = LETTERS[1:n],
               # y = replicate(n, gen_sitename()),
               clr = ifelse(x < .6, burnt_sienna, scooter))
  
  df %>% 
    ggplot(aes(x, fct_rev(y))) +
    geom_blank(aes(x = 1.1 * x)) +
    geom_point(size = 3, color = scooter) +
    geom_linerange(aes(xmin = 0, xmax = x), size = 1, color = scooter) +
    geom_vline(xintercept = 0, color = "#505050") +
    scale_x_continuous(expand = c(.005, .005)) +
    labs(x = NULL, y = NULL) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 7))
  
  ggsave("cp_position_order_notideal.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  df %>% 
    ggplot(aes(x, fct_reorder(y, x, .desc = T), color = clr)) +
    geom_blank(aes(x = 1.1 * x)) +
    geom_vline(xintercept = .6, color = "#505050", linetype = "dashed") +
    geom_point(size = 3) +
    geom_linerange(aes(xmin = 0, xmax = x), size = 1) +
    geom_vline(xintercept = 0, color = "#505050") +
    scale_x_continuous(expand = c(.005, .005)) +
    scale_color_identity() +
    labs(x = NULL, y = NULL) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 7))
  
  ggsave("cp_position_order_better.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)

# Alignment and equal intervals -------------------------------------------

  #??
  
# Axis lines start at appropriate values ----------------------------------

  #col starts above 0
  
  brewer_size %>%
    filter(brewer_size == "1 to 1,000 Barrels") %>% 
    slice_tail(n = 5) %>% 
    ggplot(aes(year, total_barrels/1000000)) +
    geom_col(fill = genoa) +
    scale_y_continuous(limits = c(.8, 1.5),
                       labels = comma_format(.1),
                       oob = rescale_none) +
    si_style_ygrid() +
    labs(x = NULL, y = NULL) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 7))

  ggsave("cp_position_baseline_notideal.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  brewer_size %>%
    filter(brewer_size == "1 to 1,000 Barrels") %>% 
    slice_tail(n = 5) %>% 
    ggplot(aes(year, total_barrels/1000000)) +
    geom_col(fill = genoa) +
    scale_y_continuous(labels = comma_format(.1)) +
    si_style_ygrid() +
    labs(x = NULL, y = NULL) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 7))
  
  ggsave("cp_position_baseline_better.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
  

# Small multiples ---------------------------------------------------------

  #spaghetti to slope small multiples
  brewing_materials %>% 
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
    ggplot(aes(month, month_current, group = type_l, color = type_l)) +
    geom_path() +
    geom_hline(yintercept = 0, color = "#505050") +
    scale_color_si("siei") +
    labs(color = NULL) +
    si_style_void() +
    theme(legend.position = "none")

  ggsave("cp_position_smallmulti_notideal.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  brewing_materials %>% 
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
    ggplot(aes(month, month_current, group = type_l, color = type_l, fill = type_l)) +
    geom_area(alpha = .2) +
    geom_hline(yintercept = 0, color = "#505050") +
    facet_wrap(~fct_reorder(type_l, month_current, max, .desc = TRUE)) +
    expand_limits(y = 0) +
    scale_color_si("siei") +
    scale_fill_si("siei") +
    labs(color = NULL) +
    si_style_void() +
    theme(legend.position = "none",
          strip.text.x = element_blank(),
          panel.spacing = unit(.2, "lines"))
  
  ggsave("cp_position_smallmulti_better.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
# No 3d -------------------------------------------------------------------

  #3d pie to bar  
  #https://github.com/coolbutuseless/ggthreed

  pie <- brewer_size %>% 
    filter(year == 2019,
           !brewer_size %in% c("Total", "Under 1 Barrel", "6,000,001 Barrels and Over"),
           !is.na(total_barrels)) %>% 
    mutate(brewer_size = fct_reorder(brewer_size, total_barrels))
  
  pie3D(pie$total_barrels, radius = 1.5,
        col = si_palettes$siei_pairs)
  
  
  # ggsave("cp_position_3d_notideal.svg", 
  #        path = "Graphics",
  #        width = 2.0119, height = 1.75)
  
  
  pie %>% 
    ggplot(aes(total_barrels, brewer_size, fill = brewer_size)) +
    geom_col(fill = trolley_grey) +
    geom_col(data = filter(pie, max(total_barrels) == total_barrels), fill = denim) +
    si_style_void() +
    theme(legend.position = "none")
    

  ggsave("cp_position_3d_better.svg",
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
# Free of chart junk ------------------------------------------------------

  #??
  
  #heat chart with 
  set.seed(42)
  df <- crossing(x = c(1:8),
                 y = LETTERS[1:8]) %>% 
    mutate(z = sample(1:100, 64))
  
  df %>% 
    ggplot(aes(x, y, fill = z)) +
    geom_tile(color = "white") +
    scale_fill_si(discrete = FALSE) +
    si_style_void() +
    theme(legend.position = "none")
  
  ggsave("cp_lines_dual_better.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  
  
  
  
  
  
set.seed(42)
type_map <- replicate(length(unique(brewing_materials$type)), gen_sitename()) %>%
  bind_cols(type_conv = .,
            type = unique(brewing_materials$type))

brewing_materials %>% 
  left_join(type_map, by = "type") %>% 
  filter(str_detect(material_type, "Total", negate = TRUE),
         type != "Other",
         month < 6,
         year == 2017) %>% 
  ggplot(aes(month_current, month_prior_year, color = type_conv)) +
  geom_blank(aes(month_prior_year, month_current)) +
  geom_abline(slope = 1, linetype = "dashed", color = "gray60", size = .5) +
  geom_point(size = 1, alpha = .4) +
  expand_limits(x = 100, y = 100) +
  scale_color_si("siei") +
  scale_x_log10(label = comma) +
  scale_y_log10(label = comma) +
  si_style_void() +
  labs(color = "") +
  theme(legend.position = "right",
        # legend.spacing.y = unit(1,"mm"), 
        legend.text = element_text(size = 4))

ggsave("cp_text_integrate_notideal.svg", 
       path = "Graphics",
       width = 2.0119, height = 1.75)


brewing_materials %>% 
  left_join(type_map, by = "type") %>% 
  filter(str_detect(material_type, "Total", negate = TRUE),
         type != "Other",
         month < 6,
         year == 2017) %>% 
  ggplot(aes(month_current, month_prior_year, color = type_conv)) +
  geom_blank(aes(month_prior_year, month_current)) +
  geom_abline(slope = 1, linetype = "dashed", color = "gray60", size = .5) +
  geom_point(size = 1, alpha = .4) +
  geom_point(data = filter(brewing_materials, type == "Hops (dry)", color == 'green'), size = 1, alpha = .4) +
  expand_limits(x = 100, y = 100) +
  scale_color_si("trolley_greys") +
  scale_x_log10(label = comma) +
  scale_y_log10(label = comma) +
  si_style_void() +
  labs(color = "") +
  theme(legend.position = "bottom",
        # legend.spacing.y = unit(1,"mm"), 
        legend.text = element_text(size = 4))







brewing_materials %>%
  filter(str_detect(type, "Total", negate = TRUE),
         type != "Malt and malt products",
         year == 2012,
         month == 6) %>%
  ggplot(aes(month_current, fct_reorder(type, month_current), group = type)) +
  geom_vline(xintercept = 0) +
  geom_linerange(aes(xmin = 0, xmax = month_current), color = denim) +
  geom_point(size = 2, color = denim) +
  scale_x_continuous(expand = c(.005, .005)) +
  labs(x = NULL, y = NULL) +
  si_style_void()


brewing_materials %>%
  filter(str_detect(type, "Total", negate = TRUE),
         type != "Malt and malt products",
         year == 2012,
         month == 6) %>%
  mutate(clr = ifelse(month_current > mean(month_current), trolley_grey, denim)) +
  ggplot(aes(month_current, fct_reorder(type, month_current), group = type)) +
  geom_vline(xintercept = 0) +
  geom_vline(aes(xintercept = mean(month_current)), linetype = "dashed") +
  geom_linerange(aes(xmin = 0, xmax = month_current, color = clr)) +
  geom_point(aes(color = clr), size = 2) +
  scale_x_continuous(expand = c(.005, .005)) +
  labs(x = NULL, y = NULL) +
  si_style_void()