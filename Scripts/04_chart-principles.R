## PROJECT: pretty_in_gray40k
## AUTHOR:  A.Chafetz & T.Essam | USAID
## PURPOSE: viz for chart principles (not ideal-> good plots)
## DATE:    2021-01-15
## UPDATED: 2021-02-17


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
library(ggthemes)


# ENSURE FOLDER STRUCTURE -------------------------------------------------

# glamr::folder_setup()

# LOAD DATA ---------------------------------------------------------------

  #using brewing data from TidyTuesday
  #github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-31/readme.md
  brew <- tt_load('2020-03-31')

  beer_states <- brew$beer_states
  beer_taxed <- brew$beer_taxed
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

beer_states %>% 
  filter(type == "On Premises",
         state == "DC",
         year >= 2010) %>% 
  ggplot(aes(year, barrels)) +
  geom_path(color = genoa) +
  expand_limits(y = 0) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(2010,2019, 2)) + 
  labs(y = 'TOTAL INDEX TESTS', x = NULL,
       title = "Recent Scale up of Index Testing",
       subtitle = "Jupiter") +
  theme_light() +
  theme(plot.title = element_text("Comic Sans MS"),
        plot.subtitle = element_text("Franklin Gothic Demi Cond"),
        axis.title.y = element_text("Times New Roman"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank()
        )

ggsave("cp_text_typeface_notideal.svg", 
       path = "Graphics",
       width = 2.0119, height = 1.75)

beer_states %>% 
  filter(type == "On Premises",
         state == "DC",
         year >= 2010) %>% 
  ggplot(aes(year, barrels)) +
  geom_path(color = genoa) +
  expand_limits(y = 0) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(2010,2019, 2)) + 
  labs(y = NULL, x = NULL,
       title = "Recent Scale up of Index Testing",
       subtitle = "Jupiter | Total Index Tests") +
  si_style_nolines()


ggsave("cp_text_typeface_better.svg", 
       path = "Graphics",
       width = 2.0119, height = 1.75)

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
  #mekko
  # brewer_size %>% 
  #   filter(year == max(year),
  #          brewer_size %in% c("30,001 to 60,000 Barrels", 
  #                             "60,001 to 100,000 Barrels",
  #                             "100,001 to 500,000 Barrels",
  #                             "500,001 to 1,000,000 Barrels")) %>%
  #   mutate(share = n_of_brewers/sum(n_of_brewers)) %>% 
  #   ggplot(aes(brewer_size, total_barrels, fill = brewer_size, width = n_of_brewers)) +
  #   geom_col() +
  #   facet_grid(~brewer_size, scales="free_x", space="free_x") +
  #   expand_limits(y = 9000000) +
  #   scale_fill_si(palette = "scooter") +
  #   si_style_void() +
  #   theme(legend.position = "none",
  #         axis.text.x = element_blank(),
  #         axis.text.y = element_blank(),
  #         strip.text = element_blank(),
  #         panel.spacing = unit(.1, "lines"))
  
  df_dual <- brewer_size %>% 
    filter(year == max(year),
           brewer_size %in% c("30,001 to 60,000 Barrels", 
                              "60,001 to 100,000 Barrels",
                              "100,001 to 500,000 Barrels",
                              "500,001 to 1,000,000 Barrels")) %>%
    mutate(share = n_of_brewers/sum(n_of_brewers)) 
  
  v_d1 <- df_dual %>% 
    ggplot(aes(total_barrels, fct_reorder(brewer_size, total_barrels, sum))) +
    geom_col(aes(fill = brewer_size)) +
    geom_vline(xintercept = 0, color = "#505050") +
    scale_fill_si(palette = "scooter") +
    scale_y_discrete(expand = c(.005, .005)) +
    labs(x = NULL, y = NULL) +
    si_style_nolines() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  v_d2 <- df_dual %>% 
    ggplot(aes(share, fct_reorder(brewer_size, total_barrels, sum), color = brewer_size)) +
    geom_blank(aes(x = 1.1 * share)) +
    geom_segment(aes(x = 0, xend = share, 
                     yend = fct_reorder(brewer_size, total_barrels, sum)), size = .9) +
    geom_point(size = 2) +
    geom_vline(xintercept = 0, color = "#505050") +
    scale_color_si(palette = "scooter") +
    scale_fill_si(palette = "scooter") +
    scale_x_discrete(expand = c(.01, .01)) +
    labs(x = NULL, y = NULL) +
    si_style_nolines() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  v_d1 + v_d2 + 
    plot_layout(widths = c(2, 1))
  
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

  beer_taxed %>% 
    glimpse()
  
  beer_taxed %>%
    filter(tax_status == "Taxable",
           type == "In bottles and cans",
           year == "2013",
           month != 12) %>% 
    mutate(group = case_when(month <= 3 ~ "Q1",
                             month <= 6 ~ "Q2",
                             month <= 9 ~ "Q3",
                             month == 10 ~ "Jul",
                             month == 11 ~ "Aug"),
           group = factor(group, c("Q1", "Q2", "Q3", "Jul", "Aug"))) %>% 
    group_by(group) %>% 
    summarise(taxable_sum = sum(month_current)) %>% 
    ungroup() %>% 
    ggplot(aes(group, taxable_sum, fill = group)) +
    geom_col() +
    scale_fill_si("denim") +
    labs(x = NULL, y = NULL, fill = NULL) +
    si_style_ygrid() +
    theme(legend.position = "none",
          axis.text.y = element_blank()) 
  
  ggsave("cp_position_intervals_notideal.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  
  beer_taxed %>%
    filter(tax_status == "Taxable",
           type == "In bottles and cans",
           year == "2013",
           month != 12) %>% 
    mutate(group = case_when(month <= 3 ~ "Q1",
                             month <= 6 ~ "Q2",
                             month <= 9 ~ "Q3",
                             TRUE ~ "Q4")) %>% 
    group_by(group) %>% 
    summarise(taxable_sum = sum(month_current)) %>% 
    ungroup() %>% 
    mutate(full_qtr = case_when(group != "Q4" ~ taxable_sum),
           partial_qtr = case_when(group == "Q4" ~ taxable_sum),
           type = group == "Q4") %>% 
    ggplot(aes(group, full_qtr, fill = type)) +
    geom_col(na.rm = TRUE) +
    geom_col(aes(y = partial_qtr), na.rm = TRUE) +
    scale_fill_manual(values = c(denim, scooter_light)) +
    labs(x = NULL, y = NULL, fill = NULL) +
    si_style_ygrid() +
    theme(legend.position = "none",
          axis.text.y = element_blank()) 
  
  ggsave("cp_position_intervals_better.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  
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
    geom_col(fill = trolley_grey, width = .8) +
    geom_col(data = filter(pie, max(total_barrels) == total_barrels), fill = denim, width = .8) +
    si_style_void() +
    theme(legend.position = "none")
    

  ggsave("cp_position_3d_better.svg",
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
# Free of chart junk ------------------------------------------------------

  
  
  brewing_materials %>% 
    filter(year == 2016,
           type %in% c("Hops (dry)", "Wheat and wheat products")) %>% 
    mutate(quarter = case_when(month <= 3 ~ "Q1",
                               month <= 6 ~ "Q2",
                               month <= 9 ~ "Q3",
                               TRUE ~ "Q4"),
           sex = ifelse(type == "Hops (dry)", "Female", "Male")) %>%
    group_by(sex, quarter) %>% 
    summarise(value = sum(month_current, na.rm = TRUE)) %>% 
    ungroup() %>% 
    ggplot(aes(sex, value, fill = sex)) +
    geom_blank(aes(y = value * 1.05)) +
    geom_col() +
    facet_wrap(~quarter, scale = "free_x", nrow = 1, strip.position = "bottom") +
    scale_y_continuous(expand = c(.01, .01)) +
    scale_fill_excel_new() +
    labs(x = NULL, y = NULL) +
    theme_excel_new() +
    theme(strip.placement = "outside",
          legend.position = "none")
  
  ggsave("cp_position_junk_notideal.svg",
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  
  
  # brewing_materials %>% 
  #   filter(year == 2016,
  #          type %in% c("Hops (dry)", "Wheat and wheat products")) %>% 
  #   mutate(quarter = case_when(month <= 3 ~ "Q1",
  #                              month <= 6 ~ "Q2",
  #                              month <= 9 ~ "Q3",
  #                              TRUE ~ "Q4"),
  #          sex = ifelse(type == "Hops (dry)", "Female", "Male"),
  #          month_current = month_current / 100000) %>%
  #   group_by(sex, quarter) %>% 
  #   summarise(value = sum(month_current, na.rm = TRUE)) %>% 
  #   ungroup() %>% 
  #   ggplot(aes(quarter, value, fill = sex)) +
  #   geom_blank(aes(y = value * 1.05)) +
  #   geom_col() +
  #   facet_wrap(~sex, scale = "free_x", nrow = 1) +
  #   scale_y_continuous(expand = c(.01, .01)) +
  #   scale_fill_manual(values = c(moody_blue, genoa)) +
  #   labs(x = NULL, y = NULL) +
  #   si_style_ygrid() +
  #   theme(legend.position = "none")
  
  
  brewing_materials %>% 
    filter(year == 2016,
           type %in% c("Hops (dry)", "Wheat and wheat products")) %>% 
    mutate(quarter = case_when(month <= 3 ~ "Q1",
                               month <= 6 ~ "Q2",
                               month <= 9 ~ "Q3",
                               TRUE ~ "Q4"),
           sex = ifelse(type == "Hops (dry)", "Female", "Male"),
           # month_current = month_current / 100000
           ) %>%
    group_by(sex, quarter) %>% 
    summarise(value = sum(month_current, na.rm = TRUE)) %>% 
    ungroup() %>% 
    ggplot(aes(quarter, value, fill = fct_rev(sex))) +
    geom_blank(aes(y = value * 1.05)) +
    geom_col() +
    # facet_wrap(~sex, scale = "free_x", nrow = 1) +
    scale_y_continuous(expand = c(.01, .01), label = comma) +
    scale_fill_manual(values = c("Female" = moody_blue, "Male" = trolley_grey_light)) +
    labs(x = NULL, y = NULL) +
    si_style_ygrid() +
    theme(legend.position = "none")
  
  
  ggsave("cp_position_junk_better.svg",
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  
  
  
  

# Subtle Annotations ------------------------------------------------------



  beer_states %>% 
    filter(type == "Bottles and Cans",
           state != "total",
           state == "PA") %>%
    mutate(min_year = case_when(year == min(year) ~ barrels),
           max_year = case_when(year == max(year) ~ barrels)) %>% 
    ggplot(aes(year, barrels)) +
    geom_line(color = moody_blue, size = .75) +
    geom_point(size = 2, color = moody_blue, na.rm = TRUE) +
    si_style_void() +
    theme(legend.position = "none")
  
  
  ggsave("cp_color_annotate_notideal.svg",
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  
  beer_states %>% 
    filter(type == "Bottles and Cans",
           state != "total",
           state == "PA") %>%
    mutate(min_year = case_when(year == min(year) ~ barrels,
                                year %in% c(2014, 2017) ~ barrels),
           max_year = case_when(year == max(year) ~ barrels)) %>% 
    ggplot(aes(year, barrels)) +
    geom_line(color = moody_blue, size = .75) +
    geom_point(aes(y = min_year), size = 2, color = moody_blue, na.rm = TRUE) +
    geom_point(aes(y = max_year), size = 2, color = moody_blue, shape = 21, fill = "white", stroke = 1, na.rm = TRUE) +
    si_style_void() +
    theme(legend.position = "none")
    
  
  ggsave("cp_color_annotate_better.svg",
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  
  

# APPROVED COLORS ---------------------------------------------------------

  set.seed(42)
  df <- crossing(x = c(1:8),
                 y = LETTERS[1:8]) %>% 
    mutate(z = sample(1:100, 64))
  
  df %>% 
    ggplot(aes(x, y, fill = z)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(colours = rainbow(20)) +
    si_style_void() +
    theme(legend.position = "none")
  
  ggsave("cp_color_approved_notideal.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
  
  df %>% 
    ggplot(aes(x, y, fill = z)) +
    geom_tile(color = "white") +
    scale_fill_si("old_roses", discrete = FALSE) +
    si_style_void() +
    theme(legend.position = "none")
  
  ggsave("cp_color_approved_better.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)  
  



# Intentional -------------------------------------------------------------


  set.seed(42)
  df_scatter <- tibble(x = runif(64),
                       y = runif(64),
                       z = runif(64, 1, 3),
                       c = sample(1:4, 64, replace = TRUE)) %>% 
    mutate(flag = x > .75 & y < .25)
  
  df_scatter %>% 
    ggplot(aes(x, y, size = z, color = c)) +
    geom_point(alpha = .8) +
    scale_size(range = c(1, 3)) +
    labs(x = NULL, y = NULL) +
    si_style_xyline() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none")
  
  ggsave("cp_color_intentional_notideal.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75) 
  
  df_scatter %>% 
    ggplot(aes(x, y, size = z, color = flag)) +
    geom_point(alpha = .6) +
    scale_color_manual(values = c(trolley_grey_light, scooter)) +
    scale_size(range = c(1, 3)) +
    labs(x = NULL, y = NULL) +
    si_style_xyline() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none")
  
  ggsave("cp_color_intentional_better.svg", 
         path = "Graphics",
         width = 2.0119, height = 1.75)
    
  
  

# Gradient ----------------------------------------------------------------

  partners <- hts_geo %>% 
    pull(primepartner)
  
  partner_vals <- tibble(primepartner = partners,
         val1 = c(2,2,2,2,2,
                  2,2,2,2,2,
                  2,4,12),
         val2 = runif(13))
  
hts_geo %>% 
  left_join(partner_vals) %>% 
    ggplot(aes(fill = val1)) +
    geom_sf(color = "white", size = .2) +
    scale_fill_si("burnt_siennas", discrete = FALSE) +
    si_style_void() +
    theme(panel.grid = element_blank(),
          legend.position = "none")

ggsave("cp_color_gradient_notideal.svg", 
       path = "Graphics",
       width = 2.0119, height = 1.75)
  
hts_geo %>% 
  left_join(partner_vals) %>% 
  ggplot(aes(fill = val2), color = "white") +
  geom_sf(color = "white", size = .2) +
  scale_fill_si("burnt_siennas", discrete = FALSE) +
  si_style_void() +
  theme(panel.grid = element_blank(),
        legend.position = "none")

ggsave("cp_color_gradient_better.svg", 
       path = "Graphics",
       width = 2.0119, height = 1.75)
    
# EXTRA -------------------------------------------------------------------

  
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
