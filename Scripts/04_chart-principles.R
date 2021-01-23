## PROJECT: pretty_in_gray40k
## AUTHOR:  A.Chafetz & T.Essam | USAID
## PURPOSE: viz for chart principles (not ideal-> good plots)
## DATE:    2021-01-15
## UPDATED: 2021-01-23


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


# ENSURE FOLDER STRUCTURE -------------------------------------------------

glamr::folder_setup()

# LOAD DATA ---------------------------------------------------------------

  #using brewing data from TidyTuesday
  brew <- tt_load('2020-03-31')

  beer_states <- brew$beer_states
  brewing_materials <- brew$brewing_materials

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
