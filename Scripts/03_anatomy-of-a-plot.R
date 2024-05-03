## PROJECT: pretty_in_gray40k
## AUTHOR:  A.Chafetz & T.Essam | USAID
## PURPOSE: anatomy of a plot
## REF ID:   8b8b72ce 
## DATE:    2020-11-03
## UPDATED: 2024-05-02


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(scales)
library(gagglr)
library(systemfonts)
library(glue)
library(ggtext)

# GLOBAL VARIABLES --------------------------------------------------------

  ref_id <- "8b8b72ce"  #a reference to be places in viz captions 
 
  main_fill <- hw_electric_indigo
  decline_fill <- hw_orchid_bloom

# MUNGE DATA --------------------------------------------------------------

df_index <- hts %>% 
  filter(indicator == "HTS_TST_POS",
         modality %in% c("Index", "IndexMod"),
         period_type == "results") %>% 
  count(operatingunit, indicator, period, wt = value, name = "results") %>% 
  mutate(fill_col = ifelse(period == "FY50Q1", decline_fill, main_fill))
  
df_facet <- hts %>% 
  filter(indicator == "HTS_TST_POS",
         modality %in% c("Index", "OtherPITC"),
         prime_partner_name %in% c("Corvus", "Capricornus"),
         period_type == "results") %>% 
  mutate(prime_partner_name = case_match(prime_partner_name,
                                         "Corvus" ~ "Small Multiples Title 2",
                                         "Capricornus" ~ "Small Multiples Title 1"),
         modality = case_match(modality,
                               "Index" ~ "Item 1", 
                               "OtherPITC" ~ "Item 2")) %>% 
  count(operatingunit, prime_partner_name, indicator, modality, period, wt = value, name = "results") 
  
  
# PLOT --------------------------------------------------------------------

df_index %>% 
  ggplot(aes(period, results, fill = fill_col)) +
  geom_blank(aes(y = results * 1.1)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = comma, expand = c(.005, .005),
                     breaks = seq(0, 8000, by = 1000)) +
  scale_fill_identity() +
  labs(x = NULL, y = NULL,
       caption = "Source: FY61Q1i Faux Training MSD | 2024-03-18 | Ref id: 8b8b72ce",
       title = "Index testing falls in FY50Q1" %>% toupper,
       subtitle = glue("Despite gains <span style = 'color:{main_fill}'> in FY49</span>, the number of positive patients <br> identified through index testing fell <span style = 'color:{decline_fill}'> in FY50Q1 </span>")
       ) +
  si_style_ygrid() +
  theme(plot.subtitle = element_markdown())

  
  
# EXPORT ------------------------------------------------------------------

#create outputfolder if not created already
  dir_create("Images")

#dimenstions
  p_w <- 10
  p_h <- p_w * (3.6/5.45)

#save
  si_save("Graphics/anatomy_plot.svg", height = p_h, width = p_w)


# PLOT 2  -----------------------------------------------------------------
  
  df_facet %>% 
    ggplot(aes(period, results, color = modality, group = modality)) +
    geom_blank(aes(y = results * 1.1)) +
    # geom_col() +
    geom_line(linewidth = 1.2) +
    facet_wrap(~prime_partner_name) +
    scale_x_discrete(labels = rep("Label", 5)) +
    scale_y_continuous(labels =  rep("Label", 4),
                       expand = c(.005, .005),
                       breaks = seq(0, 900, by = 300)
    ) +
    expand_limits(y = 0) +
    # scale_fill_identity() +
    labs(x = "Axis Title (typically incorporated into title or subtitle)", 
         y = NULL, color = NULL,
         caption = "Source/Notes",
         title = "Informative Title" %>% toupper,
         subtitle = "Subtitle sentence, tends to include legend"
    ) +
    scale_color_manual(values = c(hw_electric_indigo, hw_hunter)) +
    si_style_ygrid() +
    theme(plot.subtitle = element_markdown())
  
  
  #save
  si_save("Graphics/anatomy_plot2.svg",height = 4.3, width = p_w)
  