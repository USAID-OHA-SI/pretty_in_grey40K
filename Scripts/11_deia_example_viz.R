# PROJECT:  pretty_in_grey40K
# PURPOSE:  DEIA viz
# AUTHOR:   A.Chafetz | USAID
# REF ID:   7b33df49 
# LICENSE:  MIT
# DATE:     2024-10-24
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)
  library(glue)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "7b33df49"
  
  path_msk <- si_path() %>% 
    return_latest("TRAINING")
  
  meta <- get_metadata(path_msk)
  
  df_msk <- read_psd(path_msk) 
    
  
  # IMPORT ------------------------------------------------------------------
  
  #https://data.who.int/countries/834 
  #Tanzania 2019
  df_cod <- tibble::tribble(
    ~cause, ~deaths_per100k,
    "Tuberculosis",            54.1,
    "HIV/AIDS",            50.3,
    "Lower respiratory infections",            45.2,
    "Malaria",            40.3,
    "Diarrhoeal diseases",            31.6,
    "Stroke",            29.5,
    "Preterm birth complications",            28.4,
    "Ischaemic heart disease",            26.8,
    "Birth asphyxia and birth trauma",            22.6,
    "Road injury",            18.7
  )
  
  df_hale <- tibble::tribble(
    ~year,     ~sex,       ~hale,
    2000L, "FEMALE",    45.83544,
    2000L,   "MALE",    45.01416,
    2000L,  "TOTAL",     45.4368,
    2010L, "FEMALE",    53.70932,
    2010L,   "MALE",    52.93148,
    2010L,  "TOTAL",    53.35963,
    2015L, "FEMALE",    57.57885,
    2015L,   "MALE",    55.86159,
    2015L,  "TOTAL",    56.74443,
    2019L, "FEMALE",    59.25729,
    2019L,   "MALE",    57.62463,
    2019L,  "TOTAL",    58.46004,
    2021L, "FEMALE", 58.30716705,
    2021L,   "MALE", 57.86711884,
    2021L,  "TOTAL", 58.07286835
  )
  
  
  
  # MUNGE -------------------------------------------------------------------
  
  df_cod <- df_cod %>% 
    mutate(deaths_per100k = round(deaths_per100k))
  
  df_hale <- df_hale %>%
    mutate(sex = str_to_sentence(sex),
           year = year + 40,
           pt = case_when(year == min(year) | year == max(year) ~ hale),
           pt_lab = case_when(!is.na(pt) ~ glue("{round(hale)} yrs")))
  
  df_hts <- df_msk %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate != "Total Numerator",
           cumulative != 0,
           fiscal_year == 2061) %>% 
    mutate(grp_broad = case_when(standardizeddisaggregate == "KeyPop/Result" ~ "KeyPop",
                                 trendscoarse == "<15" ~ "Peds",
                                 TRUE ~ sex),
           grp_sub = case_when(standardizeddisaggregate == "KeyPop/Result" ~ otherdisaggregate_sub,
                               TRUE ~ ageasentered),
           fill_color = case_match(grp_broad,
                                   "Female" ~ lavender_haze,
                                   "Male" ~ hunter,
                                   "Peds" ~ viking,
                                   .default = slate)) %>% 
    filter(grp_sub != "<10")
  
  df_hts_grp <- df_hts %>% 
    count(fiscal_year, indicator, grp_broad, fill_color, wt = cumulative, name = "value") %>%
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    mutate(positivity = hts_tst_pos/hts_tst)
  
  
  df_hts_sub <- df_hts %>% 
    count(fiscal_year, indicator, grp_broad, grp_sub, fill_color, wt = cumulative, name = "value") %>%
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    mutate(positivity = hts_tst_pos/hts_tst)
  
  # VIZ ---------------------------------------------------------------------
  
  df_cod %>% 
    ggplot(aes(deaths_per100k, fct_reorder(cause, deaths_per100k))) + 
    geom_col() +
    geom_vline(xintercept = c(seq(0, 55, 5)), color = "white") +
    scale_x_continuous(expand = c(0.005, .005)) +
    labs(x = NULL, y = NULL,
         title = "TB was the leading cause of deaths in Minoria in 2019" %>% toupper,
         subtitle = "Deaths per 100k population",
         caption = glue("Source: WHO | Ref id: {ref_id}")) +
    si_style_nolines() +
    theme(axis.text.x = element_blank())
  
  si_preview(height = 3.1, width = 4.6)
  si_save("../Downloads/COD.svg", height = 3.1, width = 4.6)
  
  si_preview()
  
  df_hale %>% 
    ggplot(aes(year, hale, color = sex)) +
    geom_step() +
    geom_point(aes(y = pt), na.rm = TRUE) +
    # geom_label(aes(label = pt_lab), family = "Source Sans Pro") +
    facet_wrap(~sex) +
    scale_color_manual(values = c("Female" = "pink",
                                  "Male" = "#8ECEFD",
                                  "Total" = slate)) +
    scale_x_continuous(breaks = c(2040, 2060)) +
    labs(x = NULL, y = NULL,
         title = "LIFE EXPECTENCY FELL FOR <span style = 'color:pink;'>FEMALES</span> IN 2061",
         subtitle = "Heath Life expectency at birth | Minoria",
         caption = glue("Source: WHO | Ref id: {ref_id}")) +
    coord_cartesian(clip = "off") +
    si_style_nolines() +
    theme(legend.position = "none",
          strip.text = element_text(hjust = .5),
          title = element_markdown())
  
  si_preview(height = 3.1, width = 4.6)
  si_save("../Downloads/hale.png", height = 3.1, width = 4.6)
  
  
  df_hale %>% 
    ggplot(aes(year, hale, color = sex)) +
    geom_step() +
    geom_point(aes(y = pt), na.rm = TRUE) +
    # geom_label(aes(label = pt_lab), family = "Source Sans Pro") +
    facet_wrap(~sex) +
    scale_color_manual(values = c("Female" = lavender_haze,
                                  "Male" = hunter,
                                  "Total" = slate)) +
    scale_x_continuous(breaks = c(2040, 2060)) +
    labs(x = NULL, y = NULL,
         title = glue("LIFE EXPECTENCY FELL FOR <span style = 'color:{lavender_haze};'>FEMALES</span> IN 2061"),
         subtitle = "Heath Life expectency at birth | Minoria",
         caption = glue("Source: WHO | Ref id: {ref_id}")) +
    coord_cartesian(clip = "off") +
    si_style_nolines() +
    theme(legend.position = "none",
          strip.text = element_text(hjust = .5),
          title = element_markdown())
  
  si_save("../Downloads/hale_fixed.png", height = 3.1, width = 4.6)
  
  
  df_hts_grp %>% 
    ggplot(aes(positivity, fct_reorder(grp_broad, positivity), fill = fill_color)) +
    geom_col() +
    geom_text(aes(label = percent(positivity, .1)), 
              family = "Source Sans Pro", color = matterhorn,
              nudge_x = .0015) +
    scale_fill_identity() +
    scale_x_continuous(labels = percent, expand = c(0.0005, .0005)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = glue("POSITIVITY AMONGST <span style = 'color:{hunter};'>MALES</span> IS NEARLY DOUBLE <br>THAT OF <span style = 'color:{lavender_haze};'>FEMALES</span>"),
         subtitle = "HIV Positivity | Minoria 2061",
         caption = meta$caption
    ) +
    si_style_xgrid() +
    theme(plot.title = element_markdown(),
          axis.text.x = element_blank())
  
  # si_preview(height = 3.1, width = 4.6) 
  
  si_save("../Downloads/agg.png", height = 3.1, width = 4.6)
  
  df_hts_sub %>% 
    ggplot(aes(positivity, fct_reorder(grp_broad, positivity), color = fill_color)) +
    geom_point(alpha = .6, position = position_jitter(height = .2, seed = 42)) +
    geom_point(data = df_hts_grp, size = 5, alpha = .8) +
    geom_text(data = df_hts_grp,
              aes(label = percent(positivity, .1)), 
              family = "Source Sans Pro", color = matterhorn,
              nudge_y = -.35) +
    # geom_text(aes(label = grp_sub), 
    #           family = "Source Sans Pro", color = matterhorn, alpha = .5,
    #           position = position_jitter(height = .2, seed = 42),
    #           vjust = -.2
    #           ) +
    scale_color_identity() +
    scale_x_continuous(labels = percent, expand = c(0.0005, .0005)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = glue("POSITIVITY AMONGST <span style = 'color:{hunter};'>MALES</span> IS NEARLY DOUBLE <br>THAT OF <span style = 'color:{lavender_haze};'>FEMALES</span>"), 
         subtitle = "HIV Positivity | Minoria 2061",
         caption = meta$caption
    ) +
    si_style_xgrid() +
    theme(plot.title = element_markdown(),
          axis.text.x = element_blank())
  
  si_preview(height = 3.1, width = 4.6)  
  
  si_save("../Downloads/variation.png", height = 3.1, width = 4.6)
  
  