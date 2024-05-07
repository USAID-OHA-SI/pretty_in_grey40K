## PROJECT: pretty_in_gray40k
## AUTHOR:  A.Chafetz & T.Essam | USAID
## PURPOSE: makeover plots for style guide
## DATE:    2020-10-25
## UPDATED: 2024-05-06
# REF ID:   1251f63e 


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(scales, warn.conflicts = FALSE)
library(glitr)
library(themask)
library(ggrepel)
library(systemfonts)
library(fs)
library(glue)
library(ggtext)
library(RColorBrewer)
library(svglite)
library(patchwork)

# GLOBAL VARIABLES --------------------------------------------------------

  #create outputfolder if not created already
    dir_create("Graphics")

  #reference id
  ref_id <- "1251f63e"
  
  #caption
  source <- glue("Source: FY50Q1c MSD | Ref id: {ref_id}")
  
  #default Excel color palette
    pal_excel <- c("#4E81BE", "#C1514E", "#9CBB59", "#8165A3", "#4AABC5", "#F79647")

  #achievement palette
    pal_achv <- brewer.pal(5, "Spectral")

  #Excel style
    excel_style <- function(){
      theme_minimal() +
      theme(plot.title = element_text(family = "Calibri", size = 14, hjust = .5),
            text = element_text(#family = "Calibri", 
              size = 9, color = "#595959"),
            panel.background = element_blank(),
            panel.grid.major.y = element_line(color = "#D9D9D9"),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.ticks = element_line(color = "#D9D9D9"),
            axis.line = element_line(color = "#D9D9D9"),
            legend.position = "bottom")
    }
    
    
# PARTNER MAPPING ---------------------------------------------------------

    ptnrs <- unique(cascade$prime_partner_name)
    
    set.seed(92)
    ptnrs_new <- minoria_mechs %>% 
      slice_sample(n = length(ptnrs)) %>% 
      pull()
    
    ptnrs_map <- tibble(prime_partner_name = ptnrs,
                        prime_partner = ptnrs_new)
    

# LOAD DATA ---------------------------------------------------------------

  cascade_alt <- cascade
    
  cascade_alt <- cascade_alt %>% 
    left_join(ptnrs_map, by = join_by(prime_partner_name)) %>% 
    mutate(prime_partner_name = prime_partner) %>% 
    select(-prime_partner)
    
# LINE --------------------------------------------------------------------

  sel_ptnrs <- cascade_alt %>%
    filter(indicator == "TX_NEW",
           period == "FY50Q1") %>%
    arrange(desc(value)) %>%
    slice(3:7) %>%
    pull(prime_partner_name)

  df_line <- cascade_alt %>%
    filter(indicator == "TX_NEW",
           period_type == "results",
           prime_partner_name %in% sel_ptnrs) %>%
    mutate(focus = prime_partner_name == "Jumbo Shrimp",
           label = case_when(period %in% c("FY49Q1", "FY49Q4") & focus == TRUE ~ value))
   

  write_csv(df_line, "Data_public/line.csv", na = "")

  # df_line %>%
  #   ggplot(aes(period, value, group = prime_partner_name, color = prime_partner_name)) +
  #   geom_line(linewidth = 1.2) +
  #   expand_limits(y = c(0, 1400)) +
  #   scale_color_manual(values = pal_excel, guide = "legend") +
  #   scale_y_continuous(expand = c(0, 0), breaks = seq(from = 0, to = 1400, by = 200)) +
  #   scale_x_discrete(expand = c(0, .5)) +
  #   labs(x = NULL, y = NULL, title = "Saturn Partner Trends in TX_NEW",
  #        color = NULL
  #        ) +
  #   excel_style()

  # si_save("Graphics/line_default.svg", dpi = 300, height = 4.71, width = 7.31)


  df_line %>%
    ggplot(aes(period, value, group = prime_partner_name, color = focus)) +
    geom_line(linewidth = 1) +
    geom_line(data = filter(df_line, focus == TRUE), linewidth = 1.2) +
    geom_label(aes(label = comma(label),
                   vjust = ifelse(period == "FY49Q1", 1.2, -.2)),
               label.size = 0, family = "Source Sans Pro", alpha = .6,
               na.rm = TRUE) +
    expand_limits(y = c(0, 1400)) +
    scale_color_manual(values = c("gray80", hw_electric_indigo)) +
    scale_y_continuous(expand = c(0, 0), labels = comma,
                       breaks = seq(from = 0, to = 1400, by = 200)) +
    scale_x_discrete(breaks = unique(df_line$period)[grep("Q(1|3)", unique(df_line$period))],
                     expand = c(0, .25)) +
    labs(x = NULL, y = NULL,
         title = glue("LARGE TREATMENT GAINS MADE BY<br><span style = 'color:{hw_electric_indigo};'>JUMBO SHRIMP</span> IN FY49"),
         color = NULL,
         caption = source
    ) +
    si_style_ygrid() +
    theme(plot.title = element_markdown(),
          legend.position = "none")

  si_save("Graphics/line_remake.svg", height = 4.75, width = 4.75)



# BAR ---------------------------------------------------------------------

  df_dual <- cascade_alt %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           prime_partner_name == "Bulls",
           period_type == "results") %>% 
    pivot_wider(names_from = indicator) %>% 
    mutate(positivity = HTS_TST_POS / HTS_TST,
           focus = period %in% c("FY49Q2", "FY49Q3", "FY49Q4", "FY50Q1"),
           focus_pos = case_when(focus == TRUE ~ positivity)) %>% 
    arrange(period)

  write_csv(df_dual, "Data_public/dual-bar.csv", na = "")
  
  df_dual %>%
    ggplot(aes(period, HTS_TST)) +
    geom_col(fill = pal_excel[1], na.rm = TRUE) +
    geom_point(aes(y = positivity*800000), shape = 23, fill = pal_excel[6], size = 3,
               na.rm = TRUE) +
    scale_fill_manual(values = rep(pal_excel, 3)) +
    scale_y_continuous(expand = c(.005, .005), name = "Quarterly Results",
                       sec.axis = sec_axis(~./800000, name = "Positivity", labels = percent)) +
    expand_limits(y = 1000) +
    labs(x = NULL, title = "Bulls HTS_TST and Positivity") +
    excel_style() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  si_save("Graphics/bar_default.svg", height = 4.75, width = 4.75)
  
  (v_bar <- df_dual %>% 
      ggplot(aes(period, HTS_TST, fill = focus)) +
      geom_col() +
      scale_y_continuous(expand = c(.005, .005), labels = label_number(scale_cut = cut_si(''))) +
      scale_x_discrete(breaks = unique(df_line$period)[grep("Q(1|3)", unique(df_line$period))],
                       expand = c(.005, .005))+
      scale_fill_manual(values = c("#8dbda2", hw_hunter)) + #60% tint
      labs(x = NULL, y = NULL) +
      si_style_ygrid() +
      theme(legend.position = "none")
  )
  
  (v_line <- df_dual %>% 
      ggplot(aes(period, positivity, group = prime_partner_name)) +
      geom_point(aes(color = focus)) +
      geom_path(aes(color = focus)) +
      scale_color_manual(values = c("#8dbda2", hw_hunter)) +
      scale_y_continuous(expand = c(.005, .005), label = percent_format(1)) +
      labs(x = NULL, y = NULL) +
      si_style_nolines() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank())
  )
  
  v_line / v_bar +
    plot_layout(heights = c(1, 3)) +
    plot_annotation(
      title = glue("BULLS OPTIMIZED TESTING, DECREASING TESTS<br>WHILE INCREASING POSITIVITY IN <span style = 'color:{hw_hunter};'>LAST 4 QS</span>"),
      subtitle = 'Positivity and Total Tests in Minoria',
      caption = source,
      theme = si_style()
    ) & 
    theme(plot.title = element_markdown(),
          legend.position = "none")
  
  si_save("Graphics/bar_makeover.svg",height = 4.75, width = 4.75)


# STACKED BAR -------------------------------------------------------------


    df_stack <- cascade_alt %>%
      filter(indicator == "TX_NEW",
             period_type == "results",
             # period != "FY50Q1"
             )


    ptnr_sel <- df_stack %>%
      filter(period == max(period)) %>%
      slice_max(n = 9, order_by = value) %>%
      pull(prime_partner_name)

    df_stack <- df_stack %>%
      filter(prime_partner_name %in% ptnr_sel) %>%
      mutate(partner_fill = prime_partner_name == "IronPigs")


    write_csv(df_stack, "Data_public/stacked-bar.csv", na = "")
    
    
    # orig_stack <- 
      df_stack %>%
      ggplot(aes(period, value, fill = prime_partner_name)) +
      geom_col() +
      # geom_text(aes(label = value)) +
      labs(title = "TX_NEW for Partners in Minoria")

    si_save("Graphics/stacked_default.svg", height = 4.75, width = 4.75)


    # rmk_stack <- 
      df_stack %>%
      ggplot(aes(period, value, fill = partner_fill)) +
      geom_col(width = .8) +
      geom_hline(yintercept = 0, color = light_grey) +
      facet_wrap(~factor(prime_partner_name, ptnr_sel)) +
      scale_y_continuous(label = comma) +
      scale_x_discrete(breaks = c("FY48Q1", "FY49Q1", "FY50Q1")) +
      scale_fill_manual(values = c("gray70", hw_viking)) +
      labs(x = NULL, y = NULL,
           title = glue("<span style = 'color:{hw_viking};'>IRONPIGS</span> SAW POSITIVE NEW ON TREATMENT<br>GROWTH IN Q1 WHILE OTHERS SLOWED"),
           caption =  source
             ) +
      si_style_nolines() +
      theme(legend.position = "none",
            panel.spacing = unit(1, "lines"),
            plot.title = element_markdown())

    si_save("Graphics/stacked_makeover.svg", height = 4.75, width = 4.75)
    
    # h = 4.82
    # 
    # orig_stack + plot_spacer() + rmk_stack +
    #   plot_layout(widths = unit(c(4.9306 , .25, 4.9306), 'in'), heights = unit(4.82, 'in'))
    # 
    # si_save("Graphics/stacked_duo.svg", dpi = 300, width = 10.1112, height = 4.82)
    
    
