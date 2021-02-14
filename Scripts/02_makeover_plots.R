## PROJECT: pretty_in_gray40k
## AUTHOR:  A.Chafetz & T.Essam | USAID
## PURPOSE: makeover plots for style guide
## DATE:    2020-10-25
## UPDATED: 2020-02-14


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

# GLOBAL VARIABLES --------------------------------------------------------

  #create outputfolder if not created already
    dir_create("Images")

  #default Excel color palette
    pal_excel <- c("#4E81BE", "#C1514E", "#9CBB59", "#8165A3", "#4AABC5", "#F79647")

  #achievement palette
    pal_achv <- brewer.pal(5, "Spectral")

  #Excel style
    excel_style <- function(){
      theme_minimal() +
      theme(text = element_text(family = "Calibri", size = 9, color = "#595959"),
            plot.title = element_text(family = "Calibri", size = 14, hjust = .5),
            panel.background = element_blank(),
            panel.grid.major.y = element_line(color = "#D9D9D9"),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.ticks = element_line(color = "#D9D9D9"),
            axis.line = element_line(color = "#D9D9D9"),
            legend.position = "bottom")
    }
# LINE --------------------------------------------------------------------

#
#   glimpse(cascade)
#
#   cascade %>%
#     filter(indicator == "TX_NEW",
#            period_type == "results",
#     ) %>%
#     mutate(label = case_when(period == max(period) ~ primepartner)) %>%
#     ggplot(aes(period, value, group = primepartner, color = primepartner)) +
#     geom_line() +
#     geom_label_repel(aes(label = label))

  sel_ptnrs <- cascade %>%
    filter(indicator == "TX_NEW",
           period == "FY50Q1") %>%
    arrange(desc(value)) %>%
    slice(n = 3:7) %>%
    pull(primepartner)

  df_line <- cascade %>%
    filter(indicator == "TX_NEW",
           period_type == "results",
           primepartner %in% sel_ptnrs) %>%
    mutate(focus = primepartner == "Ursa Major",
           label = case_when(period %in% c("FY49Q1", "FY49Q4") & focus == TRUE ~ value))

  write_csv(df_line, "../../../Downloads/default_line.csv", na = "")

  # df_line %>%
  #   ggplot(aes(period, value, group = primepartner, color = primepartner)) +
  #   geom_line(size = 1.2) +
  #   expand_limits(y = c(0, 1400)) +
  #   scale_color_manual(values = pal_excel) +
  #   scale_y_continuous(expand = c(0, 0), breaks = seq(from = 0, to = 1400, by = 200)) +
  #   scale_x_discrete(expand = c(0, .5)) +
  #   labs(x = NULL, y = NULL, title = "Saturn Parnter Trends in TX_NEW",
  #        color = NULL
  #        ) +
  #   excel_style()
  #
  #
  # # ggsave("Images/line_default.png", dpi = 300, height = 4.71, width = 7.31)
  # ggsave("Images/line_default.svg", dpi = 300, height = 4.71, width = 7.31)


  df_line %>%
    ggplot(aes(period, value, group = primepartner, color = focus)) +
    geom_line(size = 1) +
    geom_line(data = filter(df_line, focus == TRUE), size = 1.2) +
    geom_label(aes(label = comma(label),
                   vjust = ifelse(period == "FY49Q1", 1.2, -.2)),
               label.size = 0, family = "Source Sans Pro", alpha = .6,
               na.rm = TRUE) +
    expand_limits(y = c(0, 1400)) +
    scale_color_manual(values = c("gray80", usaid_medblue)) +
    scale_y_continuous(expand = c(0, 0), labels = comma,
                       breaks = seq(from = 0, to = 1400, by = 200)) +
    scale_x_discrete(expand = c(0, .25)) +
    labs(x = NULL, y = NULL,
         title = "LARGE TREATMENT GAINS MADE BY<br><span style = 'color:#0067B9;'>URSA MAJOR</span> IN FY49",
         color = NULL,
         caption = glue("Source: {max(cascade$period)} MSD")
    ) +
    si_style_ygrid() +
    theme(plot.title = element_markdown(),
          legend.position = "none")

  # ggsave("Images/line_remake.png", dpi = 300, height = 4.71, width = 7.31)
  ggsave("Images/line_remake.svg", dpi = 300, height = 4.82, width = 4.9306)



# BAR ---------------------------------------------------------------------


  glimpse(hts)

  hts %>%
    filter(indicator == "HTS_TST",
           period_type == "targets") %>%
    count(primepartner, wt = value, sort = TRUE)

  df_dual <- cascade %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           primepartner == "Auriga",
           period_type == "results") %>% 
    pivot_wider(names_from = indicator) %>% 
    mutate(positivity = HTS_TST_POS / HTS_TST,
           focus = period %in% c("FY49Q2", "FY49Q3", "FY49Q4", "FY50Q1"),
           focus_pos = case_when(focus == TRUE ~ positivity)) %>% 
    arrange(period)
  
  
  df_dual %>%
    ggplot(aes(period, HTS_TST)) +
    geom_col(fill = pal_excel[1], na.rm = TRUE) +
    geom_point(aes(y = positivity*800000), shape = 23, fill = pal_excel[6], size = 3,
               na.rm = TRUE) +
    scale_fill_manual(values = rep(pal_excel, 3)) +
    scale_y_continuous(expand = c(.005, .005), name = "Quarterly Results",
                       sec.axis = sec_axis(~./800000, name = "Positivity", labels = percent)) +
    expand_limits(y = 1000) +
    labs(x = NULL, title = "Auriga HTS_TST and Positivity") +
    excel_style() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("Images/bar_default.svg", dpi = 300, height = 4.82, width = 4.9306)
  
  (v_bar <- df_dual %>% 
      ggplot(aes(period, HTS_TST, fill = focus)) +
      geom_col() +
      scale_y_continuous(expand = c(.005, .005), labels = comma) +
      scale_x_discrete(breaks = c("FY48Q1", "FY48Q3", "FY49Q1", "FY49Q3", "FY51Q1"))+
      scale_fill_manual(values = c(moody_blue_light, moody_blue)) +
      labs(x = NULL, y = NULL) +
      si_style_ygrid() +
      theme(legend.position = "none")
  )
  
  (v_line <- df_dual %>% 
      ggplot(aes(period, positivity, group = primepartner)) +
      geom_point(aes(color = focus)) +
      geom_path(aes(color = focus)) +
      scale_color_manual(values = c(trolley_grey, moody_blue)) +
      scale_y_continuous(expand = c(.005, .005), label = percent_format(1)) +
      labs(x = NULL, y = NULL) +
      si_style_xline() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank())
  )
  
  v_line / v_bar +
    plot_layout(heights = c(1, 3)) +
    plot_annotation(
      title = "AURIGA OPTIMIZING TESTING TO DECREASE TOTAL TESTS<br>WHILE INCREASING POSITIVITY IN <span style = 'color:#8980cb;'>LAST 4 QUARTERS</span>",
      subtitle = 'Positivity and Total Tests in Saturn',
      caption = 'Source: FY50Q1 MSD'
    ) & 
    si_style_ygrid() & 
    theme(plot.title = element_markdown(),
          legend.position = "none")
  
  ggsave("Images/bar_makeover.svg", dpi = 300, height = 4.82, width = 4.9306)
  
  

# STACKED BAR -------------------------------------------------------------


    glimpse(cascade)


    df_stack <- cascade %>%
      filter(indicator == "TX_NEW",
             period_type == "results",
             period != "FY50Q1")


    ptnr_sel <- df_stack %>%
      filter(period == max(period)) %>%
      slice_max(n = 9, order_by = value) %>%
      pull(primepartner)

    df_stack <- df_stack %>%
      filter(primepartner %in% ptnr_sel) %>%
      mutate(partner_fill = primepartner == "Libra")


    orig_stack <- df_stack %>%
      ggplot(aes(period, value, fill = primepartner)) +
      geom_col() +
      # geom_text(aes(label = value)) +
      # scale_fill_manual(values = rep(pal_excel, 2), name = NULL) +
      # excel_style() +
      labs(title = "TX_NEW in Saturn Partners")

    # ggsave("Images/stacked_default.png", dpi = 300, height = 4.71, width = 7.31)
    ggsave("Images/stacked_default.svg", dpi = 300, height = 4.82, width = 4.9306)


    rmk_stack <- df_stack %>%
      ggplot(aes(period, value, fill = partner_fill)) +
      geom_col() +
      geom_hline(yintercept = 0) +
      facet_wrap(~factor(primepartner, ptnr_sel)) +
      scale_y_continuous(label = comma) +
      scale_x_discrete(breaks = c("FY48Q1", "FY49Q1", "FY50Q1")) +
      scale_fill_manual(values = c("gray70", scooter)) +
      labs(x = NULL, y = NULL,
           title = "<span style = 'color:#1e87a5;'>LIBRA </span> SAW POSITIVE TX_NEW GROWTH IN Q1<br> WHILE OTHERS FALTERED",
           caption =  glue("Source: {max(hts$period)} MSD")
             ) +
      si_style_nolines() +
      theme(legend.position = "none",
            panel.spacing = unit(1, "lines"),
            plot.title = element_markdown())

    # ggsave("Images/stacked_makeover.png", dpi = 300, height = 4.71, width = 7.31)
    ggsave("Images/stacked_makeover.svg", dpi = 300,  height = 4.82, width = 4.9306)
    
    h = 4.82
    
    orig_stack + plot_spacer() + rmk_stack +
      plot_layout(widths = unit(c(4.9306 , .25, 4.9306), 'in'), heights = unit(4.82, 'in'))
    
    ggsave("Images/stacked_duo.svg", dpi = 300, width = 10.1112, height = 4.82)
    
    
