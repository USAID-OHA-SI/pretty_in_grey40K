# Generate markdown palettes for webpage
# Tim Essam
# 2020-12-01
# Notes: For use in wiki page

# 

# GLOBALS -----------------------------------------------------------------

  library(tidyverse)
  library(glitr)
  library(glamr)
  library(patchwork)
  library(purrr)
  
  pals <- c("denims", "old_roses", "moody_blues", "burnt_siennas", 
            "scooters", "golden_sands", "genoas", "trolley_greys")

  graphics <- "Graphics"

# Generate palettes with hex codes ----------------------------------------

  pal_df <-
    pals %>% 
    set_names() %>% 
    map_df(~si_rampr(.x, n = 11)) %>% 
    pivot_longer(cols = everything(), 
                 names_to = "pal_name",
                 values_to = "hex") %>% 
    arrange(match(pal_name, (pals))) %>% 
    group_by(pal_name) %>% 
    mutate(col = row_number()) %>% 
    ungroup() %>% 
    mutate(pal_name = fct_inorder(pal_name, ordered = TRUE), 
           row = as.numeric(pal_name), 
           text_color = if_else(col %in% (6:11), grey10k, "black")) 


  # GGenerate plot to clean up in AI
  pal_df %>% 
    ggplot(aes(x = col, y = rev(row))) +
    geom_tile(aes(fill = hex), color = "white") +
    geom_text(aes(label = hex, colour = text_color, size = 10)) +
    scale_fill_identity() +
    scale_color_identity() +
    facet_wrap(~(fct_reorder(str_to_upper(pal_name), row)), ncol = 1, scales = "free_y") +
    theme_void() +
    theme(strip.text = element_text(face="bold", size = 12, hjust = 0.05),
          legend.position = "none")

  ggsave(file.path(graphics, "SIEI_color_palettes_continuous.png"),
         height = 8.5,
         width = 11, 
         scale = 1,
         dpi = "retina")

  # Generate graphic show components of a plot - title, caption, annotation, gridlines, axes and source
  aop_df <- tribble(
    ~chart_part, ~hex,
    "title",       "#202020",
    "subtitle",    "#505050",
    "annotation",  "#505050",
    "axes",        "#505050",
    "source",      "#909090",
    "gridline",    "#D3D3D3",
  ) %>% 
    mutate(row = 1, 
           col = row_number(),
           text_color = if_else(chart_part == "title", grey10k, grey90k),
           part_order = paste0(chart_part, "\n", hex) %>% fct_reorder(., col)
           )
  
  aop_df %>% 
    ggplot(aes(x = col, y = row)) +
    geom_tile(aes(fill = hex), color = "white") +
    facet_wrap(~part_order, scales = "free", nrow = 1) +
    #geom_text(aes(label = paste0(chart_part, "\n", hex), colour = text_color), size = 10) +
    scale_fill_identity() +
    scale_color_identity() +
    si_style_void()
  
  ggsave(file.path(graphics, "SIEI_color_anatomy_of_plot.png"),
         height =2,
         width = 11, 
         scale = 1,
         dpi = "retina")


