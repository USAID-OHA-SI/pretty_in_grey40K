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
  
  pals <- c("royals", "old_roses", "moody_blues", "burnt_siennas", 
            "scooters", "golden_sands", "genoas")

  graphics <- "Graphics"


# Generate palettes with hex codes ----------------------------------------

  pal_df <-
    pals %>% 
    set_names() %>% 
    map_df(~si_rampr(.x)) %>% 
    pivot_longer(cols = everything(), 
                 names_to = "pal_name",
                 values_to = "hex") %>% 
    arrange(match(pal_name, (pals))) %>% 
    group_by(pal_name) %>% 
    mutate(col = row_number()) %>% 
    ungroup() %>% 
    mutate(pal_name = fct_inorder(pal_name, ordered = TRUE), 
           row = as.numeric(pal_name), 
           text_color = if_else(col %in% (7:11), "#000000", grey10k)) 


  # GGenerate plot to clean up in AI
  pal_df %>% 
    ggplot(aes(x = col, y = rev(row))) +
    geom_tile(aes(fill = hex), color = "white") +
    geom_text(aes(label = hex, colour = text_color, size = 10)) +
    scale_fill_identity() +
    scale_color_identity() +
    facet_wrap(~str_to_upper(pal_name), ncol = 1, scales = "free_y") +
    theme_void() +
    theme(strip.text = element_text(face="bold", size = 12, hjust = 0.05),
          legend.position = "none")

  ggsave(file.path(graphics, "SIEI_color_palettes_continuous.png"),
         height = 8.5,
         width = 11, 
         scale = 1,
         dpi = "retina")



