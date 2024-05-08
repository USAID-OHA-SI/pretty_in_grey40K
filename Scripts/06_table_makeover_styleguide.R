## PROJECT: pretty_in_gray40k
## AUTHOR:  A.Chafetz & T.Essam | USAID
## PURPOSE: makeover table for style guide
## DATE:    2021-02-16
## UPDATED: 2024-05-07
## REF ID:  7a608beb 

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(glitr)
  library(themask)
  library(systemfonts)
  library(gt)
  library(gtExtras)

# GLOBAL VARIABLES --------------------------------------------------------

  #reference id
  ref_id <- "7a608beb"
  
  #caption
  source <- str_glue("Source: FY50Q1c MSD | Ref id: {ref_id}")

# IMPORT ------------------------------------------------------------------

  tbl_raw <- read_csv("Data_public/table.csv")

# PARTNER MAPPING ---------------------------------------------------------
  
  ptnrs <- unique(cascade$prime_partner_name)
  
  set.seed(92)
  ptnrs_new <- minoria_mechs %>% 
    slice_sample(n = length(ptnrs)) %>% 
    pull()
  
  ptnrs_map <- tibble(prime_partner_name = ptnrs,
                      prime_partner = ptnrs_new) %>% 
    mutate(prime_partner_name = ifelse(prime_partner_name == "Boötes", "Bootes", prime_partner_name))
  
  
  tbl_adj <- tbl_raw %>% 
    left_join(ptnrs_map, by = join_by(Partner == prime_partner_name)) %>% 
    mutate(Partner = case_when(Partner == "All Partners" ~ Partner,
                               TRUE ~ prime_partner) )%>% 
    select(-prime_partner)

# REGION MAPPING ----------------------------------------------------------

  tbl_adj <- tbl_adj %>% 
    mutate(Region = case_match(Agency,
                               "Agency A" ~ "Eugene",
                               "Agency B" ~ "Greensboro",
                               .default = Agency),
           .before = 1) %>%
    select(-Agency)
  
# TABLE FUNCTION ----------------------------------------------------------

  drkn_clmn_hdr <- function(.data){
    .data %>%
      tab_style(
        style = list(
          cell_text(color = grey90k)
        ),
        locations = cells_column_labels(
        )
      )
  }
# BUILD TABLE -------------------------------------------------------------

  tbl <- tbl_adj %>%
    gt(groupname_col = "Region",
       rowname_col = "Partner") %>% 
    fmt_number(rows = Indicator %in% c("HTS_TST", "HTS_TST_POS"),
               columns = 4:9,
               decimals = 0) %>% 
    fmt_percent(rows = Indicator == "Positivity",
                columns = 4:9,
                decimals = 0) %>% 
    fmt_percent(columns = "Achievement",
                decimals = 0) %>% 
    tab_style(style = cell_fill("#697ebc", alpha = 0.5), #hw_electric_indigo @ 80 
              locations = cells_body(
                columns = c(4:5, 7:8),
                rows = Indicator == "Positivity" & Partner == "Shuckers"
              )) %>% 
    tab_style(style = cell_fill("#f8a27e", alpha = 0.5), #hw_tango @ 60%
              locations = cells_body(
                columns = "Achievement",
                rows = Achievement < 1
              )) %>% 
    tab_header("OVERALL TESTING TARGETS SURPASSED IN FY49 ON MINORIA") %>% 
    tab_spanner(
      label = "Period of Performance",
      columns = c(`Q1`, `Q2`, `Q3`, `Q4`)
    ) %>% 
    sub_missing(columns = everything(), missing_text = "-") %>% 
    tab_options(
      table.font.size = 10,
      table.font.names = "Source Sans Pro",
      source_notes.font.size = 8
    ) %>%
    # gt_theme_nytimes() %>%
    # tab_options(
    #   table.font.size = px(10),
    #   table.font.names = "Source Sans Pro",
    #   row_group.font.weight = "bold",
    #   data_row.padding = px(0.5),
    #   column_labels.font.size = px(12),
    #   row_group.padding = px(1)) %>%
    # drkn_clmn_hdr() %>% 
    tab_source_note(source)

  tbl
  
# EXPORT ------------------------------------------------------------------

  gtsave(tbl, "Images/table_remake.png")
  
