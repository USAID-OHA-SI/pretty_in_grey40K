
library(tidyverse)
library(gt)
library(glitr)
library(extrafont)
library(extrafontdb)

images <- "Images"

tbl <- tibble::tribble(
  ~Agency,      ~Partner,    ~Indicator,       ~Q1,       ~Q2,      ~Q3,      ~Q4,    ~Total,  ~Targets, ~Achievement,
  "Agency A",      "Boötes",     "HTS_TST",      1500,      2080,     1800,     1050,      6430,     13550,         0.47,
  "Agency A",      "Boötes", "HTS_TST_POS",       330,       420,      340,      330,      1420,       640,         2.22,
  "Agency A",      "Boötes",  "Positivity",      0.22,       0.2,     0.19,     0.31,      0.22,      0.05,           NA,
  "Agency A",      "Auriga",     "HTS_TST",     61470,     57310,    44340,    28720,    191840,    151790,         1.26,
  "Agency A",      "Auriga", "HTS_TST_POS",      3230,      3270,     3050,     2180,     11730,      5020,         2.34,
  "Agency A",      "Auriga",  "Positivity",      0.05,      0.06,     0.07,     0.08,      0.06,      0.03,           NA,
  "Agency B",     "Cepheus",     "HTS_TST",      9870,     10860,    10190,    10560,     41480,     41020,         1.01,
  "Agency B",     "Cepheus", "HTS_TST_POS",       680,       770,      750,     1180,      3380,      1490,         2.27,
  "Agency B",     "Cepheus",  "Positivity",      0.07,      0.07,     0.07,     0.11,      0.08,      0.04,           NA,
  "Agency B", "Capricornus",     "HTS_TST",     31560,     36680,    30850,    29710,    128800,    106000,         1.22,
  "Agency B", "Capricornus", "HTS_TST_POS",      1970,      2420,     2320,     2700,      9410,      5150,         1.83,
  "Agency B", "Capricornus",  "Positivity",      0.06,      0.07,     0.08,     0.09,      0.07,      0.05,           NA,
  "Totals",       "All Partners",     "HTS_TST",    104400,    106930,    87180,    70040,    368550,    312360,         1.18,
  "Totals",       "All Partners", "HTS_TST_POS",      6210,      6880,     6460,     6390,     25940,     12300,         2.11,
  "Totals",       "All Partners",  "Positivity",      0.06,      0.06,     0.07,     0.09,      0.07,      0.04,           NA
)



tbl <- tbl %>% 
  gt(groupname_col = "Agency",
     rowname_col = "Partner") %>% 
  fmt_number(rows = Indicator %in% c("HTS_TST", "HTS_TST_POS"),
             columns = 4:9,
             decimals = 0) %>% 
  fmt_percent(rows = Indicator == "Positivity",
              columns = 4:9,
              decimals = 0) %>% 
  fmt_percent(columns = "Achievement",
              decimals = 0) %>% 
  tab_style(style = cell_fill(genoa_light, alpha = 0.5),
            locations = cells_body(
              columns = c(4:5, 7:8),
              rows = Indicator == "Positivity" & Partner == "Boötes"
            )) %>% 
  tab_style(style = cell_fill(old_rose_light, alpha = 0.5),
            locations = cells_body(
              columns = "Achievement",
              rows = Achievement < 1
            )) %>% 
  tab_header("OVERALL TESTING TARGETS SURPASSED IN FY49 ON SATURN") %>% 
  tab_spanner(
    label = "Period of Performance",
    columns = vars(`Q1`, `Q2`, `Q3`, `Q4`)
  ) %>% 
  fmt_missing(columns = everything(), missing_text = "-") %>% 
  tab_options(
    table.font.size = 10,
    table.font.names = "SourceSansPro-Regular",
    footnotes.font.size = 8
  ) %>% 
  tab_footnote(
    footnote = "Source: Saturn MSD downloaded on 2050-01-01",
    locations = cells_title(groups = "title")
  ) 

gtsave(tbl, file.path(images,"Style_guide_table_remake.png"))
  


