## PROJECT: pretty_in_gray40k
## AUTHOR:  A.Chafetz & T.Essam | USAID
## PURPOSE: anatomy of a plot
## DATE:    2020-11-03
## UPDATED: 


# DEPENDENCIES ------------------------------------------------------------

library(fs)
library(tidyverse)
library(scales)
library(glitr)
library(extrafont)
library(glue)
library(ggtext)
library(svglite)


# MUNGE DATA --------------------------------------------------------------

df_index <- hts %>% 
  filter(indicator == "HTS_TST_POS",
         modality %in% c("Index", "IndexMod"),
         period_type == "results") %>% 
  count(operatingunit, indicator, period, wt = value, name = "results") %>% 
  mutate(fill_col = ifelse(period == "FY50Q1", "#e07653", "#1e87a5"))
  
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
       caption = "Source: FY50Q1 MSD",
       title = "DESPITE <span style = 'color:#1e87a5'>GAINS IN FY49</span>, THE NUMBER OF POSITIVE PATIENTS <br>IDENTIFIED THROUGH INDEX TESTING <span style = 'color:#e07653'>FELL IN FY50Q1</span> "
       ) +
  si_style_ygrid() +
  theme(plot.title = element_markdown())


# EXPORT ------------------------------------------------------------------

#create outputfolder if not created already
  dir_create("Images")

#dimenstions
  p_w <- 10
  p_h <- p_w * (3.6/5.45)

#save
  # ggsave("Images/anatomy_plot.png", dpi = 300, height = p_h, width = p_w)
  ggsave("Images/anatomy_plot.svg", dpi = 300, height = p_h, width = p_w)

si_save()