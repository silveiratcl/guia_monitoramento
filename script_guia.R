library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)



# Create Sample data 
# saco_do_capim,
# saco_do_batismo,
# baia_das_tartarugas,
# engenho,
# farol  

# monitoring
df_monit = read_delim("data/dados_monitoramento_cs_2025-04-30.csv",
                      col_types = list(localidade = col_character(),
                                       data = col_date(format = "%d/%m/%Y"),
                                       visib_horiz = col_double(),
                                       faixa_bat = col_character(),
                                       prof_min = col_double(),
                                       prof_max = col_double(),
                                       metodo = col_character(),
                                       observer = col_character(),
                                       n_divers = col_double(),
                                       tempo_censo = col_double(),
                                       dafor = col_double(),
                                       iar_medio = col_double(),
                                       n_trans_vis = col_double(),
                                       n_trans_pres = col_double(),
                                       dafor_id = col_double(),
                                       geo_id = col_character(),
                                       obs = col_character(), 
                                       id_horus = col_double()
                      ))





df_guia <- df_monit |> 
  filter(localidade %in% c("saco_do_capim",
                           "saco_do_batismo",
                           "baia_das_tartarugas",
                           "engenho",
                           "farol",
                           "vidal"), 
         obs != "estimado dos dados do ICMBio")



table(df_guia$localidade)

write_csv(df_guia, "guia/data_guia.csv")



# Creating a same effort data set based on the original data with unnequal effort
# keep just data with at least 30 minutes of monitoring (rows) and then sample 
# exactly 30 rows (minutes) within each dafor_id (randomly)

set.seed(123)

# A) keep only events (localidade + dafor_id) with >= 30 minutes(rows)
valid_events <- df_guia %>%
  count(localidade, dafor_id, name = "n_min") %>%
  filter(n_min >= 30)

df_filtered <- df_guia %>%
  semi_join(valid_events, by = c("localidade", "dafor_id"))

# B) sample exactly 30 minutes per event
df_30min <- df_filtered %>%
  group_by(localidade, dafor_id) %>%
  slice_sample(n = 30, replace = FALSE) %>%
  ungroup()

# C) equalize number of events per localidade
min_events <- df_30min %>%
  distinct(localidade, dafor_id) %>%
  count(localidade) %>%
  summarise(min(n)) %>%
  pull()

sampled_events <- df_30min %>%
  distinct(localidade, dafor_id) %>%
  group_by(localidade) %>%
  slice_sample(n = min_events) %>%
  ungroup()

# D) final balanced dataset (TRULY balanced by minutes)
df_guia_balanced <- df_30min %>%
  semi_join(sampled_events, by = c("localidade", "dafor_id"))



# each localidade should have exactly the same number of rows (= minutes)
df_guia_balanced %>% count(localidade)

# if you want the exact minutes:
df_guia_balanced %>% count(localidade) %>% mutate(minutes = n)


df_guia_balanced %>%
  count(localidade, dafor_id) %>%
  summarise(
    min_minutes = min(n),
    max_minutes = max(n)
  )

df_guia_balanced %>%
  distinct(localidade, dafor_id) %>%
  count(localidade) %>%
  summarise(
    min_events = min(n),
    max_events = max(n)
  )

df_guia_balanced %>%
  count(localidade) %>%
  summarise(
    min_total = min(n),
    max_total = max(n)
  )



################################################################################
### BALANCED DATA SET
################################################################################

data <- df_guia_balanced |>
  mutate(
    localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
    year = lubridate::year(data)
  )

if (nrow(data) == 0) stop("No data found")

density_data <- data |>
  group_by(year) |>
  mutate(
    n_trans_count = n(),
    total_dafor = sum(dafor, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(year) |>
  mutate(
    year_label = paste0(year, " (n=", n_trans_count, ")"),
    year_label = factor(year_label, levels = unique(year_label))
  )

sum(is.na(density_data$dafor))

################################################################################
### Stacked DAFOR by LOCALIDADE (ORDERED)
### effort = minutes (row counts), bars stacked by DAFOR
################################################################################

data_loc <- density_data |>
  mutate(
    dafor_cat = case_when(
      dafor == 10 ~ "D",
      dafor == 8  ~ "A",
      dafor == 6  ~ "F",
      dafor == 4  ~ "O",
      dafor == 2  ~ "R",
      dafor == 0  ~ "Ausente",
      TRUE        ~ NA_character_
    )
  )

# Minutes = number of rows (each row is one minute in your balanced data)
cats_loc <- data_loc |>
  filter(!is.na(dafor_cat)) |>
  count(localidade, dafor_cat, name = "minutes") |>
  complete(localidade, dafor_cat = c("D","A","F","O","R","Ausente"),
           fill = list(minutes = 0)) |>
  mutate(dafor_cat = factor(dafor_cat, levels = c("D","A","F","O","R","Ausente")))

# Order by sum of DAFOR (highest on top)
loc_order <- data_loc |>
  group_by(localidade) |>
  summarise(total_dafor = sum(dafor, na.rm = TRUE), .groups = "drop") |>
  arrange(total_dafor) |>   # ascending so highest appears on TOP after coord_flip()
  pull(localidade)

cats_loc <- cats_loc |>
  mutate(localidade = factor(localidade, levels = loc_order))

stacked_dafor_localidade_balanced <- ggplot(cats_loc,
                                   aes(x = localidade, y = minutes, fill = dafor_cat)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Esforço (minutos de monitoramento)",
    fill = ""
  ) +
  scale_fill_viridis_d(option = "plasma", begin = 0.9, end = 0.1) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.key.size = unit(.9, "cm")
  )

stacked_dafor_localidade_balanced

ggsave("plots/stacked_dafor_localidade_balanced.png",
       stacked_dafor_localidade,
       width = 12, height = 6, dpi = 300)







### UMBALANCED DATA ###

##############################
### 1. Prepare and check data
##############################


data <- df_guia |> 
  mutate(
    localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
    year = year(data)
  ) 

# Check if data exists

if(nrow(data) == 0) stop("No data found")

density_data <- data  |> 
  group_by(year) |> 
  mutate(
    n_trans_count = n(),
    total_dafor = sum(dafor, na.rm = TRUE)
  ) |> 
  ungroup()  |> 
  arrange(year) |> 
  mutate(
    year_label = paste0(year, " (n=", n_trans_count, ")"),
    year_label = factor(year_label, levels = unique(year_label))
  )

sum(is.na(density_data$dafor))






################################################################################
### Stacked DAFOR by LOCALIDADE (ORDERED)
### effort = sum(n_trans_vis), bars stacked by DAFOR
################################################################################



# 1) Prepare
data_loc <- density_data  |> 
  mutate(
    dafor_cat = case_when(
      dafor == 10 ~ "D",
      dafor == 8  ~ "A",
      dafor == 6  ~ "F",
      dafor == 4  ~ "O",
      dafor == 2  ~ "R",
      dafor == 0  ~ "Ausente",
      TRUE        ~ NA_character_
    ),
    n_trans_vis = dplyr::coalesce(n_trans_vis, 1)
  )

# 2) Category totals (stacked parts), weighted by n_trans_vis
cats_loc <- data_loc %>%
  filter(!is.na(dafor_cat)) %>%
  group_by(dafor_id, localidade, dafor_cat) %>%
  summarise(n_trans_eff = max(n_trans_vis, na.rm = TRUE), .groups = "drop") %>%
  distinct(dafor_id, localidade, dafor_cat, n_trans_eff) %>%
  group_by(localidade, dafor_cat) %>%
  summarise(count = sum(n_trans_eff, na.rm = TRUE), .groups = "drop") %>%
  complete(localidade, dafor_cat = c("D","A","F","O","R", "Ausente"), fill = list(count = 0)) %>%
  mutate(dafor_cat = factor(dafor_cat, levels = c("D","A","F","O","R", "Ausente")))

# 3) ORDER by total visual transects (sum of n_trans_vis)
loc_order <- cats_loc  |> 
  group_by(localidade)  |> 
  summarise(total = sum(count), .groups = "drop")  |> 
  arrange(total) |>  
  pull(localidade)

cats_loc <- cats_loc |> 
  mutate(localidade = factor(localidade, levels = loc_order))

# 4) Plot (horizontal)
stacked_dafor_localidade <- ggplot(cats_loc, 
                                   aes(x = localidade, 
                                       y = count, 
                                       fill = dafor_cat)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Esforço (soma de minutos de monitoramento)",
    fill = ""
  ) +
  scale_fill_viridis_d(option = "plasma", begin = 0.9, end = 0.1) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.key.size = unit(.9, "cm")
  )

stacked_dafor_localidade

ggsave("plots/stacked_dafor_localidade.png",
       stacked_dafor_localidade,
       width = 12, height = 6, dpi = 300)




