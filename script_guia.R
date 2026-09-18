library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggmap)
library(forcats)
library(viridisLite)



# monitoring
df_guia <- read_delim("dados/data_guia.csv")

df_manejo <-  read_delim("dados/dados_manejo.csv")

df_guia
df_manejo




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
    year = lubridate::year(.data$data)
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



# Generate plasma colors
#plasma_cols <- viridisLite::plasma(
 
# n = length(unique(cats_loc$dafor_cat)),
 # begin = 0.9,
#  end = 0.1
#)

# Replace the lowest value color
#plasma_cols[6] <- "#213c74"



dafor_cols <- c(
  "D"        = "#c75a24",
  "A"        = "#eea700",
  "F"        = "#f9e730",
  "O"        = "#41b5ee",
  "R"        = "#417eee",
  "Ausente"  = "#1d3c6f"
)





stacked_dafor_localidade_balanced <- ggplot(cats_loc,
                                   aes(x = localidade, y = minutes, fill = dafor_cat)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Esforço (minutos de monitoramento)",
    fill = ""
  ) +
  #scale_fill_manual(values = plasma_cols) +
 
  scale_fill_manual(values = dafor_cols) +
  
   theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 16),
    legend.text = element_text(size = 20),
    legend.key.size = unit(2.0, "cm")
  )

x11()
stacked_dafor_localidade_balanced

ggsave("plots/1_stacked_dafor_localidade_balanced.png",
       stacked_dafor_localidade_balanced,
       width = 12, height = 13, dpi = 300)


print(stacked_dafor_localidade_balanced)
################################################ parei aqui ###################

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
# Each row = 1 minute of monitoring

cats_loc <- data_loc %>%
  filter(!is.na(dafor_cat)) %>%
  count(localidade, dafor_cat, name = "minutes") %>%
  complete(
    localidade,
    dafor_cat = c("D", "A", "F", "O", "R", "Ausente"),
    fill = list(minutes = 0)
  ) %>%
  mutate(
    dafor_cat = factor(
      dafor_cat,
      levels = c("D", "A", "F", "O", "R", "Ausente")
    )
  )

# 3) Order by total monitoring effort
# Largest effort will appear on top after coord_flip()

loc_order <- cats_loc %>%
  group_by(localidade) %>%
  summarise(
    total_minutes = sum(minutes),
    .groups = "drop"
  ) %>%
  arrange(total_minutes) %>%
  pull(localidade)

cats_loc <- cats_loc %>%
  mutate(
    localidade = factor(localidade, levels = loc_order)
  )

# Generate plasma colors
#plasma_cols <- viridisLite::plasma(
#  n = length(unique(cats_loc$dafor_cat)),
#  begin = 0.9,
#  end = 0.1
#)

# Replace the lowest value color
#plasma_cols[6] <- "#213c74"







# 4) Plot (horizontal)
stacked_dafor_localidade <- ggplot(
  cats_loc, 
  aes(
    x = localidade, 
    y = minutes,
    fill = dafor_cat
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Esforço (minutos de monitoramento)",
    fill = ""
  ) +
  #scale_fill_manual(values = plasma_cols) + 
  scale_fill_manual(values = dafor_cols) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 16),
    legend.text = element_text(size = 20),
    legend.key.size = unit(2.0, "cm")
  )

x11()
stacked_dafor_localidade
 
ggsave("plots/2_stacked_dafor_localidade.png",
       stacked_dafor_localidade,
       width = 12, height = 13, dpi = 300)




# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
# library(lubridate)
# library(readr)

################################################################################
### 1. Prepare manejo data
################################################################################

df_manejo_clean <- df_manejo |>
  mutate(
    localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
    data = dmy(data),
    year = year(data),
    massa_kg = parse_number(massa_kg,
                            locale = locale(decimal_mark = ",")),
    massa_kg_por_cilindro = massa_kg / n_cilindros
  )

################################################################################
### Keep only ENGENHO and remove 2023 and 2026
################################################################################

df_manejo_clean <- df_manejo_clean %>%
  filter(
    localidade %in% c("ENGENHO"),
    !year %in% c(2023, 2026)
  )

################################################################################
### 2. Chart 1: total mass by year
################################################################################

manejo_mass_year <- df_manejo_clean |>
  group_by(year, localidade) |>
  summarise(
    massa_kg = sum(massa_kg, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(year = factor(year))



plot_massa_localidade_ano <- ggplot(
  manejo_mass_year,
  aes(x = year, y = massa_kg, fill = localidade)
) +
  geom_col() +
  labs(
    title = "Massa total manejada (Kg)",
    x = NULL,
    y = "Massa manejada (kg)",
    fill = ""
  ) +
  scale_fill_manual(values = "orange") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      size = 22,
      face = "bold",
      hjust = 0.5
    ),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20)
  )


x11()
plot_massa_localidade_ano

ggsave(
  "plots/3_plot_massa_localidade_ano.png",
  plot_massa_localidade_ano,
  width = 12,
  height = 13,
  dpi = 300
)

################################################################################
### 3. Chart 2: mass corrected by effort (n_cilindros)
################################################################################

manejo_mass_effort_year <- df_manejo_clean |>
  group_by(year, localidade) |>
  summarise(
    massa_kg = sum(massa_kg, na.rm = TRUE),
    n_cilindros = sum(n_cilindros, na.rm = TRUE),
    massa_kg_por_cilindro = massa_kg / n_cilindros,
    .groups = "drop"
  ) |>
  mutate(year = factor(year))

plot_massa_por_cilindro_localidade_ano <- ggplot(
  manejo_mass_effort_year,
  aes(x = year, y = massa_kg_por_cilindro, fill = localidade)
) +
  geom_col() +
  
  geom_text(
    aes(label = paste0("n. cilindros = ", n_cilindros)),
    vjust = 1.5,
    
    size = 10
  ) +
  
  labs(
    title = "Massa manejada por cilindro (kg/cilindro)",
    x = NULL,
    y = "Massa manejada por cilindro (kg/cilindro)",
    fill = ""
  ) +
  scale_fill_manual(values = "orange") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      size = 22,
      face = "bold",
      hjust = 0.5
    ),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20)
  )

x11()
plot_massa_por_cilindro_localidade_ano

ggsave(
  "plots/4_plot_massa_por_cilindro_localidade_ano.png",
  plot_massa_por_cilindro_localidade_ano,
  width = 12,
  height = 13,
  dpi = 300
)


################################################################################
### 4. Chart 3: mass corrected by number of management days
################################################################################

manejo_mass_day_year <- df_manejo_clean |>
  group_by(year, localidade) |>
  summarise(
    massa_kg = sum(massa_kg, na.rm = TRUE),
    n_dias_manejo = n(),
    massa_kg_por_dia = massa_kg / n_dias_manejo,
    .groups = "drop"
  ) |>
  mutate(year = factor(year))


plot_massa_por_dia_localidade_ano <- ggplot(
  manejo_mass_day_year,
  aes(x = year, y = massa_kg_por_dia, fill = localidade)
) +
  geom_col() +
  
  geom_text(
    aes(label = paste0("n. dias = ", n_dias_manejo)),
    vjust = 1.5,
    size = 10
  ) +
  
  labs(
    title = "Massa manejada por dia de manejo (kg/dia)",
    x = NULL,
    y = "Massa manejada por dia (kg/dia)",
    fill = ""
  ) +
  
  scale_fill_manual(values = "orange") +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      size = 22,
      face = "bold",
      hjust = 0.5
    ),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20)
  )

plot_massa_por_dia_localidade_ano

ggsave(
  "plots/5_plot_massa_por_dia_localidade_ano.png",
  plot_massa_por_dia_localidade_ano,
  width = 12,
  height = 13,
  dpi = 300
)


################################################################################
### 5. Chart 4: mass per colony - ILHA CAGARRA
### Each square represents 50 colonies
################################################################################

valor_por_quadrado <- 50
n_colunas_quadrados <- 10


################################################################################
### 1. Prepare data
################################################################################

cagarra_data <- tibble(
  year = c(2023, 2024, 2025, 2026),
  colonias = c(2876, 7223, 5166, 2265),
  massa_kg = c(219, 555.5, 282.5, 115)
) |>
  mutate(
    massa_kg_por_colonia = massa_kg / colonias,
    
    # Number of squares used to represent colonies
    n_quadrados = round(colonias / valor_por_quadrado),
    
    # Numeric position of each bar
    x_bar = 1:n()
  )


################################################################################
### 2. Create a fixed square grid
### Same spacing and position among years
################################################################################

# Maximum number of rows needed
n_linhas_fixas <- ceiling(
  max(cagarra_data$n_quadrados) / n_colunas_quadrados
)

# Same horizontal positions for all bars
x_offsets <- seq(
  -0.30,
  0.30,
  length.out = n_colunas_quadrados
)

# Same vertical positions for all bars
# Values kept below the smallest bar
y_positions <- seq(
  0.004,
  0.045,
  length.out = n_linhas_fixas
)


################################################################################
### 3. Generate square positions
################################################################################

square_data <- purrr::pmap_dfr(
  
  list(
    cagarra_data$year,
    cagarra_data$n_quadrados,
    cagarra_data$x_bar
  ),
  
  function(year, n_quadrados, x_bar) {
    
    tibble(
      id = seq_len(n_quadrados)
    ) |>
      mutate(
        
        # Column in the fixed grid
        col = ((id - 1) %% n_colunas_quadrados) + 1,
        
        # Row in the fixed grid
        row = ((id - 1) %/% n_colunas_quadrados) + 1,
        
        # Same grid spacing for every year
        x = x_bar + x_offsets[col],
        y = y_positions[row],
        
        year = year
      )
  }
)


################################################################################
### 4. Plot
################################################################################

plot_massa_por_colonia_cagarra <- ggplot(
  cagarra_data,
  aes(
    x = x_bar,
    y = massa_kg_por_colonia
  )
) +
  
  # Bars = kg per colony
  geom_col(
    fill = "orange",
    width = 0.8
  ) +
  
  # Squares = number of colonies
  geom_point(
    data = square_data,
    aes(
      x = x,
      y = y,
      shape = "50 colônias"
    ),
    inherit.aes = FALSE,
    size = 3.6,
    fill = "white",
    color = "gray20",
    stroke = 0.4
  ) +
  
  # kg/colony value above each bar
  geom_text(
    aes(
      label = scales::number(
        massa_kg_por_colonia,
        accuracy = 0.001,
        decimal.mark = ","
      )
    ),
    vjust = -0.7,
    size = 8
  ) +
  
  # Square legend
  scale_shape_manual(
    values = c(
      "50 colônias" = 22
    ),
    name = NULL
  ) +
  
  # Years
  scale_x_continuous(
    breaks = cagarra_data$x_bar,
    labels = cagarra_data$year
  ) +
  
  # Y axis
  scale_y_continuous(
    labels = scales::label_number(
      accuracy = 0.01,
      decimal.mark = ","
    ),
    expand = expansion(
      mult = c(0, 0.15)
    )
  ) +
  
  labs(
    title = "Massa manejada por colônia",
    x = NULL,
    y = "kg/colônia"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    
    plot.title = element_text(
      size = 22,
      face = "bold",
      hjust = 0.5
    ),
    
    legend.position = "top",
    legend.text = element_text(size = 16),
    legend.key.size = unit(1, "cm"),
    
    panel.grid = element_blank(),
    axis.line = element_line(),
    
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20)
  )


################################################################################
### 5. Visualise
################################################################################

plot_massa_por_colonia_cagarra


################################################################################
### 6. Save
################################################################################

ggsave(
  "plots/6_plot_massa_por_colonia_cagarra.png",
  plot_massa_por_colonia_cagarra,
  width = 12,
  height = 13,
  dpi = 300
)




