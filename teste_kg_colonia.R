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