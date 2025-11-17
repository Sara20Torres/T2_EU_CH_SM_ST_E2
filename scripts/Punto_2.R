###############################################################################
# Universidad de los Andes - Economía Urbana
# Taller 2 - Ejercicio 2: Distribución Racial en Chicago
###############################################################################

# ============================================================
# CONFIGURACIÓN INICIAL
# ============================================================

# Limpiar entorno

rm(list = ls())

# Cargar paquetes

if(!require(pacman)) install.packages("pacman")
require(pacman)

p_load(
  rio,          # Importación/exportación datos
  here,         # Gestión de rutas
  janitor,      # Limpieza de nombres
  sf,           # Datos espaciales
  tidyverse,    # Manipulación y visualización 
  viridis,      # Paletas de colores
  scales,       # Formatos 
  gridExtra     # Múltiples gráficos
)

options(scipen = 999)  # Desactivar notación científica
options(digits = 4)     # Decimales

# Configurar rutas

script_path <- rstudioapi::getSourceEditorContext()$path
script_dir  <- dirname(script_path)
stores_path <- file.path(dirname(script_dir), "stores")

# Crear carpeta stores si no existe

if (!dir.exists(stores_path)) {
  dir.create(stores_path, recursive = TRUE)
}

# Función auxiliar para guardar archivos

store_file <- function(filename) {
  file.path(stores_path, filename)
}

# ============================================================
# CARGAR DATOS
# ============================================================

datos_chicago <- import(here("data", "Combined_data_Panel.dta"))

chicago_shp <- st_read(here("data", "Boundaries - Census Tracts - 2010", 
                            "geo_export_a4ade1ed-743c-4dd8-ad1a-89b46b222cec.shp"),
                       quiet = TRUE)

chicago_shp <- clean_names(chicago_shp)

# Crear proporciones raciales

datos_chicago <- datos_chicago %>%
  mutate(
    # Proporciones por grupo racial
    
    prop_black = Black_Pop / Total_Pop,
    prop_hispanic = Hispanic_Pop / Total_Pop,
    prop_white = White_Pop / Total_Pop,
    prop_asian = Asian_Pop / Total_Pop,
    
    # Proporción de minorías
    
    prop_minority = 1 - prop_white,
    
    # Reemplazar valores no válidos
    
    across(starts_with("prop_"), ~replace(., is.nan(.) | is.infinite(.), 0))
  )

# ============================================================
# UNIÓN Y PROCESAMIENTO DE DATOS
# ============================================================

# Unir datos con shapefile
chicago_map <- chicago_shp %>%
  left_join(datos_chicago, by = c("geoid10" = "cod_census_track"))

n_matched <- chicago_map %>% filter(!is.na(year)) %>% nrow()

# ------------------------------------------------------------
# Mapas de población Afro-Americana
# ------------------------------------------------------------

p_black_maps <- ggplot() +
  geom_sf(data = chicago_map, aes(fill = prop_black), color = NA) +
  facet_wrap(~year, nrow = 1) +
  scale_fill_viridis_c(
    name = "Proporción\nAfro-Americana",
    option = "magma",
    labels = percent,
    na.value = "grey90",
    limits = c(0, 1)
  ) +
  labs(
    title = "Evolución de la Proporción Afro-Americana en Chicago (2000-2020)",
    subtitle = "Por Census Tract",
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave(store_file("map_black_evolution.png"), p_black_maps, 
       width = 14, height = 5, dpi = 300)

# ------------------------------------------------------------
# Mapas de población Hispana
# ------------------------------------------------------------

p_hispanic_maps <- ggplot() +
  geom_sf(data = chicago_map, aes(fill = prop_hispanic), color = NA) +
  facet_wrap(~year, nrow = 1) +
  scale_fill_viridis_c(
    name = "Proporción\nHispana",
    option = "plasma",
    labels = percent,
    na.value = "grey90",
    limits = c(0, 1)
  ) +
  labs(
    title = "Evolución de la Proporción Hispana en Chicago (2000-2020)",
    subtitle = "Por Census Tract",
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave(store_file("map_hispanic_evolution.png"), p_hispanic_maps, 
       width = 14, height = 5, dpi = 300)

# ------------------------------------------------------------
# Análisis de ubicación geográfica
# ------------------------------------------------------------

# Calcular centroides

chicago_centroids <- chicago_map %>%
  filter(!is.na(year)) %>%
  mutate(
    centroid = st_centroid(geometry),
    lon = st_coordinates(centroid)[,1],
    lat = st_coordinates(centroid)[,2]
  ) %>%
  st_drop_geometry()

# CBD aproximado 

cbd_lon <- -87.6298
cbd_lat <- 41.8781

# Distancia al CBD

chicago_centroids <- chicago_centroids %>%
  mutate(dist_cbd = sqrt((lon - cbd_lon)^2 + (lat - cbd_lat)^2) * 111)

# Correlación entre raza e ingreso

cor_income <- datos_chicago %>%
  group_by(year) %>%
  summarise(
    cor_black = cor(prop_black, Median_Inc, use = "complete.obs"),
    cor_hispanic = cor(prop_hispanic, Median_Inc, use = "complete.obs"),
    .groups = "drop"
  )


# ------------------------------------------------------------
# Funciones para calcular índices 
# ------------------------------------------------------------

# Índice de Dissimilarity

calc_dissimilarity <- function(group1_pop, group2_pop) {
  valid <- !is.na(group1_pop) & !is.na(group2_pop)
  group1_pop <- group1_pop[valid]
  group2_pop <- group2_pop[valid]
  
  A <- sum(group1_pop, na.rm = TRUE)
  B <- sum(group2_pop, na.rm = TRUE)
  
  if(A == 0 | B == 0) return(NA_real_)
  
  D <- 0.5 * sum(abs((group1_pop / A) - (group2_pop / B)), na.rm = TRUE)
  return(D)
}

# Índice de Isolation 

calc_isolation <- function(group_pop, total_pop) {
  valid <- !is.na(group_pop) & !is.na(total_pop) & total_pop > 0
  group_pop <- group_pop[valid]
  total_pop <- total_pop[valid]
  
  X_total <- sum(group_pop, na.rm = TRUE)
  
  if(X_total == 0) return(NA_real_)
  
  ISO <- sum((group_pop / X_total) * (group_pop / total_pop), na.rm = TRUE)
  return(ISO)
}

# ------------------------------------------------------------
# Calcular índices por año
# ------------------------------------------------------------

indices_temporales <- datos_chicago %>%
  group_by(year) %>%
  summarise(
    D_black_white = calc_dissimilarity(Black_Pop, White_Pop),
    D_hispanic_white = calc_dissimilarity(Hispanic_Pop, White_Pop),
    ISO_black = calc_isolation(Black_Pop, Total_Pop),
    ISO_hispanic = calc_isolation(Hispanic_Pop, Total_Pop),
    ISO_white = calc_isolation(White_Pop, Total_Pop),
    .groups = "drop"
  )


# ------------------------------------------------------------
# Visualización de índices
# ------------------------------------------------------------

indices_long <- indices_temporales %>%
  pivot_longer(cols = -year, names_to = "indice", values_to = "valor")

p_indices <- ggplot(indices_long, aes(x = year, y = valor, color = indice, group = indice)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  scale_color_brewer(
    palette = "Set1",
    labels = c(
      "D_black_white" = "Dissimilarity: Afro-Am./Blancos",
      "D_hispanic_white" = "Dissimilarity: Hispanos/Blancos",
      "ISO_black" = "Isolation: Afro-Americanos",
      "ISO_hispanic" = "Isolation: Hispanos",
      "ISO_white" = "Isolation: Blancos"
    )
  ) +
  labs(
    title = "Evolución de Índices de Segregación en Chicago (2000-2020)",
    x = "Año",
    y = "Valor del Índice",
    color = "Índice",
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(store_file("indices_evolution.png"), p_indices, 
       width = 10, height = 8, dpi = 300)

# ------------------------------------------------------------
# Funciones para tipping points
# ------------------------------------------------------------

# Método 1: Mayor valor t

find_biggest_t <- function(data, x, y) {
  t_values <- numeric()
  points <- sort(data[[x]])
  
  for (i in seq_len(length(points) - 1)) {
    subset_data <- data %>% mutate(split = as.numeric(row_number() > i))
    model <- lm(reformulate("split", response = y), data = subset_data)
    t_values[i] <- abs(summary(model)$coefficients[2, "t value"])
  }
  
  max_t_index <- which.max(t_values)
  return(points[max_t_index])
}

# Método 2: Pendiente más pronunciada (Kernel Epanechnikov)

find_steepest_slope_epanechnikov <- function(data, x, y, bandwidth) {
  slopes <- numeric(nrow(data))
  
  epanechnikov_kernel <- function(u) {
    (3 / 4) * (1 - u^2) * (abs(u) <= 1)
  }
  
  for (i in seq_len(nrow(data))) {
    xi <- data[[x]][i]
    weights <- epanechnikov_kernel((data[[x]] - xi) / bandwidth)
    model <- lm(get(y) ~ poly(get(x), 2), data = data, weights = weights)
    slopes[i] <- coef(model)[2]
  }
  
  max_slope_index <- which.max(abs(slopes))
  return(data[[x]][max_slope_index])
}

# Método 3: Punto fijo

find_fixed_point <- function(data, x, y) {
  mean_y <- mean(data[[y]], na.rm = TRUE)
  data <- data %>% mutate(deviation = abs(get(y) - mean_y))
  
  tipping_point <- data %>%
    filter(deviation == min(deviation, na.rm = TRUE)) %>%
    pull(!!sym(x))
  
  return(tipping_point[1])
}


tipping_data <- datos_chicago %>%
  arrange(cod_census_track, year) %>%
  group_by(cod_census_track) %>%
  mutate(
    white_change_norm = (lead(White_Pop) - White_Pop) / 
      (White_Pop + lead(White_Pop) + 1),
    minority_lag = prop_minority
  ) %>%
  filter(!is.na(white_change_norm)) %>%
  ungroup()

# Calcular bandwidth

fullN <- nrow(datos_chicago)
cityN <- nrow(tipping_data)
bw <- 3 * (fullN / cityN)^0.2

# ------------------------------------------------------------
# Calcular tipping points
# ------------------------------------------------------------

calculate_all_tipping_points <- function(data, x_var, y_var, bandwidth) {
  data_clean <- data %>%
    filter(!is.na(get(x_var)) & !is.na(get(y_var))) %>%
    arrange(get(x_var))
  
  tp1 <- find_biggest_t(data_clean, x_var, y_var)
  tp2 <- find_steepest_slope_epanechnikov(data_clean, x_var, y_var, bandwidth)
  tp3 <- find_fixed_point(data_clean, x_var, y_var)
  
  return(tibble(
    metodo_t_max = tp1,
    metodo_pendiente = tp2,
    metodo_punto_fijo = tp3
  ))
}

# Transición 2000-2015

data_2000 <- tipping_data %>% filter(year == 2000)

tp_min_2000 <- calculate_all_tipping_points(
  data_2000, "prop_minority", "white_change_norm", bw
) %>% mutate(periodo = "2000-2015", grupo = "Minorías")

tp_black_2000 <- calculate_all_tipping_points(
  data_2000, "prop_black", "white_change_norm", bw
) %>% mutate(periodo = "2000-2015", grupo = "Afro-Americanos")

tp_hisp_2000 <- calculate_all_tipping_points(
  data_2000, "prop_hispanic", "white_change_norm", bw
) %>% mutate(periodo = "2000-2015", grupo = "Hispanos")

# Transición 2015-2020

data_2015 <- tipping_data %>% filter(year == 2015)

tp_min_2015 <- calculate_all_tipping_points(
  data_2015, "prop_minority", "white_change_norm", bw
) %>% mutate(periodo = "2015-2020", grupo = "Minorías")

tp_black_2015 <- calculate_all_tipping_points(
  data_2015, "prop_black", "white_change_norm", bw
) %>% mutate(periodo = "2015-2020", grupo = "Afro-Americanos")

tp_hisp_2015 <- calculate_all_tipping_points(
  data_2015, "prop_hispanic", "white_change_norm", bw
) %>% mutate(periodo = "2015-2020", grupo = "Hispanos")

# Combinar resultados

tipping_results <- bind_rows(
  tp_min_2000, tp_black_2000, tp_hisp_2000,
  tp_min_2015, tp_black_2015, tp_hisp_2015
)


# ------------------------------------------------------------
# Visualización mejorada con suavizado LOESS
# ------------------------------------------------------------

plot_tipping_improved <- function(data, x_var, y_var, tp_results, titulo, subtitulo) {
  
  tp_t <- tp_results$metodo_t_max
  tp_slope <- tp_results$metodo_pendiente
  tp_fixed <- tp_results$metodo_punto_fijo
  mean_y <- mean(data[[y_var]], na.rm = TRUE)
  
  data_clean <- data %>%
    filter(!is.na(!!sym(x_var)), !is.na(!!sym(y_var))) %>%
    arrange(!!sym(x_var))
  
  p <- ggplot(data_clean, aes_string(x = x_var, y = y_var)) +
    geom_point(alpha = 0.3, size = 1, color = "gray40") +
    geom_smooth(method = "loess", se = TRUE, color = "#2C3E50", 
                fill = "#3498DB", alpha = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", 
               size = 0.5, alpha = 0.7) +
    geom_vline(aes(xintercept = tp_t, color = "T máximo"), 
               linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = tp_slope, color = "Pendiente máxima"), 
               linetype = "dotted", size = 1) +
    geom_vline(aes(xintercept = tp_fixed, color = "Punto fijo"), 
               linetype = "dotdash", size = 1) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    scale_color_manual(
      name = "Método",
      values = c("T máximo" = "#E74C3C", 
                 "Pendiente máxima" = "#3498DB", 
                 "Punto fijo" = "#27AE60"),
      labels = c(
        paste0("T máximo (", scales::percent(tp_t, accuracy = 0.1), ")"),
        paste0("Pendiente máxima (", scales::percent(tp_slope, accuracy = 0.1), ")"),
        paste0("Punto fijo (", scales::percent(tp_fixed, accuracy = 0.1), ")")
      )
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Proporción del Grupo Racial",
      y = "Cambio Normalizado en Población Blanca",
      caption = "Nota: Línea suavizada = LOESS. Puntos grises = observaciones individuales."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      plot.caption = element_text(size = 8, hjust = 0),
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90")
    )
  
  return(p)
}

# Generar gráficos
p_tp_min_2000 <- plot_tipping_improved(
  data_2000, "prop_minority", "white_change_norm", tp_min_2000,
  "Tipping Point - Minorías (2000-2015)",
  "Chicago: Cambio en Población Blanca"
)
ggsave(store_file("tipping_minorities_2000_2015.png"), p_tp_min_2000, 
       width = 10, height = 7, dpi = 300)

p_tp_black_2000 <- plot_tipping_improved(
  data_2000, "prop_black", "white_change_norm", tp_black_2000,
  "Tipping Point - Afro-Americanos (2000-2015)",
  "Chicago: Cambio en Población Blanca"
)
ggsave(store_file("tipping_black_2000_2015.png"), p_tp_black_2000, 
       width = 10, height = 7, dpi = 300)

p_tp_hisp_2000 <- plot_tipping_improved(
  data_2000, "prop_hispanic", "white_change_norm", tp_hisp_2000,
  "Tipping Point - Hispanos (2000-2015)",
  "Chicago: Cambio en Población Blanca"
)
ggsave(store_file("tipping_hispanic_2000_2015.png"), p_tp_hisp_2000, 
       width = 10, height = 7, dpi = 300)

# ------------------------------------------------------------
# Mapas de clasificación según tipping point
# ------------------------------------------------------------

tp_min_val <- tp_min_2000$metodo_t_max
tp_black_val <- tp_black_2000$metodo_t_max

chicago_map_tp <- chicago_map %>%
  filter(year == 2015) %>%
  mutate(
    tp_category_minority = case_when(
      prop_minority <= tp_min_val ~ "Debajo del TP",
      prop_minority > tp_min_val ~ "Encima del TP",
      TRUE ~ NA_character_
    ),
    tp_category_black = case_when(
      prop_black <= tp_black_val ~ "Debajo del TP",
      prop_black > tp_black_val ~ "Encima del TP",
      TRUE ~ NA_character_
    )
  )

p_tp_minority <- ggplot() +
  geom_sf(data = chicago_map_tp, aes(fill = tp_category_minority), 
          color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("Debajo del TP" = "#3288bd", "Encima del TP" = "#d53e4f"),
    na.value = "grey90"
  ) +
  labs(
    title = "Census Tracts según Tipping Point de Minorías (2015)",
    subtitle = paste0("TP: ", round(tp_min_val * 100, 1), "%"),
    fill = "Categoría"
  ) +
  theme_void() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(store_file("map_tipping_minority_2015.png"), p_tp_minority, 
       width = 8, height = 10, dpi = 300)

# ------------------------------------------------------------
# Transiciones 2000-2020
# ------------------------------------------------------------

chicago_comparison <- datos_chicago %>%
  filter(year %in% c(2000, 2020)) %>%
  dplyr::select(cod_census_track, year, prop_minority) %>%
  pivot_wider(names_from = year, values_from = prop_minority, names_prefix = "prop_minority_")

tp_avg <- mean(c(tp_min_2000$metodo_t_max, tp_min_2015$metodo_t_max))

chicago_transitions <- chicago_comparison %>%
  mutate(
    transition_minority = case_when(
      prop_minority_2000 <= tp_avg & prop_minority_2020 <= tp_avg ~ "Siempre debajo",
      prop_minority_2000 <= tp_avg & prop_minority_2020 > tp_avg ~ "Cruzó TP",
      prop_minority_2000 > tp_avg & prop_minority_2020 > tp_avg ~ "Siempre encima",
      prop_minority_2000 > tp_avg & prop_minority_2020 <= tp_avg ~ "Retrocedió",
      TRUE ~ NA_character_
    )
  )

chicago_map_transitions <- chicago_shp %>%
  left_join(chicago_transitions, by = c("geoid10" = "cod_census_track"))

p_transitions <- ggplot() +
  geom_sf(data = chicago_map_transitions, aes(fill = transition_minority), 
          color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("Siempre debajo" = "#2166ac", "Cruzó TP" = "#d6604d",
               "Siempre encima" = "#b2182b", "Retrocedió" = "#92c5de"),
    na.value = "grey90"
  ) +
  labs(
    title = "Transiciones de Census Tracts (2000-2020)",
    subtitle = paste0("TP promedio: ", round(tp_avg * 100, 1), "%"),
    fill = "Transición"
  ) +
  theme_void() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(store_file("map_transitions_2000_2020.png"), p_transitions, 
       width = 8, height = 10, dpi = 300)

transition_summary <- chicago_transitions %>%
  count(transition_minority) %>%
  mutate(pct = n / sum(n) * 100)

