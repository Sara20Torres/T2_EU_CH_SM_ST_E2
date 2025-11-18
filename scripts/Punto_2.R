###############################################################################
# Universidad de los Andes - Economía Urbana
# Taller 2 - Ejercicio 2
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
  gridExtra,    # Múltiples gráficos
  xtable        # Exportar tablas LaTeX
)

options(scipen = 999)
options(digits = 4)

# Configurar rutas
script_path <- rstudioapi::getSourceEditorContext()$path
script_dir  <- dirname(script_path)
stores_path <- file.path(dirname(script_dir), "stores")

if (!dir.exists(stores_path)) {
  dir.create(stores_path, recursive = TRUE)
}

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
    prop_black = Black_Pop / Total_Pop,
    prop_hispanic = Hispanic_Pop / Total_Pop,
    prop_white = White_Pop / Total_Pop,
    prop_minority = 1 - prop_white,
    across(starts_with("prop_"), ~replace(., is.nan(.) | is.infinite(.), 0))
  )

# ============================================================
# 1. MAPAS Y CORRELACIONES
# ============================================================

chicago_map <- chicago_shp %>%
  left_join(datos_chicago, by = c("geoid10" = "cod_census_track"))

# Mapas de población Afro-Americana
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
    subtitle = "Por Census Tract"
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

# Mapas de población Hispana
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
    subtitle = "Por Census Tract"
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

# Correlación entre raza e ingreso
cor_income <- datos_chicago %>%
  group_by(year) %>%
  summarise(
    cor_black = cor(prop_black, Median_Inc, use = "complete.obs"),
    cor_hispanic = cor(prop_hispanic, Median_Inc, use = "complete.obs"),
    .groups = "drop"
  )

print(cor_income)

cor_tabla <- cor_income %>%
  mutate(year = as.integer(year),
         across(where(is.numeric) & !year, ~round(., 3)))

tabla_cor_xtable <- xtable(cor_tabla,
                           caption = "Correlación entre proporción racial e ingreso mediano",
                           label = "tab:correlaciones",
                           align = c("l", "c", "c", "c"),
                           digits = c(0, 0, 3, 3))

print(tabla_cor_xtable,
      include.rownames = FALSE,
      caption.placement = "top",
      booktabs = TRUE,
      file = store_file("tabla_correlaciones.tex"))

write.csv(cor_tabla, store_file("correlaciones.csv"), row.names = FALSE)

# ============================================================
# 2. ÍNDICES DE SEGREGACIÓN
# ============================================================

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

calc_isolation <- function(group_pop, total_pop) {
  valid <- !is.na(group_pop) & !is.na(total_pop) & total_pop > 0
  group_pop <- group_pop[valid]
  total_pop <- total_pop[valid]
  
  X_total <- sum(group_pop, na.rm = TRUE)
  
  if(X_total == 0) return(NA_real_)
  
  ISO <- sum((group_pop / X_total) * (group_pop / total_pop), na.rm = TRUE)
  return(ISO)
}

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

print(indices_temporales)

indices_tabla <- indices_temporales %>%
  mutate(year = as.integer(year),
         across(where(is.numeric) & !year, ~round(., 3)))

tabla_indices_xtable <- xtable(indices_tabla,
                               caption = "Índices de segregación en Chicago (2000-2020)",
                               label = "tab:indices",
                               align = c("l", "c", "c", "c", "c", "c", "c"),
                               digits = c(0, 0, 3, 3, 3, 3, 3))

print(tabla_indices_xtable,
      include.rownames = FALSE,
      caption.placement = "top",
      booktabs = TRUE,
      file = store_file("tabla_indices.tex"))

write.csv(indices_tabla, store_file("indices_segregacion.csv"), row.names = FALSE)

# Gráfico evolución índices
indices_long <- indices_temporales %>%
  pivot_longer(cols = -year, names_to = "indice", values_to = "valor")

p_indices <- ggplot(indices_long, aes(x = year, y = valor, color = indice, group = indice)) +
  geom_line(linewidth = 1.2) +
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
    color = "Índice"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(store_file("indices_evolution.png"), p_indices, 
       width = 10, height = 8, dpi = 300)

# ============================================================
# 3. TIPPING POINTS
# ============================================================

# Preparar datos con cambio NORMALIZADO (Card et al. 2008)
tipping_data <- datos_chicago %>%
  arrange(cod_census_track, year) %>%
  group_by(cod_census_track) %>%
  mutate(
    # Cambio NORMALIZADO en población blanca
    norm_chg_white = (lead(White_Pop) - White_Pop) / 
      (White_Pop + lead(White_Pop) + 1),
    
    # Proporciones en año base (en %)
    frac_minority = prop_minority * 100,
    frac_black = prop_black * 100,
    frac_hispanic = prop_hispanic * 100
  ) %>%
  filter(!is.na(norm_chg_white)) %>%
  ungroup()

# Método: Mayor valor T (con restricción de rango razonable)
find_biggest_t <- function(data, x, y, min_pct = 5, max_pct = 60) {
  # Filtrar a rango razonable
  data_range <- data %>%
    filter(!!sym(x) >= min_pct, !!sym(x) <= max_pct) %>%
    arrange(!!sym(x))
  
  if(nrow(data_range) < 20) {
    # Si hay muy pocos datos, usar percentiles
    data_range <- data %>%
      filter(!!sym(x) >= quantile(!!sym(x), 0.1, na.rm = TRUE),
             !!sym(x) <= quantile(!!sym(x), 0.9, na.rm = TRUE))
  }
  
  t_values <- numeric()
  points <- sort(data_range[[x]])
  
  for (i in seq_len(length(points) - 1)) {
    subset_data <- data_range %>% mutate(split = as.numeric(row_number() > i))
    model <- lm(reformulate("split", response = y), data = subset_data)
    t_values[i] <- abs(summary(model)$coefficients[2, "t value"])
  }
  
  max_t_index <- which.max(t_values)
  return(points[max_t_index])
}

# Calcular tipping points para ambos períodos
data_2000_min <- tipping_data %>% filter(year == 2000)
data_2000_black <- tipping_data %>% filter(year == 2000, frac_black > 0)
data_2000_hisp <- tipping_data %>% filter(year == 2000, frac_hispanic > 0)

tp_min_2000 <- find_biggest_t(data_2000_min, "frac_minority", "norm_chg_white")
tp_black_2000 <- find_biggest_t(data_2000_black, "frac_black", "norm_chg_white")
tp_hisp_2000 <- find_biggest_t(data_2000_hisp, "frac_hispanic", "norm_chg_white")

data_2015_min <- tipping_data %>% filter(year == 2015)
data_2015_black <- tipping_data %>% filter(year == 2015, frac_black > 0)
data_2015_hisp <- tipping_data %>% filter(year == 2015, frac_hispanic > 0)

tp_min_2015 <- find_biggest_t(data_2015_min, "frac_minority", "norm_chg_white")
tp_black_2015 <- find_biggest_t(data_2015_black, "frac_black", "norm_chg_white")
tp_hisp_2015 <- find_biggest_t(data_2015_hisp, "frac_hispanic", "norm_chg_white")

# Tabla de resultados 
tipping_results <- tibble(
  Periodo = c("2000-2015", "2015-2020"),
  Minorías = c(tp_min_2000, tp_min_2015),
  `Afro-Americanos` = c(tp_black_2000, tp_black_2015),
  Hispanos = c(tp_hisp_2000, tp_hisp_2015)
) %>%
  mutate(across(where(is.numeric), ~round(., 1)))

cat("\n=== TIPPING POINTS (%) ===\n")
print(tipping_results)

# Exportar tabla
tabla_tp_xtable <- xtable(tipping_results,
                          caption = "Tipping points por grupo racial y período de transición (\\%)",
                          label = "tab:tipping",
                          align = c("l", "l", "c", "c", "c"),
                          digits = c(0, 0, 1, 1, 1))

print(tabla_tp_xtable,
      include.rownames = FALSE,
      caption.placement = "top",
      booktabs = TRUE,
      sanitize.text.function = function(x){x},
      file = store_file("tabla_tipping_points.tex"))

write.csv(tipping_results, store_file("tipping_points.csv"), row.names = FALSE)

# ============================================================
# VISUALIZACIÓN
# ============================================================

plot_tipping <- function(data, x_var, y_var, tp, titulo) {
  
  data_clean <- data %>%
    filter(!is.na(!!sym(x_var)), !is.na(!!sym(y_var)))
  
  p <- ggplot(data_clean, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_point(alpha = 0.3, size = 1, color = "gray40") +
    geom_smooth(method = "loess", se = TRUE, color = "#2C3E50", 
                fill = "#3498DB", alpha = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", 
               linewidth = 0.5, alpha = 0.7) +
    geom_vline(aes(xintercept = tp), 
               linetype = "dashed", color = "#E74C3C", linewidth = 1) +
    annotate("text", x = tp, y = max(data_clean[[y_var]], na.rm = TRUE) * 0.9,
             label = paste0("TP = ", round(tp, 1), "%"),
             color = "#E74C3C", size = 4, fontface = "bold") +
    scale_x_continuous(labels = percent_format(scale = 1)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01)) +
    labs(
      title = titulo,
      x = "Proporción del Grupo Racial (%)",
      y = "Cambio Normalizado en Población Blanca",
      caption = "Nota: Línea suavizada = LOESS. Línea vertical = Tipping Point estimado."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.caption = element_text(size = 8, hjust = 0),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# Generar gráficos para 2000-2015
p_tp_min <- plot_tipping(
  data_2000_min, "frac_minority", "norm_chg_white",
  tp_min_2000,
  "Tipping Point - Minorías (2000-2015)"
)
ggsave(store_file("tipping_minorities_2000.png"), p_tp_min, 
       width = 10, height = 7, dpi = 300)

p_tp_black <- plot_tipping(
  data_2000_black, "frac_black", "norm_chg_white",
  tp_black_2000,
  "Tipping Point - Afro-Americanos (2000-2015)"
)
ggsave(store_file("tipping_black_2000.png"), p_tp_black, 
       width = 10, height = 7, dpi = 300)

p_tp_hisp <- plot_tipping(
  data_2000_hisp, "frac_hispanic", "norm_chg_white",
  tp_hisp_2000,
  "Tipping Point - Hispanos (2000-2015)"
)
ggsave(store_file("tipping_hispanic_2000.png"), p_tp_hisp, 
       width = 10, height = 7, dpi = 300)

# Generar gráficos para 2015-2020
p_tp_min_2015 <- plot_tipping(
  data_2015_min, "frac_minority", "norm_chg_white",
  tp_min_2015,
  "Tipping Point - Minorías (2015-2020)"
)
ggsave(store_file("tipping_minorities_2015.png"), p_tp_min_2015, 
       width = 10, height = 7, dpi = 300)

p_tp_black_2015 <- plot_tipping(
  data_2015_black, "frac_black", "norm_chg_white",
  tp_black_2015,
  "Tipping Point - Afro-Americanos (2015-2020)"
)
ggsave(store_file("tipping_black_2015.png"), p_tp_black_2015, 
       width = 10, height = 7, dpi = 300)

p_tp_hisp_2015 <- plot_tipping(
  data_2015_hisp, "frac_hispanic", "norm_chg_white",
  tp_hisp_2015,
  "Tipping Point - Hispanos (2015-2020)"
)
ggsave(store_file("tipping_hispanic_2015.png"), p_tp_hisp_2015, 
       width = 10, height = 7, dpi = 300)

# ============================================================
# MAPAS DE TIPPING POINTS
# ============================================================

# Usar promedio de TPs de minorías para mapas
tp_avg <- mean(c(tp_min_2000, tp_min_2015)) / 100

chicago_map_tp <- chicago_map %>%
  filter(year == 2015) %>%
  mutate(
    categoria = case_when(
      prop_minority <= tp_avg ~ "Debajo del TP",
      prop_minority > tp_avg ~ "Encima del TP",
      TRUE ~ NA_character_
    )
  )

p_map_tp <- ggplot() +
  geom_sf(data = chicago_map_tp, aes(fill = categoria), 
          color = "white", linewidth = 0.1) +
  scale_fill_manual(
    values = c("Debajo del TP" = "#3288bd", "Encima del TP" = "#d53e4f"),
    na.value = "grey90"
  ) +
  labs(
    title = "Census Tracts según Tipping Point (2015)",
    subtitle = paste0("TP promedio: ", round(tp_avg * 100, 1), "%"),
    fill = "Categoría"
  ) +
  theme_void() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(store_file("map_tipping_2015.png"), p_map_tp, 
       width = 8, height = 10, dpi = 300)

# Transiciones 2000-2020
chicago_comparison <- datos_chicago %>%
  filter(year %in% c(2000, 2020)) %>%
  select(cod_census_track, year, prop_minority) %>%
  pivot_wider(names_from = year, values_from = prop_minority, 
              names_prefix = "prop_minority_")

chicago_transitions <- chicago_comparison %>%
  mutate(
    transicion = case_when(
      prop_minority_2000 <= tp_avg & prop_minority_2020 <= tp_avg ~ "Siempre debajo",
      prop_minority_2000 <= tp_avg & prop_minority_2020 > tp_avg ~ "Cruzó TP",
      prop_minority_2000 > tp_avg & prop_minority_2020 > tp_avg ~ "Siempre encima",
      prop_minority_2000 > tp_avg & prop_minority_2020 <= tp_avg ~ "Retrocedió",
      TRUE ~ NA_character_
    )
  )

chicago_map_trans <- chicago_shp %>%
  left_join(chicago_transitions, by = c("geoid10" = "cod_census_track"))

p_transitions <- ggplot() +
  geom_sf(data = chicago_map_trans, aes(fill = transicion), 
          color = "white", linewidth = 0.1) +
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
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(store_file("map_transitions_2000_2020.png"), p_transitions, 
       width = 8, height = 10, dpi = 300)

transition_summary <- chicago_transitions %>%
  count(transicion) %>%
  mutate(porcentaje = round(n / sum(n, na.rm = TRUE) * 100, 1))

print(transition_summary)

tabla_trans_xtable <- xtable(transition_summary,
                             caption = "Distribución de census tracts según transición (2000-2020)",
                             label = "tab:transiciones",
                             align = c("l", "l", "c", "c"),
                             digits = c(0, 0, 0, 1))

print(tabla_trans_xtable,
      include.rownames = FALSE,
      caption.placement = "top",
      booktabs = TRUE,
      file = store_file("tabla_transiciones.tex"))

write.csv(transition_summary, store_file("transiciones_summary.csv"), row.names = FALSE)

