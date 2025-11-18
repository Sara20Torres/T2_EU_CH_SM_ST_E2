# Punto 2 – Análisis Espacial y Segregación Urbana en Chicago

**Autores:** Corina Hernández, Santiago Melo, Sara Torres

Este directorio contiene el código necesario para reproducir el análisis del Punto 2 del Taller 2, que incluye visualización espacial, correlaciones, medición de segregación e identificación de *tipping points* para Chicago entre 2000 y 2020.

---

## 📁 Estructura del repositorio

- **data/**  
  Datos utilizados: panel socioeconómico por *census tract* y shapefiles de Chicago.

- **scripts/**  
  Script principal del ejercicio (`Ejercicio2.R`), donde se realiza todo el procesamiento, mapas, índices y gráficos.

- **stores/**  
  Salidas generadas automáticamente: mapas, figuras, tablas LaTeX y archivos `.csv`.

---

## 🔍 Qué hace el script principal

- Limpia y organiza los datos de población e ingreso.  
- Genera mapas temáticos de proporción Afro-Americana e Hispana (2000–2020).  
- Calcula correlaciones entre composición racial e ingreso mediano.  
- Estima índices de segregación (Dissimilarity e Isolation).  
- Identifica *tipping points* siguiendo Card et al. (2008).  
- Exporta todos los resultados a `stores/`.

---

## ▶️ Cómo ejecutarlo

1. Abrir el proyecto `T2_EU_CH_SM_ST_E2.Rproj`.  
2. Ejecutar `scripts/Ejercicio2.R`.

---
