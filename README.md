Punto 2 – Análisis Espacial y Segregación Urbana en Chicago

Autores: Corina Hernandez, Santiago Melo, Sara Torres

Este directorio contiene el código necesario para reproducir el análisis del Punto 2 del Taller 2. El ejercicio incluye visualización espacial, cálculo de correlaciones, medición de segregación e identificación de tipping points para Chicago entre 2000 y 2020.

Estructura del repositorio

data/
Contiene los datos utilizados: panel socioeconómico por census tract y shapefiles de Chicago.

scripts/
Incluye el script principal del ejercicio (Ejercicio2.R), con todo el procesamiento, generación de mapas, índices y gráficos.

stores/
Carpeta donde se guardan automáticamente todas las salidas generadas:
mapas, figuras, tablas LaTeX y archivos .csv.

Qué hace el script

Carga y limpia bases de población e ingreso por tract.

Crea mapas temáticos de proporción Afro-Americana e Hispana (2000–2020).

Calcula correlaciones entre composición racial e ingreso mediano.

Computa índices de segregación (Dissimilarity e Isolation) por año.

Estima tipping points siguiendo el método de Card et al. (2008).

Genera visualizaciones de tipping points y mapas de transición 2000–2020.

Exporta todos los resultados a la carpeta stores/.

Cómo ejecutarlo

Abrir el proyecto T2_EU_CH_SM_ST_E2.Rproj.

Ajustar rutas si es necesario.

Correr el script en scripts/Ejercicio2.R.

Revisar resultados dentro de stores/.
