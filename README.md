PRODUCTO ACADÉMICO COLABORATIVO 📝
TEMA: PRINCIPALES INDICADORES RELACIONADOS A LA ESPERANZA DE VIDA (1973 - 2023)
1. 🧭 Introducción
La esperanza de vida es un indicador clave del desarrollo humano, ampliamente utilizado para medir el bienestar y la calidad de vida de una población. Sin embargo, su evolución a lo largo del tiempo y sus diferencias entre regiones del mundo responden a múltiples factores socioeconómicos y de salud. Este proyecto tiene como objetivo central explorar cómo variables como la mortalidad infantil, la tasa de fertilidad, la pobreza y el PIB per cápita influyen en la esperanza de vida.

Para ello, se desarrolló un dashboard interactivo en R con Shiny, que permite comparar y visualizar la esperanza de vida por país, región y continente, a través del tiempo. Esta herramienta busca facilitar el análisis visual, promover la comprensión de las relaciones entre variables clave y apoyar estudios, investigaciones y decisiones basadas en datos.

El desarrollo de este dashboard se realizó en el marco del curso Herramientas Informáticas de la Maestría, utilizando datos de Gapminder y el Banco Mundial.

2. 🎯 Objetivo del Dashboard
El objetivo principal de este dashboard interactivo es analizar la esperanza de vida como variable central, en relación con distintos indicadores de contexto social y de salud —como el PIB per cápita, la pobreza, la tasa de fertilidad y la mortalidad infantil— a lo largo del tiempo y entre diferentes regiones del mundo.

La herramienta fue diseñada con un enfoque en la experiencia del usuario, proponiendo visualizaciones claras y datos fácilmente interpretables. A través de gráficos comparativos personalizables por continente, región o país, el dashboard permite explorar cómo ha cambiado la esperanza de vida en el tiempo y cuál es su relación con las variables seleccionadas, evidenciando posibles correlaciones de forma visual e intuitiva.

Este dashboard fue desarrollado como parte del curso Herramientas Informáticas de la Maestría, empleando R y Shiny.

🔗 Enlaces
📊 Ver dashboard en línea:
https://karlam17.shinyapps.io/pac-dashboard/

💻 Ver código fuente en GitHub:
https://github.com/Karlam17/PAC-Dashboard.git


3. 📁 Características de la base de datos
📌 1. Datos
La base de datos utilizada para el desarrollo del dashboard fue construida a partir de fuentes disponibles en gapminder.org, que recopilan información proveniente del Banco Mundial. Cada conjunto de datos contenía registros por país, año y los valores correspondientes a una variable específica. La serie temporal para la mayoría de las variables se extendía desde el año 1800 hasta 2100, con excepción de las variables de esperanza de vida masculina y femenina, cuyo rango temporal abarcaba desde 1950 hasta 2100.

Una vez extraídas, todas las bases fueron trabajadas en RStudio, transformándolas al formato de datos de panel, lo que permitió integrar todas las variables en una sola base consolidada.

🧹 2. Depuración y limpieza
Posteriormente, se realizó un proceso de depuración, tomando como referencia la variable principal de interés: esperanza de vida. Se llevó a cabo una verificación de datos perdidos (missing values), y se determinó que estos no superaban el 5% del total. Por lo tanto, se optó por eliminar únicamente los países que presentaban valores faltantes en dicha variable, sin afectar la representatividad ni la validez analítica de la base.

Además, se consideró pertinente enfocar el análisis en los últimos 50 años, lo cual permitió trabajar con un período comprendido entre 1973 y 2023, conservando la calidad y actualidad de los datos.

La base final resultante incluyó 185 países, cada uno con observaciones anuales completas dentro del período mencionado.

📊 3. Variables
Para el análisis de la esperanza de vida como variable principal de interés, se optó por incluir las variables **tasa de mortalidad infantil**, **tasa de fertilidad**, **pobreza** y **PIB per cápita**. Esta selección se fundamenta en una revisión de literatura que identifica estos factores como determinantes relevantes en la explicación de las variaciones en la esperanza de vida entre países y a lo largo del tiempo.

A continuación se describen las variables incluidas en la base de datos final:


| **Nombre de la variable**     | **Descripción**                                              | **Unidad de medida**                  |
|------------------------------|--------------------------------------------------------------|---------------------------------------|
| `País`                       | Nombre del país                                              | Texto                                 |
| `Año`                       | Año de observación                                           | Año (numérico)                        |
| `Esperanza de vida`             | Esperanza de vida promedio al nacer                         | Años                                  |
| `Esperanza de vida mujeres`     | Esperanza de vida de mujeres al nacer                       | Años                                  |
| `Esperanza de vida hombres`     | Esperanza de vida de hombres al nacer                       | Años                                  |
| `PIB per Cápita`                   | Producto Interno Bruto per cápita                           | Dólares internacionales, precios fijos del 2017                   |
| `Tasa de Pobreza`                    | Porcentaje de la población bajo la línea de pobreza ($2.15/Día)        | Porcentaje de la población (%)                        |
| `Tasa de fertilidad`            | Número promedio de hijos por mujer                          | Hijos por mujer                       |
| `Tasa de mortalidad infantil`        | Mortalidad infantil (menores de 5 años por cada 1,000 nacidos vivos) | Defunciones por cada 1,000 nacidos    |
| `Continente`                 | Continente al que pertenece el país                         | Texto                                 |


4. 🖥️ Descripción del Dashboard
Este dashboard interactivo fue desarrollado para facilitar el análisis visual y comparativo de la esperanza de vida en relación con indicadores sociales y de salud como la tasa de mortalidad infantil, tasa de fertilidad, tasa de pobreza y PIB per cápita, entre continentes, países y regiones del mundo a lo largo del tiempo.

⚙️ Funcionalidades principales
🔍 Filtros dinámicos por continente, región, país y año, para personalizar las visualizaciones.

📊 Visualizaciones interactivas con gráficos de líneas, barras y regresiones múltiples.

📈 Análisis de tendencias temporales y correlaciones entre variables clave.

🎨 Uso de gráficos interactivos con la librería Plotly para exploración intuitiva.

📋 Tablas dinámicas para visualización detallada de datos numéricos.

📊 Gráficos y tablas disponibles en el Dashboard
El dashboard cuenta con un panel principal que contiene múltiples pestañas (tabsetPanel), cada una con diferentes visualizaciones y tablas interactivas para explorar los datos:

📝 Resumen Promedios
Tabla dinámica que muestra los promedios de las variables seleccionadas para los filtros aplicados.
(Output: DTOutput("tabla_promedios"))

📉 Gráficos de Dispersión
Gráficos interactivos de dispersión donde se puede seleccionar la variable a visualizar entre PIB per cápita, mortalidad infantil, tasa de fertilidad y pobreza.
(Input: selectInput("variable_disp") - Output: plotlyOutput("grafico_disp"))

🏆 Top y Bottom 5 Esperanza de Vida
Visualización que muestra los 5 países con mayor y menor esperanza de vida para el año y filtro seleccionados.
(Output: plotlyOutput("plot_top_bottom"))

⚖️ Comparativo Esperanza de Vida Masculina y Femenina
Gráfico comparativo para analizar la esperanza de vida según género en diferentes países o regiones.
(Output: plotlyOutput("grafico_vida_mf"))

📈 Evolución de Variables
Permite seleccionar una variable (esperanza de vida, PIB per cápita, mortalidad infantil, tasa de fertilidad o pobreza) para analizar su evolución a lo largo del tiempo mediante gráficos de líneas.
(Input: selectInput("variable_evolucion") - Output: plotlyOutput("grafico_evolucion"))

🗺️ Mapa Mundial
Mapa interactivo que muestra la distribución geográfica de la esperanza de vida en el mundo.
(Output: plotlyOutput("mapa_esperanza_vida"))

🔄 Matriz de Correlación
Gráfico que presenta la matriz de correlación entre las variables analizadas para detectar relaciones importantes.
(Output: plotlyOutput("matriz_correlacion"))

📊 Regresión múltiple
Visualización del modelo de regresión múltiple que predice la esperanza de vida a partir de las variables seleccionadas.
(Output: plotlyOutput("grafico_regresion"))

🎥 Videos demostrativos
https://drive.google.com/file/d/1XoHVHiKg_GgnS7-U6JRIAPuBcnY7wxN7/view?usp=drive_link

🛠️ Detalles técnicos
📝 Lenguaje: R

🌐 Framework: Shiny

📦 Librerías principales: shiny, plotly, dplyr, readxl, DT

📊 Base de datos: Panel de datos con series temporales 1973–2023 para 185 países

🚀 Despliegue: Compatible con shinyapps.io y servidores Shiny propios


5. 📌 Conclusiones del Dashboard sobre la Esperanza de Vida
📈 Tendencia general positiva:
Se observa una tendencia creciente en la esperanza de vida a nivel mundial durante las últimas décadas. Países como Singapur, Japón y Suecia lideran con los índices más altos globalmente. En Sudamérica destacan Chile, Perú y Colombia.

🌍 Desigualdades regionales:
Existen diferencias marcadas entre regiones y países. Las naciones con mayores ingresos presentan una esperanza de vida significativamente superior a las de ingresos bajos.

🚻 Diferencias por género:
Las mujeres suelen vivir más que los hombres en casi todos los países, aunque la brecha varía según factores socioeconómicos y culturales.

6. ⚖️ Bondades, Limitaciones y Futuras Mejoras
✔️ Bondades
Automatización eficiente de tareas y análisis usando R.

Posibilidad de simular escenarios y construir algoritmos personalizados.

Uso de paquetes versátiles que facilitan la creación de visualizaciones interactivas.

Facilidad para integrar múltiples fuentes de datos y trabajar con datos de panel.

⚠️ Limitaciones
Curva de aprendizaje pronunciada para usuarios no familiarizados con R y Shiny.

Uso intensivo de recursos computacionales en ciertos análisis o despliegues.

Posibles problemas de compatibilidad entre versiones de paquetes o sistemas operativos.

🚀 Futuras mejoras
Incorporar más variables y fuentes de datos para enriquecer el análisis.

Optimizar el rendimiento para mejorar la experiencia del usuario.

Añadir funcionalidades como reportes automáticos o alertas personalizadas.

Integrar análisis predictivos avanzados y machine learning para anticipar tendencias.

7. 📚 Referencias bibliográficas
Banco Mundial. (2025). Datos del Banco Mundial. Obtenido de: https://datos.bancomundial.org/

Chen, Z., Ma, Y., Hua, J., & Wang, Y. (2021). Impacts from Economic Development and Environmental Factors on Life Expectancy: A Comparative Study Based on Data from Both Developed and Developing Countries from 2004 to 2016. International Journal of Environmental Research and Public Health, 18(16), 8559. https://doi.org/10.3390/ijerph18168559

Gapminder. (2025). Gapminder Data Portal. Obtenido de: https://www.gapminder.org/data/

Heiss, F. (2020). Using R for Introductory Econometrics. Manuscrito no publicado.

Irizarry, R. A., & Love, M. I. (2016). Data Analysis for the Life Sciences. [Libro en línea].

Miladinov, G. (2020). Socioeconomic development and life expectancy relationship: evidence from the EU accession candidate countries. Genus, 76. https://doi.org/10.1186/s41118-020-00088-9

Mitteldorf, J. (2010). Female fertility and longevity. AGE, 32, 79–84. https://doi.org/10.1007/s11357-010-9139-4

Wickham, H., Cetinkaya-Rundel, M., & Grolemund, G. (2023). R for Data Science (2nd ed.). Obtenido de: https://r4ds.hadley.nz/


8. 📎 Anexos
PPT´s: https://docs.google.com/presentation/d/1SEtNQ5W1w_loUuTAEUjR7LkUBFGegsrb/edit?slide=id.g361c1cd1fa1_3_0#slide=id.g361c1cd1fa1_3_0
