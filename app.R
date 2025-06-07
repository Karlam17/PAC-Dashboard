library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(readxl)
library(countrycode)

# Cargar datos
base <- read_excel("data/base_con_continentes.xlsx") %>%
  mutate(Codigo_iso3 = countrycode(Pais, origin = "country.name", destination = "iso3c")) %>%
  filter(Anio >= 1973 & Anio <= 2023)

# Definir regiones
regiones <- list(
  "5 Tigres Asiáticos" = c("Singapore", "South Korea", "China", "Taiwan", "Malaysia"),
  "Unión Europea" = c("Germany", "France", "Italy", "Spain", "Poland"),
  "Sudamérica" = c("Argentina", "Chile", "Uruguay", "Paraguay", "Brazil", "Colombia", "Venezuela", "Ecuador", "Peru", "Bolivia"),
  "América del Norte" = c("United States", "Canada")
)

nombres_regiones <- names(regiones)
continentes <- unique(base$Continente)
paises_todos <- sort(unique(base$Pais))

#Definimos lo que contendra nuestro panel en el Dashboard

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(readxl)
library(countrycode)

# Cargar datos
base <- read_excel("data/base_con_continentes.xlsx") %>%
  mutate(Codigo_iso3 = countrycode(Pais, origin = "country.name", destination = "iso3c")) %>%
  filter(Anio >= 1973 & Anio <= 2023)

# Definir regiones
regiones <- list(
  "5 Tigres Asiáticos" = c("Singapore", "South Korea", "China", "Taiwan", "Malaysia"),
  "Unión Europea" = c("Germany", "France", "Italy", "Spain", "Poland"),
  "Sudamérica" = c("Argentina", "Chile", "Uruguay", "Paraguay", "Brazil", "Colombia", "Venezuela", "Ecuador", "Peru", "Bolivia"),
  "América del Norte" = c("United States", "Canada")
)

ui <- fluidPage(
  titlePanel("PRINCIPALES INDICADORES RELACIONADOS A LA ESPERANZA DE VIDA (1973 - 2023)"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Selecciona filtros para actualizar gráficos y tablas."),
      
      selectInput("tipo_ubicacion", "Tipo de filtro de ubicación:",
                  choices = c("Global", "Continente", "Región", "País")),
      
      conditionalPanel(
        condition = "input.tipo_ubicacion == 'Continente'",
        selectInput("filtro_continente", "Selecciona Continente:",
                    choices = c("Todos", unique(base$Continente)))
      ),
      
      conditionalPanel(
        condition = "input.tipo_ubicacion == 'Región'",
        selectInput("filtro_region", "Selecciona Región:",
                    choices = c("Todos", names(regiones)))
      ),
      
      conditionalPanel(
        condition = "input.tipo_ubicacion == 'País'",
        selectInput("filtro_pais", "Selecciona País(es):",
                    choices = sort(unique(base$Pais)),
                    multiple = TRUE)
      ),
      
      selectInput("filtro_anio", "Selecciona Año:",
                  choices = 1973:2023, selected = 2023),
      
      selectInput("nivel_filtro", "Nivel para gráfico Vida M/F:",
                  choices = c("Global", "Continente", "Región", "País"),
                  selected = "Global")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen Promedios", DTOutput("tabla_promedios")),
        tabPanel("Gráficos de Dispersión", 
                 selectInput("variable_disp", "Selecciona Variable:", 
                             choices = c("Pib_pcap", "Mortalidad_infantil", "Tasa_fertilidad", "Pobreza")),
                 plotlyOutput("grafico_disp")),
        tabPanel("Top y Bottom 5 Esperanza de Vida", 
                 plotlyOutput("plot_top_bottom")),
        tabPanel("Comparativo Esperanza de Vida M/F",
                 plotlyOutput("grafico_vida_mf")),
        tabPanel("Evolución Variables",
                 selectInput("variable_evolucion", "Selecciona Variable para Evolución:",
                             choices = c("Esperanza_vida", "Pib_pcap", "Mortalidad_infantil", "Tasa_fertilidad", "Pobreza")),
                 plotlyOutput("grafico_evolucion")),
        tabPanel("Mapa Mundial",
                 plotlyOutput("mapa_esperanza_vida", height = "600px")),
        tabPanel("Matriz de Correlación",
                 plotlyOutput("matriz_correlacion")), 
        tabPanel("Regresión múltiple",
                 plotlyOutput("grafico_regresion"))
      )
    )
  )
)

# Construimos los graficos del dashboard

server <- function(input, output, session) {
  
  datos_filtrados <- reactive({
    datos <- base %>% filter(Anio == input$filtro_anio)
    
    if (input$tipo_ubicacion == "Continente" && input$filtro_continente != "Todos") {
      datos <- datos %>% filter(Continente == input$filtro_continente)
    }
    
    if (input$tipo_ubicacion == "Región" && input$filtro_region != "Todos") {
      paises_region <- regiones[[input$filtro_region]]
      datos <- datos %>% filter(Pais %in% paises_region)
    }
    
    if (input$tipo_ubicacion == "País" && length(input$filtro_pais) > 0) {
      datos <- datos %>% filter(Pais %in% input$filtro_pais)
    }
    
    return(datos)
  })
  
    # Tabla resumen de promedios por variables 
    output$tabla_promedios <- renderDT({
      datos <- datos_filtrados()
      
      if (input$tipo_ubicacion == "Global") {
        agrupado <- datos %>%
          summarise(
            Años = unique(Anio),
            Observaciones = n(),
            Esperanza_vida = round(mean(Esperanza_vida, na.rm = TRUE), 2),
            Pib_pcap = round(mean(Pib_pcap, na.rm = TRUE), 2),
            Mortalidad_infantil = round(mean(Mortalidad_infantil, na.rm = TRUE), 2),
            Tasa_fertilidad = round(mean(Tasa_fertilidad, na.rm = TRUE), 2),
            Pobreza = round(mean(Pobreza, na.rm = TRUE), 2)
          )
      } else if (input$tipo_ubicacion == "Continente") {
        agrupado <- datos %>%
          group_by(Continente) %>%
          summarise(
            Observaciones = n(),
            Esperanza_vida = round(mean(Esperanza_vida, na.rm = TRUE), 2),
            Pib_pcap = round(mean(Pib_pcap, na.rm = TRUE), 2),
            Mortalidad_infantil = round(mean(Mortalidad_infantil, na.rm = TRUE), 2),
            Tasa_fertilidad = round(mean(Tasa_fertilidad, na.rm = TRUE), 2),
            Pobreza = round(mean(Pobreza, na.rm = TRUE), 2)
          ) %>%
          ungroup()
      } else if (input$tipo_ubicacion == "Región") {
        datos <- datos %>%
          mutate(Region_tag = case_when(
            Pais %in% regiones[["5 Tigres Asiáticos"]] ~ "5 Tigres Asiáticos",
            Pais %in% regiones[["Unión Europea"]] ~ "Unión Europea",
            Pais %in% regiones[["Sudamérica"]] ~ "Sudamérica",
            Pais %in% regiones[["América del Norte"]] ~ "América del Norte",
            TRUE ~ NA_character_
          ))
        
        if (input$filtro_region == "Todos") {
          datos <- datos %>% filter(!is.na(Region_tag))
        } else {
          datos <- datos %>% filter(Region_tag == input$filtro_region)
        }
        
        agrupado <- datos %>%
          group_by(Region_tag) %>%
          summarise(
            Observaciones = n(),
            Esperanza_vida = round(mean(Esperanza_vida, na.rm = TRUE), 2),
            Pib_pcap = round(mean(Pib_pcap, na.rm = TRUE), 2),
            Mortalidad_infantil = round(mean(Mortalidad_infantil, na.rm = TRUE), 2),
            Tasa_fertilidad = round(mean(Tasa_fertilidad, na.rm = TRUE), 2),
            Pobreza = round(mean(Pobreza, na.rm = TRUE), 2)
          ) %>%
          ungroup()
      } else if (input$tipo_ubicacion == "País") {
        agrupado <- datos %>%
          group_by(Pais) %>%
          summarise(
            Observaciones = n(),
            Esperanza_vida = round(mean(Esperanza_vida, na.rm = TRUE), 2),
            Pib_pcap = round(mean(Pib_pcap, na.rm = TRUE), 2),
            Mortalidad_infantil = round(mean(Mortalidad_infantil, na.rm = TRUE), 2),
            Tasa_fertilidad = round(mean(Tasa_fertilidad, na.rm = TRUE), 2),
            Pobreza = round(mean(Pobreza, na.rm = TRUE), 2)
          ) %>%
          ungroup()
      }
      
      datatable(agrupado, options = list(pageLength = 10), rownames = FALSE)
    })

    #Grafico de dispesion Esperanza de vida vs variables    
    output$grafico_disp <- renderPlotly({
      datos <- base %>% filter(Anio == input$filtro_anio)
      tipo <- input$tipo_ubicacion
      var <- input$variable_disp
      
      # Filtros
      if (tipo == "Continente" && input$filtro_continente != "Todos") {
        datos <- datos %>% filter(Continente == input$filtro_continente)
      }
      
      if (tipo == "Región" && input$filtro_region != "Todos") {
        paises_region <- regiones[[input$filtro_region]]
        datos <- datos %>% filter(Pais %in% paises_region)
      }
      
      if (tipo == "País") {
        datos <- datos %>% filter(Pais == input$filtro_pais)
      }
      
      if (nrow(datos) == 0 || is.null(var)) return(NULL)
      
      # Etiquetas del eje X
      etiquetas <- c(
        "Pib_pcap" = "PIB per cápita (USD)",
        "Mortalidad_infantil" = "Tasa de Mortalidad Infantil (número de niños fallecidos de 0-5 años)",
        "Tasa_fertilidad" = "Tasa de Fertilidad (bebés por mujer)",
        "Pobreza" = "Tasa de Pobreza (% e la población)"
      )
      xlab <- etiquetas[[var]] %||% var
      
      # Definir nombre de Región para todos los países
      datos <- datos %>%
        mutate(Region_tag = case_when(
          Pais %in% regiones[["5 Tigres Asiáticos"]] ~ "5 Tigres Asiáticos",
          Pais %in% regiones[["Unión Europea"]] ~ "Unión Europea",
          Pais %in% regiones[["Sudamérica"]] ~ "Sudamérica",
          Pais %in% regiones[["América del Norte"]] ~ "América del Norte",
          TRUE ~ NA_character_
        ))
      
      # Eliminar NA si filtro es Región == Todos
      if (tipo == "Región" && input$filtro_region == "Todos") {
        datos <- datos %>% filter(!is.na(Region_tag))
      }
      
      # Asignación dinámica de color
      datos <- datos %>%
        mutate(
          GrupoColor = case_when(
            tipo == "Global" ~ Pais,
            tipo == "Continente" & input$filtro_continente == "Todos" ~ Continente,
            tipo == "Continente" & input$filtro_continente != "Todos" ~ Pais,
            tipo == "Región" & input$filtro_region == "Todos" ~ Region_tag,
            tipo == "Región" & input$filtro_region != "Todos" ~ Pais,
            tipo == "País" ~ Pais
          ),
          Label = paste("País:", Pais, "<br>", xlab, ": ", round(.data[[var]], 2), "<br>Esperanza de Vida:", round(Esperanza_vida, 1))
        )
      
      # Etiqueta de la leyenda (color)
      leyenda_color <- case_when(
        tipo == "Global" ~ "País",
        tipo == "Continente" & input$filtro_continente == "Todos" ~ "Continente",
        tipo == "Continente" ~ "País",
        tipo == "Región" & input$filtro_region == "Todos" ~ "Región",
        tipo == "Región" ~ "País",
        tipo == "País" ~ "País"
      )
      
      # Gráfico
      p <- ggplot(datos, aes(x = .data[[var]], y = Esperanza_vida,
                             color = GrupoColor, text = Label)) +
        geom_point(size = 3, alpha = 0.8) +
        labs(
          x = xlab,
          y = "Esperanza de Vida (Años)",
          color = leyenda_color,
          title = paste("Dispersión de", xlab, "vs Esperanza de Vida en", input$filtro_anio)
        ) +
        theme_minimal()
      
      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(title = list(text = leyenda_color)))
    })
    
    #Grafico 5 paises con mayor y menor esperanza de vida
    output$plot_top_bottom <- renderPlotly({
      datos <- base %>% filter(Anio == input$filtro_anio)
      tipo <- input$tipo_ubicacion
      
      # Filtro por Continente
      if (tipo == "Continente" && input$filtro_continente != "Todos") {
        datos <- datos %>% filter(Continente == input$filtro_continente)
      }
      
      # Filtro por Región
      if (tipo == "Región") {
        datos <- datos %>%
          mutate(Region_tag = case_when(
            Pais %in% regiones[["5 Tigres Asiáticos"]] ~ "5 Tigres Asiáticos",
            Pais %in% regiones[["Unión Europea"]] ~ "Unión Europea",
            Pais %in% regiones[["Sudamérica"]] ~ "Sudamérica",
            Pais %in% regiones[["América del Norte"]] ~ "América del Norte",
            TRUE ~ NA_character_
          ))
        
        if (input$filtro_region == "Todos") {
          datos <- datos %>% filter(!is.na(Region_tag))
        } else {
          datos <- datos %>% filter(Region_tag == input$filtro_region)
        }
      }
      
      # Si tipo es País, no tiene sentido mostrar top/bottom 5
      if (tipo == "País") {
        return(plotly_empty() %>%
                 layout(title = "No aplica para selección por país"))
      }
      
      # Top 5 países por esperanza de vida
      top_5 <- datos %>%
        arrange(desc(Esperanza_vida)) %>%
        slice_head(n = 5) %>%
        mutate(Categoria = "Top 5")
      
      # Bottom 5 países por esperanza de vida
      bottom_5 <- datos %>%
        arrange(Esperanza_vida) %>%
        slice_head(n = 5) %>%
        mutate(Categoria = "Bottom 5")
      
      datos_plot <- bind_rows(top_5, bottom_5)
      
      p <- ggplot(datos_plot, aes(x = reorder(Pais, Esperanza_vida), y = Esperanza_vida, fill = Categoria)) +
        geom_col() +
        coord_flip() +
        labs(
          x = "País",
          y = "Esperanza de Vida (Años)",
          title = paste("Top y Bottom 5 Esperanza de Vida en", input$filtro_anio)
        ) +
        theme_minimal() +
        scale_fill_manual(values = c("Top 5" = "forestgreen", "Bottom 5" = "firebrick"))
      
      ggplotly(p)
    })
  
    # Grafico comparativo esperanza de vida masculina y femenina
    output$grafico_vida_mf <- renderPlotly({
      nivel <- input$nivel_filtro
      datos <- base %>% filter(Anio == input$filtro_anio)
      
      if (nivel == "Global") {
        df <- datos %>%
          summarise(
            Esperanza_masculino = mean(Esperanza_masculino, na.rm = TRUE),
            Esperanza_femenina = mean(Esperanza_femenina, na.rm = TRUE)
          ) %>%
          pivot_longer(cols = c("Esperanza_masculino", "Esperanza_femenina"), 
                       names_to = "Sexo", values_to = "Esperanza")
        
        p <- ggplot(df, aes(x = Sexo, y = Esperanza, fill = Sexo)) +
          geom_bar(stat = "identity") +
          labs(title = paste("Esperanza de Vida M/F - Nivel:", nivel),
               y = "Esperanza de vida (Años)") +
          theme_minimal() +
          scale_fill_manual(
            values = c("Esperanza_masculino" = "blue", "Esperanza_femenina" = "pink"),
            labels = c("Esperanza_masculino" = "Esperanza de vida masculina",
                       "Esperanza_femenina" = "Esperanza de vida femenina")
          ) +
          scale_x_discrete(
            labels = c("Esperanza_masculino" = "Masculina", "Esperanza_femenina" = "Femenina")
          )
        
      } else if (nivel == "Continente") {
        df <- datos %>%
          group_by(Continente) %>%
          summarise(
            Esperanza_masculino = mean(Esperanza_masculino, na.rm = TRUE),
            Esperanza_femenina = mean(Esperanza_femenina, na.rm = TRUE)
          ) %>%
          pivot_longer(cols = c("Esperanza_masculino", "Esperanza_femenina"), 
                       names_to = "Sexo", values_to = "Esperanza")
        
        p <- ggplot(df, aes(x = Continente, y = Esperanza, fill = Sexo)) +
          geom_col(position = "dodge") +
          labs(title = paste("Esperanza de Vida M/F - Nivel:", nivel),
               y = "Esperanza de vida (Años)") +
          theme_minimal() +
          scale_fill_manual(
            values = c("Esperanza_masculino" = "blue", "Esperanza_femenina" = "pink"),
            labels = c("Esperanza_masculino" = "Esperanza de vida masculina",
                       "Esperanza_femenina" = "Esperanza de vida femenina")
          )
        
      } else if (nivel == "Región") {
        datos <- datos %>%
          mutate(Region = case_when(
            Pais %in% regiones[["5 Tigres Asiáticos"]] ~ "5 Tigres Asiáticos",
            Pais %in% regiones[["Unión Europea"]] ~ "Unión Europea",
            Pais %in% regiones[["Sudamérica"]] ~ "Sudamérica",
            Pais %in% regiones[["América del Norte"]] ~ "América del Norte",
            TRUE ~ "Otra"
          ))
        
        df <- datos %>%
          filter(Region != "Otra") %>%
          group_by(Region) %>%
          summarise(
            Esperanza_masculino = mean(Esperanza_masculino, na.rm = TRUE),
            Esperanza_femenina = mean(Esperanza_femenina, na.rm = TRUE)
          ) %>%
          pivot_longer(cols = c("Esperanza_masculino", "Esperanza_femenina"), 
                       names_to = "Sexo", values_to = "Esperanza")
        
        p <- ggplot(df, aes(x = Region, y = Esperanza, fill = Sexo)) +
          geom_col(position = "dodge") +
          labs(title = paste("Esperanza de Vida M/F - Nivel:", nivel),
               y = "Esperanza de vida (Años)") +
          theme_minimal() +
          scale_fill_manual(
            values = c("Esperanza_masculino" = "blue", "Esperanza_femenina" = "pink"),
            labels = c("Esperanza_masculino" = "Esperanza de vida masculina",
                       "Esperanza_femenina" = "Esperanza de vida femenina")
          )
        
      } else if (nivel == "País") {
        pais_seleccionado <- input$filtro_pais
        
        if (is.null(pais_seleccionado) || pais_seleccionado == "") {
          return(plotly_empty() %>%
                   layout(title = "Selecciona un país para ver su comparación M/F"))
        }
        
        df <- datos %>%
          filter(Pais == pais_seleccionado) %>%
          summarise(
            Esperanza_masculino = mean(Esperanza_masculino, na.rm = TRUE),
            Esperanza_femenina = mean(Esperanza_femenina, na.rm = TRUE)
          ) %>%
          pivot_longer(cols = c("Esperanza_masculino", "Esperanza_femenina"),
                       names_to = "Sexo", values_to = "Esperanza")
        
        p <- ggplot(df, aes(x = Sexo, y = Esperanza, fill = Sexo)) +
          geom_col(width = 0.6) +
          labs(
            title = paste("Esperanza de Vida M/F en", pais_seleccionado),
            y = "Esperanza de vida (Años)"
          ) +
          theme_minimal() +
          scale_fill_manual(
            values = c("Esperanza_masculino" = "blue", "Esperanza_femenina" = "pink"),
            labels = c("Esperanza_masculino" = "Esperanza de vida masculina",
                       "Esperanza_femenina" = "Esperanza de vida femenina")
          ) +
          scale_x_discrete(
            labels = c("Esperanza_masculino" = "Masculina", "Esperanza_femenina" = "Femenina")
          )
      }
      
      ggplotly(p)
    })

    #Grafico Evolucion de variables 
    output$grafico_evolucion <- renderPlotly({
      variable <- input$variable_evolucion
      datos <- base
      
      # Diccionario de etiquetas
      etiquetas_y <- c(
        "Pib_pcap" = "PIB per cápita (USD)",
        "Mortalidad_infantil" = "Tasa de Mortalidad Infantil (número de niños fallecidos de 0-5 años)",
        "Tasa_fertilidad" = "Tasa de Fertilidad (bebés por mujer)",
        "Pobreza" = "Tasa de Pobreza (% de la población)",
        "Esperanza_vida" = "Esperanza de Vida (Años)"
      )
      
      # Filtros según tipo de ubicación
      if (input$tipo_ubicacion == "Global") {
        datos_fil <- datos
      } else if (input$tipo_ubicacion == "Continente") {
        if (input$filtro_continente == "Todos") {
          datos_fil <- datos
        } else {
          datos_fil <- datos %>% filter(Continente == input$filtro_continente)
        }
      } else if (input$tipo_ubicacion == "Región") {
        if (input$filtro_region == "Todos") {
          todos_paises_regiones <- unlist(regiones)
          datos_fil <- datos %>% filter(Pais %in% todos_paises_regiones)
        } else {
          paises_region <- regiones[[input$filtro_region]]
          datos_fil <- datos %>% filter(Pais %in% paises_region)
        }
      } else if (input$tipo_ubicacion == "País") {
        datos_fil <- datos %>% filter(Pais == input$filtro_pais)
      } else {
        datos_fil <- datos
      }
      
      # Agregación
      datos_agg <- datos_fil %>%
        group_by(Anio) %>%
        summarise(valor = mean(.data[[variable]], na.rm = TRUE)) %>%
        ungroup()
      
      # Etiqueta del eje Y
      ylab <- etiquetas_y[[variable]] %||% variable
      
      # Gráfico
      p <- ggplot(datos_agg, aes(x = Anio, y = valor)) +
        geom_line(color = "darkblue") +
        geom_point(color = "red") +
        labs(title = paste("Evolución de", ylab),
             x = "Año", y = ylab) +
        theme_minimal()
      
      ggplotly(p)
    })
    
    #Mapa Mundial Esperanza de vida 
    output$mapa_esperanza_vida <- renderPlotly({
      datos <- base %>% filter(Anio == input$filtro_anio)
      tipo <- input$tipo_ubicacion
      
      if (tipo == "Continente" && input$filtro_continente != "Todos") {
        datos <- datos %>% filter(Continente == input$filtro_continente)
      }
      
      if (tipo == "Región") {
        if (input$filtro_region == "Todos") {
          paises_regiones <- unlist(regiones)
          datos <- datos %>% filter(Pais %in% paises_regiones)
        } else {
          paises_region <- regiones[[input$filtro_region]]
          datos <- datos %>% filter(Pais %in% paises_region)
        }
      }
      
      if (tipo == "País") {
        datos <- datos %>% filter(Pais == input$filtro_pais)
      }
      
      if (nrow(datos) == 0) return(NULL)
      
      plot_ly(
        data = datos,
        type = 'choropleth',
        locations = ~Pais,
        locationmode = 'country names',
        z = ~Esperanza_vida,
        colorscale = 'Viridis',
        colorbar = list(title = "Esperanza de Vida"),
        text = ~paste(Pais, "<br>Esperanza de Vida:", round(Esperanza_vida, 1)),
        marker = list(line = list(color = 'rgb(255,255,255)', width = 0.5))
      ) %>%
        layout(
          geo = list(
            scope = 'world',
            projection = list(type = 'natural earth'),
            showland = TRUE,
            landcolor = 'rgb(217, 217, 217)',
            showcountries = TRUE,
            countrycolor = 'rgb(255, 255, 255)'
          )
        )
    })
    
    #Grafico de correlaciones entre variables
    output$matriz_correlacion <- renderPlotly({
      datos <- datos_filtrados()
      
      vars_cor <- c("Esperanza_vida", "Pib_pcap", "Mortalidad_infantil", "Tasa_fertilidad", "Pobreza")
      datos_cor <- datos %>% select(all_of(vars_cor)) %>% na.omit()
      
      if (nrow(datos_cor) < 2) return(NULL)
      
      corr_mat <- cor(datos_cor, use = "complete.obs")
      
      library(plotly)
      plot_ly(
        z = corr_mat,
        x = vars_cor,
        y = vars_cor,
        type = "heatmap",
        colors = colorRamp(c("blue", "white", "red"))
      )
    })
    
    #Estimacion regresion lineal 
    output$grafico_regresion <- renderPlotly({
      # Filtrado según ubicación y año
      datos <- base %>% filter(Anio == input$filtro_anio)
      
      if (input$tipo_ubicacion == "Continente" && input$filtro_continente != "Todos") {
        datos <- datos %>% filter(Continente == input$filtro_continente)
      } else if (input$tipo_ubicacion == "Región" && input$filtro_region != "Todos") {
        datos <- datos %>% filter(Pais %in% regiones[[input$filtro_region]])
      } else if (input$tipo_ubicacion == "País" && input$filtro_pais != "Todos") {
        datos <- datos %>% filter(Pais == input$filtro_pais)
      }
      
      # Selección de variables
      datos <- datos %>%
        select(Esperanza_vida, Mortalidad_infantil, Tasa_fertilidad, Pobreza, Pib_pcap, Pais) %>%
        drop_na()
      
      if (nrow(datos) < 5) {
        return(plot_ly() %>% layout(title = "No hay suficientes datos para regresión múltiple"))
      }
      
      # Modelo lineal
      modelo <- lm(Esperanza_vida ~ Mortalidad_infantil + Tasa_fertilidad + Pobreza + Pib_pcap, data = datos)
      datos$Predicho <- predict(modelo, newdata = datos)
      
      # Gráfico: valores reales vs predichos
      plot_ly(datos, x = ~Esperanza_vida, y = ~Predicho, type = "scatter", mode = "markers",
              text = ~paste("País:", Pais),
              marker = list(size = 8, color = 'rgba(0, 100, 200, 0.6)', line = list(width = 1))) %>%
        add_trace(x = ~Esperanza_vida, y = ~Esperanza_vida, mode = 'lines', name = "Línea ideal",
                  line = list(color = 'red', dash = 'dash')) %>%
        layout(title = paste("Regresión múltiple - Año", input$filtro_anio),
               xaxis = list(title = "Esperanza de vida (Años)"),
               yaxis = list(title = "Esperanza de vida predicha (Años)"),
               showlegend = FALSE)
    })
  }
  
  shinyApp(ui, server)
  
  