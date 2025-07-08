# Dashboard Iklim dan Bencana Alam Provinsi Jawa Tengah 2024

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(plotly)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(corrplot)
library(viridis)
library(RColorBrewer)
library(lubridate)
library(shinycssloaders)
library(shinyWidgets)
library(readr)
library(stringr)
library(reshape2)
library(forecast)
library(glmnet)

# ===== PENGATURAN AWAL =====
bulan_names <- c("Januari", "Februari", "Maret", "April", "Mei", "Juni",
                 "Juli", "Agustus", "September", "Oktober", "November", "Desember")
bulan_to_num_map <- setNames(1:12, toupper(bulan_names))


# ===== LOAD DATA & SHAPEFILE =====
tryCatch({
  jateng_shp <- st_read("data/peta_jateng.geojson", quiet = TRUE)
}, error = function(e) {
  stop("Gagal memuat shapefile. Pastikan path file benar. Pesan error: ", e$message)
})

climate_data_raw <- read_csv("data/ClimateChange.csv", show_col_types = FALSE)

climate_data <- climate_data_raw %>%
  rename(
    Nama_Bulan_Teks_Raw = BULAN,
    Jumlah_Bencana_Iklim = `JUMLAH BENCANA [Y]`,
    Suhu = `SUHU [X1]`,
    Kelembapan = `KELEMBAPAN [X2]`,
    Kecepatan_Angin = `KECEPATAN ANGIN [X3]`,
    Curah_Hujan = `CURAH HUJAN [X4]`
  ) %>%
  mutate(
    Nama_Bulan_Teks_Clean = str_to_title(tolower(Nama_Bulan_Teks_Raw)),
    BULAN = as.numeric(bulan_to_num_map[toupper(Nama_Bulan_Teks_Raw)]),
    Nama_Bulan = factor(Nama_Bulan_Teks_Clean, levels = bulan_names)
  )

bencana_data_raw <- read_csv("data/Bencana.csv", show_col_types = FALSE)

# --- 3. Proses Agregasi Data Bencana ---
disaster_data <- bencana_data_raw %>%
  rename(
    Nama_Bulan_Teks_Raw = BULAN
  ) %>%
  mutate(
    Nama_Bulan_Teks_Clean = str_to_title(tolower(Nama_Bulan_Teks_Raw)),
    BULAN = as.numeric(bulan_to_num_map[toupper(Nama_Bulan_Teks_Raw)]),
    Nama_Bulan = factor(Nama_Bulan_Teks_Clean, levels = bulan_names)
  ) %>%
  group_by(BULAN, Nama_Bulan, BENCANA) %>%
  summarise(Jumlah = n(), .groups = 'drop') %>%
  rename(Jenis_Bencana = BENCANA)

# Agregasi per Kabupaten (Untuk peta dan tabel)
disaster_location <- bencana_data_raw %>%
  mutate(
    Kabupaten_Clean = toupper(KABUPATEN),
    Kabupaten_Clean = str_replace(Kabupaten_Clean, "KABUPATEN ", ""),
    Kabupaten_Clean = str_replace(Kabupaten_Clean, "KOTA ", "")
  ) %>%
  group_by(Kabupaten_Clean, BENCANA) %>%
  summarise(Total_Kejadian = n(), .groups = 'drop') %>%
  rename(Kabupaten = Kabupaten_Clean, Jenis_Bencana = BENCANA)

# Daftar Kabupaten Jawa Tengah
kabupaten_jateng <- unique(disaster_location$Kabupaten)

# ===== UI =====
ui <- dashboardPage(
  skin = "black",
  
  # Header
  dashboardHeader(
    title = tagList(
      icon("cloud-rain"),
      "Dashboard Iklim & Bencana Jateng 2024"
    ),
    titleWidth = 350
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar",
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Analisis Perubahan Iklim", tabName = "iklim",  icon = icon("thermometer-half")),
      menuItem("Analisis Bencana Alam", tabName = "bencana", icon = icon("exclamation-triangle")),
      menuItem("Peta Bencana", tabName = "peta", icon = icon("map")),
      menuItem("Ringkasan Statistik", tabName = "statistik", icon = icon("chart-bar")),
      menuItem("Prediksi Bencana", tabName = "prediksi", icon = icon("chart-line")),
      menuItem("Metodologi & Sumber Data", tabName = "info", icon = icon("book")),
      
      # Filter Data MenuItem
      menuItem("🔧 Data Filter",
               tabPanel("Filter Data",
                        div(
                          style = "padding: 15px; margin-top: 20px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 10px; margin-left: 10px; margin-right: 10px;",
                          h4("Filter Data", style = "color: white; margin-bottom: 15px;"),
                          
                          selectInput("filter_bulan",
                                      label = div(style = "color: white;", "Pilih Bulan:"),
                                      choices = c("Semua Bulan" = "all", setNames(1:12, bulan_names)),
                                      selected = "all"),
                          
                          selectInput("filter_bencana",
                                      label = div(style = "color: white;", "Jenis Bencana:"),
                                      choices = c("Semua" = "all", unique(disaster_data$Jenis_Bencana)),
                                      selected = "all")
                        )
               )
      )
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .content-wrapper, .right-side {
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
      }
      .box {
        border-radius: 15px;
        box-shadow: 0 8px 25px rgba(0,0,0,0.1);
        border: none;
        transition: transform 0.3s ease, box-shadow 0.3s ease;
      }
      .box:hover {
        transform: translateY(-5px);
        box-shadow: 0 15px 35px rgba(0,0,0,0.15);
      }
      .small-box {
        border-radius: 15px;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        transition: transform 0.3s ease;
      }
      .small-box:hover {
        transform: translateY(-3px);
      }
    "))
    ),
    
    tabItems(
      # TAB BERANDA
      tabItem(tabName = "beranda",
              fluidRow(
                column(12,
                       div(
                         style = "text-align: center; padding: 30px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border-radius: 20px; margin-bottom: 30px;",
                         h1("🌦️ Dashboard Iklim dan Bencana Alam", style = "font-size: 2.5em; margin-bottom: 15px;"),
                         h2("Provinsi Jawa Tengah Tahun 2024", style = "font-size: 1.8em; font-weight: 300; margin-bottom: 20px;"),
                         p("Analisis Komprehensif Data Cuaca, Iklim, dan Kejadian Bencana Alam", style = "font-size: 1.2em; opacity: 0.9;")
                       )
                )
              ),
              
              # Summary Cards
              fluidRow(
                valueBoxOutput("total_bencana", width = 3),
                valueBoxOutput("suhu_rata", width = 3),
                valueBoxOutput("curah_hujan_total", width = 3),
                valueBoxOutput("bulan_tertinggi", width = 3)
              ),
              
              fluidRow(
                column(width = 7,
                       box(
                         title = "📊 Ringkasan Eksekutif", status = "primary", solidHeader = TRUE,
                         width = NULL,
                         height = 450, 
                         div(
                           style = "padding: 20px; font-size: 16px; line-height: 1.8;",
                           h4("🎯 Tujuan Dashboard", style = "color: #2c3e50; margin-bottom: 15px;"),
                           p("Dashboard ini menyajikan analisis komprehensif tentang kondisi iklim dan pola bencana alam di Provinsi Jawa Tengah Tahun 2024. Di mana, ini dirancang untuk membantu pemahaman hubungan antara parameter iklim dengan kejadian bencana alam."),
                           
                           h4("Fitur Utama:", style = "color: #2c3e50; margin-top: 25px; margin-bottom: 15px;"),
                           tags$ul(
                             tags$li("📈 Visualisasi interaktif data iklim dan jumlah bencana bulanan"),
                             tags$li("🗺️ Peta distribusi bencana per kabupaten/kota"),
                             tags$li("📊 Analisis deskriptif dan inferensia"),
                             tags$li("🔮 Model prediksi dan analisis trend")
                           )
                         )
                       )
                ), 
                
                column(width = 5,
                       box(
                         title = "▶️ Tutorial Penggunaan", status = "primary", solidHeader = TRUE,
                         width = NULL,
                         height = 450,
                         
                         tags$iframe(
                           width = "100%", 
                           height = "360",
                           src = "", 
                           frameborder = "0", 
                           allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
                           allowfullscreen = TRUE
                         )
                       )
                       
                ) 
              ) 
      ),
      
      # TAB ANALISIS IKLIM
      tabItem(tabName = "iklim",
              fluidRow(
                box(
                  title = "🌡️ Parameter Iklim Bulanan", status = "primary", solidHeader = TRUE,
                  width = 12,
                  tabsetPanel(
                    tabPanel("Suhu & Kelembapan",
                             fluidRow(
                               column(6, withSpinner(plotlyOutput("plot_suhu"))),
                               column(6, withSpinner(plotlyOutput("plot_kelembapan")))
                             )
                    ),
                    tabPanel("Angin & Curah Hujan",
                             fluidRow(
                               column(6, withSpinner(plotlyOutput("plot_angin"))),
                               column(6, withSpinner(plotlyOutput("plot_hujan")))
                             )
                    ),
                    tabPanel("Trend Gabungan",
                             fluidRow(plotlyOutput("plot_climate_combined", height = "500px"))
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📊 Statistik Deskriptif Iklim", status = "info", solidHeader = TRUE,
                  width = 6,
                  withSpinner(DT::DTOutput("climate_stats"))
                ),
                box(
                  title = "🔥 Insight Iklim", status = "warning", solidHeader = TRUE,
                  width = 6,
                  div(
                    style = "padding: 15px; font-size: 14px;",
                    h4("📈 Temuan Utama:"),
                    uiOutput("climate_insights")
                  )
                )
              )
      ),
      
      # TAB ANALISIS BENCANA
      tabItem(tabName = "bencana",
              fluidRow(
                box(
                  title = "⚠️ Distribusi Bencana Alam", status = "danger", solidHeader = TRUE,
                  width = 12,
                  tabsetPanel(
                    tabPanel("Per Jenis Bencana",
                             fluidRow(plotlyOutput("plot_bencana_jenis", height = "400px"))
                    ),
                    tabPanel("Per Bulan",
                             fluidRow(plotlyOutput("plot_bencana_bulan", height = "400px"))
                    ),
                    tabPanel("Heatmap Bencana",
                             fluidRow(plotlyOutput("heatmap_bencana", height = "500px"))
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "🏘️ Top 10 Kabupaten Terdampak", status = "danger", solidHeader = TRUE,
                  width = 6,
                  withSpinner(plotlyOutput("plot_top_kabupaten"))
                ),
                box(
                  title = "📊 Statistik Bencana", status = "info", solidHeader = TRUE,
                  width = 6,
                  withSpinner(DT::DTOutput("disaster_stats"))
                )
              )
      ),
      
      # TAB PETA INTERAKTIF
      tabItem(tabName = "peta",
              fluidRow(
                box(
                  title = "🗺️ Peta Distribusi Bencana Jawa Tengah", status = "success", solidHeader = TRUE,
                  width = 12, height = 600,
                  withSpinner(leafletOutput("peta_bencana", height = "500px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "📍 Detail Lokasi", status = "info", solidHeader = TRUE,
                  width = 12,
                  withSpinner(DT::DTOutput("table_lokasi"))
                )
              )
      ),
      
      # TAB STATISTIK & KORELASI
      tabItem(tabName = "statistik",
              fluidRow(
                box(
                  title = "🔗 Analisis Korelasi", status = "primary", solidHeader = TRUE,
                  width = 6,
                  withSpinner(plotOutput("correlation_plot", height = "500px"))
                ),
                box(
                  title = "🎯 Scatter Plot Korelasi Terkuat", status = "warning", solidHeader = TRUE,
                  width = 6,
                  withSpinner(plotlyOutput("strongest_correlation_scatterplot", height = "500px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "🧮 Descriptive Statistics", status = "success", solidHeader = TRUE,
                  width = 6,
                  withSpinner(verbatimTextOutput("descriptive_stats"))
                ),
                box(
                  title = "✅ Ridge Regression Analysis", status = "success", solidHeader = TRUE,
                  width = 6,
                  withSpinner(verbatimTextOutput("ridge_regression_output"))
                )
              ),
              
              fluidRow(
                box(
                  title = "💡 Insight Regresi", status = "info", solidHeader = TRUE,
                  width = 12,
                  div(style = "padding: 15px; font-size: 14px; line-height: 1.8;",
                      uiOutput("ridge_regression_insights")
                  )
                )
              )
      ),
      
      # TAB PREDIKSI & TREND
      tabItem(tabName = "prediksi",
              fluidRow(
                box(
                  title = "🔮 Model Prediksi Bencana", status = "primary", solidHeader = TRUE,
                  width = 12,
                  tabsetPanel(
                    tabPanel("Time Series Forecast",
                             fluidRow(plotlyOutput("forecast_plot", height = "400px"))
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📊 Model Performance (Metode ETS)", status = "info", solidHeader = TRUE,
                  width = 6,
                  withSpinner(verbatimTextOutput("model_summary"))
                ),
                box(
                  title = "🎯 Prediksi 3 Bulan Kedepan", status = "warning", solidHeader = TRUE,
                  width = 6,
                  withSpinner(DT::DTOutput("prediction_table"))
                )
              )
      ),
      
      # TAB INFO
      tabItem(tabName = "info",
              fluidRow(
                box(
                  title = "Metodologi dan Sumber Data", status = "primary", solidHeader = TRUE,
                  width = 12,
                  div(
                    style = "padding: 20px; font-size: 15px; line-height: 1.8;",
                    h3("📋 Metodologi Penelitian", style = "color: #2c3e50;"),
                    h4("🔍 Sumber Data:"),
                    tags$ul(
                      tags$li("Data Iklim: BMKG (Badan Meteorologi, Klimatologi, dan Geofisika)"),
                      tags$li("Data Bencana: BNPB (Badan Nasional Penanggulangan Bencana)"),
                      tags$li("Peta Shapefile: Geoportal Indonesia")
                    ),
                    
                    h4("📊 Metode Analisis:", style = "margin-top: 25px;"),
                    tags$ul(
                      tags$li("Statistik Deskriptif: Mean, Median, Standar Deviasi"),
                      tags$li("Analisis Korelasi: Pearson Correlation"),
                      tags$li("Ridge Regression: Pengaruh Antar Variabel "),
                      tags$li("Forecasting: Error, Trend, Seasonal(ETS – Multiplicative Additive None)")
                    ),
                    
                    h4("🛠️ Tools dan Library:", style = "margin-top: 25px;"),
                    tags$ul(
                      tags$li("R (Statistical Computing) dan RStudio sebagai lingkungan pengembangan."),
                      tags$li("Shiny Framework untuk membangun dashboard web interaktif."),
                      tags$li("Paket pendukung seperti Plotly, Leaflet, dan DT untuk visualisasi dan interaktivitas.")
                    ),
                    
                    h4("👨‍🎓 Tim Pengembang:", style = "margin-top: 25px;"),
                    # Menggunakan tabel agar lurus
                    tags$table(style = "width: 100%; font-size: 16px; margin-left: 20px;",
                               tags$tr(
                                 tags$td("Kaylla Zahrani", style = "padding-bottom: 5px;"),
                                 tags$td("(222313161)", style = "padding-left: 10px; padding-bottom: 5px;")
                               ),
                               tags$tr(
                                 tags$td("M. Faruq Hafidzullah E.", style = "padding-bottom: 5px;"),
                                 tags$td("(222313186)", style = "padding-left: 10px; padding-bottom: 5px;")
                               ),
                               tags$tr(
                                 tags$td("Rizkyana Azka Akhiria Ramadhanti", style = "padding-bottom: 5px;"),
                                 tags$td("(222313354)", style = "padding-left: 10px; padding-bottom: 5px;")
                               )
                    ),
                    
                    div(
                      style = "margin-top: 30px; padding: 15px; background: #f8f9fa; border-left: 4px solid #007bff; border-radius: 5px;",
                      h4("⚠️ Disclaimer:", style = "color: #007bff;"),
                      p("Dashboard ini mencakup penyederhanaan kompleksitas visual maupun analisis untuk kemudahan pengembangan, sehingga bila terdapat bagian yang tidak sesuai dengan harapan mohon hubungi tim pengembang untuk mendapatkan informasi penjelas.")
                    )
                  )
                )
              )
      )
    )
  )
)

# ===== SERVER =====
server <- function(input, output, session) {
  
  filtered_climate <- reactive({
    data <- climate_data
    if (input$filter_bulan != "all") {
      data <- data[data$BULAN == as.numeric(input$filter_bulan), ]
    }
    return(data)
  })
  
  filtered_disaster <- reactive({
    data <- disaster_data
    if (input$filter_bulan != "all") {
      data <- data[data$BULAN == as.numeric(input$filter_bulan), ]
    }
    if (input$filter_bencana != "all") {
      data <- data[data$Jenis_Bencana == input$filter_bencana, ]
    }
    return(data)
  })
  
  filtered_location <- reactive({
    data <- disaster_location
    if (input$filter_bencana != "all") {
      data <- data[data$Jenis_Bencana == input$filter_bencana, ]
    }
    data %>%
      group_by(Kabupaten) %>%
      summarise(Total_Bencana = sum(Total_Kejadian, na.rm = TRUE), .groups = 'drop')
  })
  
  peta_data_final <- reactive({
    peta_data <- filtered_location() %>%
      mutate(Kabupaten = as.character(Kabupaten))
    
    peta_shp_clean <- jateng_shp %>%
      mutate(
        Kabupaten = str_trim(str_replace_all(toupper(NAMOBJ), c("KABUPATEN " = "", "KOTA " = ""))),
        Kabupaten = as.character(Kabupaten)
      )
    
    peta_shp_clean %>%
      left_join(peta_data, by = "Kabupaten") %>%
      mutate(
        Total_Bencana = ifelse(is.na(Total_Bencana), 0, Total_Bencana)
      )
  })
  
  
  #----- VALUE BOXES -----
  output$total_bencana <- renderValueBox({
    total <- sum(climate_data$Jumlah_Bencana_Iklim, na.rm = TRUE)
    valueBox(
      value = formatC(total, format = "d", big.mark = ","),
      subtitle = "Total Kejadian Bencana (dari Iklim)",
      icon = icon("exclamation-triangle"), color = "red"
    )
  })
  
  output$suhu_rata <- renderValueBox({
    avg_temp <- round(mean(climate_data$Suhu, na.rm = TRUE), 1)
    valueBox(
      value = paste0(avg_temp, "°C"),
      subtitle = "Rata-rata Suhu Tahunan",
      icon = icon("thermometer-half"), color = "orange"
    )
  })
  
  output$curah_hujan_total <- renderValueBox({
    total_rain <- sum(climate_data$Curah_Hujan, na.rm = TRUE)
    valueBox(
      value = paste0(formatC(total_rain, format="d", big.mark=","), " mm"), 
      subtitle = "Total Curah Hujan Tahunan",
      icon = icon("cloud-rain"),
      color = "blue"
    )
  })
  
  output$bulan_tertinggi <- renderValueBox({
    disaster_monthly_climate <- climate_data %>%
      group_by(BULAN, Nama_Bulan) %>%
      summarise(Total_Jumlah = sum(Jumlah_Bencana_Iklim, na.rm = TRUE), .groups = 'drop')
    
    if(nrow(disaster_monthly_climate) == 0) {
      return(valueBox(value = "N/A", subtitle = "Data Bulan Tertinggi Tidak Tersedia", icon = icon("calendar-alt"), color = "purple"))
    }
    
    max_month_data <- disaster_monthly_climate %>%
      filter(Total_Jumlah == max(Total_Jumlah, na.rm = TRUE)) %>%
      head(1)
    
    month_name <- as.character(max_month_data$Nama_Bulan)
    valueBox(
      value = month_name,
      subtitle = paste0("Bulan Bencana Tertinggi (", max_month_data$Total_Jumlah, " kejadian)"),
      icon = icon("calendar-alt"), color = "purple"
    )
  })
  
  
  #----- PLOTS IKLIM -----
  output$plot_suhu <- renderPlotly({
    data <- filtered_climate()
    if(nrow(data) == 0) return()
    
    p <- ggplot(data, aes(x = Nama_Bulan, y = Suhu)) +
      geom_line(aes(group = 1), color = "#e74c3c", size = 1.2) +
      geom_point(color = "#e74c3c", size = 3) +
      labs(title = "Rata-rata Suhu Bulanan", x = "Bulan", y = "Suhu (°C)") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$plot_kelembapan <- renderPlotly({
    data <- filtered_climate()
    if(nrow(data) == 0) return()
    
    p <- ggplot(data, aes(x = Nama_Bulan, y = Kelembapan)) +
      geom_area(aes(group = 1), fill = "#3498db", alpha = 0.6) +
      geom_line(aes(group = 1), color = "#2980b9", size = 1.2) +
      labs(title = "Rata-rata Kelembapan Bulanan", x = "Bulan", y = "Kelembapan (%)") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$plot_angin <- renderPlotly({
    data <- filtered_climate()
    if(nrow(data) == 0) return()
    
    p <- ggplot(data, aes(x = Nama_Bulan, y = Kecepatan_Angin)) +
      geom_col(fill = "#9b59b6", alpha = 0.8) +
      labs(title = "Rata-rata Kecepatan Angin", x = "Bulan", y = "Kecepatan (km/h)") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$plot_hujan <- renderPlotly({
    data <- filtered_climate()
    if(nrow(data) == 0) return()
    
    p <- ggplot(data, aes(x = Nama_Bulan, y = Curah_Hujan)) +
      geom_col(fill = "#1abc9c", alpha = 0.8) +
      labs(title = "Curah Hujan Bulanan", x = "Bulan", y = "Curah Hujan (mm)") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$plot_climate_combined <- renderPlotly({
    data <- filtered_climate()
    if(nrow(data) == 0) return()
    
    data_normalized <- data
    data_normalized$Suhu_norm <- (data$Suhu - min(data$Suhu, na.rm = TRUE)) / (max(data$Suhu, na.rm = TRUE) - min(data$Suhu, na.rm = TRUE)) * 100
    data_normalized$Kelembapan_norm <- (data$Kelembapan - min(data$Kelembapan, na.rm = TRUE)) / (max(data$Kelembapan, na.rm = TRUE) - min(data$Kelembapan, na.rm = TRUE)) * 100
    data_normalized$Angin_norm <- (data$Kecepatan_Angin - min(data$Kecepatan_Angin, na.rm = TRUE)) / (max(data$Kecepatan_Angin, na.rm = TRUE) - min(data$Kecepatan_Angin, na.rm = TRUE)) * 100
    data_normalized$Hujan_norm <- (data$Curah_Hujan - min(data$Curah_Hujan, na.rm = TRUE)) / (max(data$Curah_Hujan, na.rm = TRUE) - min(data$Curah_Hujan, na.rm = TRUE)) * 100
    
    plot_ly(data_normalized, x = ~Nama_Bulan) %>%
      add_trace(y = ~Suhu_norm, name = "Suhu", type = "scatter", mode = "lines+markers", line = list(color = "#e74c3c")) %>%
      add_trace(y = ~Kelembapan_norm, name = "Kelembapan", type = "scatter", mode = "lines+markers", line = list(color = "#3498db")) %>%
      add_trace(y = ~Angin_norm, name = "Kec. Angin", type = "scatter", mode = "lines+markers", line = list(color = "#9b59b6")) %>%
      add_trace(y = ~Hujan_norm, name = "Curah Hujan", type = "scatter", mode = "lines+markers", line = list(color = "#1abc9c")) %>%
      layout(
        title = "Trend Parameter Iklim Bulanan (Dinormalisasi)",
        xaxis = list(title = "Bulan"),
        yaxis = list(title = "Nilai Normalisasi (0-100)"),
        hovermode = "x"
      )
  })
  
  
  # ===== PLOTS BENCANA =====
  output$plot_bencana_jenis <- renderPlotly({
    data <- filtered_disaster()
    if(nrow(data) == 0) return()
    
    disaster_summary <- aggregate(Jumlah ~ Jenis_Bencana, data, sum)
    if(nrow(disaster_summary) == 0) return()
    
    disaster_summary <- disaster_summary[order(disaster_summary$Jumlah, decreasing = FALSE), ]
    
    num_colors <- nrow(disaster_summary)
    
    if (num_colors > 0) {
      color_palette <- RColorBrewer::brewer.pal(n = max(3, num_colors), name = "Paired")[1:num_colors]
    } else {
      color_palette <- c()
    }
    
    plot_ly(disaster_summary,
            x = ~Jumlah,
            y = ~reorder(Jenis_Bencana, Jumlah), 
            type = "bar",
            orientation = "h", 
            marker = list(color = color_palette), 
            text = ~paste("Jumlah:", Jumlah),
            textposition = "auto") %>%
      layout(
        title = "Total Kejadian Bencana per Jenis",
        xaxis = list(title = "Jumlah Kejadian"),
        yaxis = list(title = ""), 
        showlegend = FALSE 
      )
  })
  
  output$plot_bencana_bulan <- renderPlotly({
    data <- filtered_disaster()
    if(nrow(data) == 0) return()
    
    monthly_summary <- aggregate(Jumlah ~ BULAN + Nama_Bulan, data, sum)
    monthly_summary <- monthly_summary[order(monthly_summary$BULAN), ]
    
    plot_ly(monthly_summary,
            x = ~Nama_Bulan,
            y = ~Jumlah,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "#e74c3c", width = 3),
            marker = list(color = "#c0392b", size = 8),
            hovertemplate = ~paste("<b>Bulan:</b> %{x}<br>",
                                   "<b>Total Kejadian:</b> %{y}<extra></extra>")) %>%
      layout(
        title = "Trend Kejadian Bencana Bulanan",
        xaxis = list(title = "Bulan"),
        yaxis = list(title = "Total Kejadian")
      )
  })
  
  output$heatmap_bencana <- renderPlotly({
    data <- filtered_disaster()
    if(nrow(data) < 2) return()
    
    heatmap_data <- reshape2::dcast(data, Jenis_Bencana ~ Nama_Bulan, value.var = "Jumlah", fun.aggregate = sum, fill = 0)
    rownames(heatmap_data) <- heatmap_data$Jenis_Bencana
    heatmap_data$Jenis_Bencana <- NULL
    heatmap_matrix <- as.matrix(heatmap_data)
    
    plot_ly(
      z = ~heatmap_matrix,
      x = colnames(heatmap_matrix),
      y = rownames(heatmap_matrix),
      type = "heatmap",
      colorscale = "Reds",
      showscale = TRUE
    ) %>%
      layout(
        title = "Heatmap Kejadian Bencana (Jenis vs Bulan)",
        xaxis = list(title = "Bulan"),
        yaxis = list(title = "Jenis Bencana")
      )
  })
  
  output$plot_top_kabupaten <- renderPlotly({
    top_kabupaten <- disaster_location %>%
      group_by(Kabupaten) %>%
      summarise(Total = sum(Total_Kejadian, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(10)
    
    if(nrow(top_kabupaten) == 0) return()
    
    plot_ly(top_kabupaten,
            y = ~reorder(Kabupaten, Total),
            x = ~Total,
            type = "bar",
            orientation = "h",
            marker = list(color = "#e74c3c")) %>%
      layout(
        title = "Top 10 Kabupaten/Kota Terdampak Bencana",
        xaxis = list(title = "Total Kejadian"),
        yaxis = list(title = "")
      )
  })
  
  
  #----- PETA INTERAKTIF -----
  output$peta_bencana <- renderLeaflet({
    data_untuk_peta <- peta_data_final()
    if(nrow(data_untuk_peta) == 0) return()
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = data_untuk_peta$Total_Bencana
    )
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Total Bencana: %d",
      data_untuk_peta$NAMOBJ, data_untuk_peta$Total_Bencana
    ) %>% lapply(htmltools::HTML)
    
    leaflet(data_untuk_peta) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 110.2, lat = -7.2, zoom = 8) %>%
      addPolygons(
        fillColor = ~pal(Total_Bencana),
        weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px", direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal, values = ~Total_Bencana, opacity = 0.7, title = "Jumlah Bencana",
        position = "bottomright"
      )
  })
  
  
  #----- TABEL DATA -----
  output$climate_stats <- DT::renderDT({
    summary_data <- summary(climate_data[, c("Suhu", "Kelembapan", "Kecepatan_Angin", "Curah_Hujan")])
    DT::datatable(as.data.frame(summary_data), options = list(pageLength = 10, scrollX = TRUE), rownames = TRUE)
  })
  
  output$disaster_stats <- DT::renderDT({
    disaster_summary <- disaster_data %>%
      group_by(Jenis_Bencana) %>%
      summarise(
        Total_Kejadian = sum(Jumlah, na.rm = TRUE),
        Rata_rata_Bulanan = mean(Jumlah, na.rm = TRUE),
        Bulan_Tertinggi = ifelse(n() > 0, Nama_Bulan[which.max(Jumlah)], NA_character_),
        .groups = 'drop'
      )
    DT::datatable(disaster_summary,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE,
                  colnames = c("Jenis Bencana", "Total Kejadian", "Rata-rata/Bulan", "Bulan Puncak")) %>%
      DT::formatRound(columns = 3, digits = 1)
  })
  
  output$table_lokasi <- DT::renderDT({
    location_summary <- disaster_location %>%
      group_by(Kabupaten) %>%
      summarise(
        Total_Bencana = sum(Total_Kejadian, na.rm = TRUE),
        Jenis_Dominan = ifelse(n() > 0, Jenis_Bencana[which.max(Total_Kejadian)], NA_character_),
        Jumlah_Dominan = ifelse(n() > 0, max(Total_Kejadian, na.rm = TRUE), 0),
        .groups = 'drop'
      ) %>%
      arrange(desc(Total_Bencana))
    
    DT::datatable(location_summary,
                  options = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE,
                  colnames = c("Kabupaten/Kota", "Total Bencana", "Jenis Bencana Dominan", "Jumlah Dominan"))
  })
  
  
  output$climate_insights <- renderUI({
    max_temp_month <- climate_data$Nama_Bulan[which.max(climate_data$Suhu)]
    min_temp_month <- climate_data$Nama_Bulan[which.min(climate_data$Suhu)]
    max_rain_month <- climate_data$Nama_Bulan[which.max(climate_data$Curah_Hujan)]
    min_rain_month <- climate_data$Nama_Bulan[which.min(climate_data$Curah_Hujan)]
    
    tagList(
      tags$ul(
        tags$li(paste("Suhu tertinggi terjadi pada bulan", max_temp_month, "(", max(climate_data$Suhu), "°C)")),
        tags$li(paste("Suhu terendah terjadi pada bulan", min_temp_month, "(", min(climate_data$Suhu), "°C)")),
        tags$li(paste("Curah hujan tertinggi pada bulan", max_rain_month, "(", max(climate_data$Curah_Hujan), "mm)")),
        tags$li(paste("Curah hujan terendah pada bulan", min_rain_month, "(", min(climate_data$Curah_Hujan), "mm)"))
      )
    )
  })
  
  
  #----- ANALISIS STATISTIK -----
  analysis_data <- reactive({
    data <- climate_data %>%
      select(Jumlah = Jumlah_Bencana_Iklim, Suhu, Kelembapan, Kecepatan_Angin, Curah_Hujan)
    na.omit(data)
  })
  
  output$correlation_plot <- renderPlot({
    cor_data <- analysis_data()
    if(nrow(cor_data) < 2) return()
    
    cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
    
    corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
             tl.cex = 0.8, tl.col = "black", addCoef.col = "black", number.cex = 0.7,
             col = colorRampPalette(c("#D55E00", "white", "#0072B2"))(100))
  })
  
  strongest_predictor <- reactive({
    cor_data <- analysis_data()
    if (nrow(cor_data) < 2) return(NULL)
    
    cor_matrix <- cor(cor_data)
    cor_y <- cor_matrix["Jumlah", -which(colnames(cor_matrix) == "Jumlah")]
    strongest_var <- names(which.max(abs(cor_y)))
    return(strongest_var)
  })
  
  output$strongest_correlation_scatterplot <- renderPlotly({
    var_name <- strongest_predictor()
    if (is.null(var_name)) return()
    
    data <- analysis_data()
    
    formula <- as.formula(paste("Jumlah ~", var_name))
    model <- lm(formula, data = data)
    
    plot_ly(data, x = ~get(var_name), y = ~Jumlah) %>%
      add_markers(name = "Data Aktual", color = I("#0072B2")) %>%
      add_lines(y = ~fitted(model), name = "Garis Tren", color = I("#D55E00")) %>%
      layout(
        title = paste("Hubungan antara", var_name, "dan Jumlah Bencana"),
        xaxis = list(title = var_name),
        yaxis = list(title = "Jumlah Bencana")
      )
  })
  
  ridge_model_results <- reactive({
    reg_data <- analysis_data()
    if (nrow(reg_data) < 2) return(NULL)
    
    x <- as.matrix(reg_data[, -which(names(reg_data) == "Jumlah")])
    y <- reg_data$Jumlah
    
    set.seed(42)
    cv_ridge <- cv.glmnet(x, y, alpha = 0)
    best_lambda <- cv_ridge$lambda.min
    
    final_ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda)
    
    return(list(model = final_ridge, coefs = coef(final_ridge), lambda = best_lambda))
  })
  
  output$ridge_regression_output <- renderPrint({
    results <- ridge_model_results()
    if (is.null(results)) {
      cat("Data tidak cukup untuk analisis regresi.")
      return()
    }
    
    cat("=== Koefisien Model Ridge Regression ===\n\n")
    cat("Lambda terbaik (dipilih melalui CV):", round(results$lambda, 4), "\n\n")
    print(results$coefs)
  })
  
  output$ridge_regression_insights <- renderUI({
    results <- ridge_model_results()
    if (is.null(results)) return()
    
    coefs <- as.matrix(results$coefs)
    coefs <- coefs[-1, , drop = FALSE] # Menghapus intercept
    
    if (nrow(coefs) == 0) return()
    
    insights <- tagList(
      tags$h4("Apa yang dapat disimpulkan?", style = "font-weight:bold;"),
      
      HTML(paste0(
        "<blockquote>",
        "Berdasarkan model Ridge Regression dengan optimasi lambda <strong>", 
        round(results$lambda, 2), 
        "</strong>, ditemukan bahwa suhu memiliki pengaruh negatif paling kuat terhadap jumlah bencana. ",
        "Artinya, semakin tinggi suhu, maka kecenderungan terjadinya bencana justru menurun. ",
        "Sebaliknya, kelembapan dan kecepatan angin memiliki pengaruh positif, meskipun tidak terlalu besar.",
        "</blockquote>"
      )),
      
      tags$h5("Apa artinya ini?"),
      tags$ul(
        tags$li(
          tags$span("🧊 ", tags$strong("Suhu:"), 
                    " Setiap kenaikan suhu 1 derajat berpotensi menurunkan jumlah bencana sebanyak ", 
                    tags$strong("4 kejadian"),".")
        ),
        tags$li(
          tags$span("💧 ", tags$strong("Kelembapan dan curah hujan:"), 
                    " tetap memberi kontribusi terhadap bencana, namun tidak sebesar yang diperkirakan sebelumnya. ",
                    "Ini bisa jadi karena curah hujan sudah tercermin secara tidak langsung melalui kelembapan.")
        ),
        tags$li(
          tags$span("🌬️ ", tags$strong("Kecepatan angin:"), 
                    " punya efek positif, tapi relatif kecil.")
        )
      ),
      
      tags$p(
        "Model ini digunakan untuk mengurangi gangguan antar variabel (multikolinearitas), sehingga diharapkan hasil yang diperoleh lebih stabil dan bisa diandalkan sebagai dasar pengambilan keputusan."
      )
    )
    return(insights)
  })
  
  output$descriptive_stats <- renderPrint({
    data <- analysis_data()
    if (is.null(data) || nrow(data) == 0) {
      cat("Data tidak tersedia untuk statistik deskriptif.")
      return()
    }
    cat("=== Statistik Deskriptif untuk Data Iklim dan Bencana Alam ===\n\n")
    summary(data)
  })
  
  #----- PREDIKSI & FORECASTING -----
  ets_forecast_result <- reactive({
    monthly_data_for_forecast <- climate_data %>%
      group_by(BULAN) %>%
      summarise(Total_Jumlah = sum(Jumlah_Bencana_Iklim, na.rm = TRUE), .groups = 'drop') %>%
      arrange(BULAN)
    
    if(nrow(monthly_data_for_forecast) < 4) { 
      return(NULL)
    }
    
    ts_data <- ts(monthly_data_for_forecast$Total_Jumlah, frequency = 12, start = c(2024, 1))
    
    ets_model <- ets(ts_data, model = "MAN")
    
    forecast(ets_model, h = 3)
  })
  
  output$forecast_plot <- renderPlotly({
    forecast_obj <- ets_forecast_result()
    if (is.null(forecast_obj)) {
      return(plot_ly() %>% layout(title = "Data tidak cukup untuk forecasting menggunakan ETS"))
    }
    
    hist_data <- data.frame(
      Periode = as.numeric(time(forecast_obj$x)),
      Jumlah = as.numeric(forecast_obj$x)
    )
    
    forecast_data <- data.frame(
      Periode = as.numeric(time(forecast_obj$mean)),
      Jumlah = as.numeric(forecast_obj$mean),
      BatasBawah = as.numeric(forecast_obj$lower[,2]),
      BatasAtas = as.numeric(forecast_obj$upper[,2])
    )
    
    connector_data <- rbind(
      tail(hist_data, 1),
      forecast_data[1, c("Periode", "Jumlah")]
    )
    
    p <- ggplot() +
      geom_ribbon(data = forecast_data, aes(x = Periode, ymin = BatasBawah, ymax = BatasAtas), fill = "#56B4E9", alpha = 0.3) +
      geom_line(data = hist_data, aes(x = Periode, y = Jumlah, color = "Data Historis"), size = 1) +
      geom_line(data = forecast_data, aes(x = Periode, y = Jumlah, color = "Prediksi"), size = 1, linetype = "dashed") +
      geom_line(data = connector_data, aes(x = Periode, y = Jumlah), color = "#D55E00", size = 1, linetype = "dashed") +
      labs(
        title = "Forecasting Kejadian Bencana dengan Metode ETS (3 Bulan Kedepan)",
        x = "Periode",
        y = "Prediksi Jumlah Bencana",
        color = "Legenda"
      ) +
      scale_color_manual(name = "Legenda", values = c("Data Historis" = "#0072B2", "Prediksi" = "#D55E00")) +
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  output$model_summary <- renderPrint({
    forecast_obj <- ets_forecast_result()
    if (is.null(forecast_obj)) {
      cat("Data tidak cukup untuk membuat model ETS.")
      return()
    }
    
    cat("=== MODEL FORECASTING ETS ===\n")
    cat("Prediksi Bencana berdasarkan Pola Data Historis\n\n")
    print(summary(forecast_obj$model))
    cat("\n\n=== UKURAN AKURASI MODEL ===\n")
    print(accuracy(forecast_obj))
    
  })
  
  output$prediction_table <- DT::renderDT({
    forecast_obj <- ets_forecast_result()
    if (is.null(forecast_obj)) {
      return(datatable(data.frame(Pesan = "Data tidak cukup untuk melakukan prediksi.")))
    }
    
    tingkat_risiko <- function(jumlah_bencana) {
      if (is.na(jumlah_bencana)) "Tidak Diketahui"
      else if (jumlah_bencana >= 25) "Tinggi"
      else if (jumlah_bencana >= 15) "Sedang"
      else "Rendah"
    }
    
    future_predictions <- data.frame(
      Periode = paste("Prediksi Bulan ke-", 1:3),
      Prediksi_Bencana = round(as.numeric(forecast_obj$mean), 0),
      "Batas_Bawah_95_Persen" = round(as.numeric(forecast_obj$lower[,2]), 0),
      "Batas_Atas_95_Persen" = round(as.numeric(forecast_obj$upper[,2]), 0)
    )
    
    future_predictions$Tingkat_Risiko <- sapply(future_predictions$Prediksi_Bencana, tingkat_risiko)
    
    DT::datatable(future_predictions,
                  options = list(pageLength = 3, dom = 't', scrollX = TRUE),
                  rownames = FALSE,
                  colnames = c("Periode", "Prediksi Bencana", "Batas Bawah (95%)", "Batas Atas (95%)", "Tingkat Risiko"))
  })
  
}

# ===== RUN APP =====
shinyApp(ui = ui, server = server)