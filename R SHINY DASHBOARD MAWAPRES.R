library(rsconnect)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(shinythemes)
library(DT)
library(leaflet)
library(dplyr)
library(animation)
library(plotly)
library(cluster)
library(factoextra)
library(readxl)
library(pastecs)
library(psych)
library(PerformanceAnalytics)
library(QuantPsyc)
library(faoutlier)
library(FactoMineR)
library(factoextra)
library(scatterplot3d)
library(shinyStorePlus)
library(openxlsx)
library(NbClust)
library(leaflet.providers)
library(raster)
library(httr)    
set_config(use_proxy(url="10.3.100.207",port=8080))
getOption("timeout")
options(timeout=60)
setwd("C:/Users/acer/Downloads/ANNISA")

#-----------------------------------------------------UI

myui <- tagList(
  shinythemes::themeSelector(),
  navbarPage(
    "MalNutri.id",
    tabPanel("Pengenalan",
             sidebarPanel(
               h5(
                 icon("angle-right", style = "color: blue"),
                 strong("Dashboard MalNutri.id"),
                 style = "text-align: center;",
                 icon("angle-left", style = "color: blue")
               ),
               imageOutput("logo"),
               h6(
                 strong("MalNutri.id"),
                 "merupakan dashboard masalah malnutrisi di Indonesia dengan menyajikan informasi real-time terkait gizi dan kesehatan masyarakat.",
                 style = "text-align: justify;"
               ),
               hr(),
               
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Overview Tabel",
                          h2(strong("Overview Data Malnutrisi di Indonesia Tahun 2022"), align = "center"),
                          dataTableOutput("tabeldata"),
                 ),
                 tabPanel("Persebaran Peta",
                          h2(strong("Persebaran Peta Malnutrisi Seluruh Provinsi di Indonesia Tahun 2022"), align = "center"),
                          h5(icon("angle-right", style = "color: blue"),"Peta persebaran ini menunjukkan bahwa 
                             10 Provinsi dengan persentase terbesar dari kondisi Malnutrisi pada setiap variabelnya.", style="text-align: justify;"),
                          
                          selectInput("variabel", "Pilih Variabel:", choices = c("Prev Stunting", "Wasting", "Overweight", "Underweight", "Proporsi anak umur di bawah 5 tahun")),
                          leafletOutput("peta"),
                          h5(strong("Keterangan Variabel:")),
                          h5("Stunting (pendek): Stunting merujuk pada kondisi ketidakmampuan seorang anak untuk tumbuh sesuai dengan tinggi badan yang seharusnya pada usia tertentu. Ini dapat disebabkan oleh kekurangan gizi kronis, infeksi berulang, dan faktor-faktor lingkungan. (Sumber: UNICEF)", style="text-align: justify;"
                          ),
                          h5("Wasting (kurus): Wasting adalah keadaan ketika seseorang kehilangan berat badan dan massa tubuh dengan cepat, seringkali disebabkan oleh kekurangan gizi akut atau penyakit yang parah. (Sumber: WHO)", style="text-align: justify;"
                          ),
                          h5("Underweight (kurang berat badan): Underweight mengacu pada berat badan seseorang yang lebih rendah dari standar yang dianggap sehat untuk tinggi badan dan usia tertentu. Hal ini dapat disebabkan oleh berbagai faktor, termasuk kekurangan gizi dan penyakit kronis. (Sumber: WHO) ", style="text-align: justify;"),
                          h5("Overweight (berlebihan berat badan):Overweight terjadi ketika berat badan seseorang melebihi standar yang dianggap sehat untuk tinggi badan dan usia tertentu. Kelebihan berat badan biasanya disebabkan oleh asupan kalori yang berlebihan dan kurangnya aktivitas fisik. (Sumber: WHO)", style="text-align: justify;"),
                          h5("Proporsi Anak di Bawah Umur 5 Tahun: Proporsi Anak dibawah 5 tahun dari masa kelahirannya (Sumber: BPS)", style="text-align: justify;"),
                 ),
                 tabPanel("Visualisasi Statdes",
                          h2(strong("Visualisasi Statistika Desktiptif Malnutrisi Seluruh Provinsi di Indonesia Tahun 2022"), align = "center"),
                          h5(icon("angle-right", style = "color: blue"),"Adapun visualisasi statistika deskriptif sederhana terkait kondisi Malnutrisi pada setiap variabelnya.", style="text-align: justify;"),
                          selectInput("variabel2", "Pilih Variabel:", choices = c("Prev Stunting", "Wasting", "Overweight", "Underweight", "Proporsi anak umur di bawah 5 tahun")),
                          br(),
                          fluidRow(
                            column(6, uiOutput("statdes")),
                            column(6, uiOutput("statdes2"))
                          ),
                          fluidRow(
                            column(12,
                                   h3(strong("Boxplot Setiap Variabel Malnutrisi"), align = "center"),
                                   hr(),
                                   plotlyOutput("Boxplot"),
                                   p(),
                            ),
                            column(12,
                                   h3(strong("Pie Chart Jumlah Rata-rata Tingkat Pendapatan Pekerja di Indonesia"), align = "center"),
                                   hr(),
                                   plotlyOutput("pieChart"),
                            )
                          ),
                          
                          
                 )
               )
             )
    ),
    tabPanel("Analisis Multivariat",
             sidebarPanel(
               selectInput("SelectedMetode", "Pilih Metode:", choices = c(" ","K Optimum K Means"
                                                                          , "K-Means")),
               verbatimTextOutput("jumlahkk"),
               h5("Masukkan K optimum kemudian memilih metode K-Means"),
               numericInput("K","K Optimum untuk Metode K-Means", value=""),
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Informasi",
                          h2(strong("Informasi Terkait Metode Multivariat"), align = "center"),
                          h5(icon("angle-right", style = "color: blue"),"Analisis Metode Multivariat adalah analisis statistika yang menganalisis lebih dari dua variabel yang bersamaan.", style="text-align: justify;"),
                          h5(icon("angle-right", style = "color: red"),"Metode dalam analisis multivariat ada banyak, seperti Analisis Faktor (PCA), Analisis Klaster, Analisis Diskriminan, dan lainnya. Namun,
                             pada dashboard ini menggunakan", strong("Analisis Klaster."),style="text-align: justify;"),
                          h5(icon("angle-right", style = "color: red"), strong("Tujuan analisis klaster"),"dalam analisis malnutrisi di Indonesia adalah untuk mengidentifikasi dan memahami pola kelompok yang mungkin ada di antara wilayah-wilayah atau provinsi-provinsi di negara tersebut. Analisis klaster dapat memberikan wawasan yang berharga terkait karakteristik serupa atau pola kesamaan dalam tingkat malnutrisi antarprovinsi, membantu penyelidikan lebih lanjut, serta mendukung perumusan kebijakan dan intervensi yang lebih tepat dan terfokus.", style="text-align: justify;"),
                          h5(icon("angle-right", style = "color: green"),"Analisis klaster ada dua, Hierarki dan Non Hierarki. Namun, dalam dashboard ini menggunakan
                             ", strong("metode Non Hierarki (K-Means Cluster."), style="text-align: justify;"), 
                 ),
                 tabPanel("Hasil K-Means Optimum",
                          h2(strong("Hasil K-Optimum Klaster"), style = "text-align: center;"),
                          h5(icon("angle-right", style = "color: red"), 
                             "Penentuan K Optimum dengan Metode Silhouette, Garis putus-putus adalah K yang dipilih ", style="text-align: justify;"),
                          plotlyOutput("koptimum"),
                          p(),
                          verbatimTextOutput("jumlahk"),
                 ),
                 tabPanel("Hasil Analisis Klaster Dengan K-Means",
                          h2(strong("Hasil Analisis Klaster"), style = "text-align: center;"),
                          h5(icon("angle-right", style = "color: red"), 
                             "Penentuan K Optimum dengan Metode Silhouette, Garis putus-putus adalah K yang dipilih ", style="text-align: justify;"),
                          verbatimTextOutput("Clustering"),
                          plotlyOutput("koptimum2"),
                          h4("Detail Kode dan Nama Provinsi tertera pada Tabel berikut.", align="center"),
                          dataTableOutput("provinsii"),
                 )
               )
             )
    ),
    tabPanel("Pelayanan",
             sidebarPanel(
               textInput("nama", "Nama:"),
               textInput("provinsi", "Provinsi Kejadian:"),
               textInput("alamat", "Alamat Kejadian Lengkap:"),
               textInput("dinas", "Dinas Kesehatan:"),   
               dateInput("date", "Tanggal Kejadian:"),   
               textInput("teks", "Penjelasan Keluhan:"),
               textInput("link", "Masukkan Bukti Kejadian Link Gdrive Terlampir:"),
               actionButton("simpanButton", "Simpan"),
               actionButton("deleteButton", "Hapus"),
               downloadButton("unduhDataButton", "Unduh Data"),
               hr()
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Proses Input",
                          h2(strong("Hasil Input Keluhan terkait Malnutrisi di Indonesia"), style = "text-align: center;"),
                          h5(icon("angle-right", style = "color: blue"),"Tabel ini berisikan setelah menginput keluhan di setiap wilayah terkait kondisi kesehatan. ", style="text-align: justify;"),
                          h5(icon("angle-right", style = "color: red"),"Hapus Data digunakan untuk menghapus ketika terjadi kesalahan dalam menginput data. Caranya: Mengklik data yang terpilih kemudian klik", strong("Hapus Data"), style="text-align: justify;"),
                          h5(icon("angle-right", style = "color: red"),"Data juga bisa dilakukan untuk mengunduh data yang nantinya berupa format excel dengan mengeklik", strong("Unduh Data"), style="text-align: justify;"),
                          hr(),
                          DTOutput("tabelInput"),
                          fluidRow(
                            column(
                              width = 12,
                              align = "center", # Menggunakan align = "center" untuk memusatkan elemen di dalam kolom
                              uiOutput("stat")  # Output yang akan dihasilkan dan dicentangkan
                            )
                          ),
                 )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  
  
  
  # LOGO
  output$logo <- renderImage({
    list(src = "Logo Kreatif.png", width = "100%")
  }, deleteFile = FALSE)
  
  # Inisialisasi variabel untuk menyimpan data dari formulir
  dataInput <- reactiveVal(data.frame(
    Nama = character(), Provinsi = character(), Alamat = character(),
    Dinas = character(), Tanggal = as.Date(character(), format = "%Y-%m-%d"),
    Penjelasan = character(), Link = character()
  ))
  
  
  # Tentukan path file langsung
  file_path <- "Data Tersimpan.xlsx"
  

  
  # Fungsi untuk menyimpan data ke file
  saveData <- function(data) {
    write.xlsx(data, file_path, row.names = FALSE)
    new_data <<- data
  }
  
  # Fungsi untuk membaca data dari file
  loadData <- function() {
    if (file.exists(file_path)) {
      data <- read.xlsx(file_path)
      data$Tanggal <- as.Date(data$Tanggal, format = "%Y-%m-%d")
      return(data)
    } else {
      return(data.frame(Nama = character(), Provinsi = character(), Alamat = character(), Dinas = character(), Tanggal = as.Date(), Penjelasan = character(), Link = character()))
    }
  }
  
  
  # Menyimpan data baru ke dalam file
  new_data <- loadData()
  saveData(new_data)
  dataInput(new_data)
  
  observe({
    provinsiOptions <- unique(new_data$Provinsi)
    updateSelectInput(session, "provinsi", choices = c("", provinsiOptions), selected = input$provinsi)
  })
  
  observeEvent(input$simpanButton, {
    nama <- input$nama
    provinsi <- input$provinsi
    alamat <- input$alamat
    dinas <- input$dinas
    tanggal <- as.Date(input$date, format = "%Y-%m-%d", na.rm = TRUE, origin = "1970-01-01")
    teks <- input$teks
    link <- input$link
    
    newEntry <- data.frame(Nama = nama, Provinsi = provinsi, Alamat = alamat, Dinas = dinas, Tanggal = tanggal, Penjelasan = teks, Link = link)
    
    # Menambahkan data baru ke dalam reactiveVal
    dataInput(rbind(dataInput(), newEntry))
    
    # Menyimpan data baru ke dalam file
    saveData(dataInput())
  })
  
  observeEvent(input$deleteButton, {
    if (!is.null(input$tabelInput_rows_selected)) {
      selected_rows <- input$tabelInput_rows_selected
      # Hapus baris yang dipilih
      dataInput(dataInput()[-selected_rows, ])
    }
  })
  
  # Menampilkan tabel data dari formulir di "Proses Input"
  output$tabelInput <- renderDT({
    dataInput()
  })
  
  # Unduh data ke dalam file Excel
  output$unduhDataButton <- downloadHandler(
    filename = function() {
      paste("data_input_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(dataInput(), file, row.names = FALSE)
    }
  )
  
  output$stat <- renderUI({
    tags$head(
      tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css")
    )
    data <- dataInput()
    
    # Menghitung jumlah masing-masing provinsi
    province_counts <- table(data$Provinsi)
    
    # Menemukan provinsi dengan jumlah terbanyak
    max_province <- names(province_counts)[which.max(province_counts)]
    
    # Membuat teks yang akan ditampilkan di dalam box
    box_text <- paste(
      "Keluhan Malnutrisi yang paling banyak dibahas adalah Provinsi: ", max_province, "<br>",
      "Jumlah: ", province_counts[max_province], "<br>"
    )
    
    # Membuat box dengan teks hasil perhitungan
    box(
      title = "",
      status = "primary",
      solidHeader = TRUE,
      width = 6,
      div(
        style = "border: 2px solid #ddd; padding: 10px; border-radius: 10px; background-color: #fcf5c7; text-align: center; margin: 0 auto;
    font-weight: bold;",
        HTML(box_text)
      )
    )
  })
  
  
  
  #--------------------------------- BAGIAN OVERVIEW TABEL
  
  # Tabel data
  data <- reactive({
    file_path <- "Data R.xlsx"  # Replace with your actual file path
    data <- read_excel(file_path)
    return(data)
  })
  
  output$tabeldata <- renderDataTable({
    data1 <- data()
    data1
  })
  
  #PETA
  observe({
    data <- data()
    variables <- colnames(data)
    variables <- variables[!(variables %in% c("Tahun", "Id"))]
    variables2 <- variables[!(variables %in% c("Tahun", "Id"))]
    updateSelectInput(session, "selectedVariable", choices = variables)
  })
  
  # Modifikasi renderLeaflet untuk menampilkan peta Indonesia
  output$peta <- renderLeaflet({
    # Menyaring data berdasarkan variabel yang dipilih
    selected_variable <- input$variabel
    filtered_data <- data()
    
    # Menampilkan peta leaflet
    m <- leaflet() %>%
      addTiles() %>%
      setView(lng = 117.29, lat = -2.49, zoom = 5)  # Atur koordinat tengah peta
    
    # Check selected variable and customize mapping accordingly
    if (selected_variable == "Prev Stunting") {
      # Menampilkan 10 provinsi dengan tingkat "Prev Stunting" tertinggi
      top_provinces <- filtered_data %>%
        group_by(`Provinsi`, `Longitude`, `Latitude`) %>%
        summarize(prev_stunting_max = max(`Prev Stunting`, na.rm = TRUE)) %>%
        arrange(desc(prev_stunting_max)) %>%
        head(10)
      
      m <- m %>% addCircleMarkers(
        data = top_provinces,
        lng = ~`Longitude`,
        lat = ~`Latitude`,
        popup = ~paste("Provinsi:", `Provinsi`, "<br>Prev Stunting:", sprintf("%.2f%%", prev_stunting_max)),
        label = ~paste("Provinsi:", `Provinsi`),
        color = "blue",
        fillOpacity = 0.8,
        radius = 10
      )
    } else if (selected_variable == "Wasting") {
      # Handle Wasting variable mapping
      top_provinces <- filtered_data %>%
        group_by(`Provinsi`, `Longitude`, `Latitude`) %>%
        summarize(wasting_max = max(`Wasting`, na.rm = TRUE)) %>%
        arrange(desc(wasting_max)) %>%
        head(10)
      
      m <- m %>% addCircleMarkers(
        data = top_provinces,
        lng = ~`Longitude`,
        lat = ~`Latitude`,
        popup = ~paste("Provinsi:", `Provinsi`, "<br>Wasting:", sprintf("%.2f%%", wasting_max)),
        label = ~paste("Provinsi:", `Provinsi`),
        color = "green",
        fillOpacity = 0.8,
        radius = 10
      )
    } else if (selected_variable == "Overweight") {
      # Handle Overweight variable mapping
      top_provinces <- filtered_data %>%
        group_by(`Provinsi`, `Longitude`, `Latitude`) %>%
        summarize(over_max = max(`Overweight`, na.rm = TRUE)) %>%
        arrange(desc(over_max)) %>%
        head(10)
      
      m <- m %>% addCircleMarkers(
        data = top_provinces,
        lng = ~`Longitude`,
        lat = ~`Latitude`,
        popup = ~paste("Provinsi:", `Provinsi`, "<br>Overweight:", sprintf("%.2f%%", over_max)),
        label = ~paste("Provinsi:", `Provinsi`),
        color = "red",
        fillOpacity = 0.8,
        radius = 10
      )
    } else if (selected_variable == "Underweight") {
      # Handle Underweight variable mapping
      top_provinces <- filtered_data %>%
        group_by(`Provinsi`, `Longitude`, `Latitude`) %>%
        summarize(under_max = max(`Underweight`, na.rm = TRUE)) %>%
        arrange(desc(under_max)) %>%
        head(10)
      
      m <- m %>% addCircleMarkers(
        data = top_provinces,
        lng = ~`Longitude`,
        lat = ~`Latitude`,
        popup = ~paste("Provinsi:", `Provinsi`, "<br>Underweight:", sprintf("%.2f%%", under_max)),
        label = ~paste("Provinsi:", `Provinsi`),
        color = "purple",
        fillOpacity = 0.8,
        radius = 10
      )
    } else if (selected_variable == "Proporsi anak umur di bawah 5 tahun") {
      # Handle Proporsi anak variable mapping
      top_provinces <- filtered_data %>%
        group_by(`Provinsi`, `Longitude`, `Latitude`) %>%
        summarize(prop_max = max(`Proporsi anak umur di bawah 5 tahun`, na.rm = TRUE)) %>%
        arrange(desc(prop_max)) %>%
        head(10)
      
      m <- m %>% addCircleMarkers(
        data = top_provinces,
        lng = ~`Longitude`,
        lat = ~`Latitude`,
        popup = ~paste("Provinsi:", `Provinsi`, "<br>Proporsi anak umur di bawah 5 tahun:", sprintf("%.2f%%", prop_max)),
        label = ~paste("Provinsi:", `Provinsi`),
        color = "yellow",
        fillOpacity = 0.8,
        radius = 10
      )
    }
    
    return(as.list(m))
  })
  
  #----------------------------------------- BAGIAN STATDES --------------------
  
  output$Boxplot <- renderPlotly({
    req(input$variabel2)
    data <- data()
    
    # Get the selected variable
    selected_var <- input$variabel2
    
    # Boxplot for the selected variable
    boxplot_data <- data.frame(Group = "X", Value = data[[selected_var]])
    boxplot_title <- paste("Variabel", selected_var)
    
    plot_ly(data = boxplot_data, x = ~Group, y = ~Value, type = 'box',
            marker = list(color = 'rgba(7,40,89,0.5)', outliercolor = 'rgba(219, 64, 82, 0.6)')) %>%
      layout(title = boxplot_title,
             xaxis = list(title = ""),
             yaxis = list(title = "Nilai"))
  })
  
  
  output$statdes <- renderUI({
    req(input$variabel2)
    
    data <- data()
    selected_variable <- input$variabel2
    
    max_var <- data %>%
      summarise(Maxvar = max(.data[[selected_variable]], na.rm = TRUE),
                CountryMax = which.max(.data[[selected_variable]]))
    
    if (!is.finite(max_var$Maxvar)) {
      result <- "Tidak ada data yang valid untuk variabel ini."
    } else {
      country_max_index <- max_var$CountryMax
      country_max <- data$`Provinsi`[country_max_index]
      
      # Custom formatting based on the variable
      if (selected_variable == "Proporsi anak umur di bawah 5 tahun") {
        result <- paste("Variabel: ", selected_variable, "<br>",
                        "Nilai Maksimum : ", max_var$Maxvar, " persen", "<br>",
                        "Provinsi: ", country_max)
      } else {
        result <- paste("Variabel: ", selected_variable, "<br>",
                        "Nilai Maksimum : ", max_var$Maxvar, "%", "<br>",
                        "Provinsi: ", country_max)
      }
    }
    
    div(
      style = "border: 2px solid #ddd; padding: 10px; border-radius: 10px; background-color: #fcf5c7; text-align: center; margin: 0 auto;
    font-weight: bold;",
      HTML(result))
  })
  
  output$statdes2 <- renderUI({
    req(input$variabel2)
    
    data <- data()
    selected_variable <- input$variabel2
    
    min_var <- data %>%
      summarise(Minvar = min(.data[[selected_variable]], na.rm = TRUE),
                CountryMin = which.min(.data[[selected_variable]]))
    
    if (!is.finite(min_var$Minvar)) {
      result <- "Tidak ada data yang valid untuk variabel ini."
    } else {
      country_min_index <- min_var$CountryMin
      country_min <- data$`Provinsi`[country_min_index]
      
      # Custom formatting based on the variable
      if (selected_variable == "Jumlah Populasi Anak Kurang dari 5 Tahun") {
        result <- paste("Variabel: ", selected_variable, "<br>",
                        "Nilai Minimum : ", min_var$Minvar, " persen", "<br>",
                        "Provinsi: ", country_min)
      } else {
        result <- paste("Variabel: ", selected_variable, "<br>",
                        "Nilai Minimum : ", min_var$Minvar, "%", "<br>",
                        "Provinsi: ", country_min)
      }
    }
    
    div(
      style = "border: 2px solid #ddd; padding: 10px; border-radius: 10px; background-color: #fcf5c7; text-align: center; margin: 0 auto;
    font-weight: bold;",
      HTML(result))
  })
  
  # Render pie chart
  output$pieChart <- renderPlotly({
    data <- data()
    
    # Hitung Kategori Pendapatan
    category_counts <- data %>%
      group_by(`Tingkat Pendapatan Rata rata`) %>%
      summarise(count = n())
    
    # Buat pie chart
    pie_chart <- plot_ly(category_counts, labels = ~`Tingkat Pendapatan Rata rata`, values = ~count, type = "pie",
                         marker = list(colors = brewer.pal(12, "Set3")),
                         text = ~paste(`Tingkat Pendapatan Rata rata`, "<br>", "Jumlah: ", count),
                         hoverinfo = "text+percent")
    
    # Konfigurasi layout
    layout <- list(title = "Pie Chart Kategori Pendapatan",
                   showlegend = TRUE,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    # Tampilkan pie chart
    pie_chart %>% layout(layout)
  })
  
  #--------------------------------------- ANALISIS KLASTER
  output$Clustering <- renderPrint({
    data <- data()
    variables <- colnames(data)
    
    # Exclude DATA
    variables <- variables[!(variables %in% c("Id", "Tahun","Tingkat Pendapatan Rata rata", "Provinsi", "Latitude", "Longitude"))]
    selected_data <- data[, variables, drop = FALSE]
    Datastand <- scale(selected_data)
    
    if (input$SelectedMetode == "K-Means"){
      result <- kmeans(Datastand, input$K)
      result
    }
  })
  
  output$koptimum <- renderPlotly({
    data <- data()
    variables <- colnames(data)
    
    # Exclude "Negara" and "Kategori Pendapatan" from variables
    variables <- variables[!(variables %in% c("Id", "Tahun","Tingkat Pendapatan Rata rata", "Provinsi", "Latitude", "Longitude"))]
    selected_data <- data[, variables, drop = FALSE]
    Datastand <- scale(selected_data)
    
    if (input$SelectedMetode == "K Optimum K Means") {
      print("Penentuan K Optimum dengan Metode Silhouette, Garis putus-putus adalah K yang dipilih ")
      result=fviz_nbclust(Datastand, kmeans, method = "silhouette")
      result
    } 
  })
  
  output$jumlahk <- renderPrint({
    data <- data()
    variables <- colnames(data)
    
    # Exclude "Negara" and "Kategori Pendapatan" from variables
    variables <- variables[!(variables %in% c("Id", "Tahun","Tingkat Pendapatan Rata rata", "Provinsi", "Latitude", "Longitude"))]
    selected_data <- data[, variables, drop = FALSE]
    Datastand <- scale(selected_data)
    if (input$SelectedMetode == "K Optimum K Means") {
      # Use fviz_nbclust to visualize and find the optimal number of clusters (k)
      # Hitung matriks jarak menggunakan metode Euclidean
      diss_matrix <- dist(Datastand, method = "euclidean", diag = FALSE)
      
      # Hitung nilai optimal k menggunakan metode Ward dengan indeks CH
      res <- NbClust(Datastand, diss = diss_matrix, distance = NULL, min.nc = 2, max.nc = 6, 
                     method = "ward.D", index = "ch")
      
      # Dapatkan nilai optimal k
      optimal_k <- res$Best.nc[1]
      
      # Print nilai optimal k
      cat("K Optimum adalah sebesar:", optimal_k, "\n")
    } 
  })
  
  output$jumlahkk <- renderPrint({
    data <- data()
    variables <- colnames(data)
    
    # Exclude "Negara" and "Kategori Pendapatan" from variables
    variables <- variables[!(variables %in% c("Id", "Tahun","Tingkat Pendapatan Rata rata", "Provinsi", "Latitude", "Longitude"))]
    selected_data <- data[, variables, drop = FALSE]
    Datastand <- scale(selected_data)
    if (input$SelectedMetode == "K Optimum K Means" || input$SelectedMetode == "K-Means")
    {
      # Use fviz_nbclust to visualize and find the optimal number of clusters (k)
      # Hitung matriks jarak menggunakan metode Euclidean
      diss_matrix <- dist(Datastand, method = "euclidean", diag = FALSE)
      
      # Hitung nilai optimal k menggunakan metode Ward dengan indeks CH
      res <- NbClust(Datastand, diss = diss_matrix, distance = NULL, min.nc = 2, max.nc = 6, 
                     method = "ward.D", index = "ch")
      
      # Dapatkan nilai optimal k
      optimal_k <- res$Best.nc[1]
      
      # Print nilai optimal k
      cat("Jadi masukkan K Optimum sebesar:", optimal_k, "\n")
    } 
  })
  
  output$provinsii <- renderDataTable({
    data <- data()
    
    selected_columns <- c("Provinsi")
    result_data <- data[, selected_columns, drop = FALSE]
    
    
    # Tampilkan data dalam dataTable
    if (input$SelectedMetode == "K-Means"){
      datatable(result_data, options = list(scrollX = TRUE))
    }
  })
  
  output$koptimum2 <- renderPlotly({
    data <- data()
    variables <- colnames(data)
    
    # Exclude "Negara" and "Kategori Pendapatan" from variables
    variables <- variables[!(variables %in% c("Id", "Tahun","Tingkat Pendapatan Rata rata", "Provinsi", "Latitude", "Longitude"))]
    selected_data <- data[, variables, drop = FALSE]
    Datastand <- scale(selected_data)
    
    if (input$SelectedMetode == "K-Means") {
      final<-kmeans(Datastand, input$K)
      fviz_cluster(final, data = Datastand)
    } 
  })
}

shinyApp(ui = myui, server = server)
