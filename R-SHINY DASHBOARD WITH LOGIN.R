library(rsconnect)
library(shinyauthr) #fiture login
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(shinythemes)
library(tidyverse)
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
library(ggrepel)
library(leaflet.extras)
library(shinymanager)
getOption("timeout")
options(timeout=60)
setwd("C:/Users/acer/Downloads/SEMESTER 8/BISMILLAH PORTOFOLIO PROJEK/DASHBOARD MALNUTRISI")

#-----------------------------------------------------UI
#---------------------------kategorik
variabless <- read_excel("Data R.xlsx")
variabless <- variabless[, !(names(variabless) %in% c("Provinsi", "Tahun", "Id", "Longitude", "Latitude"))]
colnames(variabless) <- trimws(colnames(variabless))
kategorik_teks <- sapply(variabless, is.character)
kategorik_teks <- names(kategorik_teks[kategorik_teks])
#----------------------------numerik
variables <- read_excel("Data R.xlsx")
columns_to_exclude <- c("Provinsi", "Tahun", "Id", "Longitude", "Latitude")
variables <- variables[, !(names(variables) %in% columns_to_exclude)]
numeric_data <- variables[sapply(variables, is.numeric)]
# Get the names of numeric columns
numeric_data <- names(numeric_data)

# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("statbis", "a"),
  password = sapply(c("statbis", "a"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)


myui <- fluidPage(
  # login section
  shinyauthr::loginUI(id = "login"),
  
  # Sidebar to show user info after login
  uiOutput("sidebarpanel"),
  
  
)




server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  
  output$sidebarpanel <- renderUI({
    req(credentials()$user_auth)
    tagList(
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
                              
                              selectInput("variabel", "Pilih Variabel Numerik:", choices = numeric_data),                          
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
                              selectInput("variabel2", "Pilih Variabel Numerik:", choices = numeric_data),
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
                                selectInput("variabel3", "Pilih Variabel Kategorik:", choices = kategorik_teks),
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
                              h4("Hasil Pengelompokkan Provinsi dari Analisis Klaster", align="center"),
                              leafletOutput("koptimum2Map"),
                     ),
                     tabPanel("Karakteristik Klaster",
                              h2(strong("Hasil Karakteristik Klaster"), style = "text-align: center;"),
                              h5(icon("angle-right", style = "color: blue"), 
                                 "Karakteristik klaster digunakan untuk mengetahui kecenderungan segi mana dari setiap klasternya berdasarkan variabel
                             ", style="text-align: justify;"),
                              dataTableOutput("karakter"),
                              h3(strong("Interpretasi Karakteristik Klaster"), style = "text-align: center;"),
                              h5(icon("angle-right", style = "color: blue"), 
                                 "Rata-rata maksimum dari setiap variabelnya, sehingga Klaster mana yang memiliki dampak serius dari kasus malnutrisi
                             ", style="text-align: justify;"),
                              verbatimTextOutput("variable_info"),
                     )
                   )
                 )
        ),
        tabPanel("Pelayanan",
                 sidebarPanel(
                   textInput("nama", "Nama:"),
                   selectInput("provinsi", "Provinsi Kejadian:", choices = c(
                     "Aceh", "Bali", "Bangka Belitung", "Banten", "Bengkulu", "Gorontalo", 
                     "DKI Jakarta", "Jambi", "Jawa Barat", "Jawa Tengah", "Jawa Timur", 
                     "Kalimantan Barat", "Kalimantan Selatan", "Kalimantan Tengah", "Kalimantan Timur", 
                     "Kalimantan Utara", "Kepulauan Riau", "Lampung", "Maluku", "Maluku Utara", 
                     "Nusa Tenggara Barat", "Nusa Tenggara Timur", "Papua", "Papua Barat", 
                     "Riau", "Sulawesi Barat", "Sulawesi Selatan", "Sulawesi Tengah", "Sulawesi Tenggara", 
                     "Sulawesi Utara", "Sumatera Barat", "Sumatera Selatan", "Sumatera Utara"
                   )),
                   textInput("alamat", "Alamat Kejadian Lengkap:"),
                   textInput("dinas", "Dinas Kesehatan:"),   
                   h5(strong("Format Tanggal:"),"Hari-Bulan-Tahun (DD-MM-YYYY), seperti: 01-01-2001 (Hari 2 digit, Bulan 2 digit, dan Tahun 4 Digit)", style="text-align: justify;"),
                   textInput("date", "Tanggal Kejadian:"),
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
        ),
        tabPanel("Keluar",
                 sidebarPanel(
                   h3(strong("Silahkan Klik Tombol Keluar!"), style="text-align: justify;"),
                   actionButton("logoutButton", "Keluar"),
                   h5(icon("angle-right", style = "color: blue"),"Terima kasih telah menggunakan dashboard 
               Malnutrisi ini!", style="text-align: justify;"),
                 )
        )
      )
    )
  })
  observeEvent(input$logoutButton, {
    # Perform any necessary logout logic here
    
    # Close the app
    stopApp()
  })
  # LOGO
  
  output$logo <- renderImage({
    list(src = "Logo Kreatif.png", width = "100%")
  }, deleteFile = FALSE)
  
  
  
  
  # Inisialisasi variabel untuk menyimpan data dari formulir
  dataInput <- reactiveValues(
    data = data.frame(
      Nama = character(), Provinsi = character(), Alamat = character(),
      Dinas = character(), Tanggal = character(),
      Penjelasan = character(), Link = character()
    )
  )
  
  
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
      return(data)
    } else {
      return(data.frame(Nama = character(), Provinsi = character(), Alamat = character(), 
                        Dinas = character(), Tanggal = character(), Penjelasan = character(), Link = character()))
    }
  }
  
  
  # Menyimpan data baru ke dalam file
  new_data <- loadData()
  saveData(new_data)
  dataInput$data <- new_data
  
  
  observeEvent(input$simpanButton, {
    nama <- input$nama
    provinsi <- input$provinsi
    alamat <- input$alamat
    dinas <- input$dinas
    tanggal <- as.character(input$date)  # Mengambil nilai dari textInput
    teks <- input$teks
    link <- input$link
    
    # Pastikan bahwa tanggal memiliki format yang sesuai
    if (!is.null(tanggal) && grepl("^\\d{2}-\\d{2}-\\d{4}$", tanggal)) {
      newEntry <- data.frame(Nama = nama, Provinsi = provinsi, Alamat = alamat, Dinas = dinas, Tanggal = tanggal, Penjelasan = teks, Link = link)
      
      # Menambahkan data baru ke dalam reactiveValues
      dataInput$data <- rbind(dataInput$data, newEntry)
      
      # Menyimpan data baru ke dalam file
      saveData(dataInput$data)
    } else {
      # Tampilkan pesan kesalahan jika format tanggal tidak sesuai
      showModal(modalDialog(
        title = "Error",
        "Format tanggal tidak valid. Harap gunakan format dd-mm-yyy, seperti: 01-01-2001.",
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$deleteButton, {
    if (!is.null(input$tabelInput_rows_selected)) {
      selected_rows <- input$tabelInput_rows_selected
      # Hapus baris yang dipilih
      dataInput$data <- dataInput$data[-selected_rows, ]
      # Menyimpan data baru ke dalam file
      saveData(dataInput$data)
    }
  })
  
  # Menampilkan tabel data dari formulir di "Proses Input"
  output$tabelInput <- renderDT({
    print(dataInput$data$Tanggal)  # Cetak tanggal untuk debugging
    dataInput$data
  })
  
  # Unduh data ke dalam file Excel
  output$unduhDataButton <- downloadHandler(
    filename = function() {
      paste("data_input_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(dataInput$data, file, row.names = FALSE)
    }
  )
  
  output$stat <- renderUI({
    tags$head(
      tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css")
    )
    data <- dataInput$data
    
    # Menghitung jumlah masing-masing provinsi
    province_counts <- table(data$Provinsi)
    
    # Menemukan provinsi dengan jumlah terbanyak
    max_province <- names(province_counts)[which.max(province_counts)]
    
    # Membuat teks yang akan ditampilkan di dalam box
    box_text <- paste(
      "Keluhan Malnutrisi yang paling banyak dibahas adalah Provinsi: ", max_province, "<br>",
      "Jumlah: ", province_counts[max_province], "<br>"
    )
    
    # Menampilkan teks dalam box
    HTML(box_text)
    
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
  
  output$peta <- renderLeaflet({
    # Menyaring data berdasarkan variabel yang dipilih
    selected_variable <- input$variabel
    filtered_data <- data()
    
    # Menampilkan peta leaflet
    m <- leaflet() %>%
      addTiles() %>%
      setView(lng = 117.29, lat = -2.49, zoom = 5)  # Atur koordinat tengah peta
    
    # Check selected variable and customize mapping accordingly
    if (selected_variable %in% names(filtered_data)) {
      # Menampilkan 10 provinsi dengan tingkat variabel tertinggi
      top_provinces <- filtered_data %>%
        group_by(`Provinsi`, `Longitude`, `Latitude`) %>%
        summarize(var_max = max(!!sym(selected_variable), na.rm = TRUE)) %>%
        arrange(desc(var_max)) %>%
        head(10)
      
      m <- m %>% addCircleMarkers(
        data = top_provinces,
        lng = ~`Longitude`,
        lat = ~`Latitude`,
        popup = ~paste("Provinsi:", `Provinsi`, "<br>", selected_variable, ":", sprintf("%.2f%%", var_max)),
        label = ~paste("Provinsi:", `Provinsi`),
        color = "blue",
        fillOpacity = 0.8,
        radius = 10
      )
    }
    
    return(as.list(m))
  })
  
  
  # ----------------------------------------- BAGIAN STATDES --------------------
  
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
      if (selected_variable %in% c("Proporsi anak umur di bawah 5 tahun")) {
        result <- paste("Variabel: ", selected_variable, "<br>",
                        "Nilai Maksimum : ", max_var$Maxvar, " %", "<br>",
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
      if (selected_variable %in% c("Proporsi anak umur di bawah 5 tahun")) {
        result <- paste("Variabel: ", selected_variable, "<br>",
                        "Nilai Minimum : ", min_var$Minvar, " %", "<br>",
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
  
  output$pieChart <- renderPlotly({
    data <- data()
    variabless <- data[, !(names(data) %in% c("Provinsi", "Tahun", "Id", "Longitude", "Latitude"))]
    colnames(variabless) <- trimws(colnames(variabless))
    kategorik_teks <- sapply(variabless, is.character)
    
    # Pilih variabel kategorikal (dalam contoh ini, yang memiliki tipe data karakter)
    
    selected_var <- names(kategorik_teks[kategorik_teks])  
    
    # Hitung Kategori
    category_counts <- data %>%
      group_by(.data[[selected_var]]) %>%
      summarise(count = n())
    
    # Buat pie chart
    pie_chart <- plot_ly(category_counts, labels = ~.data[[selected_var]], values = ~count, type = "pie",
                         marker = list(colors = brewer.pal(12, "Set3")),
                         text = ~paste(.data[[selected_var]], "<br>", "Jumlah: ", count),
                         hoverinfo = "text+percent")
    
    # Konfigurasi layout
    layout <- list(title = paste("Pie Chart Kategori ", selected_var),
                   showlegend = TRUE,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    # Tampilkan pie chart
    pie_chart %>% layout(layout)
  })
  
  #--------------------------------------- ANALISIS KLASTER
  output$Clustering <- renderPrint({
    data <- data()
    # Exclude specific columns
    columns_to_exclude <- c("Provinsi", "Tahun", "Id", "Longitude", "Latitude")
    variables <- data[, !(names(data) %in% columns_to_exclude)]
    
    numeric_data <- variables[, sapply(variables, is.numeric)]
    numeric_column_names <- names(numeric_data)
    selected_data <- data[, numeric_column_names, drop = FALSE]
    Datastand <- scale(selected_data)
    
    
    if (input$SelectedMetode == "K-Means"){
      result <- kmeans(Datastand, input$K)
      result
    }
  })
  
  output$koptimum <- renderPlotly({
    data <- data()
    # Exclude specific columns
    columns_to_exclude <- c("Provinsi", "Tahun", "Id", "Longitude", "Latitude")
    variables <- data[, !(names(data) %in% columns_to_exclude)]
    
    numeric_data <- variables[, sapply(variables, is.numeric)]
    numeric_column_names <- names(numeric_data)
    selected_data <- data[, numeric_column_names, drop = FALSE]
    Datastand <- scale(selected_data)
    
    if (input$SelectedMetode == "K Optimum K Means") {
      print("Penentuan K Optimum dengan Metode Silhouette, Garis putus-putus adalah K yang dipilih ")
      result=fviz_nbclust(Datastand, kmeans, method = "silhouette")
      result
    } 
  })
  
  output$jumlahk <- renderPrint({
    data <- data()
    # Exclude specific columns
    columns_to_exclude <- c("Provinsi", "Tahun", "Id", "Longitude", "Latitude")
    variables <- data[, !(names(data) %in% columns_to_exclude)]
    
    numeric_data <- variables[, sapply(variables, is.numeric)]
    numeric_column_names <- names(numeric_data)
    selected_data <- data[, numeric_column_names, drop = FALSE]
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
    # Exclude specific columns
    columns_to_exclude <- c("Provinsi", "Tahun", "Id", "Longitude", "Latitude")
    variables <- data[, !(names(data) %in% columns_to_exclude)]
    
    numeric_data <- variables[, sapply(variables, is.numeric)]
    numeric_column_names <- names(numeric_data)
    selected_data <- data[, numeric_column_names, drop = FALSE]
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
  
  
  
  
  output$koptimum2 <- renderPlot({
    data <- data()
    
    # Exclude specific columns
    columns_to_exclude <- c("Tahun", "Id", "Longitude", "Latitude")
    variables <- data[, !(names(data) %in% columns_to_exclude)]
    
    numeric_data <- variables[, sapply(variables, is.numeric)]
    numeric_column_names <- names(numeric_data)
    selected_data <- data[, numeric_column_names, drop = FALSE]
    
    Datastand <- scale(selected_data)
    
    if (input$SelectedMetode == "K-Means") {
      final <- kmeans(Datastand, input$K)
      
      # Extracting cluster assignments from the kmeans result
      clusters <- as.factor(final$cluster)
      
      # Combining cluster assignments with scaled data and Provinsi column
      clustered_data <- data.frame(Datastand, Provinsi = data$Provinsi, Cluster = clusters)
      
      p <- fviz_cluster(final, data = Datastand, geom = "point", stand = FALSE,
                        palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","#4CAF50", "#FF4500", "#7B68EE", "#A0522D","#8A2BE2", "#008B8B"),
                        ggtheme = theme_minimal(), main = paste("K-Means Clustering (K =", input$K, ")"))
      
      # Extract principal components for x and y coordinates
      pca_result <- prcomp(Datastand)
      x_y_coordinates <- as.data.frame(pca_result$x[, 1:2])
      
      # Add province labels using geom_text_repel
      p + geom_text_repel(aes(label = Provinsi), data = clustered_data,
                          x = x_y_coordinates$PC1, y = x_y_coordinates$PC2,
                          size = 3, box.padding = 0.5)  # Adjust the 'size' parameter as needed
    } 
  })
  
  #-----------------------------------------------------------------DALAM BENTUK PETA
  output$koptimum2Map <- renderLeaflet({
    data <- data()
    
    # Exclude specific columns
    columns_to_exclude <- c("Tahun", "Id", "Longitude", "Latitude")
    variables <- data[, !(names(data) %in% columns_to_exclude)]
    
    numeric_data <- variables[, sapply(variables, is.numeric)]
    numeric_column_names <- names(numeric_data)
    selected_data <- data[, numeric_column_names, drop = FALSE]
    
    Datastand <- scale(selected_data)
    
    data1 <- read_excel("Data R.xlsx")
    variables <- colnames(data1)
    variables <- variables[!(variables %in% c("Tahun", "Id"))]
    
    if (input$SelectedMetode == "K-Means") {
      # Perform kmeans clustering
      final <- kmeans(Datastand, input$K)
      
      # Combine cluster assignments with Provinsi column
      clustered_data <- data.frame(Provinsi = data$Provinsi, Cluster = as.factor(final$cluster), 
                                   Longitude = data1$Longitude, Latitude = data1$Latitude)
      
      # Use final$cluster for cluster assignments
      cluster_labels <- as.factor(final$cluster)
      
      # Extract cluster colors directly from the kmeans result
      cluster_colors <- rainbow(input$K)  # You can use any color palette here
      
      # Create fviz_cluster plot
      cluster_plot <- fviz_cluster(final, data = Datastand, geom = "point", stand = FALSE,
                                   ggtheme = theme_minimal(), main = paste("K-Means Clustering (K =", input$K, ")"))
      
      # Debugging: Print relevant information
      print("Cluster Labels from fviz_cluster:")
      print(levels(cluster_labels))
      print("Cluster Colors:")
      print(cluster_colors)
      
      # Create a Leaflet map
      m <- leaflet(data = clustered_data) %>%
        addTiles() %>%
        setView(lng = 110, lat = -5, zoom = 5)  # Adjust the initial view as needed
      
      # Add circle markers for each province based on cluster
      for (i in levels(cluster_labels)) {
        subset_data <- clustered_data[cluster_labels == i, ]
        
        # Debugging: Print relevant information
        print(paste("Subset Data for Cluster", i))
        print(subset_data)
        
        # Use a different color for each cluster
        color_index <- as.numeric(i)  # Convert factor levels to numeric
        color <- cluster_colors[color_index]
        
        m <- addCircleMarkers(
          map = m,
          lng = ~Longitude,  # Assuming Longitude is a column in your original data
          lat = ~Latitude,   # Assuming Latitude is a column in your original data
          data = subset_data,
          radius = 8,
          color = color,
          fillOpacity = 0.7,
          popup = paste("Provinsi: ", subset_data$Provinsi, "<br>Klaster: ", i)
        )
      }
      
      # Debugging: Print final Leaflet map
      print(m)
    }
  })
  
  #------------------------------ karakter
  output$karakter <- renderDataTable({
    
    req(input$K)  # Ensure k_clusters is provided
    
    data <- data()
    
    # Exclude specific columns
    columns_to_exclude <- c("Tahun", "Id", "Longitude", "Latitude", "Provinsi")
    variables <- data[, !(names(data) %in% columns_to_exclude)]
    
    numeric_data <- variables[, sapply(variables, is.numeric)]
    numeric_column_names <- names(numeric_data)
    selected_data <- data[, numeric_column_names, drop = FALSE]
    
    Datastand <- scale(selected_data)  # Use selected_data instead of data()
    
    if (input$SelectedMetode == "K-Means") {
      k_means <- kmeans(Datastand, centers = input$K)  # Use kmeans instead of fviz_cluster
      
      cut_point <- k_means$cluster
      
      asean_data <- selected_data %>%
        mutate(Klaster = cut_point)
      
      cluster_summary <- asean_data %>%
        group_by(Klaster) %>% 
        summarise(across(everything(), list("Rata-Rata" = ~ round(mean(.), 2))))
      
      # Convert the cluster_summary to a DataTable
      datatable(cluster_summary, options = list(lengthMenu = c(5, 10, 15), pageLength = 5))
    }
  })
  
  #--------------------------------------------
  # Informasi variabel dengan mean tertinggi di setiap klaster
  output$variable_info <- renderPrint({
    
    data <- data()
    
    # Exclude specific columns
    columns_to_exclude <- c("Tahun", "Id", "Longitude", "Latitude", "Provinsi")
    variables <- data[, !(names(data) %in% columns_to_exclude)]
    
    numeric_data <- variables[, sapply(variables, is.numeric)]
    numeric_column_names <- names(numeric_data)
    selected_data <- data[, numeric_column_names, drop = FALSE]
    
    Datastand <- scale(selected_data)  # Use selected_data instead of data()
    
    if (input$SelectedMetode == "K-Means") {
      k_means <- kmeans(Datastand, centers = input$K)  # Use kmeans instead of fviz_cluster
      
      cut_point <- k_means$cluster
      
      asean_data <- selected_data %>%
        mutate(Klaster = cut_point)
      
      var_means <- asean_data %>%
        group_by(Klaster) %>%
        summarise(across(everything(), list(Mean = ~ mean(.)))) %>%
        rename_with(~ str_remove(., "_Mean"), -Klaster)
      
      var_means
      
      max_values <- var_means %>%
        pivot_longer(cols = -Klaster, names_to = "Variable", values_to = "Mean") %>%
        group_by(Variable) %>%
        filter(Mean == max(Mean)) %>%
        summarise(Klaster = toString(Klaster))
      
      # Menampilkan hasil
      max_values
      
      # Menampilkan hasil interpretatif
      interpretation <- max_values %>%
        group_by(Klaster) %>%
        summarise(Text = paste("klaster", Klaster, "memiliki kecenderungan pada variabel", Variable, collapse = ", ")) %>%
        pull(Text)
      
      # Menampilkan hasil
      cat(interpretation, sep = "\n")
    }
  })

  
  
}

shinyApp(ui = myui, server = server)
