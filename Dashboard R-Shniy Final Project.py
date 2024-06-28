library(shinydashboard)
library(DT)
library(ggplot2)
library(rpart)

# Load data
data <- read.csv("D:/Downloads/Clean Dataset (2).csv", sep=",")

# Filter only numeric columns from the dataset
numeric_cols <- data[, sapply(data, is.numeric)]

# Function to render summary statistics
renderSummaryStats <- function(data, stat) {
  switch(stat,
         "Summary" = summary(data),
         "Correlation" = cor(data),
         "Covariance" = cov(data),
         "IQR" = apply(data, 2, IQR),
         "Mean" = colMeans(data, na.rm = TRUE),
         "Median" = apply(data, 2, median, na.rm = TRUE),
         "Standard Deviation" = apply(data, 2, sd, na.rm = TRUE),
         "Variance" = apply(data, 2, var, na.rm = TRUE)
  )
}

# Function to get unique values for each column
getUniqueValues <- function(data) {
  unique_vals <- lapply(data, unique)
  unique_vals
}

# Get unique values for each column
unique_values <- getUniqueValues(data)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "MENU"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Visualisasi", tabName = "visualisasi", icon = icon("bar-chart"),
               menuSubItem("Summary Statistics", tabName = "summary_stats"),
               menuSubItem("Histogram", tabName = "histogram"),
               menuSubItem("Boxplot", tabName = "boxplot"),
               menuSubItem("Barplot", tabName = "barplot"),
               menuSubItem("Pie Chart", tabName = "pie_chart")
      ),
      menuItem("Predictive Analysis", tabName = "predict_tab", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "data",
        fluidRow(
          column(
            width = 12,
            h2("Preferensi Pembelian Makanan Data"),
            DTOutput("data_table")
          )
        ),
        downloadButton('download_data', 'Download Data')
      ),
      tabItem(
        tabName = "summary_stats",
        fluidRow(
          column(
            width = 12,
            h2("Summary Statistics"),
            selectInput(
              "summary_stat",
              "Pilih Statistik:",
              choices = c("Summary", "Correlation", "Covariance", "IQR", "Mean", "Median", "Standard Deviation", "Variance"),
              selected = "Summary"
            )
          )
        ),
        fluidRow(
          verbatimTextOutput("summary_stats_output")
        )
      ),
      tabItem(
        tabName = "visualisasi",
        fluidRow(
          column(
            width = 12,
            h2("Visualisasi"),
            selectInput(
              "variable",
              "Pilih Variabel:",
              choices = colnames(data),
              selected = colnames(data)[1]
            ),
            plotOutput("plot")
          )
        ),
        fluidRow(
          verbatimTextOutput("num_observations")
        )
      ),
      tabItem(
        tabName = "histogram",
        fluidRow(
          column(
            width = 12,
            h2("Histogram"),
            selectInput(
              "hist_var",
              "Pilih Variabel:",
              choices = c("Uang.Saku.Sebulan", "Biaya.Sekali.Makan.Online", "Biaya.Sekali.Makan.Offline"),
              selected = "Uang.Saku.Sebulan"
            ),
            plotOutput("histogram_plot")
          )
        ),
        fluidRow(
          verbatimTextOutput("num_observations_hist")
        )
      ),
      tabItem(
        tabName = "boxplot",
        fluidRow(
          column(
            width = 12,
            h2("Boxplot"),
            selectInput(
              "boxplot_var",
              "Pilih Variabel:",
              choices = c("Uang.Saku.Sebulan", "Biaya.Sekali.Makan.Online", "Biaya.Sekali.Makan.Offline", "F.Makan.Sehari", "F.Beli.Online", "F.Beli.Offline"),
              selected = "Uang.Saku.Sebulan"
            ),
            plotOutput("boxplot_plot")
          )
        ),
        fluidRow(
          verbatimTextOutput("num_observations_boxplot")
        )
      ),
      tabItem(
        tabName = "barplot",
        fluidRow(
          column(
            width = 12,
            h2("Barplot"),
            selectInput(
              "barplot_var",
              "Pilih Variabel:",
              choices = colnames(data),
              selected = colnames(data)[1]
            ),
            plotOutput("barplot_plot"),
            verbatimTextOutput("num_observations_barplot")
          )
        )
      ),
      tabItem(
        tabName = "pie_chart",
        fluidRow(
          column(
            width = 12,
            h2("Pie Chart"),
            selectInput(
              "pie_var",
              "Pilih Variabel:",
              choices = colnames(data),
              selected = colnames(data)[1]
            ),
            plotOutput("pie_chart")
          )
        )
      ),
      tabItem(
        tabName = "predict_tab",
        fluidRow(
          column(
            width = 12,
            h2("Prediksi Preferensi Pembelian"),
            selectInput(
              "Jenis_kelamin",
              "Jenis Kelamin:",
              choices = unique_values$Jenis.kelamin,
              selected = unique_values$Jenis.kelamin[1]
            ),
            selectInput(
              "F_Uang_Saku",
              "F Uang Saku:",
              choices = unique_values$F.Uang.Saku,
              selected = unique_values$F.Uang.Saku[1]
            ),
            selectInput(
              "Aplikasi_Beli_Online",
              "Aplikasi Beli Online:",
              choices = unique_values$Aplikasi.Beli.Online,
              selected = unique_values$Aplikasi.Beli.Online[1]
            ),
            selectInput(
              "Tempat_Makan_Offline",
              "Tempat Makan Offline:",
              choices = unique_values$Tempat.Makan.Offline,
              selected = unique_values$Tempat.Makan.Offline[1]
            ),
            selectInput(
              "Kepuasan_Makanan",
              "Kepuasan Makanan:",
              choices = unique_values$Kepuasan.Makanan,
              selected = unique_values$Kepuasan.Makanan[1]
            ),
            selectInput(
              "Kepuasan_Layanan",
              "Kepuasan Layanan:",
              choices = unique_values$Kepuasan.Layanan,
              selected = unique_values$Kepuasan.Layanan[1]
            ),
            selectInput(
              "Kepuasan_Transaksi",
              "Kepuasan Transaksi",
              choices = unique_values$Kepuasan.Transaksi,
              selected = unique_values$Kepuasan.Transaksi[1]
            ),
            selectInput(
              "Ketersediaan_Stock",
              "Ketersediaan Stock:",
              choices = unique_values$Ketersediaan.Stock,
              selected = unique_values$Ketersediaan.Stock[1]
            ),
            selectInput(
              "Faktor_Pilihan",
              "Faktor Pilihan:",
              choices = unique_values$Faktor.Pilihan,
              selected = unique_values$Faktor.Pilihan[1]
            ),
            selectInput(
              "Uang_Saku_Cukup",
              "Uang Saku Cukup:",
              choices = unique_values$Uang.Saku.Cukup,
              selected = unique_values$Uang.Saku.Cukup[1]
            ),
            actionButton("predict_button", "Predict"),  # Add a predict button
          ),
          # Tambahkan di dalam tabItem "predict_tab"
          column(
            width = 6,
            h3("Hasil Prediksi"),
            verbatimTextOutput("prediction_result")
          ),
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$data_table <- renderDT({
    datatable(
      data,
      options = list(
        searching = TRUE,
        lengthChange = TRUE,
        pageLength = 5,
        dom = 'lftip',
        lengthMenu = c(5, 15, 25),
        ordering = FALSE,  
        scrollY = '300px',
        scrollX = TRUE,  
        scrollCollapse = TRUE
      )
    )
  })
  
  # Download dataset
  output$download_data <- downloadHandler(
    filename = function() { paste("preferensi_pembelian.csv") },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
  # Render summary statistics
  output$summary_stats_output <- renderPrint({
    req(input$summary_stat)
    stats <- renderSummaryStats(numeric_cols, input$summary_stat)
    
    result <- switch(input$summary_stat,
                     "Summary" = capture.output(print(stats)),
                     "Correlation" = capture.output(print(stats)),
                     "Covariance" = capture.output(print(stats)),
                     "IQR" = capture.output(print(stats)),
                     "Mean" = capture.output(print(stats)),
                     "Median" = capture.output(print(stats)),
                     "Standard Deviation" = capture.output(print(stats)),
                     "Variance" = capture.output(print(stats))
    )
    
    cat(result, sep = '\n')
  })
  
  # Render selected plot
  output$plot <- renderPlot({
    req(input$variable)
    hist(data[[input$variable]], main = paste("Histogram of", input$variable), bins = 30)  # Set the number of bins here
  })
  
  # Render histogram plot using ggplot
  output$histogram_plot <- renderPlot({
    req(input$hist_var)
    
    ggplot(data, aes_string(x = input$hist_var)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") + # Change the number of bins here
      labs(title = paste("Histogram of", input$hist_var),
           x = input$hist_var,
           y = "Frequency")
  })
  
  # Render number of observations for each plot
  output$num_observations <- renderText({
    paste("Number of Observations: ", nrow(data))
  })
  
  output$num_observations_hist <- renderText({
    paste("Number of Observations: ", nrow(data))
  })
  
  output$boxplot_plot <- renderPlot({
    req(input$boxplot_var)
    
    boxplot(data[[input$boxplot_var]], main = paste("Boxplot of", input$boxplot_var))
  })
  
  output$num_observations_boxplot <- renderText({
    paste("Number of Observations: ", nrow(data))
  })
  
  output$barplot_plot <- renderPlot({
    req(input$barplot_var)
    
    ggplot(data, aes_string(x = input$barplot_var)) +
      geom_bar(fill = "skyblue", color = "black") +
      labs(title = paste("Barplot of", input$barplot_var),
           x = input$barplot_var,
           y = "Count") +
      geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5)
  })
  
  output$num_observations_barplot <- renderText({
    paste("Number of Observations: ", nrow(data))
  })
  
  output$pie_chart <- renderPlot({
    req(input$pie_var)
    
    count_data <- table(data[[input$pie_var]])
    percentage_data <- prop.table(count_data) * 100
    
    pie(percentage_data, labels = paste(names(percentage_data), "(", round(percentage_data, 1), "%)"))
  })
  
  # Initialize reactive values to store predictions
  values <- reactiveValues(predictions = NULL)
  
  observeEvent(input$predict_button, {
    # Reset hasil prediksi sebelumnya
    values$predictions <- NULL
    
    # Sisanya kode prediksi tetap sama
    if (!is.null(input$Jenis_kelamin) && !is.null(input$F_Uang_Saku) && !is.null(input$Aplikasi_Beli_Online) &&
        !is.null(input$Tempat_Makan_Offline) && !is.null(input$Kepuasan_Makanan) && !is.null(input$Kepuasan_Layanan) &&
        !is.null(input$Kepuasan_Transaksi) && !is.null(input$Ketersediaan_Stock) && !is.null(input$Faktor_Pilihan) &&
        !is.null(input$Uang_Saku_Cukup)) {
      # Sisanya kode prediksi tetap sama
      predictors <- data[, c("Jenis.kelamin", "F.Uang.Saku", "Aplikasi.Beli.Online", "Tempat.Makan.Offline", "Kepuasan.Makanan", "Kepuasan.Layanan", "Kepuasan.Transaksi", "Ketersediaan.Stock", "Faktor.Pilihan", "Uang.Saku.Cukup"), drop = FALSE]
      target <- data[["Preferensi.Pembelian"]]
      
      encoded_target <- as.numeric(as.factor(target))
      
      model <- rpart(encoded_target ~ ., data = predictors, method = "class")
      
      new_data <- data.frame(Jenis.kelamin = input$Jenis_kelamin,
                             F.Uang.Saku = input$F_Uang_Saku,
                             Aplikasi.Beli.Online = input$Aplikasi_Beli_Online,
                             Tempat.Makan.Offline = input$Tempat_Makan_Offline,
                             Kepuasan.Makanan = input$Kepuasan_Makanan,
                             Kepuasan.Layanan = input$Kepuasan_Layanan,
                             Kepuasan.Transaksi = input$Kepuasan_Transaksi,
                             Ketersediaan.Stock = input$Ketersediaan_Stock,
                             Faktor.Pilihan = input$Faktor_Pilihan,
                             Uang.Saku.Cukup = input$Uang_Saku_Cukup) 
      prediction <- predict(model, newdata = new_data, type = "class")
      
      # Simpan hasil prediksi dalam reactive values
      values$predictions <- ifelse(prediction == 1, "Online", "Offline")
    } else {
      # Sisanya kode prediksi tetap sama
    }
  })
  
  output$prediction_result <- renderPrint({
    # Tampilkan hasil prediksi dari reactive values
    if (!is.null(values$predictions)) {
      paste("Hasil Prediksi: ", values$predictions)
    } else {
      "Please select values for all predictors to make a prediction."
    }
  })
}

shinyApp(ui = ui, server = server)