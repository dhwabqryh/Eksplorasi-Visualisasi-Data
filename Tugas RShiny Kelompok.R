library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(plotly)

ui <- dashboardPage(
  skin="green",
  dashboardHeader(title = "Dashboard Kelompok C"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "Data", icon = icon("upload")),
      menuItem("Visualisasi", tabName = "Visualisasi", icon = icon("chart-simple")),
      menuItem("Interpretasi", tabName = "Interpretasi", icon = icon("comments"),
               menuSubItem("Scatter Plot", tabName = "ppq"),
               menuSubItem("Boxplot", tabName = "ppw"),
               menuSubItem("Pie Chart", tabName = "ppk"))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Data",
              h2("Upload Data"),
              fileInput("file1", "Pilih Data",
                        multiple = TRUE,
                        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
              actionButton("load_data", "Tampilkan Data"),
              tags$hr(),
              tableOutput("contents")
      ),
      tabItem(tabName = "Visualisasi",
              h2("Statistika Deskriptif"),
              verbatimTextOutput("summary"),
              
              h2("Scatterplot"),
              selectInput("x_var", "Pilih X Variable", ""),
              selectInput("y_var", "Pilih Y Variable", ""),
              sliderInput("x_range", "Range X", min = 0, max = 100, value = c(0, 100)),
              sliderInput("y_range", "Range Y", min = 0, max = 250000, value = c(0, 250)),
              plotlyOutput("plot1"),
              
              h2("BoxPlot"),
              selectInput("boxplot_var", "Pilih Variabel", ""),
              plotlyOutput("boxplot"),
              
              h2("Pie Chart"),
              selectInput("pie_var","Pilih Variabel",""),
              plotlyOutput("pie"),
              
              h2("Std Deviation and Variance"),
              selectInput("stats_var", "Pilih Variabel", ""),
              verbatimTextOutput("stats_output")
              
      ),
      tabItem(tabName = "ppq",
              h3("Interpretasi Data"),
              fluidRow(box(
                title = "Scatter Plot",
                verbatimTextOutput("vis1"),
                status = "success", solidHeader = T,
                width = 10, height = 200))
              
      ),
      tabItem(tabName = "ppw",
              h3("Interpretasi Data"),
              fluidRow(box(
                title = "Boxplot",
                textOutput("vis2"),
                status = "success", solidHeader = T,
                width = 10, height = 200))
      ),
      tabItem(tabName = "ppk",
              h3("Interpretasi Data"),
              fluidRow(box(
                title = "Pie chart",
                textOutput("vis3"),
                status = "success", solidHeader = T,
                width = 10, height = 200))
              
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$load_data, {
    req(input$file1)
    data_to_display <- read.csv(input$file1$datapath)
    data(data_to_display)
    
    # Mengisi pilihan kolom untuk selectInput
    updateSelectInput(session, "x_var", choices = colnames(data_to_display))
    updateSelectInput(session, "y_var", choices = colnames(data_to_display))
    updateSelectInput(session, 'pie_var', choices = colnames(data_to_display))
    
    # Untuk Mengisi Boxplot
    updateSelectInput(session, "boxplot_var", choices = colnames(data_to_display))
    
    updateSelectInput(session, "stats_var", choices = colnames(data_to_display))
    
    # Menghitung statistika deskriptif
    summary <- summary(data_to_display)
    output$summary <- renderPrint({
      summary
    })
    
    
  })
  
  output$contents <- renderTable({
    data_to_display <- data()
    if (!is.null(data_to_display)) {
      data_to_display
    }
  })
  output$plot1 <- renderPlotly({
    data_to_display <- data()
    if (!is.null(data_to_display)) {
      filtered_data <- filter(data_to_display, data_to_display[, input$x_var] >= input$x_range[1] & data_to_display[, input$x_var] <= input$x_range[2] & data_to_display[, input$y_var] >= input$y_range[1] & data_to_display[, input$y_var] <= input$y_range[2])
      plot <- plot_ly(filtered_data, x = ~filtered_data[, input$x_var], y = ~filtered_data[, input$y_var], type = 'scatter', mode = 'markers')
      plot <- plot %>% layout(xaxis = list(title = input$x_var), yaxis = list(title = input$y_var))
      plot
    }
  })
  output$boxplot <- renderPlotly({
    data_to_display <- data()
    if (!is.null(data_to_display)) {
      selected_var <- input$boxplot_var
      if (selected_var %in% colnames(data_to_display)) {
        boxplot_data <- data_to_display %>%
          select(selected_var)
        boxplot_plot <- plot_ly(boxplot_data, y = ~boxplot_data[, selected_var], type = "box")
        boxplot_plot <- boxplot_plot %>% layout(xaxis = list(title = selected_var), yaxis = list(title = "Nilai"))
        boxplot_plot
      }
    }
  })
  
  output$pie <- renderPlotly({
    data_to_display <- data()
    if (!is.null(data_to_display)) {
      selected_var <- input$pie_var
      if (selected_var %in% colnames(data_to_display)) {
        pie_data <- data_to_display %>%
          count(!!sym(selected_var)) %>%
          mutate(percentage = n / sum(n))
        
        pie_chart <- plot_ly(pie_data, labels = ~get(selected_var), values = ~percentage, type = 'pie')
        pie_chart <- pie_chart %>% layout(title = selected_var)
        pie_chart
      }
    }
  })
  
  output$stats_output <- renderPrint({
    data_to_display <- data()
    if (!is.null(data_to_display)) {
      selected_var <- input$stats_var
      if (selected_var %in% colnames(data_to_display)) {
        std_deviation <- sd(data_to_display[[selected_var]])
        variance <- var(data_to_display[[selected_var]])
        
        result <- paste("Standard Deviation: ", std_deviation,
                        "Variance: ", variance)
        result
      }
    }
  })
  
  output$vis1 <- renderPrint({HTML("1. Dari scatter plot yang telah dibuat, ditemukan bahwa semakin tua umur seseorang maka semakin banyak juga pengalaman yang telah dijalani dalam tahun. 2. Dari scatter plot antara pengalaman seseorang dalam tahun dengan gaji yang didapatkan dapat disimpulkan bahwa semakin lama pengalaman seseorang dalam tahun maka semakin banyak pula gaji yang diperoleh.")})
  output$vis2 <- renderText({"Berdasarkan visualisasi boxplot, biaya asuransi tertinggi ada di southeast region. Data masih terdapat outlier."})
  output$vis3 <- renderText({"Berdasarkan visualisasi boxplot, biaya asuransi tertinggi ada di southeast region. Data masih terdapat outlier."})
  
  
  
}

shinyApp(ui,server)