library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Basic Shiny App"),

  sidebarLayout(
    sidebarPanel(
      h4("Controls"),

      selectInput(
        inputId = "dataset",
        label   = "Choose a dataset:",
        choices = c("mtcars", "iris", "airquality")
      ),

      uiOutput("x_var_ui"),
      uiOutput("y_var_ui"),

      selectInput(
        inputId = "plot_type",
        label   = "Plot type:",
        choices = c("Scatter", "Histogram", "Boxplot")
      ),

      sliderInput(
        inputId = "point_size",
        label   = "Point size:",
        min = 1, max = 5, value = 2, step = 0.5
      ),

      hr(),
      actionButton("go", "Update Plot", class = "btn-primary")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Plot",  plotOutput("main_plot", height = "450px")),
        tabPanel("Table", tableOutput("data_table")),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive: load selected dataset
  selected_data <- reactive({
    switch(input$dataset,
      mtcars     = mtcars,
      iris       = iris,
      airquality = airquality
    )
  })

  # Dynamic column selectors
  output$x_var_ui <- renderUI({
    cols <- names(selected_data())
    selectInput("x_var", "X variable:", choices = cols, selected = cols[1])
  })

  output$y_var_ui <- renderUI({
    cols <- names(selected_data())
    selectInput("y_var", "Y variable:", choices = cols, selected = cols[2])
  })

  # Plot (re-renders on button click)
  plot_data <- eventReactive(input$go, {
    list(
      df        = selected_data(),
      x         = input$x_var,
      y         = input$y_var,
      type      = input$plot_type,
      point_size = input$point_size
    )
  }, ignoreNULL = FALSE)

  output$main_plot <- renderPlot({
    pd <- plot_data()
    req(pd$x, pd$y)

    df <- pd$df
    p  <- ggplot(df, aes_string(x = pd$x, y = pd$y))

    if (pd$type == "Scatter") {
      p <- p + geom_point(size = pd$point_size, colour = "#3B82F6", alpha = 0.7)
    } else if (pd$type == "Histogram") {
      p <- ggplot(df, aes_string(x = pd$x)) +
             geom_histogram(fill = "#3B82F6", colour = "white", bins = 30)
    } else if (pd$type == "Boxplot") {
      p <- ggplot(df, aes_string(x = pd$x, y = pd$y)) +
             geom_boxplot(fill = "#3B82F6", alpha = 0.6)
    }

    p + theme_minimal(base_size = 14) +
        labs(title = paste(pd$type, "–", input$dataset),
             x = pd$x, y = pd$y)
  })

  # Table (first 20 rows)
  output$data_table <- renderTable({
    head(selected_data(), 20)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Summary
  output$summary <- renderPrint({
    summary(selected_data())
  })
}

# Run
shinyApp(ui = ui, server = server)
