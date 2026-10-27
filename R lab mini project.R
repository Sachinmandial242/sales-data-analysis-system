
library(shiny)

# Sample Sales Data
sales_data <- data.frame(
  Product = c(
    "Laptop", "Mobile", "Headphones", "Keyboard",
    "Mouse", "Monitor", "Printer", "Tablet",
    "Smartwatch", "Speakers", "Webcam", "USB Drive",
    "Power Bank", "Laptop Stand", "Wi-Fi Router"
  ),
  Quantity = c(
    5, 10, 15, 12, 20, 8, 4, 7,
    10, 9, 6, 25, 14, 11, 5
  ),
  Price = c(
    50000, 20000, 1500, 1200, 500,
    12000, 15000, 18000, 3000, 2500,
    2000, 600, 1800, 1000, 2800
  ),
  stringsAsFactors = FALSE
)

sales_data$Revenue <- sales_data$Quantity * sales_data$Price


# USER INTERFACE
ui <- fluidPage(
  
  tags$head(
    tags$title("Sales Data Analysis System"),
    
    tags$style(HTML("
      body {
        background-color: #f3f6fb;
        font-family: Arial, sans-serif;
      }

      .header {
        background: #172554;
        color: white;
        padding: 22px;
        margin-bottom: 20px;
        border-radius: 8px;
      }

      .sidebar {
        background: #172554;
        padding: 20px;
        border-radius: 10px;
        min-height: 480px;
      }

      .sidebar .btn {
        width: 100%;
        margin-bottom: 12px;
        text-align: left;
        background: #263b69;
        color: white;
        border: none;
        padding: 12px;
      }

      .sidebar .btn:hover {
        background: #3b82f6;
        color: white;
      }

      .metric {
        background: white;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 18px;
        box-shadow: 0 2px 8px #dce3ee;
        min-height: 100px;
      }

      .metric h4 {
        color: #64748b;
        font-size: 15px;
      }

      .metric h2 {
        color: #172554;
        font-weight: bold;
        font-size: 22px;
      }

      .panel-box {
        background: white;
        padding: 22px;
        border-radius: 10px;
        margin-bottom: 20px;
        box-shadow: 0 2px 8px #dce3ee;
      }

      h3 {
        color: #172554;
      }
    "))
  ),
  
  # Header
  div(
    class = "header",
    h2("Sales Data Analysis System"),
    p("Manage sales, track revenue and analyze product performance")
  ),
  
  fluidRow(
    
    # Sidebar Navigation
    column(
      width = 3,
      
      div(
        class = "sidebar",
        
        h3("Navigation", style = "color:white;"),
        
        actionButton("home", "Home Dashboard"),
        actionButton("add", "Add Sales"),
        actionButton("view", "View Sales Data"),
        actionButton("delete", "Delete Sales"),
        actionButton("analysis", "Sales Analysis")
      )
    ),
    
    # Main Page
    column(
      width = 9,
      uiOutput("page_content")
    )
  )
)


# SERVER
server <- function(input, output, session) {
  
  # Reactive sales data
  sales <- reactiveVal(sales_data)
  
  # Default page
  current_page <- reactiveVal("home")
  
  # Navigation buttons
  observeEvent(input$home, {
    current_page("home")
  })
  
  observeEvent(input$add, {
    current_page("add")
  })
  
  observeEvent(input$view, {
    current_page("view")
  })
  
  observeEvent(input$delete, {
    current_page("delete")
  })
  
  observeEvent(input$analysis, {
    current_page("analysis")
  })
  
  
  # Display selected page
  output$page_content <- renderUI({
    
    if (current_page() == "home") {
      
      tagList(
        
        h3("Home Dashboard"),
        p("Welcome to your Sales Management Dashboard"),
        
        fluidRow(
          
          column(
            4,
            div(
              class = "metric",
              h4("Total Revenue"),
              h2(textOutput("total_revenue"))
            )
          ),
          
          column(
            4,
            div(
              class = "metric",
              h4("Average Revenue"),
              h2(textOutput("average_revenue"))
            )
          ),
          
          column(
            4,
            div(
              class = "metric",
              h4("Sales Records"),
              h2(textOutput("total_products"))
            )
          )
        ),
        
        div(
          class = "panel-box",
          h3("Sales Records"),
          tableOutput("recent_table")
        )
      )
      
    } else if (current_page() == "add") {
      
      div(
        class = "panel-box",
        
        h3("Add New Sales"),
        p("Enter product details below."),
        
        textInput("product", "Product Name"),
        
        numericInput(
          "quantity", "Quantity",
          value = 1, min = 1
        ),
        
        numericInput(
          "price", "Price per Unit (Rs.)",
          value = 100, min = 0
        ),
        
        actionButton(
          "save",
          "Add Sales Record",
          class = "btn-primary"
        ),
        
        br(), br(),
        textOutput("message")
      )
      
    } else if (current_page() == "view") {
      
      div(
        class = "panel-box",
        
        h3("View Sales Data"),
        p("All product sales and revenue details."),
        
        tableOutput("all_table")
      )
      
    } else if (current_page() == "delete") {
      
      div(
        class = "panel-box",
        
        h3("Delete Sales Record"),
        p("Select the specific sales record you want to delete."),
        
        uiOutput("delete_selector"),
        
        actionButton(
          "delete_record",
          "Delete Selected Record",
          class = "btn-danger"
        ),
        
        br(), br(),
        textOutput("delete_message")
      )
      
    } else {
      
      tagList(
        
        div(
          class = "panel-box",
          h3("Product-wise Revenue"),
          plotOutput("bar_chart", height = "350px")
        ),
        
        div(
          class = "panel-box",
          h3("Revenue Distribution"),
          plotOutput("pie_chart", height = "350px")
        )
      )
    }
  })
  
  
  # Dashboard calculations
  output$total_revenue <- renderText({
    
    paste0(
      "Rs. ",
      format(
        sum(sales()$Revenue),
        big.mark = ",",
        scientific = FALSE
      )
    )
  })
  
  output$average_revenue <- renderText({
    
    data <- sales()
    
    if (nrow(data) == 0) {
      return("Rs. 0")
    }
    
    paste0(
      "Rs. ",
      format(
        round(mean(data$Revenue), 2),
        big.mark = ",",
        scientific = FALSE
      )
    )
  })
  
  output$total_products <- renderText({
    nrow(sales())
  })
  
  
  # Display recent records
  output$recent_table <- renderTable({
    
    head(sales(), 5)
    
  }, striped = TRUE, bordered = TRUE)
  
  
  # Display all records
  output$all_table <- renderTable({
    
    sales()
    
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
  # Add Sales Record
  observeEvent(input$save, {
    
    product_name <- trimws(input$product)
    
    if (is.null(product_name) || product_name == "") {
      output$message <- renderText({
        "Please enter a product name."
      })
      return()
    }
    
    if (is.null(input$quantity) ||
        input$quantity < 1 ||
        input$quantity != floor(input$quantity) ||
        is.null(input$price) ||
        !is.finite(input$price) ||
        input$price < 0) {
      
      output$message <- renderText({
        "Enter a valid quantity and price."
      })
      return()
    }
    
    new_row <- data.frame(
      Product = product_name,
      Quantity = input$quantity,
      Price = input$price,
      Revenue = input$quantity * input$price,
      stringsAsFactors = FALSE
    )
    
    sales(rbind(sales(), new_row))
    
    output$message <- renderText({
      "Sales record added successfully!"
    })
    
    updateTextInput(session, "product", value = "")
    updateNumericInput(session, "quantity", value = 1)
    updateNumericInput(session, "price", value = 100)
  })
  
  
  # Dropdown for deleting a specific record
  output$delete_selector <- renderUI({
    
    data <- sales()
    
    if (nrow(data) == 0) {
      return(
        p("No sales records available to delete.")
      )
    }
    
    choices <- setNames(
      seq_len(nrow(data)),
      paste0(
        seq_len(nrow(data)), ". ",
        data$Product,
        " | Quantity: ", data$Quantity,
        " | Revenue: Rs. ",
        format(data$Revenue, big.mark = ",",
               scientific = FALSE)
      )
    )
    
    selectInput(
      "selected_record",
      "Choose Sales Record",
      choices = choices
    )
  })
  
  
  # Delete selected record
  observeEvent(input$delete_record, {
    
    data <- sales()
    
    req(input$selected_record)
    
    selected <- as.integer(input$selected_record)
    
    if (is.na(selected) ||
        selected < 1 ||
        selected > nrow(data)) {
      
      output$delete_message <- renderText({
        "Please select a valid record."
      })
      
      return()
    }
    
    deleted_product <- data$Product[selected]
    
    data <- data[-selected, , drop = FALSE]
    
    sales(data)
    
    output$delete_message <- renderText({
      paste(
        "Sales record for",
        deleted_product,
        "deleted successfully!"
      )
    })
  })
  
  
  # Bar Chart
  output$bar_chart <- renderPlot({
    
    data <- sales()
    
    if (nrow(data) == 0) {
      plot.new()
      text(0.5, 0.5, "No sales data available")
      return()
    }
    
    barplot(
      data$Revenue,
      names.arg = data$Product,
      col = "steelblue",
      main = "Revenue by Product",
      xlab = "Product",
      ylab = "Revenue (Rs.)",
      las = 2,
      cex.names = 0.75
    )
  })
  
  
  # Pie Chart
  output$pie_chart <- renderPlot({
    
    data <- sales()
    
    if (nrow(data) == 0 || sum(data$Revenue) == 0) {
      plot.new()
      text(0.5, 0.5, "No revenue data available")
      return()
    }
    
    pie(
      data$Revenue,
      labels = data$Product,
      main = "Revenue Distribution",
      col = rainbow(nrow(data))
    )
  })
}


# Run Application
shinyApp(ui = ui, server = server)
