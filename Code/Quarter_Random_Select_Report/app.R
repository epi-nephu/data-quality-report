library(shiny)
library(DT)

# Define initial data
initial_data <- data.frame(
  Disease = c("Influenza", "COVID-19", "Diabetes", "Hypertension", "Asthma"),
  `Number of records` = c(150L, 320L, 85L, 210L, 65L),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# Load records table
entry_table <- readxl::read_xlsx(here("Data", "Records Entry Table.xlsx"), sheet=2) %>% 
  rename(disease_group = "Disease Group")



# User Interface
ui <- fluidPage(
  titlePanel("Disease Record Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      p("Instructions:"),
      p("1. Double-click any cell in the 'Number of records' column to edit it."),
      p("2. Type a new integer value and press Enter."),
      p("3. The application will validate your input and update the total sum below.")
    ),
    
    mainPanel(
      h4("Editable Data Table"),
      DTOutput("disease_table"),
      br(),
      h4("Summary Statistics"),
      verbatimTextOutput("table_summary")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Create a reactive value to store the dataset
  v <- reactiveValues(data = entry_table)
  
  # Render the editable data table
  output$disease_table <- renderDT({
    datatable(
      v$data,
      editable = list(target = "cell", disable = list(columns = 1:2)), # Only allow column 3 to be edited
      options = list(dom = 't', pageLength = 10),                    # Simplified layout showing just the table
      rownames = FALSE
    )
  })
  
  # Observe cell edits and update the reactive data frame
  observeEvent(input$disease_table_cell_info, {
    info <- input$disease_table_cell_info
    
    # Extract edit details (DT uses 0-based indexing for rows and columns)
    row <- info$row + 1
    col <- info$col + 1
    value <- info$value
    
    # Try converting the entered value to an integer
    parsed_value <- as.integer(value)
    
    # Input validation: check if the input is a valid integer and not negative
    if (!is.na(parsed_value) && parsed_value >= 0) {
      v$data[row, col] <- parsed_value
    } else {
      showNotification(
        "Invalid input. Please enter a positive whole number.", 
        type = "error", 
        duration = 3
      )
    }
  })
  
  # Render dynamic summary text based on the table data
  output$table_summary <- renderPrint({
    total_records <- sum(v$data$`Number of records`, na.rm = TRUE)
    cat("Total number of records across all diseases:", total_records)
  })
}

# Run the Application
shinyApp(ui = ui, server = server)
