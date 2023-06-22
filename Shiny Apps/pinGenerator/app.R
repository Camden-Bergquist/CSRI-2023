# First, ensure these libraries are installed:
# install.packages(c("shiny", "DT", "readr", "dplyr"))

library(shiny)
library(DT)
library(readr)
library(dplyr)

ui <- fluidPage(
  titlePanel("Enter Student Names:"),
  textInput("first_name", "First Name"),
  textInput("last_name", "Last Name"),
  actionButton("add_name", "Add Name"),
  textInput("file_name", "CSV File Name", value = "pins.csv"),
  uiOutput("fileFound"),
  actionButton("create_file", "Create New File"),
  actionButton("export_csv", "Export to CSV"),
  DTOutput("names_table")
)

server <- function(input, output, session) {
  names_data <- reactiveVal(data.frame(First_Name = character(), Last_Name = character(), PIN = character(), stringsAsFactors = FALSE))
  pin_digits <- reactiveVal(NA)
  
  showModal(modalDialog(
    title = "Set Number of PIN digits",
    numericInput("modal_pin_digits", "Number of PIN digits", value = 6, min = 1),
    footer = actionButton("set_pin_digits", "Set PIN Digits")
  ))
  
  observeEvent(input$set_pin_digits, {
    pin_digits(input$modal_pin_digits)
    removeModal()
  })
  
  observeEvent(input$add_name, {
    new_pin <- paste(sample(0:9, pin_digits(), replace = TRUE), collapse = "")
    existing_pins <- c(names_data()$PIN)
    
    if(file.exists(input$file_name)) {
      existing_data <- read_csv(input$file_name)
      existing_pins <- c(existing_pins, existing_data$PIN)
    }
    
    while (new_pin %in% existing_pins) {
      new_pin <- paste(sample(0:9, pin_digits(), replace = TRUE), collapse = "")
    }
    
    names_data(rbind(names_data(), data.frame(First_Name = input$first_name, Last_Name = input$last_name, PIN = new_pin, stringsAsFactors = FALSE)))
    showNotification("PIN successfully generated", type = "message")
  })
  
  output$names_table <- renderDT({
    datatable(names_data())
  })
  
  observeEvent(input$create_file, {
    if (!file.exists(paste0(input$file_name))) {
      write_csv(data.frame(First_Name = character(), Last_Name = character(), PIN = character(), stringsAsFactors = FALSE), paste0(input$file_name))
      showNotification(paste("File", input$file_name, "created"), type = "message")
    } else {
      showNotification(paste("File", input$file_name, "already exists"), type = "warning")
    }
  })
  
  observeEvent(input$export_csv, {
    write_csv(names_data(), paste0(input$file_name), append = TRUE)
    showNotification(paste("Data exported to", input$file_name), type = "message")
  })
  
  observe({
    output$fileFound <- renderUI({
      invalidateLater(1000)  # Checks file status every second
      if (file.exists(paste0(input$file_name))) {
        tags$span("\u2714 File found!", 
                  style = "color: green; padding-left: 5px; padding-bottom: 3px;", 
                  title = "File has been found in the directory")
      } else {
        tags$span("\u2718 File not found.", 
                  style = "color: red; padding-left: 5px; padding-bottom: 3px;", 
                  title = "File not found in the directory")
      }
    })
  })
}

shinyApp(ui, server)
