# First, ensure these libraries are installed:
# install.packages(c("shiny", "DT", "readr", "dplyr"))

library(shiny)
library(DT)
library(readr)
library(dplyr)

ui <- fluidPage(
  textInput("first_name", "First Name"),
  textInput("last_name", "Last Name"),
  actionButton("add_name", "Add Name"),
  textInput("file_name", "CSV File Name", value = "names_and_pins.csv"),
  actionButton("export_csv", "Export to CSV"),
  DTOutput("names_table")
)

server <- function(input, output, session) {
  names_data <- reactiveVal(data.frame(First_Name = character(), Last_Name = character(), PIN = character(), stringsAsFactors = FALSE))
  pin_digits <- reactiveVal(NA)
  
  showModal(modalDialog(
    title = "Set Number of PIN digits",
    numericInput("modal_pin_digits", "Number of PIN digits", value = 4, min = 1),
    footer = actionButton("set_pin_digits", "Set PIN Digits")
  ))
  
  observeEvent(input$set_pin_digits, {
    pin_digits(input$modal_pin_digits)
    removeModal()
  })
  
  observeEvent(input$add_name, {
    new_pin <- as.character(sample.int(10^pin_digits() - 1, 1) + 10^(pin_digits() - 1))
    while (new_pin %in% names_data()$PIN) {
      new_pin <- as.character(sample.int(10^pin_digits() - 1, 1) + 10^(pin_digits() - 1))
    }
    
    names_data(rbind(names_data(), data.frame(First_Name = input$first_name, Last_Name = input$last_name, PIN = new_pin, stringsAsFactors = FALSE)))
  })
  
  output$names_table <- renderDT({
    datatable(names_data())
  })
  
  observeEvent(input$export_csv, {
    write_csv(names_data(), paste0(input$file_name))
  })
}

shinyApp(ui, server)
