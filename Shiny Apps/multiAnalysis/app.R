library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Homework Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("run_analysis", "Run Analysis")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("data")),
        tabPanel("Grades by Assignment", 
                 plotOutput("user_plot"),
                 plotOutput("user_attempt_plot")
        ),
        tabPanel("Assignments by Student", 
                 plotOutput("question_plot"), 
                 plotOutput("question_attempt_plot")
        ),
        tabPanel("Grades", tableOutput("grades"))
      )
    )
  )
)

server <- function(input, output) {
  
  data <- eventReactive(input$run_analysis, {
    homework_dir <- "./Assignments"
    
    filePaths <- list.files(path = homework_dir, full.names = TRUE, pattern = "*.csv")
    
    if(length(filePaths) == 0) {
      return(NULL)
    }
    
    rawData <- filePaths %>%
      set_names(nm = basename(.)) %>%
      map_df(~read_csv(.x, guess_max = 1e6), .id = 'assignment') %>%
      mutate(assignment = str_remove(assignment, ".csv")) %>%
      type_convert() # convert column types
    
    # Change points to 0 where corresponding _correct column is not TRUE
    pointsColumns <- grep("_points", names(rawData), value = TRUE)
    correctColumns <- sub("_points", "_correct", pointsColumns)
    for(i in seq_along(pointsColumns)) {
      rawData <- rawData %>%
        mutate_at(vars(pointsColumns[i]), ~ifelse(get(correctColumns[i]) == TRUE, ., 0))
    }
    
    dataPoints <- rawData %>%
      select(c("assignment", "pin", pointsColumns)) %>%
      mutate(total_points = rowSums(select(., -c("assignment", "pin")), na.rm = TRUE)) %>%
      select("assignment", "pin", "total_points")
    
    # Create a new data frame with each pin as a column
    newData <- dataPoints %>%
      pivot_wider(names_from = pin, values_from = total_points)
    
    # Get first attempts columns
    firstAttemptCols <- grep("Q.*\\.1_points", names(rawData), value = TRUE)
    
    # Get the max sum of each first attempt of each question for each user
    userMaxSum <- rawData %>%
      group_by(assignment, pin) %>%
      summarize(across(all_of(firstAttemptCols), sum, na.rm = TRUE), .groups = "drop") %>%
      mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
      group_by(assignment) %>%
      summarize(max_total = max(total, na.rm = TRUE), .groups = "drop")
    
    # Add the max_total column
    newData <- left_join(newData, userMaxSum, by = "assignment")
    
    return(newData)
  })
  
  output$grades <- renderTable({
    req(data())
    data()
  })
  
}

shinyApp(ui = ui, server = server)
