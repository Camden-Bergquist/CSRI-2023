## Shiny app that performs analysis on a multiple assignment CSVs
## (pulled out of ./Assignments directory).

library(shiny)
library(tidyverse)

# UI with four tabs:
## Data, to load in the assignment CSVs and render the collated data.
## Average assignment grade graphs.
## Student grades by assignment graphs.
## Combined grades.
ui <- fluidPage(
  titlePanel("Homework Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("run_analysis", "Run Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("data")),
        tabPanel("Average Assignment Grades", 
                 plotOutput("assignment_plot")
        ),
        tabPanel("Assignments by Student", 
                 plotOutput("student_plot")
        ),
        tabPanel("Grades", tableOutput("grades"))
      )
    )
  )
)

server <- function(input, output) {
  
 
  data <- eventReactive(input$run_analysis, {
    
    # List all files in the directory "./Assignments" with their full names
    files <- list.files("./Assignments", full.names = TRUE)
    
    # Read all the CSV files in the list, and combine them into a single data frame
    all_data <- files %>%
      set_names() %>%
      map_df(~{
        read_csv(.x, col_types = cols()) %>%
          # Add a new column called 'file' which contains the name of the file from which each row was read
          mutate(file = tools::file_path_sans_ext(basename(.x)))
      })
    
    # Calculate the maximum point value for the assignment. This is done by adding up the point
    # values for each first attempt of a question. We focus only on the first attempt to account
    # for point-degradation functionality.
    max_scores <- all_data %>%
      filter(attempt_num == 1) %>% # Only include first attempts.
      group_by(file, question_num) %>%
      summarize(max_points = max(points)) %>% # get max points per question.
      ungroup() 
    
    # Calculate total maximum score for all assignments.
    total_max <- sum(max_scores$max_points)
    
    # Add the total maximum score to the data frame.
    all_data <- mutate(all_data, total_max = total_max)
    
    # Return the final data frame.
    all_data
  })
  
  # Render the data table in the Shiny app.
  output$data <- renderTable({
    data <- req(data())
    data
  })
  
  # Renders the average scores per assignment plot in the Shiny app.
  output$assignment_plot <- renderPlot({
    data <- req(data())
    
    # Calculate average scores for each assignment.
    averages <- data %>%
      group_by(file) %>%
      summarise(avg = mean(points)) %>%
      ungroup()
    
    # Creates the plot.
    ggplot(averages, aes(x = file, y = avg)) +
      geom_col() +
      theme_minimal() +
      labs(x = "Assignment", y = "Average Score", title = "Average Scores per Assignment")
  })
  
  # Renders the scores by student plot.
  output$student_plot <- renderPlot({
    data <- req(data())
    
    # Calculate average scores for each student.
    student_avgs <- data %>%
      group_by(user_id) %>%
      summarise(avg = mean(points)) %>%
      ungroup()
    
    # Creates the plot.
    ggplot(student_avgs, aes(x = user_id, y = avg)) +
      geom_col() +
      theme_minimal() +
      labs(x = "Student", y = "Average Score", title = "Average Scores per Student")
  })
  
  # Render the final grades table.
  output$grades <- renderTable({
    data <- req(data())
    
    # Calculate total maximum points possible from first attempts of each question across all assignments.
    total_max <- data %>%
      filter(attempt_num == 1) %>%
      group_by(file, question_num) %>%
      summarize(max_points = max(points)) %>%
      ungroup() %>%
      summarise(total_max = sum(max_points)) %>%
      pull(total_max)
    
    # Calculate the final grades for each student.
    grades <- data %>%
      group_by(user_id) %>%
      summarise(achieved_score = sum(points[correct]),
                norm_score = achieved_score / total_max) %>%
      
      # Convert the normalized scores to letter grades.
      mutate(grade = case_when(
        norm_score >= 0.9 ~ "A",
        norm_score >= 0.8 ~ "B",
        norm_score >= 0.7 ~ "C",
        norm_score >= 0.6 ~ "D",
        TRUE ~ "F"
      )) %>%
      ungroup() %>%
      # Remove duplicate rows (if any) based on user_id.
      distinct(user_id, .keep_all = TRUE)
    
    grades
  })
}

shinyApp(ui = ui, server = server)
