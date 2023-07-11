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
        tabPanel("Average Assignment Grades", 
                 plotOutput("average_plot")
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
    files <- list.files("./Assignments", full.names = TRUE)
    
    all_data <- files %>%
      set_names() %>%
      map_df(~{
        read_csv(.x, col_types = cols()) %>%
          mutate(file = tools::file_path_sans_ext(basename(.x)))
      })
    
    # Calculate the maximum point value for each question for each assignment
    max_scores <- all_data %>%
      filter(attempt_num == 1) %>% # Only include first attempts
      group_by(file, question_num) %>%
      summarize(max_points = max(points)) %>% # get max points per question
      ungroup() 

    # Calculate total maximum score for all assignments
    total_max <- sum(max_scores$max_points)
    
    # Add total_max to the data
    all_data <- mutate(all_data, total_max = total_max)
    
    all_data
  })
  
  output$data <- renderTable({
    data <- req(data())
    data
  })
  
  output$average_plot <- renderPlot({
    data <- req(data())
    
    averages <- data %>%
      group_by(file) %>%
      summarise(avg = mean(points)) %>%
      ungroup()
    
    ggplot(averages, aes(x = file, y = avg)) +
      geom_col() +
      theme_minimal() +
      labs(x = "Assignment", y = "Average Score", title = "Average Scores per Assignment")
  })
  
  output$student_plot <- renderPlot({
    data <- req(data())
    
    student_avgs <- data %>%
      group_by(user_id) %>%
      summarise(avg = mean(points)) %>%
      ungroup()
    
    ggplot(student_avgs, aes(x = user_id, y = avg)) +
      geom_col() +
      theme_minimal() +
      labs(x = "Student", y = "Average Score", title = "Average Scores per Student")
  })
  
  output$grades <- renderTable({
    data <- req(data())
    
    # Calculate total_max from first attempts of each question across all assignments
    total_max <- data %>%
      filter(attempt_num == 1) %>%
      group_by(file, question_num) %>%
      summarize(max_points = max(points)) %>%
      ungroup() %>%
      summarise(total_max = sum(max_points)) %>%
      pull(total_max)
    
    # Create grades dataframe, filtering for distinct user_id values after summarising
    grades <- data %>%
      group_by(user_id) %>%
      summarise(achieved_score = sum(points[correct]),
                norm_score = achieved_score / total_max) %>%
      mutate(grade = case_when(
        norm_score >= 0.9 ~ "A",
        norm_score >= 0.8 ~ "B",
        norm_score >= 0.7 ~ "C",
        norm_score >= 0.6 ~ "D",
        TRUE ~ "F"
      )) %>%
      ungroup() %>%
      distinct(user_id, .keep_all = TRUE)
    
    grades
  })
}

shinyApp(ui = ui, server = server)
