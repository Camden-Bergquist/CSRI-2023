library(shiny)
library(shinyWidgets)
library(stringr)
library(DT)

ui <- fluidPage(
  titlePanel("Learnr Question Generator"),
  sidebarLayout(
    sidebarPanel(
      textInput('fileName', 'Enter Rmd file name', value = "Assignment.Rmd"),
      uiOutput("fileFound"),
      actionButton("newFileButton", "Create New File"),
      numericInput('points', 'Points', value = 1, min = 1),
      textInput('query', 'Question Query'),
      radioButtons('questionType', 'Question type',
                   choices = c('Multiple Choice' = 1, 'Numeric' = 2)),
      conditionalPanel(
        condition = "input.questionType == 1",
        sliderInput('numCorrect', 'Number of correct answers', min = 1, max = 10, value = 1),
        tags$span("Multiple correct answers create a checkbox question.", 
                  style = "font-size: 12px; padding-left: 5px;", 
                  title = "Multiple correct answers create a checkbox question."),
        sliderInput('numIncorrect', 'Number of incorrect answers', min = 1, max = 10, value = 3),
        uiOutput("correctAnswers"),
        uiOutput("incorrectAnswers")
      ),
      conditionalPanel(
        condition = "input.questionType == 2",
        numericInput('correctAnswer', 'Correct answer', value = 1),
        numericInput('tolerance', 'Tolerance', value = 0.1)
      ),
      actionButton("addQuestion", "Add Question"),
      actionButton("writeToFile", "Write Questions to File")
    ),
    mainPanel(
      DTOutput('table'),
      verbatimTextOutput("statusText")
    )
  )
)

server <- function(input, output, session) {
  selectedFilePath <- reactiveVal(NULL)
  
  observe({
    output$fileFound <- renderUI({
      invalidateLater(1000)  # Checks file status every second
      if (file.exists(paste0("./Assignments/", input$fileName))) {
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
  
  questions <- reactiveValues(data = data.frame(
    Type = character(0), Query = character(0), Answers = character(0), Points = numeric(0), Tolerance = numeric(0)
  ))
  
  observe({
    req(input$numCorrect)
    output$correctAnswers <- renderUI({
      lapply(seq_len(input$numCorrect), function(i) {
        textInput(paste0('correctAnswer', i), paste0('Correct answer ', i))
      })
    })
  })
  
  observe({
    req(input$numIncorrect)
    output$incorrectAnswers <- renderUI({
      lapply(seq_len(input$numIncorrect), function(i) {
        textInput(paste0('incorrectAnswer', i), paste0('Incorrect answer ', i))
      })
    })
  })
  
  observeEvent(input$fileName, {
    selectedFilePath(paste0("./Assignments/", input$fileName))
  })
  
  observeEvent(input$newFileButton, {
    rmd_file <- selectedFilePath()
    if (file.exists(rmd_file)) {
      showNotification("File already exists. Please choose a different name.", type = "warning")
    } else {
      dir.create("./Assignments", showWarnings = FALSE)
      fileConn <- file(rmd_file, open = "w")
      setup_chunk <- '---
title: ""
output: learnr::tutorial
runtime: shiny_prerendered
---

```{r setup, include = FALSE}
library(learnr)
library(dplyr)
library(tidyr)
library(purrr)
library(googlesheets4)
library(googledrive)

source("~/CSRI-2023/Scripts/eventRecorderLocal.R")

# REQUIRED: Set the global option to use our event recorder function.
options(tutorial.event_recorder = eventRecorder)

# Set knitr options
knitr::opts_chunk$set(echo = FALSE)
```'
      cat(file=fileConn, setup_chunk, "\n")
      
      close(fileConn)
      showNotification("New file created!", type = "message")
    }
  })
  
  observeEvent(input$addQuestion, {
    req(input$questionType, input$query)
    if (input$questionType == 1) {
      req(input$numCorrect, input$numIncorrect)
    } else {
      req(input$correctAnswer, input$tolerance)
    }
    
    question <- list(
      Type = input$questionType,
      Query = input$query,
      Points = input$points,
      Tolerance = NA
    )
    if (input$questionType == 1) {
      question$Answers <- paste(c(
        sapply(seq_len(input$numCorrect), function(i) input[[paste0('correctAnswer', i)]]),
        sapply(seq_len(input$numIncorrect), function(i) input[[paste0('incorrectAnswer', i)]])
      ), collapse = "|")
    } else {
      question$Answers <- input$correctAnswer
      question$Tolerance <- input$tolerance
    }
    
    questions$data <- rbind(questions$data, question)
    showNotification("Question added!", type = "message")
  })
  
  output$table <- renderDT({
    questions$data
  })
  
  observeEvent(input$writeToFile, {
    req(nzchar(selectedFilePath()))
    rmd_file <- selectedFilePath()
    if (file.exists(rmd_file)) {
      # Read the file content
      file_content <- readLines(rmd_file)
      # Find the line with the last question number
      last_question_index <- grep("<!-- Last question number: Question", file_content)
      last_question_number <- 0
      # If found, delete this line and all lines after it
      if (length(last_question_index) > 0) {
        last_question_number <- as.integer(str_extract(file_content[last_question_index], "\\d+"))
        file_content <- file_content[-c(last_question_index:length(file_content))]
      }
      # Write the (possibly modified) file content back to the file
      writeLines(file_content, rmd_file)
      
      fileConn <- file(rmd_file, open = "a")
      cat("\n", file = fileConn)
      
      for (i in seq_len(nrow(questions$data))) {
        question_number <- last_question_number + i
        question <- questions$data[i, ]
        if (question$Type == 1) {
          answers <- strsplit(question$Answers, "\\|")[[1]]
          correct_count <- ifelse(question$Points > length(answers), length(answers), question$Points)
          answers <- c(
            paste0("answer('", answers[1:correct_count], "', correct = TRUE)"),
            paste0("answer('", answers[(correct_count + 1):length(answers)], "', correct = FALSE)")
          )
          question_block <- paste0(
            "\n\n```{r question_", question_number, "}\n",
            "question(\"[Question ", question_number, "] [Points: ", question$Points, "] ", question$Query, "\",\n",
            "    ", paste(answers, collapse = ",\n    "), ",\n",
            "    allow_retry = TRUE,\n",
            "    random_answer_order = TRUE)\n",
            "```\n"
          )
        } else {
          question_block <- paste0(
            "\n\n```{r question_", question_number, "}\n",
            "question_numeric(\"[Question ", question_number, "] [Points: ", question$Points, "] ", question$Query, "\",\n",
            "    answer(", question$Answers, ", correct = TRUE),\n",
            "    allow_retry = TRUE,\n",
            "    tolerance = ", question$Tolerance, ")\n",
            "```\n"
          )
        }
        cat(question_block, file = fileConn, append = TRUE)
      }
      
      # Write the last question number as a comment at the end of the file
      cat(file=fileConn, paste0("<!-- Last question number: Question ", question_number, " -->"), "\n")
      
      close(fileConn)
      showNotification("Questions written to file!", type = "message")
    } else {
      showNotification("Please choose an existing file or enter a new file name.", type = "warning")
    }
  })
  
}

shinyApp(ui = ui, server = server)