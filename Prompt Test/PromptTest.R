generate_learnr_question <- function(file_name = "learnr_questions.Rmd") {
  # Initialize question number
  question_number <- 0
  if (file.exists(file_name)) {
    lines <- readLines(file_name)
    last_question_line <- grep("<!-- Last question number:", lines, value = TRUE)
    if (length(last_question_line) > 0) {
      question_number <- as.numeric(sub(".*Question ([0-9]+).*", "\\1", last_question_line))
    }
    # Now remove the comment tracking the last question number
    lines <- lines[-grep("<!-- Last question number:", lines)]
    writeLines(lines, file_name)
  }
  
  repeat {
    # Increment question number
    question_number <- question_number + 1
    
    # Open the RMarkdown file
    fileConn <- file(file_name, open = "a")
    
    # Prompt for question type
    cat("Please enter the question type. [1] Multiple choice / [2] Numeric): ")
    question_type <- readLines(n = 1)
    
    # Prompt for points
    cat("Please enter the points for this question: ")
    points <- gsub(",", "", readLines(n = 1))
    
    if (question_type == "1") {
      # Prompt for multiple choice question information
      cat("Please enter the question query: ")
      question_query <- readLines(n = 1)
      
      # Add question number and points to the query
      question_query <- paste0("[Question ", question_number, "] [Points: ", points, "] ", question_query)
      
      # Prompt for the number of correct answers
      cat("Please enter the number of correct answers: ")
      num_correct <- as.integer(readLines(n = 1))
      
      # Create a list to hold all the answers
      answers <- list()
      
      # Loop to prompt for each correct answer
      for (i in 1:num_correct) {
        cat(paste0("Please enter correct answer ", i, ": "))
        correct_answer <- gsub(",", "", readLines(n = 1))
        answers[[i]] <- paste0("answer('", correct_answer, "', correct = TRUE)")
      }
      
      # Prompt for the number of incorrect answers
      cat("Please enter the number of incorrect answers: ")
      num_incorrect <- as.integer(readLines(n = 1))
      
      # Loop to prompt for each incorrect answer
      for (i in 1:num_incorrect) {
        cat(paste0("Please enter incorrect answer ", i, ": "))
        incorrect_answer <- gsub(",", "", readLines(n = 1))
        answers[[i + num_correct]] <- paste0("answer('", incorrect_answer, "', correct = FALSE)")
      }
      
      # Generate the learnr multiple choice question syntax
      question_block <- paste0(
        "\n\n```{r question", question_number, "}\n",
        "question(\"", question_query, "\",\n",
        "    ", paste(answers, collapse = ",\n    "), ",\n",
        "    allow_retry = TRUE,\n",
        "    random_answer_order = TRUE)\n",
        "```"
      )
    } else if (question_type == "2") {
      # Prompt for numeric question information
      cat("Please enter the question query: ")
      question_query <- readLines(n = 1)
      
      # Add question number and points to the query
      question_query <- paste0("[Question ", question_number, "] [Points: ", points, "] ", question_query)
      
      cat("Please enter the correct answer: ")
      correct_answer <- gsub(",", "", readLines(n = 1))
      
      # Prompt for tolerance
      cat("Please enter the tolerance: ")
      tolerance <- gsub(",", "", readLines(n = 1))
      
      # Generate the learnr numeric question syntax
      question_block <- paste0(
        "\n\n```{r question", question_number, "}\n",
        "question(\"", question_query, "\",\n",
        "    answer(", correct_answer, ", correct = TRUE),\n",
        "    allow_retry = TRUE,\n",
        "    tolerance = ", tolerance, ")\n",
        "```"
      )
    }
    
    # Write the question block to the file
    cat(file=fileConn, question_block, "\n")
    
    # Close the RMarkdown file
    close(fileConn)
    
    # Ask the user if they want to generate another question
    cat("Would you like to generate another question? [1] Yes / [0] No: ")
    another <- readLines(n = 1)
    if (another != "1") break
  }
  
  # Open the RMarkdown file
  fileConn <- file(file_name, open = "a")
  
  # Ensure the file connection is closed when exiting this function or in case of error
  on.exit(close(fileConn), add = TRUE)
  
  # Write the last question number as a comment at the end of the file
  cat(file=fileConn, paste0("<!-- Last question number: Question ", question_number, " -->"), "\n")

}
