# Function to record tutorial events
event_recorder <- function(tutorial_id, tutorial_version, user_id, event, data) {
  
  # Check if event is exercise_submission or question_submission
  if (event %in% c("exercise_submission", "question_submission")) {
    
    # Extract question number from the question
    question_num <- ifelse(grepl("\\[Question \\d+\\]", data$question), 
                           gsub(".*\\[Question (\\d+)\\].*", "\\1", data$question), NA)
    
    # Extract points from the question
    points <- ifelse(grepl("\\[Points: \\d+\\]", data$question), 
                     as.integer(gsub(".*\\[Points: (\\d+)\\].*", "\\1", data$question)), 1)
    
    # Extract problem without [Question X] and [Points: X]
    problem <- gsub("\\[Question \\d+\\]|\\[Points: \\d+\\]", "", data$question)
    
    # Check if there's already a row with the same user_id and question_num
    previous_rows <- df[df$user_id == user_id & gsub("_.*", "", df$question_num) == paste0("Q", question_num), ]
    if (nrow(previous_rows) > 0) {
      attempt_num <- max(as.numeric(gsub(".*_(\\d+)$", "\\1", previous_rows$question_num))) + 1
      # Adjust points for retries
      points <- points - 0.5 * (attempt_num - 1)
    } else {
      attempt_num <- 1
    }
    
    # Create new row with event data
    new_row <- data.frame(
      user_id = user_id,
      problem = problem,  # use the new problem variable here
      points = points,
      answer = data$answer, 
      correct = ifelse(data$correct, TRUE, FALSE),
      question_num = paste0("Q", question_num, "_", attempt_num),  # Add "Q" before the question number and "_attempt_num"
      stringsAsFactors = FALSE  # Prevent strings being converted to factors
    )
    
    # Append new row to global data frame only if the data's question is not "Submit Assignment?"
    if(data$question != "Submit Assignment?") {
      df <<- rbind(df, new_row)
    }
    
    # If it's the final question, reshape and write data frame to Google Sheets
    if(data$question == "Submit Assignment?") {
      
      # Reshape data frame to long format
      df_long <- df %>% 
        gather(key = "key", value = "value", -user_id, -question_num)
      
      # Pivot the data frame back to wide format
      df_wide <- df_long %>% 
        pivot_wider(names_from = c("question_num", "key"), names_sep = "_", values_from = "value")
      
      # Generate the column order
      unique_questions <- sort(unique(df$question_num))
      col_order <- c("user_id", unlist(lapply(unique_questions, function(i) paste0(i, c("_problem", "_answer", "_correct", "_points")))))
      
      # Reorder the columns
      df_wide <- df_wide[, col_order]
      
      # Create a new Google Sheet and write the data frame to it
      ss <- gs4_create(name = "logfile")
      sheet_write(df_wide, ss = ss, sheet = "Sheet1")
    }
  }
}