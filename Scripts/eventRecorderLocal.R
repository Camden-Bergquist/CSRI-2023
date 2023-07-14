## This function records learnr tutorial events and writes the data to a csv.

eventRecorder <- function(tutorial_id, tutorial_version, event, data, user_id) {
  
  # This 'if' condition filters incoming events. The goal is to only process 
  # 'exercise_submission' and 'question_submission' events, as these contain meaningful user data.
  # All other event types aren't useful for our current purposes and will be ignored.
  if (event %in% c("exercise_submission", "question_submission")) {
    
    # Extract the question number from the data if it exists.
    question_num <- ifelse(grepl("\\[Question \\d+\\]", data$question), 
                           as.integer(gsub(".*\\[Question (\\d+)\\].*", "\\1", data$question)), NA)
    
    # Extract the points from the data if it exists, else set it to 1.
    points <- ifelse(grepl("\\[Points: \\d+\\]", data$question), 
                     as.integer(gsub(".*\\[Points: (\\d+)\\].*", "\\1", data$question)), 1)
    
    # Define the path to the CSV file where event data will be recorded.
    csv_file_path <- "./data/logfile.csv"
    
    # If the CSV file exists.
    if (file.exists(csv_file_path)) {
      # Load the existing data.
      existing_data <- read.csv(csv_file_path, stringsAsFactors = FALSE)
      # Filter the data for previous attempts by the same user on the same question.
      previous_rows <- existing_data[existing_data$question_num == question_num & existing_data$user_id == user_id, ]
      
      # If previous attempts exist.
      if (nrow(previous_rows) > 0) {
        # Increment the attempt number and calculate the points.
        attempt_num <- max(previous_rows$attempt_num, na.rm = TRUE) + 1
        points <- points - 0.5 * (attempt_num - 1)
      } else {
        # If no previous attempts exist, this is the first one.
        attempt_num <- 1
      }
    } else {
      # If the CSV file does not exist, this is the first attempt.
      attempt_num <- 1
    }
    
    # Create a new row of data for the current event submission.
    new_row <- data.frame(
      user_id = user_id,
      question_num = question_num,
      attempt_num = attempt_num,
      correct = ifelse(data$correct, TRUE, FALSE),
      points = points,
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
    
    # If the CSV file exists, append the new row to it.
    if (file.exists(csv_file_path)) {
      combined_data <- dplyr::bind_rows(existing_data, new_row)
      write.table(combined_data, file = csv_file_path, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      # If the CSV file does not exist, create it and write the new row to it.
      write.table(new_row, file = csv_file_path, sep = ",", row.names = FALSE, col.names = TRUE)
    }
  }
}
