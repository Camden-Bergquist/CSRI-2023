eventRecorder <- function(tutorial_id, tutorial_version, event, data, user_id) {
  if (event %in% c("exercise_submission", "question_submission")) {
    
    question_num <- ifelse(grepl("\\[Question \\d+\\]", data$question), 
                           as.integer(gsub(".*\\[Question (\\d+)\\].*", "\\1", data$question)), NA)
    
    points <- ifelse(grepl("\\[Points: \\d+\\]", data$question), 
                     as.integer(gsub(".*\\[Points: (\\d+)\\].*", "\\1", data$question)), 1)
    
    csv_file_path <- "./data/logfile.csv"
    if (file.exists(csv_file_path)) {
      existing_data <- read.csv(csv_file_path, stringsAsFactors = FALSE)
      previous_rows <- existing_data[existing_data$question_num == question_num & existing_data$user_id == user_id, ]
      
      if (nrow(previous_rows) > 0) {
        attempt_num <- max(previous_rows$attempt_num, na.rm = TRUE) + 1
        points <- points - 0.5 * (attempt_num - 1)
      } else {
        attempt_num <- 1
      }
    } else {
      attempt_num <- 1
    }
    
    new_row <- data.frame(
      user_id = user_id,
      question_num = question_num,
      attempt_num = attempt_num,
      correct = ifelse(data$correct, TRUE, FALSE),
      points = points,
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
    
    if (file.exists(csv_file_path)) {
      combined_data <- dplyr::bind_rows(existing_data, new_row)
      write.table(combined_data, file = csv_file_path, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(new_row, file = csv_file_path, sep = ",", row.names = FALSE, col.names = TRUE)
    }
  }
}
