# Author:  Jarad Niemi
# Date:    2025-10-15
# Purpose: HW09_ProgrammingPart02 solutions
# ------------------------------------------------------------------------------
# library("ggplot2")

classify_number <- function(x) {
  if (x < 0) {
    return("Negative")
  } else if (x == 0) {
    return("Zero")
  } else if (x > 0 && x < 10) {
    return("Small")
  } else {
    return("Large")
  }
}



filter_even_positive <- function(vec) {
  vec[vec > 0 & vec %% 2 == 0]
}



assign_grade <- function(scores) {
  ifelse(scores >= 90, "A",
    ifelse(scores >= 80, "B",
      ifelse(scores >= 70, "C",
        ifelse(scores >= 60, "D", "F")
      )
    )
  )
}



simple_calculator <- function(x, y, operation) {
  switch(operation,
         add      = x + y,
         subtract = x - y,
         multiply = x * y,
         divide   = x / y,
         stop("Invalid operation")
  )
}



count_conditions <- function(vec) {
  pos <- 0
  neg <- 0
  zero <- 0
  na_count <- 0
  
  for (val in vec) {
    if (is.na(val)) {
      na_count <- na_count + 1
    } else if (val > 0) {
      pos <- pos + 1
    } else if (val < 0) {
      neg <- neg + 1
    } else {
      zero <- zero + 1
    }
  }
  
  return(list(
    positives = pos,
    negatives = neg,
    zeros     = zero,
    nas       = na_count
  ))
}



summarize_scores <- function(df) {
  num_students   <- nrow(df)
  num_passed    <- sum(df$passed, na.rm = TRUE)
  average_score <- mean(df$score, na.rm = TRUE)
  highest_score <- df$name[which.max(df$score)]
  
  return(list(
    num_students  = num_students,
    num_passed    = num_passed,
    average_score = average_score,
    highest_score = highest_score
  ))
}

student_scores <- data.frame(
  name = c("Alice", "Bob", "Charlie", "Diana", "Eli"),
  score = c(88, 92, 67, 75, 59),
  passed = c(TRUE, TRUE, FALSE, TRUE, FALSE)
)

summarize_scores(student_scores)

summarize_by_group <- function(df, var, group) {
  levels   <- sort(as.character(unique(df[, group])))
  n_levels <- length(levels)
  
  n <- mean <- sd <- numeric(0)
  for (i in 1:n_levels) {
    # Subset data.frame
    sub_df  <- df[df[, group] == levels[i],]
    
    # Calculate summary statistics
    n[i]    <- sum(!is.na(sub_df[,var]))
    mean[i] <- mean(      sub_df[,var], na.rm = TRUE)
    sd[i]   <- sd(        sub_df[,var], na.rm = TRUE)
  }
  
  df_s <- data.frame(
    group = levels,
    n     = n,
    mean = mean,
    sd   = sd
  )
  names(df_s)[1] = group
  
  return(df_s)
}





modify_plot <- function(g, log_x = FALSE, log_y = FALSE, 
                        x_label = NULL, y_label = NULL, title = NULL) {
  if (log_x) 
    g <- g + scale_x_log10()
  else
    g <- g + scale_x_continuous()
  
  if (log_y) 
    g <- g + scale_y_log10()
  else
    g <- g + scale_y_continuous()
  
  if (!is.null(x_label)) g <- g + labs(x = x_label)
  if (!is.null(y_label)) g <- g + labs(y = y_label)
  if (!is.null(title))   g <- g + labs(title = title)
  
  return(g)
}
