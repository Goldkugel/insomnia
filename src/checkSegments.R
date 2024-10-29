library(tidyr)

# Checks the created segments, if they are suitable of all four possible
# function which can determine the periodicity of the function. A new column 
# will be added to the data which denotes if a segment is valid or not. If
# the column exists already and is set to False, it will be changed to True
# if the segment is not valid but the other way around will not happen. With
# this feature it is possible to verify if a segment can be used to calculate
# the periodicity based on different columns in data.
#
# data: a dataframe.
# data_column: the column in which the data is stored e.g. heartrate.
# id_column: the column in which the ids of interest are saved e.g. ICU stay 
#   ids.
# segment_column: the column in which the id/number of segment is stored.
# non_NA_values: the minimum amount of values which need to be set per segment.
# different_values: the minimum amount of different values in one segment.
# output: the file in which the output is stored. This file will be cleared at
#   the beginning.
# statistics: the file in which the statistics are stored. This file will be
#   cleared in the beginning. 
#
checkSegments <- function
(
    data, 
    data_column,
    id_column = "ICUSTAY_ID",
    segment_column = "segment_id",
    non_NA_values = TWENTY,
    different_values = FIVE,
    output = "./.checkSegmentsResult.csv",
    statistics = "./.checkSegmentsStatistics.csv"
) 
{
  
  if(!(data_column %in% names(data))) {
    print("The data_column has not be found in the data.\n");
    return(NULL)
  }
  
  if(!(id_column %in% names(data))) {
    print("The id_column has not be found in the data.\n");
    return(NULL)
  }
  
  if(!(segment_column %in% names(data))) {
    print("The segment_column has not be found in the data.\n");
    return(NULL)
  }
  
  if (non_NA_values < ONE) {
    print("The non_NA_values has to be greater than one\n");
    return(NULL)
  }
  
  if (different_values < ONE) {
    print("The different_values has to be greater than one\n");
    return(NULL)
  }
  
  # Since binding one data frame to each other is time consuming, the data will
  # be exported in a csv file. 
  result <- data.frame(data)
  result[segment_column] <- rep(-ONE, nrow(data))
  result <- data.frame(result)[FALSE,]
  
  # Write just the header in the file.
  write.table(
    result, 
    output, 
    sep = ";", 
    row.names = FALSE, 
    col.names = TRUE, 
    append = FALSE
  )
  
  # How many ICU stays does the data contain?
  icu_stay_ids <- unique(data[id_column])
  row.names(icu_stay_ids) <- NULL
  
  # For statistic and logging purposes any segments which failed the checks 
  # will be monitored.
  statistics_data <- data.frame(matrix(ncol = FOUR, nrow = ZERO))
  colnames(statistics_data) <- c("id", "segment_id", "non_NA_values", "different_values")
  write.table(
    statistics_data, 
    statistics, 
    sep = ";", 
    row.names = FALSE, 
    col.names = TRUE, 
    append = FALSE
  )
  
  # The amount of ICU stays which have been processed.
  icu_stay_count <- ZERO
  
  # Foreach ICU stay.
  for (icu_stay_id in icu_stay_ids[, id_column]) {
    icu_stay_count <- icu_stay_count + ONE
    
    # Print the current progress.
    print(icu_stay_count / length(icu_stay_ids[, id_column]))
    
    # Get all the rows of the ICU stay.
    icu_stay_rows <- data[data[, id_column] == icu_stay_id, ]
    
    # Get the number of all used segments.
    segment_ids <- unique(icu_stay_rows[segment_column])
    
    # Reset row indexes.
    row.names(segment_ids) <- NULL
    
    # For each segment certain conditions need to be fulfilled. 
    for (segment_id in segment_ids[, segment_column]) {
      
      # Get the rows of the segment.
      segment_rows <- icu_stay_rows[icu_stay_rows[, segment_column] == segment_id, ]
      
      # Get the values of interest of the segment.
      segment_values <- segment_rows[data_column]
      
      # Drop all values which are NA and get all unique values.
      segment_no_NA_values = drop_na(segment_values)
      segment_unique_values = unique(segment_no_NA_values)
      
      unique_values = nrow(segment_unique_values)
      no_NA_values = nrow(segment_no_NA_values)

      # If the conditions are fulfilled, the segments can be saved.
      if (unique_values >= different_values & no_NA_values >= non_NA_values) {
        # Save the segment rows in the file. 
        write.table(
          segment_rows, 
          file = output, 
          append = TRUE, 
          col.names = FALSE, 
          sep = ";", 
          row.names = FALSE
        )
      } else {
        # If the conditions are not fulfilled, the segment is dropped and
        # logged.
        statistics_data <- data.frame(
          id = c(icu_stay_id), 
          segment_id = c(segment_id),
          no_NA_values = c(no_NA_values), 
          unique_values = c(unique_values)
        )
        
        write.table(
          statistics_data, 
          statistics, 
          sep = ";", 
          row.names = FALSE, 
          col.names = FALSE, 
          append = TRUE
        )
      }
    }
  }
}