# This function splits the given data into time windows called segments. Since 
# rows might be contained in multiple segments, they will be listed multiple 
# times in the resulting data frame. For each value in "id_column" segments
# will be created with length "segment_length" and with an offset of
# "segment_offset". 
#
# Example:
# 
# If data[time_column] looks like [0,1,2,3,5,6,7,10,12,13,14,15,16] and segments
# with length 4 and offset 3 needs to be created, the segments will be: 
# [0, 1, 2, 3], [3, 5, 6, 7], [6, 7, 10], [10, 12, 13], [12, 13, 14, 15], [15, 16]
# The first segment starts from 0 and having length 4 it ends with 3. The second 
# segment starts from 3 and ends with 7. The third segment starts with 6
# end ends with 10. The fourth starts from 9, which is not in the data and ends 
# at 13. The fifth starts at 12 and ends at 15. The last segment starts at 15
# and ends at 16.
#
# data: a dataframe.
# time_column: the column in which the time in milliseconds is saved.
# id_column: the column in which the ids of interest are saved e.g. ICU stay 
#   ids.
# segment_length: the length of the segments in seconds.
# segment_offset: the offset of each segment.
# segment_column: the column in which the id/number of segment is stored.
# output: the file in which the output is stored. This file will be cleared at
#   the beginning.
# statistics: the file in which the statistics are stored. This file will be
#   cleared in the beginning. 
#
addSegment <- function
(
    data,
    time_column = "t",
    id_column = "ICUSTAY_ID",
    segment_length = THREE * SEC_PER_DAY,
    segment_offset = SEC_PER_DAY,
    segment_column = "segment_id",
    output = "./.addSegmentsResult.csv",
    statistics = "./.addSegmentsStatistics.csv"
) 
{
  
  # Check if the parameters are fine.
  if(!(time_column %in% names(data))) {
    print("The time_column has not be found in the data.\n");
    return(NULL)
  }
  
  if(!(id_column %in% names(data))) {
    print("The id_column has not be found in the data.\n");
    return(NULL)
  }
  
  if (segment_length <= ZERO) {
    print("The segment_length has to be greater than zero.\n");
    return(NULL)
  }
  
  if (segment_offset <= ZERO) {
    print("The segment_offset has to be greater than zero.\n");
    return(NULL)
  }
  
  # Since binding one data frame to each other is time consuming, the data will
  # be exported in a csv file. 
  result <- data.frame(data)
  result[segment_column] <- rep(-ONE, nrow(data))
  result <- data.frame(result)[FALSE,]
  
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
  
  statistics_data <- data.frame(matrix(ncol = THREE, nrow = ZERO))
  colnames(statistics_data) <- c("id", "segments_count", "segments_written")
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
    rows <- data[data[, id_column] == icu_stay_id, ]
    
    max_time <- max(rows[time_column])
    min_time <- ZERO
    
    # The total amount of segments for this stay is calculated.
    segments_count <- max(c(ONE, 
       ceiling((max_time - segment_length) / segment_offset))
    )
    
    segments_written <- ZERO
    
    # calculate the rows which are part of the segment with the segment id 
    # "segment_id".
    for (segment_id in ONE:segments_count) {
      
      # Get all rows which belong to the segment. 
      segment_rows <- rows[
        rows[time_column] >= segment_offset * (segment_id - ONE) & 
        rows[time_column] <  segment_offset * segment_id + segment_length
      ,]
      
      # If the segment contains some data it is worthy to be saved.
      if(nrow(segment_rows[]) > ZERO) {
        segment_rows[segment_column] <- segment_id
        row.names(result) <- NULL
        
        # Save the segment rows in the file. 
        write.table(
          segment_rows, 
          file = output, 
          append = TRUE, 
          col.names = FALSE, 
          sep = ";", 
          row.names = FALSE
        ) 
        
        segments_written <- segments_written + ONE
      }
    }
    
    if (segments_count != segments_written) {
      statistics_data <- data.frame(
        id = c(icu_stay_id), 
        segments_count = c(segments_count), 
        segments_written = c(segments_written)
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