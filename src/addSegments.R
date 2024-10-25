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
addSegment <- function
(
    data,
    time_column = "t",
    id_column = "ICUSTAY_ID",
    segment_length = THREE * SEC_PER_DAY,
    segment_offset = SEC_PER_DAY,
    segment_column = "segment_id",
    tmp = "./.tmp.csv"
) 
{
  result <- data.frame(data)
  result[segment_column] <- rep(-ONE, nrow(data))
  result <- data.frame(result)[FALSE,]
  
  write.table(
    result, 
    tmp, 
    sep = ";", 
    row.names = FALSE, 
    col.names = TRUE, 
    append = FALSE
  )
  
  ids <- unique(data[id_column])
  row.names(ids) <- NULL
  
  count <- ZERO
  for (id in ids[, id_column]) {
    count <- count + ONE
    
    # Print the current progress.
    print(count / length(ids[, id_column]))
    rows <- data[data[, id_column] == id, ]
    
    max_time <- max(rows[time_column])
    min_time <- ZERO
    
    segment_count <- max(c(ONE, 
       ceiling((max_time - segment_length) / segment_offset))
    )
    
    for (segment_id in ONE:segment_count) {
      segment_rows <- rows[
        rows[time_column] >= segment_offset * (segment_id - ONE) & 
        rows[time_column] <  segment_offset * segment_id + segment_length
      ,]
      
      if(nrow(segment_rows[]) > ZERO) {
        segment_rows[segment_column] <- segment_id
        row.names(result) <- NULL
        
        write.table(
          segment_rows, 
          file = tmp, 
          append = TRUE, 
          col.names = FALSE, 
          sep = ";", 
          row.names = FALSE
        ) 
      }
    }
  }

  return(read.table(file = tmp, sep = ";", row.names = NULL))
}