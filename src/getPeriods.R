# This function calculates the periods of the segment "segment" of the ICU stay 
# "id" depending on the given funtion "fun". "fun" can be one of the following 
# values: "ls_periodogram", "fourier_periodogram", "ac_periodogram", 
# "cwt_periodogram", "chi_sq_periodogram"
# "data_column" contains the column which contains the values on which the
# periods will be calculated. "start" and "end" define the boundaries in which 
# periods will be searched.
# 
# Tested:
# fourier_periodogram ok
# cwt_periodogram start > 0
# ls_periodogram start > 0
# chi_sq_periodogram ok
# 
# data: the data frame containing the segmented data. 
# data_column: a string, denoting the column which contains the data to analyze.
# id: a string or integer denoting the ID in the "id_column" which needs to be 
#   analyzed.
# fun: the function as a string which should be used. One of the following is
#   possible: fourier_periodogram, cwt_periodogram, ls_periodogram start, 
#   chi_sq_periodogram. If using cwt_periodogram or ls_periodogram, make sure
#   that "start" is greater than zero.
# time_column: a string denoting the column in which the time is stored. 
# id_column: a string denoting the column in which the IDs are stored.
# segment_column: a string denoting the column in which the segment numbers are 
#   stored.
# start: an integer denoting the amount of seconds from which period length 
#   should be started.
# end: an integer denoting the amount of seconds at which period length should 
#   be ended.
# segment_number: an integer denoting the target segment.
# output: a string denoting the path of the output file.
# statistics: a string denoting the path of the statistics file.
# append: if the statistics an output file should be overwritten or extended.
#
getPeriods <- function
(
  data, 
  data_column, 
  id,
  fun, 
  time_column = "t", 
  id_column = "ICUSTAY_ID", 
  segment_column = "segment_id",
  start = ZERO, 
  end = THREE * SEC_PER_DAY, 
  segment_number = ONE,
  output = "./getPeriodsResult.csv",
  statistics = "./getPeriodsStatistics.csv",
  append = TRUE
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
  
  if (start >= end) {
    print("end has to be greater than start\n");
    return(NULL)
  }

  icu_stay_data = data[data[id_column] == id, ]
  segment_data = icu_stay_data[icu_stay_data[segment_column] == segment_number, ]
  
  if (nrow(segment_data) > ZERO) {
    data_values = segment_data[, data_column]
    data_time = segment_data[, time_column]
    
    df_time    <- tibble()
    df_time$id <- segment_number
    df_time    <- data.table(df_time)
    
    df_values     <- as_tibble(data_time)
    df_values     <- df_values %>% mutate(t = data_time)
    df_values$val <- data_values
    df_values     <- df_values %>% select(-value)
    df_values$id  <- segment_number
    df_values     <- data.table(df_values)
    df_values     <- na.omit (df_values, "val") 
    no_NA_values  <- nrow(df_values)
    
    setkey(df_time,   id)
    setkey(df_values, id)
    
    tryCatch(
      {
        eval <- behavr(df_values, df_time)
        periods <- periodogram(val, eval, period_range = c(start, end), FUN = get(fun))
        
        periods <- data.frame(periods)
        periods[id_column] <- id

        write.table(
          periods, 
          file = output, 
          append = append, 
          col.names = !append, 
          sep = ";", 
          row.names = FALSE
        )
      },
      error = function(cond) {
        print(cond)
        statistics_data <- data.frame(
          id = c(icu_stay_id), 
          segment_column = c(segment_id),
          values = c(no_NA_values), 
          msg = c(paste("Error:", conditionMessage(cond), sep = " "))
        )
          
        colnames(statistics_data) <- c(id_column, segment_column, "values", "msg")
        write.table(
          statistics_data, 
          statistics, 
          sep = ";", 
          row.names = FALSE, 
          col.names = !append, 
          append = append
        )
      },
      warning = function(cond) {
        statistics_data <- data.frame(
          id = c(icu_stay_id), 
          segment_column = c(segment_id),
          values = c(no_NA_values), 
          msg = c(paste("Warning:", conditionMessage(cond), sep = " "))
        )
        
        colnames(statistics_data) <- c(id_column, segment_column, "values", "msg")
        write.table(
          statistics_data, 
          statistics, 
          sep = ";", 
          row.names = FALSE, 
          col.names = !append, 
          append = append
        )
      },
      finally = {
      }
    )

  } else {
    statistics_data <- data.frame(
      id = c(icu_stay_id), 
      segment_column = c(segment_id),
      values = c(no_NA_values), 
      msg = c(paste("Warning:", "No data", sep = " "))
    )
    
    colnames(statistics_data) <- c(id_column, segment_column, "values", "msg")
    write.table(
      statistics_data, 
      statistics, 
      sep = ";", 
      row.names = FALSE, 
      col.names = !append, 
      append = append
    )
    
    return(NULL)
  }
}
