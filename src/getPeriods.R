# This function calculates the periods of the segment "segment" of the ICU stay 
# "id" depending on the given funtion "fun". "fun" can be one of the following 
# values: "ls_periodogram", "fourier_periodogram", "ac_periodogram", 
# "cwt_periodogram"
# "data_column" contains the column which contains the values on which the
# periods will be calculated. "start" and "end" define the boundaries in which 
# periods will be searched.
#
getPeriods <- function
(
  data, 
  id, 
  data_column, 
  fun, 
  time_column = "t", 
  id_column = "ICUSTAY_ID", 
  start = 0, 
  end = 1, 
  segment = NULL
) 
{
  
}