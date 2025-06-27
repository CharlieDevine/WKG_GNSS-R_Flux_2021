datefill.fun = function(data.in){
  data.in$Dates = as.Date(data.in$Dates)
  data.out = padr::pad(data.in, start_val = start.date, end_val = end.date, interval = 'day', by = 'Dates')
  return(data.out)
}