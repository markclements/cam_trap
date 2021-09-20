delete_record <- function(df, ev, re){
  
  if(dim(df)[1] <= 1) return(tibble())
  df %>%
    filter(.data$event != .env$ev | 
             .data$record != .env$re) %>%
    group_by(event) %>%
    mutate(record = 1:n()) %>%
    ungroup() -> x
  return(x)

}
