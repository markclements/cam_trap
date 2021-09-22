#
# processes the input directory of images 
# output is a tibble with image, date_time (created), and event number
# inputs are the directory of images and the length of time for the interval detection
#
determine_capture_intervals <- function(directory, interval = 15) {


  dir_ls(directory) %>%
    keep(str_detect(.,".JPG")) -> temp

  if(length(temp>0)){
    print("processing file")
  temp %>%  
    read_exif(.) %>% 
    select(c("FileName","CreateDate")) %>%
    mutate(date_time = lubridate::ymd_hms(CreateDate)) %>%
    select(-CreateDate) %>%
    arrange(date_time) %>%
    mutate(duration = interval(lag(date_time),date_time)/dminutes(1)) %>% 
    mutate(event = cumsum(!is.na(duration) & duration >= interval) + 1) %>%
    select(-duration) %>%
    rename(image = "FileName") -> tbl
  return(tbl)
  } else {
    print("directory empty")
    tbl <- tibble(image = NA, date_time = NA, event = NA)
    return(tbl)
  }
}
