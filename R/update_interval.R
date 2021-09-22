
update_interval <- function(control_file, annotations, interval = 15){

control_file %>% 
  rename("old_event" = "event") %>% 
  arrange(date_time) %>%
  mutate(duration = interval(lag(date_time),date_time)/dminutes(1)) %>% 
  mutate(event = cumsum(!is.na(duration) & duration >= interval) + 1) -> control_updated

control_updated %>% 
  select(old_event,event) %>% 
  distinct() -> event_look_up

annotations %>% 
  rename("old_event" = "event") %>%
  left_join(.,event_look_up) %>% 
  select(-old_event) %>%
  group_by(event) %>%
  mutate(record = 1:n()) %>% 
  ungroup() -> annotations

control_updated %>% 
  select(-old_event) -> control_file

return(list(control_file = control_file,annotations = annotations))
  
} 
  


