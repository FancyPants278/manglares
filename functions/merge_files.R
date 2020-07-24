
merge_files <- function(list_of_files, .x, file_name, X1, X2, date) {
        list_of_files %>%
                set_names() %>% 
                map_dfr(
                        ~ read_csv(.x, col_types = cols(), col_names = FALSE, skip = 1),
                        .id = "file_name"
                ) %>% 
                rename(file_name=file_name, date=X1, var=X2) %>% 
                mutate(date = parse_date_time(date, orders = c("mdy")))
}
