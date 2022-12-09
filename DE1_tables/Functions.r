table_formatting <- function(sum_table) {

    year <- substr(deparse(substitute(sum_table)), 9, 12)
    variable_name <- substr(deparse(substitute(sum_table)), 1, 7)

    sum_table <- sum_table %>%
        mutate(Reference_year = year,
               Variable_code = variable_name,
               Flags = "",
               Footnote = "") %>%
        select(code,
               Variable_code,
               Reference_year,
               Value,
               Flags,
               Footnote) %>%
        rename(City_code = code) %>%
        arrange(City_code)
    
    return(sum_table)
}

sum_rows <- function(filter_by, filter_to) {
    
    x <- deparse(substitute(filter_by))

    df_DE1 %>%
        filter(df_DE1[x] == filter_to) %>%
        group_by(code) %>%
        summarise(Value = sum(value))
}

sum_rows_2 <- function(Alder_filter, Kjonn_filter) {

    df_DE1 %>%
        filter(Alder == Alder_filter,
               Kjonn == Kjonn_filter) %>%
        select(code, value) %>%
        rename(Value = value)
}