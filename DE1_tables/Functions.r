###############################################################
# Functions made for production of demograhpy tables (DE1_2022)
# Project: CITYSTATISTICS
# Author: Stian Nergård
###############################################################

# Summarise rows function:
# Takes a variable to filter on and what to filter to as inputs,
#    and outputs a summary table.
# Eg. sum_rows(Alder, "F00-04")

sum_rows <- function(filter_by, filter_to) {
    
    x <- deparse(substitute(filter_by)) # creates a string

    df_DE1 %>%
        filter(df_DE1[x] == filter_to) %>%
        group_by(code) %>%
        summarise(Value = sum(value))
}

# Filter rows function:
# Takes two strings to specify age group and gender filter as inputs,
#    and outputs a summary table.
# Eg. filter_rows("F")

filter_rows <- function(Alder_filter, Kjonn_filter) {

    df_DE1 %>%
        filter(Alder == Alder_filter,
               Kjonn == Kjonn_filter) %>%
        select(code, value) %>%
        rename(Value = value)
}

# Table formatting function:
# Takes a summary table (as produced by the other functions),
#    and formats correctly for Eurostat export.
# Eg. table_formatting(DE1001V_2022)

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