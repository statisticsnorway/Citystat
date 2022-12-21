# #################################################
# Project: Grant_Aid
# Script purpose: Create tables DE2 (nationality)
# Author: Rebekka Arnesen
# #################################################

# load packages 

library(readr)      # Read and write csv, tsv etc.
library(haven)      # SPSS, Stata, and SAS files
library(dplyr)      # A grammar of data manipulation
library(readxl)     # Read excel files (.xls and .xlsx)
library(PxWebApiData)
library(klassR)
library(tidyverse)

# import data and filter by region

sas_path <- "/ssb/stamme03/bestat/innvandr/wk14/bosatt/g2022m01d01.sas7bdat"
df_DE2 <- read_sas(sas_path, NULL)

df_DE2 <- df_DE2 %>%
          filter(kommnr %in% c("0301", "1103", "3005", "4601", "5001"))


#Nationals: DE2001V_2022

DE2001V_2022 <- df_DE2 %>%
                    filter(statsborgerskap == "000") %>%
                    group_by(kommnr) %>%
                    summarise(statsborgerskap = n()) %>%
                    rename(Value = statsborgerskap) %>%
                    mutate(Reference_year = 2022,
                           Variable_code = "DE2001V",
                           Flags = "",
                           Footnote = "")

DE2001V_2022$navn <- ApplyKlass(DE2001V_2022$kommnr, klass = 131)

city_correspondence <- GetKlass(550) %>% select(code, name)

DE2001V_2022 <- merge.data.frame(DE2001V_2022,
                                 city_correspondence,
                                 by.x = "navn",
                                 by.y = "name")

DE2001V_2022 <- DE2001V_2022 %>%
                    select(code,
                           Variable_code,
                           Reference_year,
                           Value,
                           Flags,
                           Footnote) %>%
                    arrange(code) %>%
                    rename(City_code = code)

#EU foreigners: DE2002V

eu_foreign <- c("101", "103", "106", "112", "113", "115", "117", "119", "121",
                "123", "122", "124", "136", "129", "126", "127", "131", "132",
                "133", "157", "146", "137", "158", "144", "152", "153", "500")

DE2002V_2022 <- df_DE2 %>%
                    filter(statsborgerskap %in% eu_foreign) %>%
                    group_by(kommnr) %>%
                    summarise(statsborgerskap = n()) %>%
                    rename(Value = statsborgerskap) %>%
                    mutate(Reference_year = 2022,
                           Variable_code = "DE2002V",
                           Flags = "",
                           Footnote = "")

DE2002V_2022$navn <- ApplyKlass(DE2002V_2022$kommnr, klass = 131)

DE2002V_2022 <- merge.data.frame(DE2002V_2022,
                                 city_correspondence,
                                 by.x = 'navn',
                                 by.y = 'name') %>% 
                    select(code,
                           Variable_code,
                           Reference_year,
                           Value,
                           Flags,
                           Footnote) %>%
                    arrange(code) %>%
                    rename(City_code = code)


#Non-EU foreigners: DE2003V

DE2003V_2022 <- df_DE2 %>%
                    filter(!statsborgerskap %in% c("000", eu_foreign)) %>%
                    group_by(kommnr) %>%
                    summarise(statsborgerskap = n()) %>%
                    rename(Value = statsborgerskap) %>%
                    mutate(Reference_year = 2022,
                           Variable_code = "DE2003V",
                           Flags = "",
                           Footnote = "")

DE2003V_2022$navn <- ApplyKlass(DE2003V_2022$kommnr, klass = 131)

DE2003V_2022 <- merge.data.frame(DE2003V_2022,
                                 city_correspondence,
                                 by.x = "navn",
                                 by.y = "name") %>%
                    select(code,
                           Variable_code,
                           Reference_year,
                           Value, Flags,
                           Footnote) %>%
                    arrange(code) %>%
                    rename(City_code = code)


#Native-born: DE2008V

DE2008V_2022 <- df_DE2 %>%
                    filter(fodeland == "000") %>%
                    group_by(kommnr) %>%
                    summarise(fodeland = n()) %>%
                    rename(Value = fodeland) %>%
                    mutate(Reference_year = 2022,
                           Variable_code = "DE2008V",
                           Flags = "",
                           Footnote = "")

DE2008V_2022$navn <- ApplyKlass(DE2008V_2022$kommnr, klass = 131)

DE2008V_2022 <- merge.data.frame(DE2008V_2022,
                                 city_correspondence,
                                 by.x = "navn",
                                 by.y = "name") %>%
                    select(code,
                           Variable_code,
                           Reference_year,
                           Value,
                           Flags,
                           Footnote) %>%
                    arrange(code) %>%
                    rename(City_code = code)


#Foreign-born: DE2009V

DE2009V_2022 <- df_DE2 %>% 
                    filter(fodeland != "000") %>%
                    group_by(kommnr) %>%
                    summarise(fodeland=n()) %>%
                    rename(Value = fodeland) %>%
                    mutate(Reference_year = 2022,
                           Variable_code = "DE2009V",
                           Flags = "",
                           Footnote = "") 

DE2009V_2022$navn <- ApplyKlass(DE2009V_2022$kommnr, klass = 131)

DE2009V_2022 <- merge.data.frame(DE2009V_2022,
                                 city_correspondence,
                                 by.x = 'navn',
                                 by.y = 'name') %>%
                    select(code,
                           Variable_code,
                           Reference_year,
                           Value,
                           Flags,
                           Footnote) %>%
                    arrange(code) %>%
                    rename(City_code = code)


#Foreign-born in a EU country: DE2010V

DE2010V_2022 <- df_DE2 %>%
                    filter(fodeland %in% eu_foreign) %>%
                    group_by(kommnr) %>%
                    summarise(fodeland = n()) %>%
                    rename(Value = fodeland) %>%
                    mutate(Reference_year = 2022,
                           Variable_code = "DE2010V",
                           Flags = "",
                           Footnote = "")

DE2010V_2022$navn <- ApplyKlass(DE2010V_2022$kommnr, klass = 131)

DE2010V_2022 <- merge.data.frame(DE2010V_2022,
                                 city_correspondence,
                                 by.x = "navn",
                                 by.y = "name") %>% 
                    select(code,
                           Variable_code,
                           Reference_year,
                           Value,
                           Flags,
                           Footnote) %>%
                    arrange(code) %>%
                    rename(City_code = code)


#Foreign-born in a non-EU country: DE2011V

DE2011V_2022 <- df_DE2 %>%
                    filter(!fodeland %in% c("000", eu_foreign)) %>%
                    group_by(kommnr) %>%
                    summarise(fodeland = n()) %>%
                    rename(Value = fodeland) %>%
                    mutate(Reference_year = 2022,
                           Variable_code = "DE2011V",
                           Flags = "",
                           Footnote = "")

DE2011V_2022$navn <- ApplyKlass(DE2011V_2022$kommnr, klass = 131)

DE2011V_2022 <- merge.data.frame(DE2011V_2022,
                                 city_correspondence,
                                 by.x = "navn",
                                 by.y = "name") %>%
                    select(code,
                           Variable_code,
                           Reference_year,
                           Value,
                           Flags,
                           Footnote) %>%
                    arrange(code) %>%
                    rename(City_code = code)


#Foreigners: DE2012V

DE2012V_2022 <- df_DE2 %>%
                    filter(statsborgerskap != "000") %>%
                    group_by(kommnr)%>%
                    summarise(statsborgerskap=n())%>%
                    rename(Value = statsborgerskap) %>%
                    mutate(Reference_year = 2022,
                           Variable_code = "DE2012V",
                           Flags = "",
                           Footnote = "")

DE2012V_2022$navn <- ApplyKlass(DE2012V_2022$kommnr, klass = 131)

DE2012V_2022 <- merge.data.frame(DE2012V_2022,
                                 city_correspondence,
                                 by.x = "navn",
                                 by.y = "name") %>%
                    select(code,
                           Variable_code,
                           Reference_year,
                           Value,
                           Flags,
                           Footnote) %>%
                    arrange(code) %>% 
                    rename(City_code = code)
