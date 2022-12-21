# #################################################
# Project: Grant_Aid
# Script purpose: Create tables DE3 (household structure)
# Author: Linn Krokedal
# #################################################

library(PxWebApiData)
library(tidyverse)
library(readr)      # Read and write csv, tsv etc.
library(haven)      # SPSS, Stata, and SAS files
library(dplyr)      # A grammar of data manipulation
library(readxl)     # Read excel files (.xls and .xlsx)
library(klassR)

# DE3011V_2022

# import data
# only include family-types 002-005 (housholds with children below 18)

df_DE3011V <- ApiData(06083,
                      Region = c("0301", "1103", "3005", "4601", "5001"),
                      FamilieType = c("002", "003", "004", "005"),
                      Tid = "2022")[[2]]

DE3011V_2022 <- df_DE3011V %>%
                    group_by(Region) %>%
                    select(c("Region", "value")) %>%
                    summarise(Value = sum(value)) %>%
                    mutate(Reference_year = 2022,
                           Variable_code = "DE3011V",
                           Flags = "",
                           Footnote = "")

DE3011V_2022$navn <- ApplyKlass(DE3011V_2022$Region, klass = 131)

city_correspondence <- GetKlass(550) %>% select(code, name)

DE3011V_2022 <- merge.data.frame(DE3011V_2022,
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

# DE3005V_2022

# import data
# only include familiy types 004, 005 (loine parent private household)

df_DE3005V <- ApiData(06083,
                      Region = c("0301", "1103", "3005", "4601", "5001"),
                      FamilieType = c("004", "005"),
                      Tid = "2022")[[2]]


DE3005V_2022 <- df_DE3005V %>%
                    group_by(Region) %>%
                    select(c("Region", "value"))%>%
                    summarise(Value = sum(value))%>%
                    mutate(Reference_year = 2022,
                           Variable_code = "DE3005V",
                           Flags = "",
                           Footnote = "")

DE3005V_2022$navn <- ApplyKlass(DE3005V_2022$Region, klass = 131)

DE3005V_2022 <- merge.data.frame(DE3005V_2022,
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

# DE3017V_2022
#Population living in private households (excluding institutional households)

df_DE3017V <- ApiData(09747,
                    Region = c("0301", "1103", "3005", "4601", "5001"),
                    ContentsCode = "Personer",
                    Tid = "2022")[[2]]

DE3017V_2022 <- df_DE3017V %>%
                    group_by(Region) %>%
                    select(c("Region", "value")) %>%
                    summarise(Value = sum(value)) %>%
                    mutate(Reference_year = 2022,
                           Variable_code = "DE3017V",
                           Flags = "",
                           Footnote = "")

DE3017V_2022$navn <- ApplyKlass(DE3017V_2022$Region, klass = 131)

DE3017V_2022 <- merge.data.frame(DE3017V_2022,
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

# DE3001V_2022
#Population living in private households (excluding institutional households)

df_DE3001V <- ApiData(09747,
                    Region = c("0301", "1103", "3005", "4601", "5001"),
                    ContentsCode = "Husholdniger",
                    Tid = "2022")[[2]]

DE3001V_2022 <- df_DE3017V %>%
                    group_by(Region) %>%
                    select(c("Region", "value")) %>%
                    summarise(Value = sum(value)) %>%
                    mutate(Reference_year = 2022,
                           Variable_code = "DE3001V",
                           Flags = "",
                           Footnote = "")

DE3001V_2022$navn <- ApplyKlass(DE3001V_2022$Region, klass = 131)

DE3001V_2022 <- merge.data.frame(DE3001V_2022,
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

# DE3002V_2022
# Oneperson households

df_DE3002V <- ApiData(06083,
                  Region = c("0301", "1103", "3005", "4601", "5001"),
                  FamilieType = c("001"),
                  Tid = "2022")[[2]]

DE3002V_2022 <- df_DE3002V %>%
                    group_by(Region) %>%
                    select(c('Region','value'))%>%
                    mutate(Reference_year = 2022,
                           Variable_code = "DE3002V",
                           Flags = "",
                           Footnote = "")

DE3002V_2022$navn <- ApplyKlass(DE3002V_2022$Region,klass = 131)

DE3002V_2022 <- merge.data.frame(DE3002V_2022,
                                 city_correspondence,
                                 by.x = 'navn',
                                 by.y = 'name') %>%
                    select(code,
                           Variable_code,
                           Reference_year,
                           value,
                           Flags,
                           Footnote) %>%
                    arrange(code) %>%
                    rename(City_code = code) %>%
                    rename(Value = value)
