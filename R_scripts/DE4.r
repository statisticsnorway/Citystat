# #################################################
# Project: Grant_Aid
# Script purpose: Create tables DE4
# Author: Rebekka Arnesen
# #################################################

library(readr)      # Read and write csv, tsv etc.
library(haven)      # SPSS, Stata, and SAS files
library(dplyr)      # A grammar of data manipulation
library(readxl)     # Read excel files (.xls and .xlsx)
library(PxWebApiData)
library(klassR)
library(tidyverse)

# import data

df_DE4 <- read_sas("/ssb/stamme01/bediv/rar/wk03/bosatt.sas7bdat", NULL)
df_DE4 <- df_DE4 %>%
              filter(kommnr %in% c("0301", "1103", "3005", "4601", "5001"))

# load city correspondence

city_correspondence <- GetKlass(550) %>% select(code, name)

# DE4001V
# Population with the place of residence in the same dwelling, in the year before :

DE4001V <- df_DE4 %>%
                filter(ikkeflyttet == "1") %>%
                group_by(kommnr) %>%
                summarise(Value = n()) %>%
                mutate(Reference_year = 2022,
                       Variable_code = "DE4001V",
                       Flags = "",
                       Footnote = "")

DE4001V$navn <- ApplyKlass(DE4001V$kommnr, klass = 131)

DE4001V_2022 <- merge.data.frame(DE4001V,
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

# DE4002V
# Population with the place of residence in another dwelling in the same city/FUA, in the year before:

DE4002V <- df_DE4 %>%
                filter(samme_kommnr == "1") %>%
                group_by(til_kommnr) %>%
                summarise(Value = n()) %>%
                mutate(Reference_year = 2022,
                       Variable_code = "DE4002V",
                       Flags = "",
                       Footnote = "")

DE4002V$navn <- ApplyKlass(DE4002V$til_kommnr, klass = 131)

DE4002V_2022 <- merge.data.frame(DE4002V,
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

# DE4003V
# Population with the place of residence in another dwelling outside the current city/FUA in the same country, in the year before:

DE4003V <- df_DE4 %>%
                filter(annenby == "1") %>%
                group_by(til_kommnr) %>%
                summarise(Value = n()) %>%
                mutate(Reference_year = 2022,
                       Variable_code = "DE4003V",
                       Flags = "",
                       Footnote = "")

DE4003V$navn <- ApplyKlass(DE4003V$til_kommnr, klass = 131)

DE4003V_2022 <- merge.data.frame(DE4003V,
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

# DE4004V
# opulation with the place of residence in another EU country, in the year before:

DE4004V <- df_DE4 %>%
                filter(fraeu == "1") %>%
                group_by(til_kommnr) %>%
                summarise(Value = n()) %>%
                mutate(Reference_year = 2022,
                       Variable_code = "DE4004V",
                       Flags = "",
                       Footnote = "")

DE4004V$navn <-ApplyKlass(DE4004V$til_kommnr, klass = 131)

DE4004V_2022 <- merge.data.frame(DE4004V,
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

# DE4005V
# Population with the place of residence outside the EU, in the year before:

DE4004V <- df_DE4 %>%
                filter(fraeu == "1") %>%
                group_by(til_kommnr) %>%
                summarise(Value = n()) %>%
                mutate(Reference_year = 2022,
                       Variable_code = "DE4004V",
                       Flags = "",
                       Footnote = "")

DE4004V$navn <- ApplyKlass(DE4004V$til_kommnr, klass = 131)

DE4005V_2022 <- merge.data.frame(DE4004V,
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


