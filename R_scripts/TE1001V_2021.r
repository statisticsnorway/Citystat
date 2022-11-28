# -*- coding: utf-8 -*-
# #################################################
# # Project: Grant_Aid
# # Script purpose: Create table TE1001V (number og children that are 4 years old or younger in kindergarden)
# # Date: 17-11-2022
# # Author: Erling Johan Haakerud Kvalø
# #################################################


# Laster inn pakker
library(haven)
library(dplyr)
library(readxl)
suppressPackageStartupMessages(library(tidyverse))
library(klassR)

#laster inn datasett
bhg <- read_sas('/ssb/stamme03/bestat/overf/sas_data.sas7bdat')
bhg

# Velger variabler, bykommuner, summerer opp antall barn fra 4 år og yngre og summerer antall barn på kommunenivå
bhg<-bhg %>% 
    select('SMT_IDENT', 'INSTITUSJON_NAVN', 'KOMMUNE_NR', 'BARN0', 'BARN1a', 'BARN1b','BARN3', 'BARN2', 'BARN4')%>%
    filter(KOMMUNE_NR %in% c('0301', '1103', '4601', '5001', '3005', '1103')) %>%
    mutate(sum = BARN0 + BARN1a + BARN1b + BARN2 + BARN3 + BARN4)%>%
    group_by(KOMMUNE_NR) %>%
    summarise(value = sum(sum))


#legger til city-klass, for å få inn bykoder og by navn
bhg$name <- ApplyKlass(bhg$KOMMUNE_NR, klass = 131)

city_correspondence <- GetKlass(550) %>%
                           select(code, name)


#Legger inn variabler for flags og footnotes, fjerner stavnanger og endrer navn for å korespondere med ønsket formatering
TE1001V_2021 <- TE1001V_2021 %>%
        mutate(Reference_year = 2021,
            Variable_code = "TE1001V_2021",
            Flags = "",
            Footnote = "") %>%
        select(code,
            Variable_code,
            Reference_year,
            value,
            Flags,
            Footnote) %>%
            filter(code != 'NO005C')%>%
            rename(City_code = code,
            Value = value)%>%
            arrange(City_code)
TE1001V_2021
