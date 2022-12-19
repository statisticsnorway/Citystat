library(readr)      # Read and write csv, tsv etc. 
library(haven)      # SPSS, Stata, and SAS files 
library(dplyr)      # A grammar of data manipulation 
library(readxl)     # Read excel files (.xls and .xlsx) 
library(PxWebApiData) 
library(klassR) 
library(tidyverse) 

bosatt <- read_sas("/ssb/stamme01/bediv/rar/wk03/bosatt.sas7bdat", NULL)

#Population with the place of residence in the same dwelling, in the year before: DE4001V
ikkeflyttet <- bosatt %>% 
    filter(ikkeflyttet == "1") %>%
    filter(kommnr%in% c("0301", "1103", "3005", "4601", "5001")) %>%
    group_by(kommnr)%>%
    summarise(ikkeflyttet=n())%>%
    rename(Value=ikkeflyttet) %>%
    mutate(Reference_year = 2022, Variable_code = 'DE4001V', Flags = '', Footnote = '') 

ikkeflyttet$navn <-ApplyKlass(ikkeflyttet$kommnr, klass = 131) 

city_correspondence <- GetKlass(550) %>% select(code, name) 

DE4001V_2022 <- merge.data.frame(ikkeflyttet, city_correspondence, by.x = 'navn', by.y = 'name') %>% 
    select(code, Variable_code, Reference_year, Value, Flags, Footnote) %>% 
    arrange(code) %>% 
    rename(City_code = code) 

DE4001V_2022


#Population with the place of residence in another dwelling in the same city/FUA, in the year before: DE4002V
sammeby <- bosatt %>% 
    filter(samme_kommnr == "1") %>%
    filter(til_kommnr%in% c("0301", "1103", "3005", "4601", "5001")) %>%
    group_by(til_kommnr)%>%
    summarise(samme_kommnr=n())%>%
    rename(Value=samme_kommnr) %>%
    mutate(Reference_year = 2022, Variable_code = 'DE4002V', Flags = '', Footnote = '') 

sammeby$navn <-ApplyKlass(sammeby$til_kommnr, klass = 131) 

city_correspondence <- GetKlass(550) %>% select(code, name) 

DE4002V_2022 <- merge.data.frame(sammeby, city_correspondence, by.x = 'navn', by.y = 'name') %>% 
    select(code, Variable_code, Reference_year, Value, Flags, Footnote) %>% 
    arrange(code) %>% 
    rename(City_code = code) 

DE4002V_2022

#Population with the place of residence in another dwelling outside the current city/FUA in the same country, in the year before: DE4003V
annenby <- bosatt %>% 
    filter(annenby == "1") %>%
    filter(til_kommnr%in% c("0301", "1103", "3005", "4601", "5001")) %>%
    group_by(til_kommnr)%>%
    summarise(annenby=n())%>%
    rename(Value=annenby) %>%
    mutate(Reference_year = 2022, Variable_code = 'DE4003V', Flags = '', Footnote = '') 

annenby$navn <-ApplyKlass(annenby$til_kommnr, klass = 131) 

city_correspondence <- GetKlass(550) %>% select(code, name) 

DE4003V_2022 <- merge.data.frame(annenby, city_correspondence, by.x = 'navn', by.y = 'name') %>% 
    select(code, Variable_code, Reference_year, Value, Flags, Footnote) %>% 
    arrange(code) %>% 
    rename(City_code = code) 

DE4003V_2022

#Population with the place of residence in another EU country, in the year before: DE4004V
fraeu <- bosatt %>% 
    filter(fraeu == "1") %>%
    filter(til_kommnr%in% c("0301", "1103", "3005", "4601", "5001")) %>%
    group_by(til_kommnr)%>%
    summarise(fraeu=n())%>%
    rename(Value=fraeu) %>%
    mutate(Reference_year = 2022, Variable_code = 'DE4004V', Flags = '', Footnote = '') 

fraeu$navn <-ApplyKlass(fraeu$til_kommnr, klass = 131) 

city_correspondence <- GetKlass(550) %>% select(code, name) 

DE4004V_2022 <- merge.data.frame(fraeu, city_correspondence, by.x = 'navn', by.y = 'name') %>% 
    select(code, Variable_code, Reference_year, Value, Flags, Footnote) %>% 
    arrange(code) %>% 
    rename(City_code = code) 

DE4004V_2022

#Population with the place of residence outside the EU, in the year before: DE4005V
ikkefraeu <- bosatt %>% 
    filter(ikkefraeu == "1") %>%
    filter(til_kommnr%in% c("0301", "1103", "3005", "4601", "5001")) %>%
    group_by(til_kommnr)%>%
    summarise(ikkefraeu=n())%>%
    rename(Value=ikkefraeu) %>%
    mutate(Reference_year = 2022, Variable_code = 'DE4005V', Flags = '', Footnote = '') 

ikkefraeu$navn <-ApplyKlass(ikkefraeu$til_kommnr, klass = 131) 

city_correspondence <- GetKlass(550) %>% select(code, name) 

DE4005V_2022 <- merge.data.frame(ikkefraeu, city_correspondence, by.x = 'navn', by.y = 'name') %>% 
    select(code, Variable_code, Reference_year, Value, Flags, Footnote) %>% 
    arrange(code) %>% 
    rename(City_code = code) 

DE4005V_2022

