library(readr)      # Read and write csv, tsv etc. 
library(haven)      # SPSS, Stata, and SAS files 
library(dplyr)      # A grammar of data manipulation 
library(readxl)     # Read excel files (.xls and .xlsx) 
library(PxWebApiData) 
library(klassR) 
library(tidyverse) 

population <- read_sas("/ssb/stamme03/bestat/innvandr/wk14/bosatt/g2022m01d01.sas7bdat", NULL)



#Nationals: DE2001V_2022 
norsk <- population %>% 
    filter(statsborgerskap == "000") %>%
    filter(kommnr%in% c("0301", "1103", "3005", "4601", "5001")) %>%
    group_by(kommnr)%>%
    summarise(statsborgerskap=n()) %>%
    rename(Value = statsborgerskap) %>%
    mutate(Reference_year = 2022, Variable_code = 'DE2001V', Flags = '', Footnote = '') 

norsk$navn <-ApplyKlass(norsk$kommnr, klass = 131) 

city_correspondence <- GetKlass(550) %>% select(code, name) 

DE2001V_2022 <- merge.data.frame(norsk, city_correspondence, by.x = 'navn', by.y = 'name') %>% 
    select(code, Variable_code, Reference_year, Value, Flags, Footnote) %>% 
    arrange(code) %>% 
    rename(City_code = code) 

DE2001V_2022 



#EU foreigners: DE2002V
euforeign <- population %>% 
filter(statsborgerskap %in% c("101", "103", "106", "112", "113", "115", "117", "119", "121",
                              "123", "122", "124", "136", "129", "126", "127", "131", "132", 
                              "133", "157", "146", "137", "158", "144", "152", "153", "500")) %>%
    filter(kommnr%in% c("0301", "1103", "3005", "4601", "5001")) %>%
    group_by(kommnr)%>%
    summarise(statsborgerskap=n())%>%
    rename(Value = statsborgerskap) %>%
    mutate(Reference_year = 2022, Variable_code = 'DE2002V', Flags = '', Footnote = '') 

euforeign$navn <-ApplyKlass(euforeign$kommnr, klass = 131) 

city_correspondence <- GetKlass(550) %>% select(code, name) 

DE2002V_2022 <- merge.data.frame(euforeign, city_correspondence, by.x = 'navn', by.y = 'name') %>% 
    select(code, Variable_code, Reference_year, Value, Flags, Footnote) %>% 
    arrange(code) %>% 
    rename(City_code = code) 

DE2002V_2022



#Non-EU foreigners: DE2003V
noneuforeign <- population %>% 
filter(!statsborgerskap %in% c("000", "101", "103", "106", "112", "113", "115", "117", "119", "121",
                               "123", "122", "124", "136", "129", "126", "127", "131", "132", 
                               "133", "157", "146", "137", "158", "144", "152", "153", "500")) %>%
    filter(kommnr%in% c("0301", "1103", "3005", "4601", "5001")) %>%
    group_by(kommnr)%>%
    summarise(statsborgerskap=n())%>%
    rename(Value = statsborgerskap) %>%
    mutate(Reference_year = 2022, Variable_code = 'DE2003V', Flags = '', Footnote = '') 

noneuforeign$navn <-ApplyKlass(noneuforeign$kommnr, klass = 131) 

city_correspondence <- GetKlass(550) %>% select(code, name) 

DE2003V_2022 <- merge.data.frame(noneuforeign, city_correspondence, by.x = 'navn', by.y = 'name') %>% 
    select(code, Variable_code, Reference_year, Value, Flags, Footnote) %>% 
    arrange(code) %>% 
    rename(City_code = code) 

DE2003V_2022



#Native-born: DE2008V
nativeborn <- population %>% 
    filter(fodeland == "000") %>%
    filter(kommnr%in% c("0301", "1103", "3005", "4601", "5001")) %>%
    group_by(kommnr)%>%
    summarise(fodeland=n())%>%
    rename(Value = fodeland) %>%
    mutate(Reference_year = 2022, Variable_code = 'DE2008V', Flags = '', Footnote = '') 

nativeborn$navn <-ApplyKlass(nativeborn$kommnr, klass = 131) 

city_correspondence <- GetKlass(550) %>% select(code, name) 

DE2008V_2022 <- merge.data.frame(nativeborn, city_correspondence, by.x = 'navn', by.y = 'name') %>% 
    select(code, Variable_code, Reference_year, Value, Flags, Footnote) %>% 
    arrange(code) %>% 
    rename(City_code = code) 

DE2008V_2022



#Foreign-born: DE2009V
foreignborn <- population %>% 
    filter(fodeland != "000") %>%
    filter(kommnr%in% c("0301", "1103", "3005", "4601", "5001")) %>%
    group_by(kommnr)%>%
    summarise(fodeland=n())%>%
    rename(Value = fodeland) %>%
    mutate(Reference_year = 2022, Variable_code = 'DE2009V', Flags = '', Footnote = '') 

foreignborn$navn <-ApplyKlass(foreignborn$kommnr, klass = 131) 

city_correspondence <- GetKlass(550) %>% select(code, name) 

DE2009V_2022 <- merge.data.frame(foreignborn, city_correspondence, by.x = 'navn', by.y = 'name') %>% 
    select(code, Variable_code, Reference_year, Value, Flags, Footnote) %>% 
    arrange(code) %>% 
    rename(City_code = code) 

DE2009V_2022



#Foreign-born in a EU country: DE2010V
euforeignborn <- population %>% 
    filter(fodeland %in% c("101", "103", "106", "112", "113", "115", "117", "119", "121", 
                           "123", "122", "124", "136", "129", "126", "127", "131", "132", 
                           "133", "157", "146", "137", "158", "144", "152", "153", "500")) %>%
    filter(kommnr%in% c("0301", "1103", "3005", "4601", "5001")) %>%
    group_by(kommnr)%>%
    summarise(fodeland=n())%>%
    rename(Value = fodeland) %>%
    mutate(Reference_year = 2022, Variable_code = 'DE2010V', Flags = '', Footnote = '') 

euforeignborn$navn <-ApplyKlass(euforeignborn$kommnr, klass = 131) 

city_correspondence <- GetKlass(550) %>% select(code, name) 

DE2010V_2022 <- merge.data.frame(euforeignborn, city_correspondence, by.x = 'navn', by.y = 'name') %>% 
    select(code, Variable_code, Reference_year, Value, Flags, Footnote) %>% 
    arrange(code) %>% 
    rename(City_code = code) 

DE2010V_2022



#Foreign-born in a non-EU country: DE2011V
noneuforeignborn <- population %>% 
    filter(!fodeland %in% c("000", "101", "103", "106", "112", "113", "115", "117", "119", "121",
                            "123", "122", "124", "136", "129", "126", "127", "131", "132", 
                             "133", "157", "146", "137", "158", "144", "152", "153", "500")) %>%
    filter(kommnr%in% c("0301", "1103", "3005", "4601", "5001")) %>%
    group_by(kommnr)%>%
    summarise(fodeland=n())%>%
    rename(Value = fodeland) %>%
    mutate(Reference_year = 2022, Variable_code = 'DE2011V', Flags = '', Footnote = '') 

noneuforeignborn$navn <-ApplyKlass(noneuforeignborn$kommnr, klass = 131) 

city_correspondence <- GetKlass(550) %>% select(code, name) 

DE2011V_2022 <- merge.data.frame(noneuforeignborn, city_correspondence, by.x = 'navn', by.y = 'name') %>% 
    select(code, Variable_code, Reference_year, Value, Flags, Footnote) %>% 
    arrange(code) %>% 
    rename(City_code = code) 

DE2011V_2022



#Foreigners: DE2012V
foreign <- population %>% 
    filter(statsborgerskap != "000") %>%
    filter(kommnr%in% c("0301", "1103", "3005", "4601", "5001")) %>%
    group_by(kommnr)%>%
    summarise(statsborgerskap=n())%>%
    rename(Value = statsborgerskap) %>%
    mutate(Reference_year = 2022, Variable_code = 'DE2012V', Flags = '', Footnote = '') 

foreign$navn <-ApplyKlass(foreign$kommnr, klass = 131) 

city_correspondence <- GetKlass(550) %>% select(code, name) 

DE2012V_2022 <- merge.data.frame(foreign, city_correspondence, by.x = 'navn', by.y = 'name') %>% 
    select(code, Variable_code, Reference_year, Value, Flags, Footnote) %>% 
    arrange(code) %>% 
    rename(City_code = code) 

DE2012V_2022
