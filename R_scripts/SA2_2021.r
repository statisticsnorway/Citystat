# -*- coding: utf-8 -*-

library(PxWebApiData) 
library(tidyverse) 
library(readr)      # Read and write csv, tsv etc.
library(haven)      # SPSS, Stata, and SAS files
library(dplyr)      # A grammar of data manipulation
library(readxl)     # Read excel files (.xls and .xlsx)
library(klassR)

#DØDFØDTE
getwd()
setwd("/ssb/stamme03/bestat/fodte/wk14/statfil/")
library(haven)
fodte_2021 <- read_sas("./g2021.sas7bdat")

dodfodte <- fodte_2021 %>%
      filter(KOMMNR %in% c('0301', '1103', '4601', '5001' )) %>%
      filter(STATUSKODE == '0')

dodfodt_tab <- dodfodte %>%
    group_by(KOMMNR) %>%
    select('KOMMNR', 'STATUSKODE') %>%
    mutate(y = 1)

x <- dodfodt_tab %>% 
    group_by(KOMMNR) %>%
    summarise(value = sum(y)) %>%
    rename(Region = KOMMNR)

x$komm_navn <- ApplyKlass(x$Region,
                                     klass = 131)

city_correspondence <- GetKlass(550) %>% 
                    select(code, name)

x_ny <- merge.data.frame(x,city_correspondence, by.x="komm_navn",by.y="name")

SA2004V_2021 <- x_ny %>%
    select(code, value) %>%
    mutate(Reference_year = 2021, variable_code = 'SA2004V', Flags = '', Footnote = '') %>%
    relocate(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code)%>%
    rename (Value = value)
SA2004V_2021


# UNGE MØDRE 

getwd()
setwd("/ssb/stamme03/bestat/fodte/wk14/statfil/")
fodte_2021 <- read_sas("./g2021.sas7bdat")

um_21 <- fodte_2021 %>%
      filter(KOMMNR %in% c('0301', '1103', '4601', '5001')) %>%
      filter(mor_aldh <= '019')

y <- um_21 %>%
    group_by(KOMMNR) %>%
    select('KOMMNR', 'STATUSKODE') %>%
    mutate(y = 1)

x <- y %>% 
    group_by(KOMMNR) %>%
    summarise(value = sum(y)) %>%
    rename(Region = KOMMNR)

# Antall unge mødre (19 år og yngre) 
ungmor <- x %>%
    mutate(Reference_year = 2021, variable_code = 'SA2010V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote) 

ungmor

# Få riktig by-format fra Klass
ungmor$navn <- ApplyKlass(ungmor$Region,
                                     klass = 131)


city_correspondence <- GetKlass(550) %>% 
                    select(code, name)

SA2010V_2021 <- merge.data.frame(ungmor, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code)%>%
    rename (Value = value)
SA2010V_2021

# LEVENDEFØDTE - FRA STATBANK

fodte <- ApiData(04231,
                  Region = c('0301', '1103', '4601', '5001'),
                  Tid = '2021')[[2]]

View(fodte)

# Alle fødsler - riktig

a_fodte <- fodte %>% 
    group_by(Region) %>%
    select(c('Region','value'))%>%
    summarise(value=sum(value))%>%
    mutate(Reference_year = 2021, variable_code = 'SA2007V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote)

a_fodte$navn <- ApplyKlass(a_fodte$Region,
                                     klass = 131)

city_correspondence <- GetKlass(550) %>% 
                    select(code, name)

SA2007V_2021 <- merge.data.frame(a_fodte, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code)%>%
    rename (Value = value)
SA2007V_2021


library(PxWebApiData) 
library(tidyverse) 
library(readr)      # Read and write csv, tsv etc.
library(haven)      # SPSS, Stata, and SAS files
library(dplyr)      # A grammar of data manipulation
library(readxl)     # Read excel files (.xls and .xlsx)
library(klassR)


#Alle dode 
alle_dode <- ApiData(08425, 
             Region = c('0301', '1103', '4601', '5001'), Tid = '2021', Kjonn ='0') [[2]]
View(alle_dode)

a_dode <- alle_dode %>% 
    group_by(Region) %>%
    select(c('Region','value'))%>%
    mutate(Reference_year = 2021, variable_code = 'SA2019V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote) 

View(a_dode)

# Få riktig by-format fra Klass
a_dode$navn <- ApplyKlass(a_dode$Region,
                                     klass = 131)


city_correspondence <- GetKlass(550) %>% 
                    select(code, name)

SA2019V_2021 <- merge.data.frame(a_dode, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code)%>%
    rename (Value = value)
SA2019V_2021


#Menn dode 
menn_dode <- ApiData(08425, 
             Region = c('0301', '1103', '4601', '5001'), Tid = '2021', Kjonn ='1') [[2]]
View(menn_dode)

m_dode <- menn_dode %>% 
    group_by(Region) %>%
    select(c('Region','value'))%>%
    mutate(Reference_year = 2021, variable_code = 'SA2020V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote) 

# Få riktig by-frmat fra Klass
m_dode$navn <- ApplyKlass(m_dode$Region,
                                     klass = 131)

SA2020V_2021 <- merge.data.frame(m_dode, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code)%>%
    rename (Value = value)
SA2020V_2021


#Kvinner dode 
kvinner_dode <- ApiData(08425, 
             Region = c('0301', '1103', '4601', '5001'), Tid = '2021', Kjonn ='2') [[2]]
View(kvinner_dode)

k_dode <- kvinner_dode %>% 
    group_by(Region) %>%
    select(c('Region','value'))%>%
    mutate(Reference_year = 2021, variable_code = 'SA2021V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote) 

View(k_dode)


# Få riktig by-format fra Klass
k_dode$navn <- ApplyKlass(k_dode$Region,
                                     klass = 131)

SA2021V_2021 <- merge.data.frame(k_dode, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code)%>%
    rename (Value = value)
SA2021V_2021



# Alle dode  under 65 - går tydligvis ikke ann med interne tabeller og det finnes ikke i de offentlge.# 

dode <- ApiData("https://i.ssb.no/pxwebi/api/v0/no/prod_24v_intern/START/be/be06/dode/Rd1055AaX1",
        Region = c('0301', '5001', '1103', '4601'), 
        AlderVedDod = TRUE,
        Tid = '2021')[[2]] 

#dode under 65 
a_u65 <- dode %>%
    filter(AlderVedDod < '065')

#Alle døde under 65
u65 <- dode %>%
    filter(AlderVedDod < '065') %>%
    group_by(Region) %>%
    summarise(value = sum(value))%>%
    select(c('Region','value'))%>%
    mutate(Reference_year = 2021, variable_code = 'SA2016V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote) 

u65

# Få riktig by-format fra Klass
u65$navn <- ApplyKlass(u65$Region,
                                     klass = 131)

SA2016V_2021 <- merge.data.frame(u65, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code)%>%
    rename (Value = value)
SA2016V_2021

#Døde menn under 65 
m_u65 <- dode %>%
    filter(Kjonn== '1') %>%
    group_by(Region) %>%
    summarise(value = sum(value))%>%
    select(c('Region','value'))%>%
    mutate(Reference_year = 2021, variable_code = 'SA2017V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote) 

m_u65

# Få riktig by-format fra Klass
m_u65$navn <- ApplyKlass(m_u65$Region,
                                     klass = 131)

SA2017V_2021 <- merge.data.frame(m_u65, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code)%>%
    rename (Value = value)
SA2017V_2021

#Døde kvinner under 65
k_u65 <- dode %>%
    filter(Kjonn== '2') %>%
    group_by(Region) %>%
    summarise(value = sum(value))%>%
    select(c('Region','value'))%>%
    mutate(Reference_year = 2021, variable_code = 'SA2018V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote) 

k_u65

# Få riktig by-frmat fra Klass
k_u65$navn <- ApplyKlass(k_u65$Region,
                                     klass = 131)

SA2018V_2021 <- merge.data.frame(k_u65, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code)%>%
    rename (Value = value)
SA2018V_2021
