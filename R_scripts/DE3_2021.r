# -*- coding: utf-8 -*-
library(PxWebApiData) 
library(tidyverse) 
library(readr)      # Read and write csv, tsv etc.
library(haven)      # SPSS, Stata, and SAS files
library(dplyr)      # A grammar of data manipulation
library(readxl)     # Read excel files (.xls and .xlsx)
library(klassR)



# Ser her på husholdninger med barn under 18. Familietyper med par og enslige med barn mellom 0 - 18 år inkluderes derfor. Dette inkluderer her kode:002 - 005.

fam_barn <- ApiData(06083,
                  Region = c('0301', '1103', '3005', '4601', '5001'),
                  FamilieType = c('002','003', '004','005'),
                  Tid = '2022')[[2]]

f_barn <- fam_barn %>% 
    group_by(Region) %>%
    select(c('Region','value'))%>%
    summarise(value=sum(value))%>%
    mutate(Reference_year = 2022, variable_code = 'DE3011V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote)

# Få riktig by-format fra Klass
f_barn$navn <- ApplyKlass(f_barn$Region,
                                     klass = 131)


city_correspondence <- GetKlass(550) %>% 
                    select(code, name)

DE3011V_2022 <- merge.data.frame(f_barn, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code)%>%
    rename(Value = value)
DE3011V_2022


# Ser her på aleneforeldre med barn under 18. Familietyper med enslige med barn mellom 0 - 18 år inkluderes derfor. Dette inkluderer her kode:004 - 005.

enslige_barn <- ApiData(06083,
                  Region = c('0301', '1103', '3005', '4601', '5001'),
                  FamilieType = c( '004','005'),
                  Tid = '2022')[[2]]

View(enslige_barn)

e_barn <- enslige_barn %>% 
    group_by(Region) %>%
    select(c('Region','value'))%>%
    summarise(value=sum(value))%>%
    mutate(Reference_year = 2022, variable_code = 'DE3005V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote)


# Få riktig by-format fra Klass
e_barn$navn <- ApplyKlass(e_barn$Region,
                                     klass = 131)


city_correspondence <- GetKlass(550) %>% 
                    select(code, name)

DE3005V_2022 <- merge.data.frame(e_barn, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code)  %>%
    rename(Variable_code = variable_code) %>%
    rename(Value = value)
DE3005V_2022

#Population living in private households (excluding institutional households)
pop_hush <- ApiData(09747,
                  Region = c('0301', '1103', '3005', '4601', '5001'),
                  ContentsCode = 'Personer',
                  Tid = '2022')[[2]]

View(pop_hush)

p_hush <- pop_hush %>% 
    group_by(Region) %>%
    select(c('Region','value'))%>%
    summarise(value=sum(value))%>%
    mutate(Reference_year = 2022, variable_code = 'DE3017V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote)


# Få riktig by-format fra Klass
p_hush$navn <- ApplyKlass(p_hush$Region,
                                     klass = 131)


city_correspondence <- GetKlass(550) %>% 
                    select(code, name)

DE3017V_2022 <- merge.data.frame(p_hush, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code) %>%
    rename(Value = value)
DE3017V_2022


#Population living in private households (excluding institutional households)
ant_hush <- ApiData(09747,
                  Region = c('0301', '1103', '3005', '4601', '5001'),
                  ContentsCode = 'Husholdniger',
                  Tid = '2022')[[2]]

View(ant_hush)

a_hush <- ant_hush %>% 
    group_by(Region) %>%
    select(c('Region','value'))%>%
    mutate(Reference_year = 2022, variable_code = 'DE3001V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote)


# Få riktig by-frmat fra Klass
a_hush$navn <- ApplyKlass(a_hush$Region,
                                     klass = 131)


city_correspondence <- GetKlass(550) %>% 
                    select(code, name)

DE3001V_2022 <- merge.data.frame(a_hush, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code) %>%
    rename(Value = value)
DE3001V_2022


# Oneperson households 
# Sorterer her ut enkeltperosnfamilier som er familiekode 001

enkeltpersons_familier <- ApiData(06083,
                  Region = c('0301', '1103', '3005', '4601', '5001'),
                  FamilieType = c( '001'),
                  Tid = '2022')[[2]]

enkeltpersons_familier

e_familier <- enkeltpersons_familier %>% 
    group_by(Region) %>%
    select(c('Region','value'))%>%
    mutate(Reference_year = 2022, variable_code = 'DE3002V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote)


# Få riktig by-frmat fra Klass
e_familier$navn <- ApplyKlass(e_familier$Region,
                                     klass = 131)


city_correspondence <- GetKlass(550) %>% 
                    select(code, name)

DE3002V_2022 <- merge.data.frame(e_familier, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code) %>%
    rename(Value = value)

DE3002V_2022
