# -*- coding: utf-8 -*-
library(PxWebApiData) 
library(tidyverse) 
library(readr)      # Read and write csv, tsv etc.
library(haven)      # SPSS, Stata, and SAS files
library(dplyr)      # A grammar of data manipulation
library(readxl)     # Read excel files (.xls and .xlsx)
library(klassR)


# Enigboliger (SA1052V_2021) 
# Laster inn alle KvPriser for eldre boliger og nye boliger (eneboliger og rekkehus) og tar videre vektede gjennomsnitt.

a <- ApiData(06035,
        Boligtype = c('01'),
        ContentsCode = c('KvPris'),
        Region = c('0301', '1103', '4601', '5001'),
        Tid = '2021')[[2]]

b <- ApiData(06035,
        Boligtype = c('01'),
        ContentsCode = c('Omsetninger'),
        Region = c('0301', '1103', '4601', '5001'),
        Tid = '2021')[[2]]

c <- ApiData(06035,
        Boligtype = c('02'),
        ContentsCode =c('KvPris'),
        Region = c('0301', '1103', '4601', '5001'),
        Tid = '2021')[[2]]

d <- ApiData(06035,
        Boligtype = c('02'),
        ContentsCode =c('Omsetninger'),
        Region = c('0301', '1103', '4601', '5001'),
        Tid = '2021')[[2]]


e <- ApiData(13500,
        ContentsCode ='KvPris',
        Region = c('0301', '1103', '4601', '5001'),
        Tid = '2021')[[2]]

f <- ApiData(13500,
        ContentsCode ='Omsetninger',
        Region = c('0301', '1103', '4601', '5001'),
        Tid = '2021')[[2]]

ab <- merge.data.frame(a, b, by.x = 'Region', by.y = 'Region') %>%
    mutate(value = value.x *value.y)
ab
cd <- merge.data.frame(c, d, by.x = 'Region', by.y = 'Region') %>%
    mutate(value = value.x *value.y)
cd
ef <- merge.data.frame(e, f, by.x = 'Region', by.y = 'Region') %>%
    mutate(value = value.x *value.y)
ef

enebolig_greier <- merge.data.frame(ab, cd, by.x = 'Region', by.y = 'Region') 
enebolig_greier


enebolig_greier <- merge.data.frame(ab, cd, by.x = 'Region', by.y = 'Region') %>%
    mutate(KvPris_tot = value.x + value.y) %>%
    mutate(Omsetninger = value.y.x + value.y.y) %>%
    mutate(value = KvPris_tot/Omsetninger) %>%
    select(c('Omsetninger','KvPris_tot','Region'))
enebolig_greier

ef %>%
    rename(Omsetninger_ny = value.y) %>%
    rename(KvPris_tot_ny = value) %>%
    select(c('Omsetninger_ny','KvPris_tot_ny','Region'))
ef

enebolig_greier_videre <- merge.data.frame(enebolig_greier,ef , by.x = 'Region', by.y = 'Region') 
enebolig_greier_videre



enebolig_greier_videre <- merge.data.frame(enebolig_greier,ef , by.x = 'Region', by.y = 'Region') %>%
    mutate(KvPris_tot = KvPris_tot + value) %>%
    mutate(Omsetninger = Omsetninger + value.y) %>%
    mutate(value = KvPris_tot/Omsetninger)
enebolig_greier_videre

# Som delav kvadratmeterpris for brukte eneboliger. Legg sammen med nye. 
# Tar her gjennomsnittet av Eneboliger og småhus
# Må ta vektet gjennomsnitt - da det er flere eneboliger enn småhus

enebolig <-enebolig_greier_videre %>%
    group_by(Region) %>%
    select(c('Region','value'))%>%
    mutate(value = value*0.0984) %>% #Ganger med den gjennomsnittlige valutakursen for nok til eur i 2021 (0.0984) 
    mutate_if(is.numeric,round, digits = 0) %>%
    mutate(Reference_year = 2021, variable_code = 'SA1052V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote) 
enebolig

# Få riktig by-format fra Klass
enebolig$navn <- ApplyKlass(enebolig$Region,
                                     klass = 131)


city_correspondence <- GetKlass(550) %>% 
                    select(code, name)

SA1052V_2021 <- merge.data.frame(enebolig, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code)%>%
    rename (Value = value)
SA1052V_2021

# Leiligheter - denne er grei - litt forskjellig definisjon på hva leilighet inkluderer i statistikkbanken og eurostat 
# (skriver om dette i dokumentasjonen)

b <- ApiData(06035,
        Boligtype = '03',
        ContentsCode ='KvPris',
        Region = c('0301', '1103', '4601', '5001'),
        Tid = '2021')[[2]]

b

# Legg merke til at definisjonen er litt forskjellig fra Eurostat. Det finnes ikke data på nye leiligheter, slik som det gjør med eneboliger.
# Må også gjøre om til euro på et eller annet tidspunkt. Finn ut hvilken rate du sksl bruke. 

leilighet <- b %>%
    group_by(Region) %>%
    summarise(value=sum(value)) %>%
    mutate(value = value*0.0984) %>% #Ganger med den gjennomsnittlige valutakursen for nok til eur i 2021 (0.0984) . 
    mutate_if(is.numeric,round, digits = 0) %>%
    select(c('Region','value'))%>%
    mutate(Reference_year = 2021, variable_code = 'SA1053V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote) 
leilighet


# Få riktig by-format fra Klass
leilighet$navn <- ApplyKlass(leilighet$Region,
                                     klass = 131)


city_correspondence <- GetKlass(550) %>% 
                    select(code, name)

SA1053V_2021 <- merge.data.frame(leilighet, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code)%>%
    rename (Value = value)
SA1053V_2021


# Dwellings 

bolig_1 <- ApiData("https://i.ssb.no/pxwebi/api/v0/no/prod_24v_intern/START/bb/bb01/boligstat/BoligerA")[[1]] 

bolig_1


bolig1 <- bolig_1 %>%
    select(region, statistikkvariabel)


bolig <- ApiData("https://i.ssb.no/pxwebi/api/v0/no/prod_24v_intern/START/bb/bb01/boligstat/BoligerA",
        Region = c('0301', '5001', '1103', '4601','3005'), 
        BygnType = c('01','02','03','04','05'),
        Tid = '2022')[[2]] 

bolig


a_bolig <- bolig %>% 
    group_by(Region) %>%
    summarise(value = sum(value))%>%
    select(c('Region','value'))%>%
    mutate(Reference_year = 2022, variable_code = 'SA1001V', Flags = '', Footnote = '') %>%
    relocate(Region, variable_code, Reference_year, value, Flags, Footnote) 
View(a_bolig)


# Få riktig by-foSrmat fra Klass
a_bolig$navn <- ApplyKlass(a_bolig$Region,
                                     klass = 131)


city_correspondence <- GetKlass(550) %>% 
                    select(code, name)

SA1001V_2022 <- merge.data.frame(a_bolig, city_correspondence, by.x = 'navn', by.y = 'name') %>%
    select(code, variable_code, Reference_year, value, Flags, Footnote) %>%
    arrange(code) %>%
    rename(City_code = code) %>%
    rename(Variable_code = variable_code)%>%
    rename (Value = value)
SA1001V_2022
