# #################################################
# Project: Grant_Aid
# Script purpose: Create tables EC1
# Author: Hans Martin Corneliussen
# #################################################

# last inn pakker

library(PxWebApiData)
library(tidyverse)
library(readxl)
library(klassR)

# last inn funksjoner

notebook_path <- getwd()
setwd('..')
base_path <- getwd()
my_functions <- list.files(paste(base_path, "functions", sep = "/"),
                      pattern = "*.r")
for (i in my_functions) {
    x <- paste(paste(base_path, "functions", sep = "/"), i, sep = "/")
    suppressMessages(source(x))
}
setwd(notebook_path)

# last inn korrespondanse

city_correspondence <- GetKlass(550) %>%
                    select(code, name)

#EC1177V - EC1179V
#Persons employed, 20-64:

# import data and data setup

df_EC1177V <- read_excel("/ssb/stamme01/bediv/project/CITYSTATISTICS/06040_20221110-135541.xlsx")

colnames(df_EC1177V) <- c("kommnr",
                          "kommnavn",
                          "alder1",
                          "alder2",
                          "male",
                          "female")                # set column names

df_EC1177V <- df_EC1177V[c(5:21544), c("kommnr",
                                       "alder1",
                                       "male",
                                       "female")]  # select rows and columns

# fill kommnr column, substring and filter

df_EC1177V <- df_EC1177V %>% fill(kommnr)
df_EC1177V$kommnr <- substring(df_EC1177V$kommnr, 3)
df_EC1177V <- df_EC1177V %>%
                    filter(kommnr %in% c("0301", "5001", "1103", "4601"))

# change dtypes and filter to ages 20-64

df_EC1177V$male <- as.numeric(df_EC1177V$male)
df_EC1177V$female <- as.numeric(df_EC1177V$female)
df_EC1177V$alder1 <- as.numeric(df_EC1177V$alder1)

df_EC1177V <- df_EC1177V[df_EC1177V$alder1 >= 20 & df_EC1177V$alder1 <= 64, ]

# create table EC1177V

EC1177V <- df_EC1177V %>%
              group_by(kommnr) %>%
              summarise(Value = sum(male + female))

EC1177V$navn <- ApplyKlass(EC1177V$kommnr, klass = 131)

EC1177V_2021 <-  merge.data.frame(EC1177V,
                                  city_correspondence,
                                  by.x = "navn",
                                  by.y = "name")

EC1177V_2021 <- table_formatting("EC1177V_2021")

# create table EC1178V

EC1178V <- df_EC1177V %>%
              group_by(kommnr) %>%
              summarise(Value = sum(male))

EC1178V$navn <- ApplyKlass(EC1178V$kommnr, klass = 131)

EC1178V_2021 <-  merge.data.frame(EC1178V,
                                  city_correspondence,
                                  by.x = "navn",
                                  by.y = "name")

EC1177V_2021 <- table_formatting("EC1177V_2021")

# create table EC1179V

EC1179V <- df_EC1177V %>%
              group_by(kommnr) %>%
              summarise(Value = sum(female))

EC1179V$navn <- ApplyKlass(EC1179V$kommnr, klass = 131)

EC1179V_2021 <-  merge.data.frame(EC1179V,
                                  city_correspondence,
                                  by.x = "navn",
                                  by.y = "name")

EC1177V_2021 <- table_formatting("EC1177V_2021")

##########################################################################################################

#EC2008V
#Employment in agriculture, fishery (NACE Rev. 2: A)

# import data with API

df_EC2008V <- ApiData(13472,
                      ContentsCode = "SysselEtterBoste",
                      Region = c("0301", "5001", "1103", "4601"),
                      NACE2007 = "01-03",
                      Sektor = "ALLE",
                      Tid = "2021")[[2]]

# create table (group_by and sum not needed here)

EC2008V <- df_EC2008V %>%
                rename(Value = value)

# apply klass and merge with correspondence

EC2008V$navn <- ApplyKlass(EC2008V$Region, klass = 131)

EC2008V_2021 <-  merge.data.frame(EC2008V,
                                  city_correspondence,
                                  by.x = "navn",
                                  by.y = "name")

# table formatting

EC2008V_2021 <- table_formatting("EC2008V_2021")

##########################################################################################################

#EC2034V
#Employment in financial and insurance activities (NACE Rev. 2: K)

# import data with API

df_EC2034V <- ApiData(08536,
                      ContentsCode = "SysselBosted",
                      Region =  c("0301", "5001", "1103", "4601"),
                      NACE2007 = "66",
                      Kjonn = "0",
                      Tid = "2021")[[2]]

# create table

EC2034V <- df_EC2034V %>%
                rename(Value = value)

# apply klass and merge with correspondence

EC2034V$navn <- ApplyKlass(EC2034V$Region, klass = 131)

EC2034V_2021 <-  merge.data.frame(EC2034V,
                                  city_correspondence,
                                  by.x = "navn",
                                  by.y = "name")

# table formatting

EC2034V_2021 <- table_formatting("EC2034V_2021")

##########################################################################################################

#EC2035V
#Employment in real estate activities (NACE Rev. 2: L)

df_EC2035V <- ApiData(08536,
                   ContentsCode = "SysselBosted",
                   Region =  c("0301", "5001", "1103", "4601"),
                   NACE2007 = "68",
                   Kjonn = "0",
                   Tid = "2021")[[2]]

# create table

EC2035V <- df_EC2035V %>%
                rename(Value = value)

# apply klass and merge with correspondence

EC2035V$navn <- ApplyKlass(EC2035V$Region, klass = 131)

EC2035V_2021 <-  merge.data.frame(EC2035V,
                                  city_correspondence,
                                  by.x = "navn",
                                  by.y = "name")

# table formatting

EC2035V_2021 <- table_formatting("EC2035V_2021")


#######################################################################################################

# START HERE:::

# EC2022V
# Employment in construction (NACE Rev. 2: F)

df_EC2022V <- ApiData(08536,
                      ContentsCode = "SysselBosted",
                      Region = c("0301", "5001", "1103", "4601"),
                      NACE2007 = c("42", "43"),
                      Kjonn = "0",
                      Tid = "2021")[[2]]

EC2022V <- df_EC2022V %>%
                rename(Value = value)

EC2022V$Variable_code <- "EC2022V"
EC2022V$Flags <- NA
EC2022V$Footnote <- NA

names(EC2022V)[names(EC2022V) == "Region"] <- "City_code"
names(EC2022V)[names(EC2022V) == "Tid"] <- "Reference_year"

EC2022V$City_code <- case_when(EC2022V$City_code == "0301" ~ "NO001C", 
                               EC2022V$City_code == "1103" ~ "NO002C",
                               EC2022V$City_code == "4601" ~ "NO003C",
                               EC2022V$City_code == "5001" ~ "NO004C")


#' Employment in arts, entertainment and recreation; other service activities; activities of households (NACE Rev. 2: R to T)
#EC2038V
# clear for rbind
EC2038V <- ApiData(08536,
                ContentsCode = "SysselBosted",
                Region = c('0301', '5001', '1103', '4601'),
                NACE2007 = c("90", "96", "97"),
                Tid = '2021')[[2]]

EC2038V <- EC2038V[EC2038V$Kjonn == "0",]

EC2038V <- EC2038V %>%
  group_by(Region, Tid) %>%
  summarise(Value = sum(value))

EC2038V$Variable_code <- "EC2038V"
EC2038V$Flags <- NA
EC2038V$Footnote <- NA

names(EC2038V)[names(EC2038V) == "Region"] <- "City_code"
names(EC2038V)[names(EC2038V) == "Tid"] <- "Reference_year"

EC2038V$City_code <- case_when(EC2038V$City_code == "0301" ~ "NO001C", 
                               EC2038V$City_code == "1103" ~ "NO002C",
                               EC2038V$City_code == "4601" ~ "NO003C",
                               EC2038V$City_code == "5001" ~ "NO004C")


#Employment in public administration, defence, education, human health and social work activities (NACE Rev. 2: O to Q)
#EC2037V
# clear for rbind

EC2037V <- ApiData(08536,
                    ContentsCode = "SysselBosted",
                    Region = c('0301', '5001', '1103', '4601'),
                    NACE2007 = c("84", "85", "86"),
                    Tid = '2021')[[2]]

EC2037V <- EC2037V[EC2037V$Kjonn == "0",]

EC2037V <- EC2037V %>%
  group_by(Region, Tid) %>%
  summarise(Value = sum(value))
EC2037V

EC2037V$Variable_code <- "EC2037V"
EC2037V$Flags <- NA
EC2037V$Footnote <- NA

names(EC2037V)[names(EC2037V) == "Region"] <- "City_code"
names(EC2037V)[names(EC2037V) == "Tid"] <- "Reference_year"

EC2037V$City_code <- case_when(EC2037V$City_code == "0301" ~ "NO001C", 
                               EC2037V$City_code == "1103" ~ "NO002C",
                               EC2037V$City_code == "4601" ~ "NO003C",
                               EC2037V$City_code == "5001" ~ "NO004C")

#Employment in information and communication (NACE Rev. 2: J)
#EC2033V
# clear for rbind
EC2033V <- ApiData(08536,
                     ContentsCode = "SysselBosted",
                     Region = c('0301', '5001', '1103', '4601'),
                     NACE2007 = c("61", "62", "63"),
                     Tid = '2021')[[2]]

EC2033V <- EC2033V[EC2033V$Kjonn == "0",]
EC2033V

EC2033V <- EC2033V %>%
  group_by(Region, Tid) %>%
  summarise(Value = sum(value))
EC2033V

EC2033V$Variable_code <- "EC2033V"
EC2033V$Flags <- NA
EC2033V$Footnote <- NA

names(EC2033V)[names(EC2033V) == "Region"] <- "City_code"
names(EC2033V)[names(EC2033V) == "Tid"] <- "Reference_year"

EC2033V$City_code <- case_when(EC2033V$City_code == "0301" ~ "NO001C", 
                               EC2033V$City_code == "1103" ~ "NO002C",
                               EC2033V$City_code == "4601" ~ "NO003C",
                               EC2033V$City_code == "5001" ~ "NO004C")



#Employment in professional, scientific and technical activities; administrative
#and support service activities (NACE Rev. 2: M and N)
#EC2036V
##clear for rbind

EC2036V <- ApiData(08536,
                     ContentsCode = "SysselBosted",
                     Region = c('0301', '5001', '1103', '4601'),
                     NACE2007 = c("70", "71", "72", "74"),
                     Tid = '2021')[[2]]

EC2036V <- EC2036V[EC2036V$Kjonn == "0",]

EC2036V <- EC2036V %>%
  group_by(Region, Tid) %>%
  summarise(Value = sum(value))
EC2036V

EC2036V$Variable_code <- "EC2036V"
EC2036V$Flags <- NA
EC2036V$Footnote <- NA

names(EC2036V)[names(EC2036V) == "Region"] <- "City_code"
names(EC2036V)[names(EC2036V) == "Tid"] <- "Reference_year"

EC2036V$City_code <- case_when(EC2036V$City_code == "0301" ~ "NO001C", 
                               EC2036V$City_code == "1103" ~ "NO002C",
                               EC2036V$City_code == "4601" ~ "NO003C",
                               EC2036V$City_code == "5001" ~ "NO004C")



#Employment in mining, manufacturing, energy
#EC2009V
##clear for rbind

EC2009V <- ApiData(08536,
                     ContentsCode = "SysselBosted",
                     Region = c('0301', '5001', '1103', '4601'),
                     NACE2007 = c("05", "06", "07", "08", "09", "10", "11", "12", "13", "14",
                                  "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
                                  "25", "26", "27", "28", "29", "30", "31", "32", "33", "35",
                                  "36", "37", "38", "39"),
                     Tid = '2021')[[2]]

EC2009V <- EC2009V[EC2009V$Kjonn == "0",]


EC2009V <- EC2009V %>%
  group_by(Region) %>%
  summarise(Value = sum(value))

EC2009V$Variable_code <- "EC2009V"
EC2009V$Flags <- NA
EC2009V$Footnote <- NA
EC2009V$Reference_year <- "2021"

names(EC2009V)[names(EC2009V) == "Region"] <- "City_code"

EC2009V$City_code <- case_when(EC2009V$City_code == "0301" ~ "NO001C", 
                               EC2009V$City_code == "1103" ~ "NO002C",
                               EC2009V$City_code == "4601" ~ "NO003C",
                               EC2009V$City_code == "5001" ~ "NO004C")

colnames(EC2009V)
#
EC2009V <- EC2009V[, c(1,6,2,3,4,5)]


#Total employment (work place based)
#EC2020V
##clear for rbind

EC2020V <- ApiData(08536,
                    ContentsCode = "SysselArbsted",
                    Region = c('0301', '5001', '1103', '4601'),
                    NACE2007 = c("00-99"),
                    Tid = '2021')[[2]]
EC2020V <- EC2020V[EC2020V$Kjonn == "0",]
EC2020V


EC2020V$Variable_code <- "EC2020V"
EC2020V$Flags <- NA
EC2020V$Footnote <- NA

EC2020V <- EC2020V %>% select(Region, Tid, value, Variable_code, Flags, Footnote)

colnames(EC2020V) <- c("City_code", "Reference_year", "Value", "Variable_code", "Flags", "Footnote")

EC2020V$City_code <- case_when(EC2020V$City_code == "0301" ~ "NO001C", 
                               EC2020V$City_code == "1103" ~ "NO002C",
                               EC2020V$City_code == "4601" ~ "NO003C",
                               EC2020V$City_code == "5001" ~ "NO004C")







#Economically active population, total
#EC1001V
sysekoaktiv <- ApiData(12539,
                       ContentsCode = "LonnstakerBosted",
                       Region = c('0301', '5001', '1103', '4601'),
                       NACE2007 = c("00-99"),
                       Alder = c("15-74"),
                       Tid = '2021')[[2]]


#Economically active population, total
#EC1001V # EC1002V # EC1003V
sysekoaktiv <- ApiData(12539,
                       ContentsCode = "LonnstakerBosted",
                       Region = c('0301', '5001', '1103', '4601'),
                       NACE2007 = c("00-99"),
                       Alder = c("15-74"),
                       Tid = '2021')[[2]]

sysekoaktiv <- sysekoaktiv[sysekoaktiv$ArbeidsTidRen == "01+",]





sysekoaktiv <- ApiData(07984,
                       ContentsCode = "Sysselsatte",
                       Region = c('0301', '5001', '1103', '4601'),
                       NACE2007 = c("00-99"),
                       Alder = c("15-74"),
                       Tid = '2021')[[2]]
sysekoaktiv



#Economically active population, total
#EC1001V
#clear for rbind

EC1001V <- sysekoaktiv[sysekoaktiv$Kjonn == "0",]

EC1001V$Variable_code <- "EC1001V"
EC1001V$Flags <- NA
EC1001V$Footnote <- NA

EC1001V <- EC1001V %>% select(Region, Tid, value, Variable_code, Flags, Footnote)

colnames(EC1001V) <- c("City_code", "Reference_year", "Value", "Variable_code", "Flags", "Footnote")

EC1001V$City_code <- case_when(EC1001V$City_code == "0301" ~ "NO001C", 
                               EC1001V$City_code == "1103" ~ "NO002C",
                               EC1001V$City_code == "4601" ~ "NO003C",
                               EC1001V$City_code == "5001" ~ "NO004C")






#Economically active population, male
#EC1002V
#clear for rbind

EC1002V <- sysekoaktiv[sysekoaktiv$Kjonn == "1",]
EC1002V



EC1002V$Variable_code <- "EC1002V"
EC1002V$Flags <- NA
EC1002V$Footnote <- NA
colnames(EC1002V)

EC1002V <- EC1002V %>% select(Region, Tid, value, Variable_code, Flags, Footnote)

colnames(EC1002V) <- c("City_code", "Reference_year", "Value", "Variable_code", "Flags", "Footnote")

EC1002V$City_code <- case_when(EC1002V$City_code == "0301" ~ "NO001C", 
                               EC1002V$City_code == "1103" ~ "NO002C",
                               EC1002V$City_code == "4601" ~ "NO003C",
                               EC1002V$City_code == "5001" ~ "NO004C")



#Economically active population, female
#EC1003V
#clear for rbind

EC1003V <- sysekoaktiv[sysekoaktiv$Kjonn == "2",]
EC1003V

 



EC1003V$Variable_code <- "EC1003V"
EC1003V$Flags <- NA
EC1003V$Footnote <- NA
colnames(EC1003V)

EC1003V <- EC1003V %>% select(Region, Tid, value, Variable_code, Flags, Footnote)

colnames(EC1003V) <- c("City_code", "Reference_year", "Value", "Variable_code", "Flags", "Footnote")

EC1003V$City_code <- case_when(EC1003V$City_code == "0301" ~ "NO001C", 
                               EC1003V$City_code == "1103" ~ "NO002C",
                               EC1003V$City_code == "4601" ~ "NO003C",
                               EC1003V$City_code == "5001" ~ "NO004C")



names(which(unlist(eapply(.GlobalEnv,is.data.frame))))

stacks <- list()

for (i in ls(pattern='^EC.*V')){
  x <- get(i)
  stacks[[i]] <- x
}

EC_2021 <- do.call("bind_rows", stacks)
EC_2021

write.csv(EC_2021, "/ssb/stamme01/bediv/project/CITYSTATISTICS/EC_2021.csv", row.names=FALSE)
