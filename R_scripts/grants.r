###
rm(list = ls())

getwd()



# last inn pakker  

library(PxWebApiData) 
library(tidyverse) 
library(readxl)

col_order <- c("City_code", "Reference_year", "Value", "Variable_code", "Flags", "Footnote")
#my_data2 <- my_data[, col_order]


#Persons employed, 20-64, total
#EC1177V

aktiv <- read_excel("/ssb/stamme01/bediv/project/CITYSTATISTICS/06040_20221109-180202.xlsx")

colnames(aktiv) <- c("kommnr", "kommnavn", "alder1", "alder2", "value")
aktiv <- aktiv[,c("kommnr", "alder1", "value")]
aktiv <- aktiv[c(4:21543),]
aktiv <- aktiv  %>% fill(kommnr)
aktiv$kommnr <- substring(aktiv$kommnr, 3)
aktiv$value <- as.numeric(aktiv$value)
aktiv$alder1 <- as.numeric(aktiv$alder1)
aktiv <- aktiv[aktiv$alder1 >19 & aktiv$alder1 < 65 ,]

aktiv <- aktiv[aktiv$kommnr == "0301" | aktiv$kommnr == "5001" |
                 aktiv$kommnr == "1103" | aktiv$kommnr == "4601" ,]

aktiv <- aktiv %>%
  group_by(kommnr) %>%
  summarise(tot = sum(value))
aktiv


#Persons employed, male/female
aktivmf <- read_excel("/ssb/stamme01/bediv/project/CITYSTATISTICS/06040_20221110-135541.xlsx")
#aktivmf <- read_excel("06040_20221110-135541.xlsx")
aktivmf <- aktivmf[c(4:21544),]

names(aktivmf) <- aktivmf[1,]
aktivmf <- aktivmf[-1,]

colnames(aktivmf) <- c("City_code", "kommnavn", "alder1", "alder2", "Menn", "Kvinner")
aktivmf <- aktivmf  %>% fill(City_code)
aktivmf$City_code <- substring(aktivmf$City_code, 3)
aktivmf$alder1 <- as.numeric(aktivmf$alder1)

aktivmf <- aktivmf[aktivmf$alder1 >19 & aktivmf$alder1 < 65 ,]

aktivmf <- aktivmf[aktivmf$City_code == "0301" | aktivmf$City_code == "5001" |
                     aktivmf$City_code == "1103" | aktivmf$City_code == "4601" ,]

aktivmf$Reference_year <- "2021"

aktivmf$Kvinner <- as.numeric(aktivmf$Kvinner)
aktivmf$Menn <- as.numeric(aktivmf$Menn)


aktivm <- aktivmf[,c("City_code", "Reference_year", "Menn")]
aktivf <- aktivmf[,c("City_code", "Reference_year", "Kvinner")]

#Persons employed, 20-64, total
#EC1177V
# clear for rbind
EC1177V <- aktivmf %>%
  group_by(City_code, Reference_year) %>%
  summarise(Value = sum(Menn+Kvinner))
EC1177V
EC1177V$Flags <- NA
EC1177V$Footnote <- NA
EC1177V$Variable_code <- "EC1177V"

EC1177V$City_code <- case_when(EC1177V$City_code == "0301" ~ "NO001C", 
                               EC1177V$City_code == "1103" ~ "NO002C",
                               EC1177V$City_code == "4601" ~ "NO003C",
                               EC1177V$City_code == "5001" ~ "NO004C")


EC1177V <- EC1177V[, col_order]


#Persons employed, 20-64, male
#EC1178V
# clear for rbind

EC1178V <- aktivm %>%
  group_by(City_code, Reference_year) %>%
  summarise(Value = sum(Menn))
EC1178V
EC1178V$Flags <- NA
EC1178V$Footnote <- NA
EC1178V$Variable_code <- "EC1178V"
EC1178V$City_code <- case_when(EC1178V$City_code == "0301" ~ "NO001C", 
                               EC1178V$City_code == "1103" ~ "NO002C",
                               EC1178V$City_code == "4601" ~ "NO003C",
                               EC1178V$City_code == "5001" ~ "NO004C")


EC1178V <- EC1178V[, col_order]


#Persons employed, 20-64, female
#EC1179V
# clear for rbind

EC1179V <- aktivf %>%
  group_by(City_code, Reference_year) %>%
  summarise(Value = sum(Kvinner))
EC1179V
EC1179V$Flags <- NA
EC1179V$Footnote <- NA
EC1179V$Variable_code <- "EC1179V"
EC1179V$City_code <- case_when(EC1179V$City_code == "0301" ~ "NO001C", 
                               EC1179V$City_code == "1103" ~ "NO002C",
                               EC1179V$City_code == "4601" ~ "NO003C",
                               EC1179V$City_code == "5001" ~ "NO004C")

EC1179V <- EC1179V[, col_order]




#Employment in agriculture, fishery (NACE Rev. 2: A)
#EC2008V
# clear for rbind

sysagri <- ApiData(13472,
                   ContentsCode = "SysselEtterBoste",
                   Region = c('0301', '5001', '1103', '4601'),
                   NACE2007 = "01-03",
                   Tid = '2021')[[2]]
sysagri$value <- as.numeric(sysagri$value)
sysagri <- sysagri[sysagri$Sektor == "ALLE",]
sysagri
sysagri$Variable_code <- "EC2008V"
sysagri$Flags <- NA
sysagri$Footnote <- NA

EC2008V <- sysagri %>% select(Region, Tid, value, Variable_code, Flags, Footnote)

colnames(EC2008V) <- c("City_code", "Reference_year", "Value", "Variable_code", "Flags", "Footnote")

EC2008V$City_code <- case_when(EC2008V$City_code == "0301" ~ "NO001C", 
                               EC2008V$City_code == "1103" ~ "NO002C",
                               EC2008V$City_code == "4601" ~ "NO003C",
                               EC2008V$City_code == "5001" ~ "NO004C")

#write.csv(EC2008V, "X:/320/1_regional/0_oppdrag/2022/2021-NO-CITY/Deliveries/EC2008V.csv")





#Employment in financial and insurance activities (NACE Rev. 2: K)
#EC2034V
EC2034V <- ApiData(08536,
                  ContentsCode = "SysselBosted",
                  Region = c('0301', '5001', '1103', '4601'),
                  NACE2007 = "66",
                  Tid = '2021')[[2]]
EC2034V$value <- as.numeric(EC2034V$value)

EC2034V <- EC2034V[EC2034V$Kjonn == "0",]

EC2034V <- EC2034V %>%
  group_by(Region, Tid) %>%
  summarise(Value = sum(value))

EC2034V$Variable_code <- "EC2034V"
EC2034V$Flags <- NA
EC2034V$Footnote <- NA

names(EC2034V)[names(EC2034V) == "Region"] <- "City_code"
names(EC2034V)[names(EC2034V) == "Tid"] <- "Reference_year"

EC2034V$City_code <- case_when(EC2034V$City_code == "0301" ~ "NO001C", 
                               EC2034V$City_code == "1103" ~ "NO002C",
                               EC2034V$City_code == "4601" ~ "NO003C",
                               EC2034V$City_code == "5001" ~ "NO004C")




#Employment in real estate activities (NACE Rev. 2: L)
#EC2035V
EC2035V <- ApiData(08536,
                    ContentsCode = "SysselBosted",
                    Region = c('0301', '5001', '1103', '4601'),
                    NACE2007 = "68",
                    Tid = '2021')[[2]]

EC2035V <- EC2035V[EC2035V$Kjonn == "0",]

EC2035V <- EC2035V %>%
  group_by(Region, Tid) %>%
  summarise(Value = sum(value))

EC2035V$Variable_code <- "EC2035V"
EC2035V$Flags <- NA
EC2035V$Footnote <- NA

names(EC2035V)[names(EC2035V) == "Region"] <- "City_code"
names(EC2035V)[names(EC2035V) == "Tid"] <- "Reference_year"

EC2035V$City_code <- case_when(EC2035V$City_code == "0301" ~ "NO001C", 
                               EC2035V$City_code == "1103" ~ "NO002C",
                               EC2035V$City_code == "4601" ~ "NO003C",
                               EC2035V$City_code == "5001" ~ "NO004C")




#' Employment in construction (NACE Rev. 2: F)
#EC2022V
# clear for rbind
EC2022V <- ApiData(08536,
                ContentsCode = "SysselBosted",
                Region = c('0301', '5001', '1103', '4601'),
                NACE2007 = c("42", "43"),
                Tid = '2021')[[2]]

EC2022V <- EC2022V[EC2022V$Kjonn == "0",]

EC2022V <- EC2022V %>%
  group_by(Region, Tid) %>%
  summarise(Value = sum(value))

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
