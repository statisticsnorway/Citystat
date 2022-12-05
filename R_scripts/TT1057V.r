# #################################################
# # Project: Grant_Aid
# # Script purpose: Create table: TT1057V (number of private cars)
# # Date: 17-11-2022
# # Author: Stian Nergård
# #################################################

# load packages

library(PxWebApiData)
suppressPackageStartupMessages(library(tidyverse))
library(klassR)

# import data

df_TT1057V <- ApiData(07849,
                  Region = c("0301", "1103", "4601", "5001"),
                  ContentsCode = "Personbil1",
                  Tid = "2021",
                  KjoringensArt = 1)[[2]]

# import klass correspondence for city names

df_TT1057V$navn <- ApplyKlass(df_TT1057V$Region, klass = 131)

city_correspondence <- GetKlass(550) %>%
                    select(code, name)

# merge klass correspondence with dataframe

TT1057V_2021 <- merge.data.frame(df_TT1057V,
                                 city_correspondence,
                                 by.x = "navn",
                                 by.y = "name")
# sum rows

TT1057V_2021 <- TT1057V_2021 %>%
                    group_by(code) %>%
                    summarise(Value = sum(value))

# table formatting

TT1057V_2021 <- TT1057V_2021 %>%
                    mutate(Reference_year = 2021,
                           Variable_code = "TT1057V",
                           Flags = "",
                           Footnote = "") %>%
                    select(code,
                           Variable_code,
                           Reference_year,
                           Value,
                           Flags,
                           Footnote) %>%
                    rename(City_code = code) %>%
                    arrange(City_code)
