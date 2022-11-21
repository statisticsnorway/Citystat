# -*- coding: utf-8 -*-
# #################################################
# # Project: Grant_Aid
# # Script purpose: Create table CR1003V (cinema seats)
# # Date: 17-11-2022
# # Author: Stian Nergård
# #################################################

# load packages

library(PxWebApiData)
suppressPackageStartupMessages(library(tidyverse))
library(klassR)

# import data

df_CR1003V <- ApiData(11817,
                  KOKkommuneregion0000 = c("0301", "1103", "4601", "5001"),
                  ContentsCode = "KOSkinoseter0000",
                  Tid = "2021")[[2]]

# import klass correspondence for city names

df_CR1003V$navn <- ApplyKlass(df_CR1003V$KOKkommuneregion0000, klass = 131)

city_correspondence <- GetKlass(550) %>%
                    select(code, name)

# merge klass correspondence with dataframe

CR1003V_2021 <- merge.data.frame(df_CR1003V,
                                 city_correspondence,
                                 by.x = "navn",
                                 by.y = "name")

# table formatting

CR1003V_2021 <- CR1003V_2021 %>%
                    mutate(Reference_year = 2021,
                           Variable_code = "CR1003V",
                           Flags = "",
                           Footnote = "") %>%
                    select(code,
                           Variable_code,
                           Reference_year,
                           value,
                           Flags,
                           Footnote) %>%
                    rename(City_code = code,
                           Value = value) %>%
                    arrange(City_code)
