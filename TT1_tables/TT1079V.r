# -*- coding: utf-8 -*-
# #################################################
# # Project: Grant_Aid
# # Script purpose: Create table: TT1060V (length of bicycle network)
# # Date: 17-11-2022
# # Author: Stian Nergård
# #################################################

# load packages

library(PxWebApiData)
suppressPackageStartupMessages(library(tidyverse))
library(klassR)

# import data 

df_TT1079V <- ApiData(11845,
                  KOKkommuneregion0000 = c("0301", "1103", "4601", "5001"),
                  ContentsCode = c("KOSgstilrettelag0000"),
                  Tid = "2021")[[2]]

# import klass correspondence for city names

df_TT1079V$navn <- ApplyKlass(df_TT1079V$KOKkommuneregion0000, klass = 131)

city_correspondence <- GetKlass(550) %>%
                    select(code, name)

# merge klass correspondence with dataframe

TT1079V_2021 <- merge.data.frame(df_TT1079V,
                                 city_correspondence,
                                 by.x = "navn",
                                 by.y = "name")

# table formatting

TT1079V_2021 <- TT1079V_2021 %>%
                    mutate(Reference_year = 2021,
                           Variable_code = "TT1079V",
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
