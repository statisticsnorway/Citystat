# -*- coding: utf-8 -*-
# #################################################
# # Project: Grant_Aid
# # Script purpose: Create table CR1010V (number of public libraries)
# # Date: 17-11-2022
# # Author: Stian Nergård
# #################################################

# load packages

library(readxl)
suppressPackageStartupMessages(library(tidyverse))
library(klassR)

# import data from excel and create dataframe

path = "/ssb/stamme01/bediv/project/CITYSTATISTICS/grd_Radata-folkebibliotek-2021.xlsx"
df_CR1010V <- janitor::row_to_names(read_excel(path, sheet = 2), 1)

# select data from excel sheet

df_CR1010V <- df_CR1010V %>%
    select("Kommune", "Kommunenr.", "Antall faste bibliotek-enheter") %>%
    filter(Kommunenr. %in% c("301", "4601", "1103", "5001")) %>%
    rename(Value = "Antall faste bibliotek-enheter")

# load correspondence with city_code from Klass

city_correspondence <- GetKlass(550) %>%
                           select(code, name)

# merge dataframe with city_code correspondence

CR1010V_2021 <- merge.data.frame(df_CR1010V,
                                 city_correspondence,
                                 by.x = "Kommune",
                                 by.y = "name")

# table formatting

CR1010V_2021 <- CR1010V_2021 %>%
                    mutate(Reference_year = 2021,
                           Variable_code = "CR1010V",
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
