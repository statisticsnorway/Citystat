# #################################################
# # Project: Grant_Aid
# # Script purpose: Create table CR1015V (number of public swimming pools)
# # Date: 17-11-2022
# # Author: Stian Nergård
# #################################################

# load packages

library(readxl)
suppressPackageStartupMessages(library(tidyverse))
library(klassR)

# import data from excel and create dataframe

path = "/ssb/stamme01/bediv/project/CITYSTATISTICS/grd_public_swimmingpools.xlsx"

sheets <- excel_sheets(path) # list to iterate over
list1 <- list() # list to fill with dfs

for (i in sheets) {
    x <- read_excel(path, sheet = i)
    list1[[i]] <- x
    }

df_CR1015V <- do.call(rbind, list1)

# group by region and count

df_CR1015V <- df_CR1015V %>%
                  group_by(Kommune) %>%
                  summarise(Value = n())

# load correspondence with city_code from Klass

city_correspondence <- GetKlass(550) %>%
                           select(code, name)

# merge dataframe with city_code correspondence

CR1015V_2021 <- merge.data.frame(df_CR1015V,
                                 city_correspondence,
                                 by.x = "Kommune",
                                 by.y = "name")

# table formatting

CR1015V_2021 <- CR1015V_2021 %>%
                    mutate(Reference_year = 2021,
                           Variable_code = "CR1015V",
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
