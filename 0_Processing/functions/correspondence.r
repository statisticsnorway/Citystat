#####################################################################
# Correspondence made for production of demograhpy tables (DE1_2022)
# Project: CITYSTATISTICS
# Author: Stian Nergård
#####################################################################

var_name <- list("DE1040V", "DE1041V", "DE1042V", # 0-4 years
                 "DE1046V", "DE1047V", "DE1048V", # 15-19 years
                 "DE1049V", "DE1050V", "DE1051V", # 20-24 years
                 "DE1074V", "DE1075V", "DE1076V", # 5-9 years
                 "DE1077V", "DE1078V", "DE1079V", # 10-14 years
                 "DE1101V", "DE1102V", "DE1103V", # 25-29 years
                 "DE1104V", "DE1105V", "DE1106V", # 30-34 years
                 "DE1107V", "DE1108V", "DE1109V", # 35-39 years
                 "DE1110V", "DE1111V", "DE1112V", # 40-44 years
                 "DE1113V", "DE1114V", "DE1115V", # 45-49 years
                 "DE1116V", "DE1117V", "DE1118V", # 50-54 years
                 "DE1119V", "DE1120V", "DE1121V", # 55-59 years
                 "DE1122V", "DE1123V", "DE1124V", # 60-64 years
                 "DE1125V", "DE1126V", "DE1127V", # 65-69 years
                 "DE1128V", "DE1129V", "DE1130V", # 70-74 years
                 "DE1131V", "DE1132V", "DE1133V", # 75-79 years
                 "DE1134V", "DE1135V", "DE1136V") # 80-84 years

var_age <- list("F00-04", "F00-04", "F00-04",
                "F15-19", "F15-19", "F15-19",
                "F20-24", "F20-24", "F20-24",
                "F05-09", "F05-09", "F05-09",
                "F10-14", "F10-14", "F10-14",
                "F25-29", "F25-29", "F25-29",
                "F30-34", "F30-34", "F30-34",
                "F35-39", "F35-39", "F35-39",
                "F40-44", "F40-44", "F40-44",
                "F45-49", "F45-49", "F45-49",
                "F50-54", "F50-54", "F50-54",
                "F55-59", "F55-59", "F55-59",
                "F60-64", "F60-64", "F60-64",
                "F65-69", "F65-69", "F65-69",
                "F70-74", "F70-74", "F70-74",
                "F75-79", "F75-79", "F75-79",
                "F80-84", "F80-84", "F80-84")

var_gender <- list("total", "1", "2",
                   "total", "1", "2",
                   "total", "1", "2",
                   "total", "1", "2",
                   "total", "1", "2",
                   "total", "1", "2",
                   "total", "1", "2",
                   "total", "1", "2",
                   "total", "1", "2",
                   "total", "1", "2",
                   "total", "1", "2",
                   "total", "1", "2",
                   "total", "1", "2",
                   "total", "1", "2",
                   "total", "1", "2",
                   "total", "1", "2",
                   "total", "1", "2")

variable_correspondence <- data.frame(cbind(var_name, var_age, var_gender))