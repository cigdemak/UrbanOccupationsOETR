#Sys.setlocale("LC_CTYPE", "UTF-8")

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyTree)
library(rgdal)
library(colourpicker)
library(digest)
library(googleVis)
library(leaflet)
library(maps)
library(maptools)
library(stringi)
library(xts)
library(mapview)
library(htmlwidgets)
library(leaflet.minicharts)

load("data/all_index_map_census_data.RData")
if(FALSE)
  {
  
  # load district indexes 
  district_index_bul_1881 <- as.data.frame(readxl::read_xlsx("./data/index_data/district_index_bul.xlsx", sheet = "district_index_bul_1881"))
  rownames(district_index_bul_1881) <- as.character(unlist(district_index_bul_1881$NAME))

  district_index_bul_1892 <- as.data.frame(readxl::read_xlsx("./data/index_data/district_index_bul.xlsx", sheet = "district_index_bul_1892"))
  rownames(district_index_bul_1892) <- as.character(unlist(district_index_bul_1892$NAME))
  
  district_index_bul_1897 <- as.data.frame(readxl::read_xlsx("./data/index_data/district_index_bul.xlsx", sheet = "district_index_bul_1897"))
  rownames(district_index_bul_1897) <- as.character(unlist(district_index_bul_1897$NAME))
  
  district_index_bul_1956 <- as.data.frame(readxl::read_xlsx("./data/index_data/district_index_bul.xlsx", sheet = "district_index_bul_1956"))
  rownames(district_index_bul_1956) <- as.character(unlist(district_index_bul_1956$NAME))
  
  district_index_bul_1934 <- as.data.frame(readxl::read_xlsx("./data/index_data/district_index_bul.xlsx", sheet = "district_index_bul_1934"))
  rownames(district_index_bul_1934) <- as.character(unlist(district_index_bul_1934$NAME))
  
  district_index_bul_1905 <- as.data.frame(readxl::read_xlsx("./data/index_data/district_index_bul.xlsx", sheet = "district_index_bul_1905"))
  rownames(district_index_bul_1905) <- as.character(unlist(district_index_bul_1905$NAME))
  
  district_index_tur_2000 <- as.data.frame(readxl::read_xlsx("./data/index_data/district_index_tur.xlsx", sheet = "district_index_tur_2000"))
  rownames(district_index_tur_2000) <- as.character(unlist(district_index_tur_2000$NAME))
  
  district_index_tur_1990 <- as.data.frame(readxl::read_xlsx("./data/index_data/district_index_tur.xlsx", sheet = "district_index_tur_1990"))
  rownames(district_index_tur_1990) <- as.character(unlist(district_index_tur_1990$NAME))
  
  district_index_tur_1985 <- as.data.frame(readxl::read_xlsx("./data/index_data/district_index_tur.xlsx", sheet = "district_index_tur_1985"))
  rownames(district_index_tur_1985) <- as.character(unlist(district_index_tur_1985$NAME))
  
  district_index_tur_1970 <- as.data.frame(readxl::read_xlsx("./data/index_data/district_index_tur.xlsx", sheet = "district_index_tur_1970"))
  rownames(district_index_tur_1970) <- as.character(unlist(district_index_tur_1970$NAME))
  
  district_index_tur_1950 <- as.data.frame(readxl::read_xlsx("./data/index_data/district_index_tur.xlsx", sheet = "district_index_tur_1950"))
  rownames(district_index_tur_1950) <- as.character(unlist(district_index_tur_1950$NAME))
  
  district_index_tur_1935 <- as.data.frame(readxl::read_xlsx("./data/index_data/district_index_tur.xlsx", sheet = "district_index_tur_1935"))
  rownames(district_index_tur_1935) <- as.character(unlist(district_index_tur_1935$NAME))
  
  # bring together all district indexes of all countries
  all_district_index <- list("BUL" = list("1881/1885" = district_index_bul_1881, "1892" = district_index_bul_1892, "1897" = district_index_bul_1897, "1956" = district_index_bul_1956, "1934" = district_index_bul_1934, "1905" = district_index_bul_1905), 
                         "TUR" = list("2000" = district_index_tur_2000, "1990" = district_index_tur_1990, "1985" = district_index_tur_1985, "1970" = district_index_tur_1970, "1950" = district_index_tur_1950, "1935" = district_index_tur_1935))
  
  # load subdistrict indexes  
  subdistrict_index_bul_1881 <- as.data.frame(readxl::read_xlsx("./data/index_data/subdistrict_index_bul.xlsx", sheet = "subdistrict_index_bul_1881"))
  rownames(subdistrict_index_bul_1881) <- as.character(unlist(subdistrict_index_bul_1881$NAME))
  
  subdistrict_index_bul_1892 <- as.data.frame(readxl::read_xlsx("./data/index_data/subdistrict_index_bul.xlsx", sheet = "subdistrict_index_bul_1892"))
  rownames(subdistrict_index_bul_1892) <- as.character(unlist(subdistrict_index_bul_1892$NAME))
  
  subdistrict_index_bul_1897 <- as.data.frame(readxl::read_xlsx("./data/index_data/subdistrict_index_bul.xlsx", sheet = "subdistrict_index_bul_1897"))
  rownames(subdistrict_index_bul_1897) <- as.character(unlist(subdistrict_index_bul_1897$NAME))
  
  subdistrict_index_bul_1905 <- as.data.frame(readxl::read_xlsx("./data/index_data/subdistrict_index_bul.xlsx", sheet = "subdistrict_index_bul_1905"))
  rownames(subdistrict_index_bul_1905) <- as.character(unlist(subdistrict_index_bul_1905$NAME))
  
  subdistrict_index_bul_1934 <- as.data.frame(readxl::read_xlsx("./data/index_data/subdistrict_index_bul.xlsx", sheet = "subdistrict_index_bul_1934"))
  rownames(subdistrict_index_bul_1934) <- as.character(unlist(subdistrict_index_bul_1934$NAME))
  
  subdistrict_index_bul_1956 <- as.data.frame(readxl::read_xlsx("./data/index_data/subdistrict_index_bul.xlsx", sheet = "subdistrict_index_bul_1956"))
  rownames(subdistrict_index_bul_1956) <- as.character(unlist(subdistrict_index_bul_1956$NAME))
  
  subdistrict_index_tur_1970 <- as.data.frame(readxl::read_xlsx("./data/index_data/subdistrict_index_tur.xlsx", sheet = "subdistrict_index_tur_1970"))
  rownames(subdistrict_index_tur_1970) <- as.character(unlist(subdistrict_index_tur_1970$NAME))

  subdistrict_index_tur_1990 <- as.data.frame(readxl::read_xlsx("./data/index_data/subdistrict_index_tur.xlsx", sheet = "subdistrict_index_tur_1990"))
  rownames(subdistrict_index_tur_1990) <- as.character(unlist(subdistrict_index_tur_1990$NAME))
  
  # bring together all subdistrict indexes of all countries  
  all_subdistrict_index <- list("BUL" = list("1881/1885" = subdistrict_index_bul_1881,
                                             "1892" = subdistrict_index_bul_1892, 
                                             "1897" = subdistrict_index_bul_1897,
                                             "1956" = subdistrict_index_bul_1956, 
                                             "1934" = subdistrict_index_bul_1934, 
                                             "1905" = subdistrict_index_bul_1905), 
                                "TUR" = list("1990" = subdistrict_index_tur_1990,
                                             "1970" = subdistrict_index_tur_1970))
  # load point indexes  
  point_index_tur_1990 <- as.data.frame(readxl::read_xlsx("./data/index_data/point_index_tur.xlsx", sheet = "point_index_tur_1990"))
  point_index_tur_1990$NAME[which(point_index_tur_1990$NAME == "NA, NA, NA")] <- as.character(point_index_tur_1990$LOCATION_ID[which(point_index_tur_1990$NAME == "NA, NA, NA")])
  rownames(point_index_tur_1990) <- as.character(unlist(point_index_tur_1990$NAME))
  
  load("data/index_data/point_index_oe.Rdata")
  point_index_oe_1840s <- as.data.frame(point_index_1840s)
  rownames(point_index_oe_1840s) <- as.character(unlist(point_index_oe_1840s$NAME))
  
  # bring together all point indexes of all countries  
  all_point_index <- list("OE" = list("1840s" = point_index_oe_1840s), 
                          "TUR" = list("1990" = point_index_tur_1990))
  
  # bringtogether all theree district, subdistrict and point indexes  
  all_index <- list("dist" = all_district_index, "subdist" = all_subdistrict_index, "point" = all_point_index)
  
  # load Turkey's district maps (shape files)  
  district_maps_tur_2000 <- readOGR("./data/map_data/Turkey_district_2000.shp", encoding = "UTF-8", use_iconv = TRUE)
  names(district_maps_tur_2000)[names(district_maps_tur_2000) == "ILKOD"] <- "MAP_ID"
  
  district_maps_tur_1990 <- readOGR("./data/map_data/Turkey_district_1990.shp", encoding = "UTF-8", use_iconv = TRUE)
  names(district_maps_tur_1990)[names(district_maps_tur_1990) == "ILKOD"] <- "MAP_ID"
  
  district_maps_tur_1985 <- readOGR("./data/map_data/Turkey_district_1985.shp", encoding = "UTF-8", use_iconv = TRUE)
  names(district_maps_tur_1985)[names(district_maps_tur_1985) == "ILKOD"] <- "MAP_ID"
  
  district_maps_tur_1970 <- readOGR("./data/map_data/Turkey_district_1970.shp", encoding = "UTF-8", use_iconv = TRUE)
  names(district_maps_tur_1970)[names(district_maps_tur_1970) == "ILKOD"] <- "MAP_ID"
  
  load("./data/map_data/Turkey_district_1950.shp")
  district_maps_tur_1950 <- district_maps_1950
  names(district_maps_tur_1950)[names(district_maps_tur_1950) == "ILKOD"] <- "MAP_ID"
  
  load("./data/map_data/Turkey_district_1935.shp")
  district_maps_tur_1935 <- district_maps_1935
  names(district_maps_tur_1935)[names(district_maps_tur_1935) == "ILKOD"] <- "MAP_ID"
    
  # load Bulgaria's district maps (shape files)  
  district_maps_bul_1956 <- readOGR("./data/map_data/Bulgaria_subdistrict_1956.shp", encoding = "UTF-8", use_iconv = TRUE)
  names(district_maps_bul_1956)[names(district_maps_bul_1956) == "ILKOD"] <- "MAP_ID"
  
  district_maps_bul_1934 <- readOGR("./data/map_data/Bulgaria_subdistrict_1934.shp", encoding = "UTF-8", use_iconv = TRUE)
  names(district_maps_bul_1934)[names(district_maps_bul_1934) == "ILKOD"] <- "MAP_ID"
  
  district_maps_bul_1905 <- readOGR("./data/map_data/Bulgaria_subdistrict_1905.shp", encoding = "UTF-8", use_iconv = TRUE)
  names(district_maps_bul_1905)[names(district_maps_bul_1905) == "ILKOD"] <- "MAP_ID"
  
  district_map_bul_1881 <- readOGR("data/map_data/BG_1881_District.shp", encoding = "UTF-8", use_iconv = TRUE)
  district_map_bul_1881$"MAP_ID" <- as.numeric(district_map_bul_1881$GIS_Comp_1)
  district_map_bul_1892 <- readOGR("data/map_data/BG_1892_District.shp", encoding = "UTF-8", use_iconv = TRUE)
  district_map_bul_1892$"MAP_ID" <- as.numeric(district_map_bul_1892$GIS_Com_Co)
  
  # bring together all district maps of all countries
  all_district_maps <- list("BUL" = list("1881/1885" = district_map_bul_1881, "1892" = district_map_bul_1892, "1897" = district_map_bul_1892,"1956" = district_maps_bul_1956, "1934" = district_maps_bul_1934, "1905" = district_maps_bul_1905),
                        "TUR" = list("1990" = district_maps_tur_1990, "1985" = district_maps_tur_1985, "1970" = district_maps_tur_1970, "1950" = district_maps_tur_1950, "1935" = district_maps_tur_1935))
  
  # load Turkey's subdistrict maps  
  subdistrict_map_tur_1970 <- readOGR("./data/map_data/TR1970_Subdistrict.shp", encoding = "UTF-8", use_iconv = TRUE)
  subdistrict_map_tur_1970$"MAP_ID" <- as.numeric(subdistrict_map_tur_1970$Joinfld)
  
  subdistrict_maps_tur_1990 <- readOGR("./data/map_data/Turkey_subdistrict_1990.shp", encoding = "UTF-8", use_iconv = TRUE)
  subdistrict_maps_tur_1990@data <- cbind(subdistrict_maps_tur_1990@data, subdistrict_index_tur_1990$LOCATION_ID, subdistrict_index_tur_1990$NAME)
  colnames(subdistrict_maps_tur_1990@data)[8:9] <- c("ILKOD", "NAME")
  
  # load Bulgaria's subdistrict maps
  subdistrict_map_bul_1881 <- readOGR("data/map_data/BG_1881_Sub_district.shp", encoding = "UTF-8", use_iconv = TRUE)
  subdistrict_map_bul_1881$"MAP_ID" <- as.numeric(subdistrict_map_bul_1881$GIS_Comp_3)
  
  subdistrict_map_bul_1892 <- readOGR("data/map_data/BG_1892_Subdistrict.shp", encoding = "UTF-8", use_iconv = TRUE)
  subdistrict_map_bul_1892$"MAP_ID" <- as.numeric(subdistrict_map_bul_1892$GIS_Comp_3)
  
  subdistrict_map_bul_1956 <- district_maps_bul_1956
  names(subdistrict_map_bul_1956)[names(subdistrict_map_bul_1956) == "MAP_ID"] <- "OkrugCode"
  names(subdistrict_map_bul_1956)[names(subdistrict_map_bul_1956) == "OkoliyaI"] <- "MAP_ID"
  
  subdistrict_map_bul_1934 <- district_maps_bul_1934
  names(subdistrict_map_bul_1934)[names(subdistrict_map_bul_1934) == "MAP_ID"] <- "OkrugCode"
  names(subdistrict_map_bul_1934)[names(subdistrict_map_bul_1934) == "OkoliyaI"] <- "MAP_ID"
  
  subdistrict_map_bul_1905 <- district_maps_bul_1905
  names(subdistrict_map_bul_1905)[names(subdistrict_map_bul_1905) == "MAP_ID"] <- "OkrugCode"
  names(subdistrict_map_bul_1905)[names(subdistrict_map_bul_1905) == "OkoliyaI"] <- "MAP_ID"
  
  # bring together all subdistrict maps of all countries
  all_subdistrict_maps <- list("BUL"= list("1881/1885" = subdistrict_map_bul_1881,
                                           "1892" = subdistrict_map_bul_1892,
                                           "1897" = subdistrict_map_bul_1892,
                                           "1956" = subdistrict_map_bul_1956, 
                                           "1934" = subdistrict_map_bul_1934,
                                           "1905" = subdistrict_map_bul_1905),
                               "TUR"= list("1970" = subdistrict_map_tur_1970,
                                           "1990" = subdistrict_maps_tur_1990))
  
  # load Turkey's point map
  point_map_tur_90 <-  readOGR("./data/map_data/Turkey_point_1990.shp", encoding = "UTF-8", use_iconv = TRUE)
  point_names <- sprintf("%s, %s, %s",as.character(point_map_tur_90$ILI), as.character(point_map_tur_90$ILCESI), as.character(point_map_tur_90$KOYU))
  point_map_tur_90@data <- cbind(point_map_tur_90@data, 1:length(point_names), point_names)
  colnames(point_map_tur_90@data)[14:15] <- c("MAP_ID", "NAME")
  
  # load Bulgaria's point map
  point_map_BUL_1840 <- readOGR("./data/map_data/Turkey_point_1990.shp", encoding = "UTF-8", use_iconv = TRUE)
  
  # bring together all point maps of all countries
  all_point_maps <- list("OE"= list("1840s" = point_map_tur_90),
                         "TUR"= list("1990" = point_map_tur_90))
  
  # bring together all three district, subdistrict and point maps of all countries
  all_maps <- list("dist" = all_district_maps, "subdist" = all_subdistrict_maps, "point" = all_point_maps)
  
  # load Bulgaria's district level occupational data
  economic_data_bul <- read.csv("./data/occupation_data/economic_data_district_bul.csv", check.names = FALSE)
  economic_data_bul <- economic_data_bul[, c("YEAR", "LOCATION_INDEX", "Both", "Male", "Female", "PST CONC")]
  economic_data_bul <- economic_data_bul[which(economic_data_bul$LOCATION_INDEX != "NA"),]
  
  # load Bulgaria's subdistrict level occupatianal data
  economic_data_subdist_bul <- read.csv("./data/occupation_data/economic_data_subdistrict_bul.csv", check.names = FALSE)
  economic_data_subdist_bul <- economic_data_subdist_bul[, c("YEAR", "LOCATION_INDEX", "Both", "Male", "Female", "PST CONC")]
  economic_data_subdist_bul <- economic_data_subdist_bul[which(economic_data_subdist_bul$LOCATION_INDEX != "NA"),]
  economic_data_subdist_bul <- economic_data_subdist_bul[which(economic_data_subdist_bul$LOCATION_INDEX != "NA"),]
  
  # load Turkey's dşstrict level occupational data
  economic_data_tur <- readxl::read_xlsx("./data/occupation_data/economic_data_district_tur.xlsx", sheet = "main")
  economic_data_tur <- economic_data_tur[, c("YEAR", "LOCATION_INDEX", "Both", "Male", "Female", "PST I", "PST CONC")]
  
  # bring together all level occupational data of all countries
  all_occupational_data <- list("dist" = list("BUL" = list("1905" = economic_data_bul[economic_data_bul$YEAR == 1905, ],
                                                           "1934" = economic_data_bul[economic_data_bul$YEAR == 1934, ],
                                                           "1956" = economic_data_bul[economic_data_bul$YEAR == 1956, ]),
                                              "TUR" = list("1970" = economic_data_tur[economic_data_tur$YEAR == 1970, ],
                                                           "1985" = economic_data_tur[economic_data_tur$YEAR == 1985, ],
                                                           "1990" = economic_data_tur[economic_data_tur$YEAR == 1990, ],
                                                           "2000" = economic_data_tur[economic_data_tur$YEAR == 2000, ])),
                                "subdist" = list("BUL" = list(
                                  # "1905" = economic_data_subdist_bul[economic_data_subdist_bul$YEAR == 1905, ],
                                  # "1934" = economic_data_subdist_bul[economic_data_subdist_bul$YEAR == 1934, ],
                                  # "1956" = economic_data_subdist_bul[economic_data_subdist_bul$YEAR == 1956, ]
                                )
                                )
  )
  
 
  # load  Bulgaria's all level population data for other years
  pop_data_dist_bul <- read.csv(file = "./data/population_data/population_data_district_BUL.csv")
  
  # load Turkey's district level population data
  pop_data_dist_tur <- read.csv(file = "./data/population_data/population_data_district_TUR.csv")
  
  # bring together district level population data of all countries
  all_population_dist_data <- list("BUL" = list("1881/1885" = pop_data_dist_bul[pop_data_dist_bul$YEAR == 1881, !(colnames(pop_data_dist_bul)=="YEAR")], 
                                                "1892" = pop_data_dist_bul[pop_data_dist_bul$YEAR == 1892, !(colnames(pop_data_dist_bul)=="YEAR")],
                                                "1905" = pop_data_dist_bul[pop_data_dist_bul$YEAR == 1905, !(colnames(pop_data_dist_bul)=="YEAR")],
                                                "1934" = pop_data_dist_bul[pop_data_dist_bul$YEAR == 1934, !(colnames(pop_data_dist_bul)=="YEAR")],
                                                "1956" = pop_data_dist_bul[pop_data_dist_bul$YEAR == 1956, !(colnames(pop_data_dist_bul)=="YEAR")]),
                                   "TUR" = list("1935" = pop_data_dist_tur[pop_data_dist_tur$YEAR == 1935, !(colnames(pop_data_dist_tur)=="YEAR")],
                                                "1950" = pop_data_dist_tur[pop_data_dist_tur$YEAR == 1950, !(colnames(pop_data_dist_tur)=="YEAR")],
                                                "1970" = pop_data_dist_tur[pop_data_dist_tur$YEAR == 1970, !(colnames(pop_data_dist_tur)=="YEAR")],
                                                "1985" = pop_data_dist_tur[pop_data_dist_tur$YEAR == 1985, !(colnames(pop_data_dist_tur)=="YEAR")],
                                                "1990" = pop_data_dist_tur[pop_data_dist_tur$YEAR == 1990, !(colnames(pop_data_dist_tur)=="YEAR")],
                                                "2000" = pop_data_dist_tur[pop_data_dist_tur$YEAR == 2000, !(colnames(pop_data_dist_tur)=="YEAR")]))
  

  # load  Bulgaria's all level population data for other years
  pop_data_subdist_bul <- read.csv(file = "./data/population_data/population_data_subdistrict_BUL.csv")
  
  # load Turkey's subdistrict level population data
  pop_data_subdist_tur <- read.csv(file = "./data/population_data/population_data_subdistrict_TUR.csv")
  
  # bring together subdistrict level population data of all countries
  all_population_subdist_data <- list("BUL" = list("1881/1885" = pop_data_subdist_bul[pop_data_subdist_bul$YEAR == 1881, !(colnames(pop_data_subdist_bul)=="YEAR")],
                                                   "1892" = pop_data_subdist_bul[pop_data_subdist_bul$YEAR == 1892, !(colnames(pop_data_subdist_bul)=="YEAR")],
                                                   "1905" = pop_data_subdist_bul[pop_data_subdist_bul$YEAR == 1905, !(colnames(pop_data_subdist_bul)=="YEAR")],
                                                   "1934" = pop_data_subdist_bul[pop_data_subdist_bul$YEAR == 1934, !(colnames(pop_data_subdist_bul)=="YEAR")],
                                                   "1956" = pop_data_subdist_bul[pop_data_subdist_bul$YEAR == 1956, !(colnames(pop_data_subdist_bul)=="YEAR")]),
                                      "TUR" = list("1970" = pop_data_subdist_tur[pop_data_subdist_tur$YEAR == 1970, !(colnames(pop_data_subdist_tur)=="YEAR")],
                                                   "1990" = pop_data_subdist_tur[pop_data_subdist_tur$YEAR == 1990, !(colnames(pop_data_subdist_tur)=="YEAR")]
                                      ))
  
  # load Ottoman Empire's and Turkey's point level population data
  pop_data_point_bul <- read.csv(file =  "./data/population_data/population_data_point_OE.csv", check.names = FALSE)
  pop_data_point_tur <- read.csv(file = "./data/population_data/population_data_point_TUR.csv")
  
  # combine all point data of all countries 
  all_population_point_data <- list("OE" = list("1840s" = pop_data_point_bul),
                                    "TUR" = list("1990"= pop_data_point_tur[pop_data_point_tur == 1990,!(colnames(pop_data_point_tur)=="YEAR")]))
  
  # combine all district, subdistrict and point level population data of all countries
  all_population_data <- list("dist" = all_population_dist_data, "subdist" = all_population_subdist_data, "point" = all_population_point_data)
  
  # load subdistrict level agricultural data of Turkey and Bulgaria
  agri_data_subdist_tur <- read.csv(file = "data/agriculture_data/agri_data_1927_TUR.csv")
  agri_data_subdist_bul <- read.csv(file = "data/agriculture_data/agri_data_1897_BUL.csv")
  
  # combine the agricultural data of all countries with respect to district, subdistrict and point level
  all_agricultural_dist_data <- list("BUL" = list("1897" = agri_data_subdist_bul), "TUR" = list("1927" = agri_data_subdist_tur))
  all_agricultural_subdist_data <- list("BUL" = list("1897" = agri_data_subdist_bul), "TUR" = list("1927" = agri_data_subdist_tur))
  all_agricultural_point_data <- list()
  
  # combine all level agricultural data
  all_agricultural_data <- list("dist" = all_agricultural_dist_data, "subdist" = all_agricultural_subdist_data, "point" = all_agricultural_point_data)
  
  save(all_index, all_maps, all_occupational_data, all_population_data, all_agricultural_data, file = "data/all_index_map_census_data.RData")
}


point_data_gender_names <- c("Female", "Male", "Both")
polygon_gender_names <- c("Female", "Male", "Both" = "TOTAL")
pop_gender_names <- list("point" = point_data_gender_names, "dist" = polygon_gender_names, "subdist" = polygon_gender_names)

gender_names <- c("Female" = "Female", "Male" = "Male", "Both" = "Both")
default_gender_names <- gender_names[3]

pstv <-  readxl::read_xlsx("./data/occupation_data/pstv.xlsx")
occupation_groups <- as.list(pstv[1:148, c("PSTall", "PSTI minor sub-sector with code")])
occupation_sectors <- c("Primary" = 1, "Secondary" = 2, "Tertiary" = 3)
default_occupation_sectors <- occupation_sectors[1]

occupation_group_names <- occupation_groups$`PSTI minor sub-sector with code`
occupation_group_codes <- occupation_groups$PSTall
names(occupation_group_codes) <- occupation_group_names

sector_index <- substring(occupation_group_codes, 1, 1)

primary_sector <- as.list(sub(pattern = "^.*?,", "", occupation_group_codes)[which(sector_index == 1)], all.names = TRUE)
secondary_sector <- as.list(sub(pattern = "^.*?,", "", occupation_group_codes)[which(sector_index == 2)], all.names = TRUE)
tertiary_sector <- as.list(sub(pattern = "^.*?,", "", occupation_group_codes)[which(sector_index == 3)], all.names = TRUE)
unstated <- as.list(sub(pattern = "^.*?,", "", occupation_group_codes)[which(sector_index == 9)], all.names = TRUE)
unspecified <- as.list(sub(pattern = "^.*?,", "", occupation_group_codes)[which(sector_index == 8)], all.names = TRUE)
occupation_filter <- sub(pattern = "^.*?,", "", occupation_group_codes)
working_pop_filter <- occupation_filter[substring(occupation_filter, 1, 2) != "99"]

default_occupation_groups <- ""

er_names <- c("Muslim" = "muslim", "Non-Muslim" = "non_muslim", "All" = "total")
default_er_names <- er_names[3]

#Blue, Green, Gray, Orange, Purple, Red
map_min_color <- c("#4292C6" = "#C6DBEF",
                   "#41AB5D" = "#C7E9C0",
                   "#737373" = "#D9D9D9",
                   "#F16913" = "#FDD0A2",
                   "#807DBA" = "#DADAEB",
                   "#EF3B2C" = "#FCBBA1")
map_max_color <- c("#4292C6" = "#08306B", 
                   "#41AB5D" = "#00441B", 
                   "#737373" = "#000000", 
                   "#F16913" = "#7F2704", 
                   "#807DBA" = "#3F007D", 
                   "#EF3B2C" = "#67000D")
map_colors <- list()
for (color_mode in names(map_min_color)) {
  map_colors[[color_mode]] <- colorRampPalette(colors = c(map_min_color[color_mode], map_max_color[color_mode]))(101) 
}

bar_plot_colors <- c("#4292C6" = "#4292C6",
                     "#41AB5D" = "#41AB5D",
                     "#737373" = "#737373",
                     "#F16913" = "#F16913",
                     "#807DBA" = "#807DBA",
                     "#EF3B2C" = "#EF3B2C")
er_colors <- list("Orthodox Christian" = "#e41a1c", "Muslim" = "#4daf4a",  "Armenien" = "#984ea3", "Jew" = "#ffff33", "Population" = "#377eb8")

default_household <- c()
default_gender <- "Both"

country_codes <- list("OE" ="The Ottoman Empire", "TUR" = "Turkey" , "BUL" = "Bulgaria")

