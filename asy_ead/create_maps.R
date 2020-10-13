library(sf) # note sf should be loaded before dplyr/tidyverse, or it seems to give error regarding group_map()
library(tidyverse)
library(lubridate)
library(flextable)
library(officer)
library(fs)
library(janitor)
library(scales)
library(testthat)
library(extrafont)
library(magick)
library(pdftools)
library(rgeos)
library(maptools)
library(rgdal)
library(viridis)
library(rmapshaper)
library(broom)
library(ggalt)
library(forcats)
library(devEMF)
library(cowplot)
library(RColorBrewer)


# setwd
# setwd("X:/Asylum_EAD_study")
setwd("C:/Users/sjdevine/Work Folders/Desktop/asylum_ead")

options(scipen=999)


# load validate_anumbers function
source("code/helper_scripts/validate_anumbers.R")

# load get_invalid_anumbers function
source("code/helper_scripts/get_invalid_anumbers.R")

# load as_percent()
source("code/helper_scripts/as_percent.R")

options(scipen=999)


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# load data
dir_ls("data")
data <- read_csv("data/joined_data_20200408.csv", col_types = cols(reopen_date = col_date())) 

# inspect
data %>% glimpse()
data %>% nrow() # 672403
data %>% distinct(anumber) %>% nrow() # 672403


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create world_map ####

# load natural earth maps
# see sf_scratchpad.R on github and in sf folder

# world shapefiles from naturalearth - this is the best option for world maps
# https://hub.arcgis.com/datasets/a21fdb46d23e4ef896f31475217cbb08_1
# https://www.programmableweb.com/news/how-to-access-any-restful-api-using-r-language/how-to/2017/07/21

# download zip file of shapefiles
# had to download manually - got 403 Forbidden error when trying to download via R
url <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_sovereignty.zip"
# temp <- tempfile(fileext = ".zip")
# download.file(url, temp)
# unzip(temp, exdir = "world_shapefiles_arcgis_uia")

# read in shapefile as sf
# using epsg: 4326 by default
world_ne_sovereign_sp <- readOGR(dsn = "./data/maps/world_shapefiles_naturalearth/sovereignty", layer = "ne_50m_admin_0_sovereignty")
# head(world_ne_sovereign_sp)
# glimpse(world_ne_sovereign_sp)
# class(world_ne_sovereign_sp)
# world_ne_sovereign_sp 

# get data from shapefile
world_ne_sovereign_sp_data <- world_ne_sovereign_sp@data
glimpse(world_ne_sovereign_sp_data)
class(world_ne_sovereign_sp_data)
world_ne_sovereign_sp_data %>% distinct(NE_ID) %>% nrow()

# use tidy from broom to convert shapefile into a df (tidy is the replacement for deprecated fortify function)
# note that tidy drops other non-map variables in shapefile, and assigns "id" var starting with 0, in order of regions in shapefile
world_ne_sovereign <- tidy(world_ne_sovereign_sp)
head(world_ne_sovereign)
glimpse(world_ne_sovereign)
class(world_ne_sovereign)

# add back the non-map variables from the shapefile that were dropped when running tidy function
# add id variable to sp_data to use for join with tidy sp_data (relying on fact that tidy assigns "id" var in order of regions in shapefile)
# https://stackoverflow.com/questions/40576457/keep-region-names-when-tidying-a-map-using-broom-package
world_ne_sovereign_sp_data <- world_ne_sovereign_sp_data %>% mutate(id = as.character(seq(from = 0, to = nrow(.) - 1, by = 1)))
glimpse(world_ne_sovereign_sp_data)

# join sp_data with tidy sp data by id
world_ne_sovereign <- world_ne_sovereign %>% left_join(., world_ne_sovereign_sp_data, by = "id")
glimpse(world_ne_sovereign)


#/////////////////////////////////////////////////////////////////////////////////////////////////////


# clean cigp country names to match natural earth world map country names

# inspect world_ne country names
world_ne_sovereign %>% glimpse()
world_ne_sovereign %>% distinct(TYPE, SOVEREIGNT, SOV_A3, NAME, NAME_LONG, NAME_EN, FORMAL_EN, ABBREV, BRK_NAME, BRK_A3, GEOUNIT, GU_A3, SUBUNIT, SU_A3)
world_ne_sovereign %>% distinct(TYPE, SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% arrange(NAME)
world_ne_sovereign %>% distinct(TYPE, NAME) %>% count(TYPE)

# inspect asylum country names
data %>% distinct(citizenship_output_country_name)

# inspect differences
data %>% count(citizenship_output_country_name) %>% 
        anti_join(., world_ne_sovereign %>% distinct(SOVEREIGNT) %>% mutate(SOVEREIGNT = as.character(SOVEREIGNT)), 
                                                            by = c("citizenship_output_country_name" = "SOVEREIGNT")) %>% print(n = nrow(.))
world_ne_sovereign %>% distinct(SOVEREIGNT) %>% arrange(SOVEREIGNT) %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("^B", ignore_case = TRUE)))

world_ne_sovereign %>% distinct(SOVEREIGNT) %>% 
        anti_join(., data %>% count(citizenship_output_country_name),
                                        by = c("SOVEREIGNT" = "citizenship_output_country_name")) %>% arrange(SOVEREIGNT) %>% print(n = nrow(.))

# note bermuda is not in ne
# bermuda will be mapped to britain
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("bermuda", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("bermuda", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("bermuda", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("bermuda", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("bermuda", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("bermuda", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("bermuda", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("bermuda", ignore_case = TRUE)))
cigp %>% filter(str_detect(string = country_name_at_program_entry, pattern = regex("Bermuda", ignore_case = TRUE))) %>% count(country_name_at_program_entry) # 2

# note aruba is not in ne
# aruba will be mapped to netherlands
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("aruba", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("aruba", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("aruba", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("aruba", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("aruba", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("aruba", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("aruba", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("aruba", ignore_case = TRUE)))
cigp %>% filter(country_name_at_program_entry == "Aruba") %>% nrow() # 2

# note american samoa is not in ne
# american samoa will be mapped to samoa
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("samoa", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("samoa", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("samoa", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("samoa", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("samoa", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("samoa", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("samoa", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("samoa", ignore_case = TRUE)))
cigp %>% filter(str_detect(string = country_name_at_program_entry, pattern = regex("samoa", ignore_case = TRUE))) %>% count(country_name_at_program_entry) # 1

# note bahamas is listed as "The Bahamas" in ne
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("bahamas", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("bahamas", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("bahamas", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("bahamas", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("bahamas", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("bahamas", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("bahamas", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("bahamas", ignore_case = TRUE)))

# note burma is listed in ne as myanmar
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("burma|myanmar", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("burma|myanmar", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("burma|myanmar", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("burma|myanmar", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("burma|myanmar", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("burma|myanmar", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("burma|myanmar", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("burma|myanmar", ignore_case = TRUE)))

# note cayman islands are not listed in ne
# note cayman islands will be mapped to britain
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("cayman", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("cayman", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("cayman", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("cayman", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("cayman", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("cayman", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("cayman", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("cayman", ignore_case = TRUE)))
cigp %>% filter(str_detect(string = country_name_at_program_entry, pattern = regex("cayman", ignore_case = TRUE))) %>% count(country_name_at_program_entry) # 2
# note china, people's republic is listed in ne as china
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("china", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("china", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("china", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("china", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("china", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("china", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("china", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("china", ignore_case = TRUE)))

# note congo, dem. rep is listed in ne as Democratic Republic of the Congo
# note congo, republic is listed in ne as Republic of the Congo
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("congo", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("congo", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("congo", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("congo", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("congo", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("congo", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("congo", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("congo", ignore_case = TRUE))) %>% select(SOVEREIGNT)

# note cote d'ivoire is listed in ne as ivory coast
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("ivo", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("ivo", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("ivo", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("ivo", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("ivo", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("ivo", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("ivo", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("ivo", ignore_case = TRUE))) 

# note czechoslovakia will be mapped to czech republic in ne
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("czech", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("czech", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("czech", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("czech", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("czech", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("czech", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("czech", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("czech", ignore_case = TRUE)))
cigp %>% count(country_name_at_program_entry) %>% filter(str_detect(string = country_name_at_program_entry, pattern = regex("czec", ignore_case = TRUE)))
cigp %>% count(country_name_at_program_entry) %>% filter(str_detect(string = country_name_at_program_entry, pattern = regex("slovak", ignore_case = TRUE)))
# note czechoslovakia split into czech republic and slovakia, but cigp anumbers will be mapped to czech republic
world_ne_sovereign %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("^slov", ignore_case = TRUE))) %>% count(NAME_EN)

# note kazak will be mapped to the kazak
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("kazak", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("kazak", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("kazak", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("kazak", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("kazak", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("kazak", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("kazak", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("kazak", ignore_case = TRUE)))
cigp %>% count(country_name_at_program_entry) %>% filter(str_detect(string = country_name_at_program_entry, pattern = regex("kazak", ignore_case = TRUE)))

# note hong kong will be mapped to china
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("hong", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("hong", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("hong", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("hong", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("hong", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("hong", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("hong", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("hong", ignore_case = TRUE)))
cigp %>% count(country_name_at_program_entry) %>% filter(str_detect(string = country_name_at_program_entry, pattern = regex("hong", ignore_case = TRUE)))
# note korea, south will be mapped to South Korea
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("korea", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("korea", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("korea", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("korea", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("korea", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("korea", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("korea", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("korea", ignore_case = TRUE)))

# note macau will be mapped to china
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("macau", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("macau", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("macau", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("macau", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("macau", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("macau", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("macau", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("macau", ignore_case = TRUE)))
# note montserrat will be mapped to uk
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("montserrat", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("montserrat", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("montserrat", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("montserrat", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("montserrat", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("montserrat", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("montserrat", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("montserrat", ignore_case = TRUE)))

# note netherlands antilles will be mapped to netherlands; there is no curacao, bonaire, etc
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("antill", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("antill", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("antill", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("antill", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("antill", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("antill", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("antill", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("antill", ignore_case = TRUE)))
world_ne_sovereign %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("^n", ignore_case = TRUE))) %>% count(NAME_EN)
world_ne_sovereign %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("^c", ignore_case = TRUE))) %>% count(NAME_EN)
world_ne_sovereign %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("^b", ignore_case = TRUE))) %>% count(NAME_EN)

# note mariana islands is not listed in ne - 1 record will be omitted
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("mariana", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("mariana", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("mariana", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("mariana", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("mariana", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("mariana", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("mariana", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("mariana", ignore_case = TRUE)))
cigp %>% filter(str_detect(string = country_name_at_program_entry, pattern = regex("mariana", ignore_case = TRUE))) %>% count(country_name_at_program_entry) # 1

# note serbia and montenegro will be mapped to serbia
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("serbia", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("serbia", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("serbia", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("serbia", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("serbia", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("serbia", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("serbia", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("serbia", ignore_case = TRUE)))
cigp %>% filter(str_detect(string = country_name_at_program_entry, pattern = regex("serbia", ignore_case = TRUE))) %>% count(country_name_at_program_entry)
cigp %>% filter(str_detect(string = country_name_at_program_entry, pattern = regex("^m", ignore_case = TRUE))) %>% count(country_name_at_program_entry)
# note S&A split into serbia and montenegro, but cigp anumbers will be mapped to serbia
world_ne_sovereign %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("^m", ignore_case = TRUE))) %>% count(NAME_EN)

# note soviet union will be mapped to russia
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("soviet", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("soviet", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("soviet", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("soviet", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("soviet", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("soviet", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("soviet", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("soviet", ignore_case = TRUE)))
world_ne_sovereign %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("^r", ignore_case = TRUE))) %>% count(NAME_EN)

# note tanzania will be mapped to united republic of tanzania
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("tanzania", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("tanzania", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("tanzania", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("tanzania", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("tanzania", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("tanzania", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("tanzania", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("tanzania", ignore_case = TRUE)))
world_ne_sovereign %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("^r", ignore_case = TRUE))) %>% count(NAME_EN)

# note tuvalu will be mapped to uk
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("tuvalu", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("tuvalu", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("tuvalu", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("tuvalu", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("tuvalu", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("tuvalu", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("tuvalu", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("tuvalu", ignore_case = TRUE)))
world_ne_sovereign %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("^t", ignore_case = TRUE))) %>% count(NAME_EN)

# note virgin islands will be mapped to uk
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("virgin", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("virgin", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("virgin", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("virgin", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("virgin", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("virgin", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("virgin", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("virgin", ignore_case = TRUE)))
world_ne_sovereign %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("^v", ignore_case = TRUE))) %>% count(NAME_EN)

# note sao tome and principe has non-standad characters in ne; will edit it to match "Sao Tome and Principe" in asylum data
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("princip", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("princip", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("princip", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("princip", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("princip", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("princip", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("princip", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("princip", ignore_case = TRUE)))
world_ne_sovereign %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("^p", ignore_case = TRUE))) %>% count(NAME_EN)

# note swaziland
world_ne_sovereign %>% distinct(SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% 
        filter(str_detect(string = SOVEREIGNT, pattern = regex("swa", ignore_case = TRUE)) |
                       str_detect(string = NAME, pattern = regex("swa", ignore_case = TRUE)) |
                       str_detect(string = NAME_LONG, pattern = regex("swa", ignore_case = TRUE)) |
                       str_detect(string = NAME_EN, pattern = regex("swa", ignore_case = TRUE)) |
                       str_detect(string = FORMAL_EN, pattern = regex("swa", ignore_case = TRUE)) |
                       str_detect(string = BRK_NAME, pattern = regex("swa", ignore_case = TRUE)) | 
                       str_detect(string = GEOUNIT, pattern = regex("swa", ignore_case = TRUE)) |
                       str_detect(string = SUBUNIT, pattern = regex("swa", ignore_case = TRUE)))
world_ne_sovereign %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("^v", ignore_case = TRUE))) %>% count(NAME_EN)

# note also that 1712 records with "stateless" citizenship_output_country_name in asylum data will be omitted from map
data %>% filter(citizenship_output_country_name == "Stateless") %>% nrow() # 1712
data %>% filter(is.na(citizenship_output_country_name)) %>% nrow() # 0


#//////////////


# total anumbers omitted because data country was not listed in ne: 2129
# stateless - 1712
# united states - 1
# unknown - 416

# add country_name_ne variable to data to match SOVEREIGNT (NAME_EN is weird sometimes - eg kazakstan, though tanzania/czechia is weird for SOVEREIGNT...)
data <- data %>% mutate(country_name_ne = case_when(citizenship_output_country_name == "American Samoa" ~ "Samoa",
                                            citizenship_output_country_name == "Aruba" ~ "Netherlands",
                                            citizenship_output_country_name == "Bermuda" ~ "United Kingdom",
                                            citizenship_output_country_name == "Bahamas" ~ "The Bahamas",
                                            citizenship_output_country_name == "Burma" ~ "Myanmar",
                                            citizenship_output_country_name == "Cayman Islands" ~ "United Kingdom",
                                            citizenship_output_country_name == "China, People's Republic" ~ "China",
                                            citizenship_output_country_name == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
                                            citizenship_output_country_name == "Congo (Brazzaville)" ~ "Republic of the Congo",
                                            citizenship_output_country_name == "Cote d'Ivoire" ~ "Ivory Coast",
                                            citizenship_output_country_name == "Czechoslovakia (former)" ~ "Czechia",
                                            citizenship_output_country_name == "Hong Kong" ~ "China",
                                            citizenship_output_country_name == "Korea, South" ~ "South Korea",
                                            citizenship_output_country_name == "Macau" ~ "China",
                                            citizenship_output_country_name == "Montserrat" ~ "United Kingdom",
                                            citizenship_output_country_name == "Netherlands Antilles (former)" ~ "Netherlands",
                                            citizenship_output_country_name == "Serbia" ~ "Republic of Serbia",
                                            citizenship_output_country_name == "Serbia and Montenegro (former)" ~ "Republic of Serbia",
                                            citizenship_output_country_name == "Soviet Union (former)" ~ "Russia",
                                            citizenship_output_country_name == "Tanzania" ~ "United Republic of Tanzania",
                                            citizenship_output_country_name == "Tuvalu" ~ "United Kingdom",
                                            citizenship_output_country_name == "Virgin Islands, British" ~ "United Kingdom",
                                            citizenship_output_country_name == "Swaziland" ~ "eSwatini",
                                            TRUE ~ citizenship_output_country_name)) 

# inspect
# note that Sao Tome and Principe has weird characters in world_ne_sovereign, so will edit it there directly to match asylum data
# the other unmatched citizenship_output_country_name are stateless, US, and Unknown
data %>% anti_join(., world_ne_sovereign %>% mutate(SOVEREIGNT = as.character(SOVEREIGNT)) %>% distinct(SOVEREIGNT), 
                   by = c("country_name_ne" = "SOVEREIGNT")) %>%
        count(citizenship_output_country_name, country_name_ne)


#///////////////////////////////////////////////////////////////////////////////////////////


# prepare world_ne_sovereign for mapping

# add citizenship_output_country_name counts to world_ne_sovereign
data %>% count(country_name_ne) %>% arrange(desc(n))
data %>% filter(is.na(country_name_ne)) %>% nrow() # 0
data %>% nrow() # 672403
cigp %>% count(country_name_ne) %>% summarize(sum_participants = sum(n)) # 32540

world_ne_sovereign <- world_ne_sovereign %>% mutate(SOVEREIGNT = as.character(SOVEREIGNT),
                                                    SOVEREIGNT = case_when(BRK_NAME == "Sao Tome and Principe" ~ "Sao Tome and Principe",
                                                                          TRUE ~ SOVEREIGNT)) %>% 
        left_join(., data %>% count(country_name_ne), by = c("SOVEREIGNT" = "country_name_ne")) 

# inspect join
world_ne_sovereign %>% filter(!is.na(n)) %>% distinct(SOVEREIGNT) # 181
world_ne_sovereign %>% filter(n > 0) %>% distinct(SOVEREIGNT, n) # 181
world_ne_sovereign %>% filter(n > 0) %>% distinct(SOVEREIGNT, n) %>% arrange(desc(n))
world_ne_sovereign %>% filter(n > 0) %>% distinct(SOVEREIGNT, n) %>% summarize(sum_participants = sum(n, na.rm = TRUE)) # 670274

# see 5 countries in cigp but not joined in world_ne_sovereign, and the 74 participants from cigp therefore not counted in world_ne_sovereign
data %>% count(country_name_ne) %>% anti_join(., world_ne_sovereign %>% count(SOVEREIGNT), by = c("country_name_ne" = "SOVEREIGNT"))
data %>% count(country_name_ne) %>% anti_join(., world_ne_sovereign %>% count(SOVEREIGNT), by = c("country_name_ne" = "SOVEREIGNT")) %>%
        summarize(participants_not_matched_sum = sum(n)) # 2129
672403 - 2129 == 670274


#/////////////


# inspect cut points
world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% ggplot(data = ., aes(x = n)) + geom_histogram()
world_ne_sovereign %>% filter(n < 20000) %>% distinct(SOVEREIGNT, n) %>% ggplot(data = ., aes(x = n)) + geom_histogram()
world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% arrange(n) %>% print(n = 50)
world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% arrange(desc(n)) %>% print(n = 50)

world_ne_sovereign %>% mutate(interval_cuts = cut_interval(n, n = 5)) %>% distinct(SOVEREIGNT, n, interval_cuts) %>% 
        group_by(interval_cuts) %>% summarize(sum_n = sum(n)) %>% mutate(total = sum(sum_n, na.rm = TRUE))
world_ne_sovereign %>% mutate(number_cuts = cut_number(n, n = 5)) %>% distinct(SOVEREIGNT, n, number_cuts) %>% 
        group_by(number_cuts) %>% summarize(sum_n = sum(n)) %>% mutate(total = sum(sum_n, na.rm = TRUE))
world_ne_sovereign %>% mutate(number_cuts = cut_number(n, n = 5)) %>% distinct(SOVEREIGNT, n, number_cuts) %>% 
        count(number_cuts)


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add fill_color_bin and fill_color
world_ne_sovereign <- world_ne_sovereign %>% 
        mutate(fill_color_bin = case_when(is.na(n) ~ "0_to_1",
                                                         n > 0 & n < 50 ~ "1_to_50",
                                                         n >= 50 & n < 150 ~ "50_to_150",
                                                         n >= 150 & n < 1500 ~ "150_to_1500",
                                                         n >= 1500 & n < 5000 ~ "1500_to_5000",
                                                         n >= 5000 ~ "5000_to_130000"),
                              fill_color = case_when(fill_color_bin == "0_to_1" ~ "#ffffff",
                                                     fill_color_bin == "1_to_50" ~ color_palette %>% slice(3) %>% pull(hex),
                                                     fill_color_bin == "50_to_150" ~ color_palette %>% slice(5) %>% pull(hex),
                                                     fill_color_bin == "150_to_1500" ~ color_palette %>% slice(7) %>% pull(hex),
                                                     fill_color_bin == "1500_to_5000" ~ color_palette %>% slice(8) %>% pull(hex),
                                                     fill_color_bin == "5000_to_130000" ~ color_palette %>% slice(9) %>% pull(hex)))

# inspect
world_ne_sovereign %>% distinct(SOVEREIGNT, fill_color_bin, fill_color) %>% count(fill_color_bin, fill_color)
data %>% count(country_name_ne) %>% arrange(desc(n)) %>% print(n = nrow(.))
world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% filter(is.na(n))        


#///////////


# create fill_color_list for to pass to scale_color_manual
world_fill_color_list <- world_ne_sovereign %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
names(world_fill_color_list) <- world_ne_sovereign %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
world_fill_color_list


#/////////////


# drop pacific countries to crop map 
# note that neither using lat/long or country names works very well as a way to crop, so abandoned this effort
# can still crop using magick, but will attempt to keep everything in ggplot to avoid image quality issues from magick png conversion

# world_ne_sovereign %>% glimpse()
# world_ne_sovereign %>% count(SOVEREIGNT) %>% arrange(desc(n))
# world_ne_sovereign %>% filter(SOVEREIGNT == "United States of America") %>% filter(long == min(long)) %>% 
#         select(SOVEREIGNT, lat, long)
# world_ne_sovereign %>% filter(long < -178) %>% count(SOVEREIGNT)
# world_ne_sovereign %>% count(SOVEREIGNT) %>% arrange(desc(n)) %>% print(n = nrow(.))
# world_ne_sovereign %>% filter(!(SOVEREIGNT %in% c("Nauru", "Palau", "eSwatini", "Federated States of Micronesia",
#                                                   "Vanuatu", "Kiribati", "Solomon Islands")))


#///////////////////////////////////////////////////////////////////////////////////////////


# world map using winkel tripel projection

# geom_sf has difficult time getting winkel tripel
# here are notes on a workaround, but they didn't work for me
# https://github.com/r-spatial/sf/issues/509

# need to use shapefile that is converted to df by broom::tidy, then use bob rudis' ggalt::geom_cartogram and ggalt::coord_proj
# https://rud.is/b/2018/01/18/bitcoin-world-map-bubbles/

# this map also uses naturalearth 50m shapefile, which is better than arcgis
# ggplot2 has map_data function built-in, which in turn gets data from the maps package
# https://www.rdocumentation.org/packages/ggplot2/versions/3.0.0/topics/map_data

# the maps package in turn gets world shapefile from natural earth project (see pg. 34 of docs)
# NE is supported by North American Cartographic Information Society (see NE homepage)
# https://cran.r-project.org/web/packages/maps/maps.pdf

# rnaturalearth has function to fetch ne shapefiles, but you can go direct to website
# https://cran.r-project.org/web/packages/rnaturalearth/vignettes/rnaturalearth.html
# https://www.naturalearthdata.com/downloads/50m-cultural-vectors/

# as can be seen on rnaturalearth github code, their ne_download function is just a wrapper to hit NE zip download urls
# https://github.com/ropensci/rnaturalearthdata/blob/master/data-raw/data_download_script.r
# https://github.com/ropensci/rnaturalearth/blob/master/R/ne_download.r
# https://github.com/ropensci/rnaturalearth/blob/master/R/ne_file_name.r

# note that NE has shapefiles for sovereign states (~197), but also countries (including breakaway provinces, etc ~ 247)
# sovereign states issue passports - more details in "About" section beneath each download option on NE website

# import extra fonts from extrafont package
windowsFonts()
# font_import()
# loadfonts(device = "win")
windowsFonts()

world_map <- world_ne_sovereign %>% filter(NAME != "Antarctica") %>%
        filter(!(SOVEREIGNT %in% c("Nauru", "Palau", "eSwatini", "Federated States of Micronesia",
                                   "Vanuatu", "Kiribati", "Solomon Islands"))) %>%
        ggplot() +
        geom_cartogram(map = world_ne_sovereign, 
                       aes(x = long, y = lat, map_id = id, 
                           fill = factor(fill_color_bin, levels = c("0_to_1", "1_to_50", "50_to_150", "150_to_1500", "1500_to_5000", "5000_to_130000"))), 
                       color = "#a6a6a6", size = 0.05) +
        scale_fill_manual(values = world_fill_color_list, name = " \n ", 
                          labels = c(1, 50, 150, comma(1500), comma(5000), comma(130000))) +
        coord_proj("+proj=wintri") +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), 
              plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                          color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
              legend.position = "right",
              legend.key.size = unit(2, "mm"),
              legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")), 
              legend.title = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + 
        guides(fill = guide_legend(title.hjust = .5, title.vjust = 1.5, reverse = TRUE, keyheight = 1, label.vjust = 1.42))
        
world_map


#///////////////////


# add zero to legend, add legend title, add source
world_map_output <- world_map %>% 
        ggdraw(xlim = c(0, 1.1)) + draw_label("0", x = .984, y = .360, size = 12, fontfamily = "Calibri") +
        draw_label(label = "Number of", x = 1.00, y = .64, size = 12, fontfamily = "Calibri", hjust = .5, vjust = .5) +
        draw_label(label = "I-589 filings", x = 1.00, y = .609, size = 12, fontfamily = "Calibri", hjust = .5, vjust = .5) +
        draw_label(label = "Source: USCIS RAIO; USCIS OP&S", x = .25, y = .29, size = 11, fontfamily = "Calibri", color = "#000000")


#///////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(world_map_output)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 7, height = 7) %>% 
        print(target = "output/charts/world_map.docx")


#/////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////


# create us_state_map ////

setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/sf")

# download zip folder with shapefile for state 
# https://github.com/UrbanInstitute/urbnmapr/blob/master/data-raw/ccdf-map.R
url <- "http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_state_5m.zip"
# temp <- tempfile(fileext = ".zip")
# download.file(url, temp)
# unzip(temp, exdir = "states_tiger_shapefiles_ftp")

# read in shapefile as sf
states_sf <- st_read("data/maps/states_tiger_shapefiles_ftp/cb_2017_us_state_5m.shp")
states_sf
glimpse(states_sf)

# read in shapefile manually downloaded from census tiger web interface (it's 5k resolution, so not very detailed)
# states_sf <- st_read("state_tiger_shapefiles/tl_2017_us_state.shp")
# states_sf
# glimpse(states_sf)

# set coordinate reference system to US National Atlas Equal Area (EPSG 2163)
# when we plot the map at the end, we'll use USA Contiguous Albers Equal Area Conic (EPSG: 102003)
# even though the map will ultimately use 102003, we need to capture hawaii/alaska using 2163 to avoid distortions
# because i think 102003 focuses on projecting conus only
states_sf <- states_sf %>% 
        st_transform(2163) %>%
        identity()


#/////////////////////////////////////////////////////////////////////////#


# transform points for non-conus geographies

# create rotation function for use in transform_states function
rotation = function(a){
        r = a * pi/180 #degrees to radians
        matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 

# create transform_states function
# for shift, format is c(horizontal (more negative is west), vertical (more negative is south))
transform_states <- function(points_sf, shift = c(0, 0), scale = 1, rotate_degrees = 0) {
        
        # get original_crs to make sure the output has same crs
        original_crs <- st_crs(points_sf)
        
        # get geometry for points
        points_transformed_sfc <- st_geometry(points_sf$geometry)
        
        # get initial centroid for points
        points_centroid_sfc <- st_centroid(points_transformed_sfc)
        
        # transform points
        points_transformed_sfc <- (points_transformed_sfc - points_centroid_sfc) * scale * rotation(rotate_degrees) + 
                shift + points_centroid_sfc
        
        # replace geometry of points_sf with the transformed points
        points_sf_transformed <- st_set_geometry(points_sf, points_transformed_sfc)
        
        # ensure original_crs is retained
        points_sf_transformed <- points_sf_transformed %>% st_set_crs(original_crs) %>% st_transform(original_crs)
        
        # return points_sf_transformed
        points_sf_transformed
}


#/////////////////////////////////////////////////////////////////////////////


# transform alaska state
alaska_state <- states_sf %>% filter(NAME == "Alaska") %>%
        # st_crop(c(xmin = -4000000, ymin = 1466024, xmax = -1512211, ymax = 3909687)) %>%
        transform_states(points_sf = ., shift = c(820000, -4730000), scale = .35, rotate_degrees = -50) %>%
        identity()
alaska_state

# alaska_state %>% ggplot() + geom_sf()


#///////////////////////////


# transform hawaii state
hawaii_state <- states_sf %>% filter(NAME == "Hawaii") %>%
        # st_crop(c(xmin = -6000000, ymin = -4000000, xmax = 4000000, ymax = -400000)) %>%
        transform_states(points_sf = ., shift = c(5000000, -1150000), scale = 1, rotate_degrees = -35) %>%
        identity()
hawaii_state

# hawaii_state %>% ggplot() + geom_sf()


#/////////////////////////////////////////////////////////////////////////////////


# combine transformed state geographies
states_sf_transformed <- states_sf %>% filter(!(NAME %in% c("Alaska", "Hawaii"))) %>%
        rbind(alaska_state) %>%
        rbind(hawaii_state) %>%
        identity()

# remove unused geographies
states_sf_transformed <- states_sf_transformed %>% filter(!(NAME %in% c("American Samoa", "Guam", 
                                                                        "Commonwealth of the Northern Mariana Islands", "United States Virgin Islands", "Puerto Rico")))

# states_sf_transformed %>% ggplot() + geom_sf()


#/////////////////////////////////////////////////////////////////////////////////


# join data to states_sf_transformed
data %>% glimpse()
data %>% count(state) %>% arrange(n) %>% print(n = nrow(.))

states_sf_transformed %>% glimpse()
states_sf_transformed <- states_sf_transformed %>% mutate(STUSPS = as.character(STUSPS))
states_sf_transformed <- states_sf_transformed %>% left_join(., data %>% count(state), by = c("STUSPS" = "state"))
states_sf_transformed %>% glimpse()


#///////////////////////////////////////////////////////////////////////////////#


# inspect cut points for fill_color_bin
states_sf_transformed %>% ggplot(data = ., aes(x = n)) + geom_histogram()
states_sf_transformed %>% distinct(STUSPS, n) %>% arrange(desc(n)) %>% print(n = nrow(.))

states_sf_transformed %>% mutate(interval_cuts = cut_interval(n, n = 5)) %>% distinct(STUSPS, n, interval_cuts) %>% 
        group_by(interval_cuts) %>% summarize(sum_n = sum(n)) %>% mutate(total = sum(sum_n, na.rm = TRUE))
states_sf_transformed %>% mutate(number_cuts = cut_number(n, n = 5)) %>% distinct(STUSPS, n, number_cuts) %>% 
        group_by(number_cuts) %>% summarize(sum_n = sum(n)) %>% mutate(total = sum(sum_n, na.rm = TRUE))
states_sf_transformed %>% mutate(number_cuts = cut_number(n, n = 5)) %>% distinct(STUSPS, n, number_cuts) %>% 
        count(number_cuts)


#///////////////////////


# add fill_color_bin and fill_color
# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

states_sf_transformed <- states_sf_transformed %>% mutate(n_for_plotting = ifelse(!is.na(n), comma(n), n),
                                                        fill_color_bin = case_when(is.na(n) ~ "0_to_1",
                                                                                     n > 0 & n < 1000 ~ "1_to_1000",
                                                                                     n >= 1000 & n < 2000 ~ "1000_to_2000",
                                                                                     n >= 2000 & n < 4000 ~ "2000_to_4000",
                                                                                     n >= 4000 & n < 10000 ~ "4000_to_10000",
                                                                                     n >= 10000 ~ "10000_to_160000"),
                                                    fill_color = case_when(fill_color_bin == "0_to_1" ~ "#ffffff",
                                                                           fill_color_bin == "1_to_1000" ~ color_palette %>% slice(3) %>% pull(hex),
                                                                           fill_color_bin == "1000_to_2000" ~ color_palette %>% slice(5) %>% pull(hex),
                                                                           fill_color_bin == "2000_to_4000" ~ color_palette %>% slice(7) %>% pull(hex),
                                                                           fill_color_bin == "4000_to_10000" ~ color_palette %>% slice(8) %>% pull(hex),
                                                                           fill_color_bin == "10000_to_160000" ~ color_palette %>% slice(9) %>% pull(hex)))

# inspect
states_sf_transformed %>% count(n, fill_color_bin, fill_color) %>% arrange(n)
states_sf_transformed %>% count(fill_color_bin, fill_color) 
states_sf_transformed %>% select(NAME, n, n_for_plotting) %>% arrange(NAME) %>% print(n = nrow(.))
states_sf_transformed %>% select(NAME, n) %>% summarize(sum_n = sum(n))
states_sf_transformed %>% filter(n > 0) %>% distinct(NAME) %>% nrow() # 51, all states have at least one i589 residence at filing
data %>% distinct(state) %>% nrow() # 55
data %>% count(state) %>% arrange(desc(n)) %>% print(n = nrow(.)) # note that PR (109), VI (2762), MP(4), and GU (196) are excluded; 3071 total, or 0.5%


#////////////


# create fill_color_list for to pass to scale_color_manual
state_fill_color_list <- c("#ffffff", states_sf_transformed %>% count(fill_color_bin, fill_color) %>% pull(fill_color))
names(state_fill_color_list) <- c("0_to_1", states_sf_transformed %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin))
state_fill_color_list


#//////////////////////////////////////////////////////////////////////////////////////////////


# create states_map
states_map <- states_sf_transformed %>% 
        st_transform(102003) %>%
        mutate(centroid = st_centroid(geometry)) %>%
        mutate(lon = unlist(map(.x = .$centroid, .f = ~ st_coordinates(.x)[[1]])),
               lat = unlist(map(.x = .$centroid, .f = ~ st_coordinates(.x)[[2]]))) %>%
        ggplot() + geom_sf(aes(geometry = geometry, 
                               fill = factor(fill_color_bin, levels = c("0_to_1", "1_to_1000", "1000_to_2000", "2000_to_4000", "4000_to_10000", "10000_to_160000"))), 
                color = "#a6a6a6", size = .125) + 
        coord_sf(crs = st_crs(102003)) +
        scale_fill_manual(values = state_fill_color_list, name = " \n ",
                          labels = c(1, comma(1000), comma(2000), comma(4000), comma(10000), comma(160000)), drop = FALSE) +
        scale_color_manual(values = text_color_list, guide = FALSE) +
        labs(caption = "              Source: USCIS RAIO; USCIS OP&S") +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), legend.position = "right",
              legend.key.size = unit(2, "mm"), 
              legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")), 
              legend.title = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + 
        guides(fill = guide_legend(title.hjust = .5, title.vjust = 1.5, reverse = TRUE, keyheight = 1, label.vjust = 1.42)) 

states_map


#////////////////////


# add zero to legend, add legend title, add source
states_map_output <- states_map %>% 
        ggdraw(xlim = c(0, 1.1)) + draw_label(label = "0", x = .983, y = .362, size = 11, fontfamily = "Calbiri", fontface = "plain", hjust = .5, vjust = .5) +
        draw_label(label = "Number of", x = 1.00, y = .64, size = 12, fontfamily = "Calibri", hjust = .5, vjust = .5) +
        draw_label(label = "I-589 filings", x = 1.00, y = .609, size = 12, fontfamily = "Calibri", hjust = .5, vjust = .5) +
        draw_label(label = "Source: USCIS RAIO; USCIS OP&S", x = .25, y = .24, size = 11, fontfamily = "Calibri", color = "#000000")


#////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(states_map_output)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 7, height = 7) %>% 
        print(target = "output/charts/states_map.docx")





