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


# load validate_anumbers function
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/helper_scripts")
source("validate_anumbers.R")
setwd(current_wd)

# load get_invalid_anumbers function
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/helper_scripts")
source("get_invalid_anumbers.R")
setwd(current_wd)

# load as_percent()
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/helper_scripts")
source("as_percent.R")
setwd(current_wd)

# load add_totals_row()
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/helper_scripts")
source("add_totals_row.R")
setwd(current_wd)

# load save_flextable()
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/helper_scripts")
source("save_flextable.R")
setwd(current_wd)

# load ggplot_center_legend_title function
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/helper_scripts")
source("ggplot_center_legend_title.R")
setwd(current_wd)


options(scipen=999)

# setwd
# setwd("H:/RED/CIGP")
# setwd("C:/users/sjdevine/Work Folders/Desktop/cigp")
setwd("X:/CIGP Eval Study/fy_2015")


##############################################################################


# load cigp
dir_ls("data")
cigp <- read_csv("data/cigp_combined_data_subset_20190610.csv")
cigp %>% glimpse()
cigp %>% validate_anumbers(anumber_var = "a_number")
cigp %>% dim() # 32540
cigp %>% distinct(a_number) %>% nrow() # 32540

cigp %>% map(.x = ., .f = ~ sum(is.na(.x))) %>% enframe() %>% unnest() %>% arrange(value) %>% data.frame()


##################################################################################


# load natural earth maps
# see sf_scratchpad.R on github and in sf folder

# load natuaral earth world map
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/sf")

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
world_ne_sovereign_sp <- readOGR(dsn = "./world_shapefiles_naturalearth/sovereignty", layer = "ne_50m_admin_0_sovereignty")
# head(world_ne_sovereign_sp)
glimpse(world_ne_sovereign_sp)
class(world_ne_sovereign_sp)
world_ne_sovereign_sp 

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

# reset working directory
# setwd("H:/RED/CIGP")
# setwd("C:/users/sjdevine/Work Folders/Desktop/cigp")
setwd("X:/CIGP Eval Study/fy_2015")


######################################################################################################


# clean cigp country names to match natural earth world map country names

# inspect world_ne country names
world_ne_sovereign %>% glimpse()
world_ne_sovereign %>% distinct(TYPE, SOVEREIGNT, SOV_A3, NAME, NAME_LONG, NAME_EN, FORMAL_EN, ABBREV, BRK_NAME, BRK_A3, GEOUNIT, GU_A3, SUBUNIT, SU_A3)
world_ne_sovereign %>% distinct(TYPE, SOVEREIGNT, NAME, NAME_LONG, NAME_EN, FORMAL_EN, BRK_NAME, GEOUNIT, SUBUNIT) %>% arrange(NAME)
world_ne_sovereign %>% distinct(TYPE, NAME) %>% count(TYPE)

# inspect cigp country names
cigp %>% count(country_name_at_program_entry) %>% arrange(country_name_at_program_entry)

# inspect differences
cigp %>% count(country_name_at_program_entry) %>% anti_join(., world_ne_sovereign %>% distinct(SOVEREIGNT), 
                                                            by = c("country_name_at_program_entry" = "SOVEREIGNT")) %>% data.frame()
world_ne_sovereign %>% distinct(SOVEREIGNT) %>% arrange(SOVEREIGNT) %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("^B", ignore_case = TRUE)))

world_ne_sovereign %>% distinct(SOVEREIGNT) %>% anti_join(., cigp %>% count(country_name_at_program_entry),
                                                                by = c("SOVEREIGNT" = "country_name_at_program_entry")) %>% arrange(SOVEREIGNT) %>% data.frame()

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

# inspect
cigp %>% filter(str_detect(string = country_name_at_program_entry, pattern = regex("^c", ignore_case = TRUE))) %>% count(country_name_at_program_entry)
world_ne_sovereign %>% filter(str_detect(string = NAME_EN, pattern = regex("^n", ignore_case = TRUE))) %>% count(NAME_EN)


###############


# total anumbers omitted because cigp country was not listed in ne
# total: 82
# unknown - 33
# NA - 46
# us virgin islands - 1
# northern mariana islands - 1
# united states - 1

# add country_name_ne variable to cigp to match SOVEREIGNT (NAME_EN is weird sometimes - eg kazakstan, though tanzania/czechia is weird for SOVEREIGNT...)
cigp <- cigp %>% mutate(country_name_ne = case_when(country_name_at_program_entry == "American Samoa" ~ "Samoa",
                                            country_name_at_program_entry == "Aruba" ~ "Netherlands",
                                            country_name_at_program_entry == "Bermuda" ~ "United Kingdom",
                                            country_name_at_program_entry == "Bahamas" ~ "The Bahamas",
                                            country_name_at_program_entry == "Burma" ~ "Myanmar",
                                            country_name_at_program_entry == "Cayman Islands" ~ "United Kingdom",
                                            country_name_at_program_entry == "China, People's Republic" ~ "China",
                                            country_name_at_program_entry == "Congo, Democratic Republic" ~ "Democratic Republic of the Congo",
                                            country_name_at_program_entry == "Congo, Republic" ~ "Republic of the Congo",
                                            country_name_at_program_entry == "Cote d'Ivoire" ~ "Ivory Coast",
                                            country_name_at_program_entry == "Czechoslovakia (former)" ~ "Czechia",
                                            country_name_at_program_entry == "Hong Kong" ~ "China",
                                            country_name_at_program_entry == "Korea, South" ~ "South Korea",
                                            country_name_at_program_entry == "Macau" ~ "China",
                                            country_name_at_program_entry == "Montserrat" ~ "United Kingdom",
                                            country_name_at_program_entry == "Netherlands Antilles (former)" ~ "Netherlands",
                                            country_name_at_program_entry == "Serbia and Montenegro (former)" ~ "Republic of Serbia",
                                            country_name_at_program_entry == "Soviet Union (former)" ~ "Russia",
                                            country_name_at_program_entry == "Tanzania" ~ "United Republic of Tanzania",
                                            country_name_at_program_entry == "Tuvalu" ~ "United Kingdom",
                                            country_name_at_program_entry == "Virgin Islands, British" ~ "United Kingdom", 
                                            TRUE ~ country_name_at_program_entry)) 

# inspect
cigp %>% anti_join(., world_ne_sovereign %>% distinct(SOVEREIGNT), by = c("country_name_ne" = "SOVEREIGNT")) %>%
        count(country_name_at_program_entry, country_name_ne)


############################################################################################


# prepare world_ne_sovereign for mapping

# add cigp participant counts to world_ne_sovereign
# note there are 165 countries in cigp based on country_name_ne (though 1 is NA)
# but only 160 countries in world_ne_sovereign
# these are northern mariana, us, unknown, and virgin islands, us, and total 74 participants
cigp %>% nrow() # 32540
cigp %>% count(country_name_ne) # 165
cigp %>% count(country_name_ne) %>% summarize(sum_participants = sum(n)) # 32540

world_ne_sovereign <- world_ne_sovereign %>% mutate(SOVEREIGNT = as.character(SOVEREIGNT)) %>% 
        left_join(., cigp %>% count(country_name_ne), by = c("SOVEREIGNT" = "country_name_ne")) 


# inspect join
world_ne_sovereign %>% filter(!is.na(n)) %>% distinct(SOVEREIGNT) # 160
world_ne_sovereign %>% filter(n > 0) %>% distinct(SOVEREIGNT, n) # 160
world_ne_sovereign %>% filter(n > 0) %>% distinct(SOVEREIGNT, n) %>% arrange(desc(n))
world_ne_sovereign %>% filter(n > 0) %>% distinct(SOVEREIGNT, n) %>% summarize(sum_participants = sum(n, na.rm = TRUE)) # 32466

# see 5 countries in cigp but not joined in world_ne_sovereign, and the 74 participants from cigp therefore not counted in world_ne_sovereign
cigp %>% count(country_name_ne) %>% anti_join(., world_ne_sovereign %>% count(SOVEREIGNT), by = c("country_name_ne" = "SOVEREIGNT"))
cigp %>% count(country_name_ne) %>% anti_join(., world_ne_sovereign %>% count(SOVEREIGNT), by = c("country_name_ne" = "SOVEREIGNT")) %>%
        summarize(participants_not_matched_sum = sum(n))
32466 + 74 == 32540


##############


# add fill_color_bin and fill_color
world_ne_sovereign <- world_ne_sovereign %>% 
        mutate(fill_color_bin = case_when(is.na(n) ~ "0_to_1",
                                                         n > 0 & n < 10 ~ "1_to_10",
                                                         n >= 10 & n < 100 ~ "10_to_100",
                                                         n >= 100 & n < 1000 ~ "100_to_1000",
                                                         n >= 1000 & n < 5000 ~ "1000_to_5000",
                                                         n >= 5000 & n < 10000 ~ "5000_to_10000"),
                              fill_color = case_when(fill_color_bin == "0_to_1" ~ "#ffffff",
                                                     fill_color_bin == "1_to_10" ~ viridis_pal()(5)[1],
                                                     fill_color_bin == "10_to_100" ~ viridis_pal()(5)[2],
                                                     fill_color_bin == "100_to_1000" ~ viridis_pal()(5)[3],
                                                     fill_color_bin == "1000_to_5000" ~ viridis_pal()(5)[4],
                                                     fill_color_bin == "5000_to_10000" ~ viridis_pal()(5)[5]))

# inspect
world_ne_sovereign %>% count(fill_color_bin, fill_color)
world_ne_sovereign %>% count(n, fill_color_bin, fill_color) %>% arrange(n)
world_ne_sovereign %>% count(n, fill_color_bin, fill_color) %>% arrange(desc(n))
        

#############


# create fill_color_list for to pass to scale_color_manual
world_fill_color_list <- world_ne_sovereign %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
names(world_fill_color_list) <- world_ne_sovereign %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
world_fill_color_list


############


# inspect countries with values and those without
cigp %>% count(country_name_ne) %>% arrange(desc(n)) %>% data.frame()
world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% filter(is.na(n)) %>% data.frame()


##############


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


############################################################################################


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

cigp_world_map <- world_ne_sovereign %>% filter(NAME != "Antarctica") %>%
        filter(!(SOVEREIGNT %in% c("Nauru", "Palau", "eSwatini", "Federated States of Micronesia",
                                   "Vanuatu", "Kiribati", "Solomon Islands"))) %>%
        ggplot() +
        geom_cartogram(map = world_ne_sovereign, 
                       aes(x = long, y = lat, map_id = id, fill = fill_color_bin), color = "#a6a6a6", size = 0.05) +
        scale_fill_manual(values = world_fill_color_list, name = "Number\nof\nCAGP\nparticipants\n",
                          labels = c(1, 10, 100, comma(1000), comma(5000), comma(10000))) +
        coord_proj("+proj=wintri") +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), 
              legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 8, family = "Trebuchet MS"),
              legend.text = element_text(size = 8, family = "Trebuchet MS"), 
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + 
        guides(fill = guide_legend(title.hjust = .5, reverse = TRUE, keyheight = 1, label.vjust = 1.1))
        
cigp_world_map

# # call ggplot_center_legend_title
# cigp_world_map_grob <- ggplot_center_legend_title(plot = cigp_world_map)
# grid.newpage()
# grid.draw(cigp_world_map_grob)
# 
# # save map
# ggsave(filename = "output/cigp_world_map.pdf", plot = cigp_world_map_grob, units = "in", dpi = 300, device = cairo_pdf)


#####################


# add zero to legend
cigp_world_map_output <- cigp_world_map %>% ggdraw(xlim = c(0, 1.1)) + draw_label("0", x = .984, y = .353, size = 8)


####################


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename, width = 7, height = 7)
print(cigp_world_map_output)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 7, height = 7) %>% 
        print(target = "output/cigp_world_map.docx")


###################


# # convert pdf to png with magick and pdftools, then crop
# 
# # read pdf
# cigp_world_map_pdf <- pdf_render_page(pdf = "output/cigp_world_map.pdf", page = 1, dpi = 300)
# 
# # convert to magick image
# cigp_world_map_image <- image_read(cigp_world_map_pdf)
# 
# # crop
# image_info(cigp_world_map_image)
# cigp_world_map_image <- image_crop(image = cigp_world_map_image, geometry = geometry_area(width = 1370, height = 750, x_off = 180, y_off = 450))


################


# add 0 tick mark to legend

# # get color_list for manual legend
# starwars2 <- starwars %>% mutate(fill_color = ifelse(species == "Human", "green", "blue"), 
#                                  fill_color_bin = ifelse(species == "Human", "color_green", "color_blue"))
# starwars_color_list <- starwars2 %>% pull(fill_color)
# names(starwars_color_list) <- starwars2 %>% pull(fill_color_bin) 
# starwars_color_list
# 
# # create plot with manual legend
# starwars_plot <- starwars2 %>% ggplot(data = ., aes(x = mass, fill = fill_color_bin)) + geom_bar() +
#         scale_fill_manual(values = starwars_color_list, labels = c(0, 100, 1000)) +
#         theme(legend.text = element_text(size = 12, family = "Trebuchet MS"))
# starwars_plot
# 
# # save plot as png
# ggsave(filename = "starwars_plot.png", plot = starwars_plot, dpi = 300, width = 15)
# 
# # load plot as image
# starwars_plot_image <- image_read(path = "output/starwars_plot.png")
# starwars_plot_image
# image_info(starwars_plot_image)
# 
# # crop image to just the zero
# zero_image <- image_crop(image = starwars_plot_image, geometry = geometry_area(width = 50, height = 50, x_off = 4280, y_off = 750))
# zero_image
# image_info(zero_image)
# 
# # save cropped_image
# image_write(image = zero_image, path = "output/zero_image.png", format = 'png', density = '300x300')
# 
# # load zero_image
# zero_image <- image_read(path = "output/zero_image.png")
# 
# # load map as image and add zero to legend
# image_info(cigp_world_map_image)
# cigp_world_map_image <- image_composite(image = cigp_world_map_image, composite_image = image_resize(image = zero_image, 
#                                                 geometry = geometry_size_pixels(width = 17, height = 17)), offset = geometry_point(x = 1242, y = 647))
# cigp_world_map_image
# 
# # save cigp_world_map_image
# image_write(image = cigp_world_map_image, path = "output/cigp_world_map.png", format = "png", density = "300x300")


###########################################################################################
###########################################################################################


# create country_bar_chart

# inspect distribution of country_counts by participant_count values
world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% filter(!is.na(n)) %>% arrange(n)
world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% filter(!is.na(n)) %>% arrange(desc(n))
world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% filter(n < 5) %>% nrow() # 23
world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% filter(!is.na(n), n < 2000) %>% ggplot(data = ., aes(x = n)) + geom_histogram(binwidth = 1)
world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% filter(!is.na(n)) %>% ggplot(data = ., aes(x = n)) + stat_ecdf()

world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% rename(participant_count = n) %>% filter(!is.na(participant_count)) %>% 
        count(participant_count) %>% arrange(participant_count) %>%
        rename(country_count = n) %>% mutate(cum_sum_country_count = cumsum(country_count),
                                             cum_pct_country_count = cum_sum_country_count / sum(country_count)) %>% data.frame()


######################


# create country_counts with all countries w < 100 participants lumped in other category
country_counts_large <- world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% rename(participant_count = n) %>% filter(!is.na(participant_count)) %>% 
        filter(participant_count >= 150)
country_counts_large

country_counts_small <- world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% rename(participant_count = n) %>% filter(!is.na(participant_count)) %>%
        filter(participant_count < 150) %>% mutate(SOVEREIGNT = "Other countries", participant_count = sum(participant_count)) %>% slice(1)
country_counts_small

country_counts <- country_counts_large %>% bind_rows(., country_counts_small) %>%
        mutate(SOVEREIGNT = case_when(SOVEREIGNT == "Democratic Republic of the Congo" ~ "Dem. Rep. of the Congo", 
                                      SOVEREIGNT == "Dominican Republic" ~ "Dominican Rep.", 
                                      SOVEREIGNT == "Myanmar" ~ "Burma", TRUE ~ SOVEREIGNT))

# test that counts sum to cigp participants
# note that 74 anumbers from either NA country, or US, or country without map polygon will be lumped into Other
# note there are 165 countries in cigp based on country_name_ne (though 1 is NA)
# but only 160 countries in world_ne_sovereign
# these are northern mariana, us, unknown, and virgin islands, us, and total 74 participants
country_counts %>% summarize(sum_participant_count = sum(participant_count))
world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% summarize(sum_n = sum(n, na.rm = TRUE))

cigp %>% nrow()
cigp %>% anti_join(., world_ne_sovereign %>% distinct(SOVEREIGNT), by = c("country_name_ne" = "SOVEREIGNT")) %>%
        count(country_name_at_program_entry, country_name_ne)
cigp %>% anti_join(., world_ne_sovereign %>% distinct(SOVEREIGNT), by = c("country_name_ne" = "SOVEREIGNT")) %>%
        count(country_name_at_program_entry, country_name_ne) %>% summarize(sum = sum(n))

# get count_unmapped_anumbers
count_unmapped_anumbers <- cigp %>% anti_join(., world_ne_sovereign %>% distinct(SOVEREIGNT), by = c("country_name_ne" = "SOVEREIGNT")) %>%
        count(country_name_at_program_entry, country_name_ne) %>% summarize(sum_n = sum(n)) %>% pull(sum_n)
count_unmapped_anumbers
32540 - 74 == 32466

# update country_counts manually to add 74 anumbers from either NA country, or US, or country without map polygon
country_counts <- country_counts %>% 
        mutate(participant_count = case_when(SOVEREIGNT == "Other countries" ~ (participant_count + count_unmapped_anumbers),
                                             TRUE ~ participant_count))
country_counts %>% filter(SOVEREIGNT == "Other countries")
3878 + 74 == 3952

# test
test_that(desc = "ensure country_counts total used for country_bar_chart matchs total cigp anumbers", {
        expect_equal(country_counts %>% summarize(sum_participant_count = sum(participant_count)) %>% pull(sum_participant_count),
                     expected = cigp %>% nrow())
})

# relevel SOVEREIGNT in country_counts so that Other Countries is first, then ascending order
country_counts <- country_counts %>% mutate(SOVEREIGNT = fct_reorder(.f = factor(SOVEREIGNT), .x = participant_count)) %>%
        mutate(SOVEREIGNT = fct_relevel(.f = SOVEREIGNT, "Other countries", after = 0))


###################


# add fill_color_bin and fill_color
country_counts <- country_counts %>% rename(n = participant_count) %>% mutate(fill_color_bin = case_when(is.na(n) ~ "0_to_1",
                       n > 0 & n < 10 ~ "1_to_10",
                       n >= 10 & n < 100 ~ "10_to_100",
                       n >= 100 & n < 1000 ~ "100_to_1000",
                       n >= 1000 & n < 5000 ~ "1000_to_5000",
                       n >= 5000 & n < 10000 ~ "5000_to_10000"),
            fill_color = case_when(fill_color_bin == "0_to_1" ~ "#ffffff",
                                   fill_color_bin == "1_to_10" ~ viridis_pal()(5)[1],
                                   fill_color_bin == "10_to_100" ~ viridis_pal()(5)[2],
                                   fill_color_bin == "100_to_1000" ~ viridis_pal()(5)[3],
                                   fill_color_bin == "1000_to_5000" ~ viridis_pal()(5)[4],
                                   fill_color_bin == "5000_to_10000" ~ viridis_pal()(5)[5]))

# inspect
country_counts %>% count(fill_color_bin, fill_color)
country_counts %>% count(n, fill_color_bin, fill_color) %>% arrange(n)
country_counts %>% count(n, fill_color_bin, fill_color) %>% arrange(desc(n))


###################


# create country_bar_chart_fill_color_list for to pass to scale_color_manual
country_bar_chart_fill_color_list <- country_counts %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
names(country_bar_chart_fill_color_list) <- country_counts %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
country_bar_chart_fill_color_list


####################


# create bar chart
country_bar_chart <- country_counts %>%
        ggplot(data = ., aes(x = SOVEREIGNT, y = n, fill = fill_color_bin)) + geom_col(width = .5) + 
        scale_fill_manual(values = country_bar_chart_fill_color_list) +
        scale_y_continuous(labels = comma, expand = expand_scale(mult = 0, add = c(0, 400))) +
        # labs(y = "Number of\nCAGP participants") +
        geom_text(aes(label = comma(n), y = n), vjust = 0, 
                  size = 2, # size = 2.7,
                  nudge_y = case_when(country_counts$SOVEREIGNT == "Colombia" ~ 200,
                                      country_counts$SOVEREIGNT == "El Salvador" ~ 700,
                                      country_counts$SOVEREIGNT == "Vietnam" ~ 100,
                                      country_counts$SOVEREIGNT == "Burma" ~ 600,
                                      country_counts$SOVEREIGNT == "Bhutan" ~ 100,
                                      country_counts$SOVEREIGNT == "Dominican Rep." ~ 400,
                                      country_counts$SOVEREIGNT == "Iraq" ~ 500,
                                      TRUE ~ 100)) +
        theme(
                panel.grid.major = element_line(color = "transparent"),
                    plot.background = element_blank(),
                    panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              axis.title.y = element_blank(),
                    # axis.title.y = element_text(family = "Trebuchet MS", size = 10, color = "black", hjust = 0),
                axis.text.y = element_blank(),
                    axis.text.x = element_text(angle = -45, hjust = 0, family = "Trebuchet MS", size = 10, color = "black"), 
              axis.title.x = element_blank(),
              plot.margin = margin(0, 10, 20, 25),
                    plot.title = element_blank(),
              legend.position = "none") 

country_bar_chart

# save
# ggsave(filename = "output/country_bar_chart.pdf", plot = country_bar_chart, units = "in", dpi = 300, device = cairo_pdf, height = 3, width = 10)


#####################


# add footnote and y-axis label
country_bar_chart_output <- country_bar_chart %>% ggdraw() + 
        draw_label("Source: USCIS Lawful Permanent Resident data; CAGP data.", x = .28, y = .02, size = 10,
                   fontfamily = "Trebuchet MS", color = "#808080") +
        draw_label("Number of\nCAGP participants", x = .02, y = .7, size = 10,
                   fontfamily = "Trebuchet MS", angle = 90)


#####################


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename, width = 7, height = 2.8)
print(country_bar_chart_output)
dev.off()

# add emf to word doc - will manually crop map in word doc
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>%
        body_add_img(src = filename, width = 7, height = 2.8) %>%
        print(target = "output/country_bar_chart.docx")


##########


# # add source footnote
# 
# # convert to png
# country_bar_chart_pdf <- pdf_render_page(pdf = "output/country_bar_chart.pdf", page = 1, dpi = 300)
# country_bar_chart_image <- image_read(country_bar_chart_pdf)
# image_info(country_bar_chart_image)
# 
# # save country_bar_chart_source_plot as pdf
# country_bar_chart_source_plot <- starwars %>% ggplot(data = ., aes(x = mass)) + geom_histogram() +
#         ggtitle("Source: USCIS Lawful Permanent Resident data; CAGP data.") +
#         theme(plot.title = element_text(size = 12, hjust = 0, family = "Trebuchet MS", color = "#808080"))
# ggsave(filename = "output/country_bar_chart_source_plot.pdf", plot = country_bar_chart_source_plot, units = "in", dpi = 300, device = cairo_pdf)
# 
# # load country_bar_chart_source_plot as image
# country_bar_chart_source_pdf <- pdf_render_page(pdf = "output/country_bar_chart_source_plot.pdf", page = 1, dpi = 300)
# 
# # convert pdf to image
# country_bar_chart_source_image <- image_read(country_bar_chart_source_pdf)
# 
# # crop out country_bar_chart_source_image
# image_info(country_bar_chart_source_image)
# country_bar_chart_source_image <- image_crop(image = country_bar_chart_source_image, 
#                                              geometry = geometry_area(width = 1500, height = 80, x_off = 130, y_off = 0))
# country_bar_chart_source_image
# 
# # overlay source_footnote_image
# image_info(country_bar_chart_image)
# country_bar_chart_image <- image_composite(image = country_bar_chart_image, 
#                                            composite_image = image_resize(image = country_bar_chart_source_image,
#                                 geometry = geometry_size_pixels(width = 950, height = 300)), offset = geometry_point(x = 0, y = 850))
# country_bar_chart_image
# 
# # save as png
# image_write(image = country_bar_chart_image, path = "output/country_bar_chart.png", format = 'png', density = '300x300')


###########################################################################################
###########################################################################################
###########################################################################################


# create us_state_map

setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/sf")

# download zip folder with shapefile for state 
# https://github.com/UrbanInstitute/urbnmapr/blob/master/data-raw/ccdf-map.R
url <- "http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_state_5m.zip"
# temp <- tempfile(fileext = ".zip")
# download.file(url, temp)
# unzip(temp, exdir = "states_tiger_shapefiles_ftp")

# read in shapefile as sf
states_sf <- st_read("states_tiger_shapefiles_ftp/cb_2017_us_state_5m.shp")
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


###########################################################################


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


###############################################################################


# transform alaska state
alaska_state <- states_sf %>% filter(NAME == "Alaska") %>%
        # st_crop(c(xmin = -4000000, ymin = 1466024, xmax = -1512211, ymax = 3909687)) %>%
        transform_states(points_sf = ., shift = c(820000, -4730000), scale = .35, rotate_degrees = -50) %>%
        identity()
alaska_state

# alaska_state %>% ggplot() + geom_sf()


#############################


# transform hawaii state
hawaii_state <- states_sf %>% filter(NAME == "Hawaii") %>%
        # st_crop(c(xmin = -6000000, ymin = -4000000, xmax = 4000000, ymax = -400000)) %>%
        transform_states(points_sf = ., shift = c(5000000, -1150000), scale = 1, rotate_degrees = -35) %>%
        identity()
hawaii_state

# hawaii_state %>% ggplot() + geom_sf()


##################################################################################


# combine transformed state geographies
states_sf_transformed <- states_sf %>% filter(!(NAME %in% c("Alaska", "Hawaii"))) %>%
        rbind(alaska_state) %>%
        rbind(hawaii_state) %>%
        identity()

# remove unused geographies
states_sf_transformed <- states_sf_transformed %>% filter(!(NAME %in% c("American Samoa", "Guam", 
                                                                        "Commonwealth of the Northern Mariana Islands", "United States Virgin Islands", "Puerto Rico")))

# states_sf_transformed %>% ggplot() + geom_sf()


##################################################################################


# join cigp to states_sf_transformed
cigp %>% glimpse()
cigp %>% count(grantee_state) %>% arrange(n) %>% data.frame()

states_sf_transformed %>% glimpse()
states_sf_transformed <- states_sf_transformed %>% mutate(STUSPS = as.character(STUSPS))
states_sf_transformed <- states_sf_transformed %>% left_join(., cigp %>% count(grantee_state), by = c("STUSPS" = "grantee_state"))
states_sf_transformed %>% glimpse()


#################################################################################


# add fill_color_bin and fill_color
states_sf_transformed %>% ggplot(data = ., aes(x = n)) + geom_histogram()

states_sf_transformed <- states_sf_transformed %>% mutate(n_for_plotting = ifelse(!is.na(n), comma(n), n),
                                                        fill_color_bin = case_when(is.na(n) ~ "c1_0_to_1",
                                                                                     n > 0 & n < 200 ~ "c2_1_to_200",
                                                                                     n >= 200 & n < 400 ~ "c3_200_to_400",
                                                                                     n >= 400 & n < 600 ~ "c4_400_to_600",
                                                                                     n >= 600 & n < 800 ~ "c5_600_to_800",
                                                                                     n >= 800 & n < 1000 ~ "c6_800_to_1000",
                                                                               n >= 1000 & n < 2000 ~ "c7_1000_to_2000",
                                                                               n >= 2000 & n < 5000 ~ "c8_2000_to_5000"),
                                                    fill_color = case_when(fill_color_bin == "c1_0_to_1" ~ "#ffffff",
                                                                           fill_color_bin == "c2_1_to_200" ~ viridis_pal()(7)[1],
                                                                           fill_color_bin == "c3_200_to_400" ~ viridis_pal()(7)[2],
                                                                           fill_color_bin == "c4_400_to_600" ~ viridis_pal()(7)[3],
                                                                           fill_color_bin == "c5_600_to_800" ~ viridis_pal()(7)[4],
                                                                           fill_color_bin == "c6_800_to_1000" ~ viridis_pal()(7)[5],
                                                                           fill_color_bin == "c7_1000_to_2000" ~ viridis_pal()(7)[6],
                                                                           fill_color_bin == "c8_2000_to_5000" ~ viridis_pal()(7)[7]))

# inspect
states_sf_transformed %>% count(n, fill_color_bin, fill_color) %>% arrange(n)
states_sf_transformed %>% count(fill_color_bin, fill_color) %>% arrange(fill_color_bin)
states_sf_transformed %>% count(fill_color_bin, fill_color) %>% arrange(desc(fill_color_bin))
states_sf_transformed %>% select(NAME, n, n_for_plotting) %>% arrange(NAME) %>% data.frame()


#############


# create fill_color_list for to pass to scale_color_manual
state_fill_color_list <- states_sf_transformed %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
names(state_fill_color_list) <- states_sf_transformed %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
state_fill_color_list


##############


# create text_color_list
text_color_list <- c("#000000", "#ffffff", "#ffffff", "#ffffff", "#ffffff", "#000000", "#000000", "#000000")
names(text_color_list) <- states_sf_transformed %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
text_color_list


##############


# create geom_segment_tbl for line segment pointing out geom_text for east coast states
# note will get warning about st_centroid assumptions - no problem
geom_segment_list <- states_sf_transformed %>% filter(NAME %in% c("New Jersey", "District of Columbia", "Maryland", "Rhode Island", "Connecticut")) %>% 
                                         st_transform(102003) %>% st_centroid(geometry) %>% pull(geometry) %>% unlist()
geom_segment_list

geom_segment_tbl_id <- states_sf_transformed %>% filter(NAME %in% c("New Jersey", "District of Columbia", "Maryland", "Rhode Island", "Connecticut")) %>% 
        st_transform(102003) %>% st_centroid(geometry) %>% mutate(NAME = as.character(NAME)) %>% pull(NAME)
geom_segment_tbl_id

geom_segment_tbl <- tibble(NAME = geom_segment_tbl_id,
        centroid_x = geom_segment_list [seq(from = 1, to = length(geom_segment_list), by = 2)],
        centroid_y = geom_segment_list[seq(from = 2, to = length(geom_segment_list), by = 2)])
geom_segment_tbl


##############################################################################################


# create states_map
states_map <- states_sf_transformed %>% 
        mutate(text_color = case_when(NAME == "District of Columbia" ~ "c8_2000_to_5000",
                                      NAME == "New Jersey" ~ "c8_2000_to_5000",
                                      NAME == "Rhode Island" ~ "c8_2000_to_5000",
                                      NAME == "Connecticut" ~ "c8_2000_to_5000",
                                      TRUE ~ fill_color_bin)) %>%
        st_transform(102003) %>%
        mutate(centroid = st_centroid(geometry)) %>%
        mutate(lon = unlist(map(.x = .$centroid, .f = ~ st_coordinates(.x)[[1]])),
               lat = unlist(map(.x = .$centroid, .f = ~ st_coordinates(.x)[[2]]))) %>%
        ggplot() + geom_sf(aes(geometry = geometry, fill = fill_color_bin), color = "#a6a6a6", size = .125) + 
        coord_sf(crs = st_crs(102003)) +
        scale_fill_manual(values = state_fill_color_list, name = " \n ",
                          labels = c(1, 200, 400, 600, 800, comma(1000), comma(2000), comma(5000))) +
        geom_text(aes(label = n_for_plotting, x = lon, y = lat, color = text_color), check_overlap = TRUE, family = "Trebuchet MS", size = 2.5,
                  nudge_x = case_when(states_sf_transformed$NAME == "New Jersey" ~ 500000,
                                      states_sf_transformed$NAME == "Louisiana" ~ 30000,
                                      states_sf_transformed$NAME == "Rhode Island" ~ 500000,
                                      states_sf_transformed$NAME == "District of Columbia" ~ 500000,
                                      states_sf_transformed$NAME == "Connecticut" ~ 500000,
                                      states_sf_transformed$NAME == "Maryland" ~ 540000,
                                      states_sf_transformed$NAME == "Louisiana" ~ -63000,
                                      states_sf_transformed$NAME == "Florida" ~ 130000,
                                      states_sf_transformed$NAME == "Michigan" ~ 80000,
                                      states_sf_transformed$NAME == "New York" ~ 30000,
                                      states_sf_transformed$NAME == "California" ~ -30000,
                                      states_sf_transformed$NAME == "Virginia" ~ 30000,
                                      TRUE ~ 0),
                  nudge_y = case_when(states_sf_transformed$NAME == "District of Columbia" ~ -210000,
                                      states_sf_transformed$NAME == "Rhode Island" ~ 170000, 
                                      states_sf_transformed$NAME == "Maryland" ~ 30000,
                                      states_sf_transformed$NAME == "Michigan" ~ -150000,
                                      states_sf_transformed$NAME == "Florida" ~ -120000,
                                      states_sf_transformed$NAME == "Louisiana" ~ -70000,
                                      TRUE ~ 0)) +
        scale_color_manual(values = text_color_list, guide = FALSE) +
        geom_segment(data = geom_segment_tbl, size = .1, 
                     x = case_when(geom_segment_tbl$NAME == "Maryland" ~ geom_segment_tbl$centroid_x - 50000,
                                   geom_segment_tbl$NAME == "New Jersey" ~ geom_segment_tbl$centroid_x + 20000,
                                   TRUE ~ geom_segment_tbl$centroid_x),
                     y = case_when(geom_segment_tbl$NAME == "Maryland" ~ geom_segment_tbl$centroid_y + 30000,
                                   TRUE ~ geom_segment_tbl$centroid_y),
                     xend = geom_segment_tbl$centroid_x + 400000,
                     yend = case_when(geom_segment_tbl$NAME == "District of Columbia" ~ geom_segment_tbl$centroid_y - 200000,
                                      geom_segment_tbl$NAME == "Rhode Island" ~ geom_segment_tbl$centroid_y + 170000,
                                      geom_segment_tbl$NAME == "Maryland" ~ geom_segment_tbl$centroid_y + 30000,
                                      TRUE ~ geom_segment_tbl$centroid_y)) +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), legend.position = "right",
              legend.key.size = unit(2, "mm"), 
              legend.title = element_text(size = 10, family = "Trebuchet MS"),
              legend.text = element_text(size = 10, family = "Trebuchet MS"),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + 
        guides(fill = guide_legend(title.hjust = .5, reverse = TRUE, keyheight = 1, label.vjust = 1.2)) 

states_map


#####################


# add zero to legend, add legend title, add source
states_map_output <- states_map %>% 
        ggdraw(xlim = c(0, 1.1)) + draw_label(label = "0", x = .982, y = .35, size = 10, fontfamily = "Trebuchet MS") +
        draw_label(label = "Number of", x = .99, y = .64, size = 10, fontfamily = "Trebuchet MS") +
        draw_label(label = "CAGP particpants", x = .99, y = .615, size = 10, fontfamily = "Trebuchet MS") +
        draw_label(label = "Source: CAGP data.", x = .15, y = .25, size = 10, fontfamily = "Trebuchet MS", color = "#808080")


####################


# setwd
setwd("X:/CIGP Eval Study/fy_2015")

# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename, width = 7, height = 7)
print(states_map_output)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 7, height = 7) %>% 
        print(target = "output/states_map.docx")


###################


# note that for some reason ggplot_center_legend_title fails when geom_text is added, but the legend is roughly centered anyway (could also add w magick)
# call ggplot_center_legend_title
# states_map_grob <- ggplot_center_legend_title(plot = states_map)
# grid.newpage()
# grid.draw(states_map_grob)

# save map as pdf (note that cairo_pdf is needed to render non-standard fonts, and it can't save as png, so pdf is only option)
# ggsave(filename = "output/states_map.pdf", plot = states_map, units = "in", dpi = 300, device = cairo_pdf)


#########


# # read pdf as image with magick and pdftools, then crop out white space, then save as a png
# 
# # read pdf
# states_map_pdf <- pdf_render_page(pdf = "output/states_map.pdf", page = 1, dpi = 300)
# 
# # convert to magick image
# cigp_state_map_image <- image_read(states_map_pdf)
# 
# # crop out white space
# image_info(states_map_image)
# cigp_state_map_image <- image_crop(image = cigp_state_map_image, geometry = geometry_area(width = 1500, height = 900, x_off = 80, y_off = 420))


##########


# add source footnote

# # save world_map_source_plot as pdf
# state_map_source_plot <- starwars %>% ggplot(data = ., aes(x = mass)) + geom_histogram() + 
#         ggtitle("Source: CAGP data.") +
#         theme(plot.title = element_text(size = 12, hjust = 0, family = "Trebuchet MS", color = "#808080"))
# ggsave(filename = "output/state_map_source_plot.pdf", plot = state_map_source_plot, units = "in", dpi = 300, device = cairo_pdf)
# 
# # load state_map_source_plot as image
# state_map_source_pdf <- pdf_render_page(pdf = "output/state_map_source_plot.pdf", page = 1, dpi = 300)
# 
# # convert pdf to image
# state_map_source_image <- image_read(state_map_source_pdf)
# 
# # crop out state_map_source_image
# image_info(state_map_source_image)
# state_map_source_image <- image_crop(image = state_map_source_image, geometry = geometry_area(width = 500, height = 80, x_off = 130, y_off = 0))
# state_map_source_image 
# 
# # overlay source_footnote_image
# image_info(cigp_state_map_image)
# cigp_state_map_image <- image_composite(image = cigp_state_map_image, composite_image = image_resize(image = state_map_source_image, 
#                                                 geometry = geometry_size_pixels(width = 165, height = 200)), offset = geometry_point(x = 0, y = 850))


################


# add 0 tick mark to legend

# load map as image and add zero to legend
# zero_image <- image_read("output/zero_image.png")
# image_info(cigp_state_map_image)
# cigp_state_map_image <- image_composite(image = cigp_state_map_image, composite_image = image_resize(image = zero_image, 
#                                 geometry = geometry_size_pixels(width = 17, height = 17)), offset = geometry_point(x = 1339, y = 740))
# cigp_state_map_image
# 
# # save states_map_image
# image_write(image = cigp_state_map_image, path = "output/cigp_state_map.png", format = "png", density = "300x300")
