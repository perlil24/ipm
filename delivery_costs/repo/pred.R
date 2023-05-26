# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 01/18/2023
# Purpose: Prepare datasets and run delivery costs analysis
# source("/homes/jfrostad/_code/start/ipm/delivery_costs/calc.R", echo=T)
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

#set opts
set.seed(98118)
options(scipen=999) #readability
#use cairo to render instead of quartz (quartz causes slowdowns with geom_sf)
if(!identical(getOption("bitmapType"), "cairo") && isTRUE(capabilities()[["cairo"]])){
  options(bitmapType = "cairo")
}

#set control flow params
rerun_model <- T #set true if you want to rerun the benchmark model, otherwise read in the obj
examine_model <- F #set true if you want model diagnostics
i_disease <- 'schisto'

## Set core_repo locations
user            <- Sys.info()['user']
box_name        <- 'Joseph Frostad' #TODO find out how to pull this relative
local_dir       <- ifelse(Sys.info()["sysname"] == "Linux",
                          file.path('/homes', user, ''),
                          file.path('C:/Users', user, 'Documents/start/ipm/delivery_costs/'))
my_repo <- file.path(local_dir, 'repo')
my_dropbox <- file.path('C:/Users', user, 'UW START Dropbox', box_name, '/INTERNAL IPM Vx Valuations/NTD 2022') 
setwd(my_repo)

#TODO categorize and comment packages
#TODO cleanup here
pacman::p_load(readxl, janitor, data.table, naniar, dplyr, stringr, magrittr, snakecase, #data wrangling
               scales, ggplot2, ggridges, ggrepel, gridExtra, RColorBrewer, viridis, sf, biscale,cowplot, #viz
               plm, lme4, merTools, #modelling tools
               stargazer)
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
code_dir <- file.path(my_repo, 'code')
data_dir <- file.path(local_dir, 'data')

#dirs
model_data_dir <- file.path(my_dropbox, '/1 - Data gathering - all diseases/Costs/Delivery costs/')
shape_dir <- file.path(my_dropbox, '/1 - Data gathering - all diseases/Shapefiles')
#paths
param_path <- file.path(model_data_dir, "delivery_cost_parameters.xlsx") #TODO document

###Output###
#drop_out_dir <- file.path(my_dropbox, i_disease, paste0('OutputData ', dis_version))
out_dir <- file.path(local_dir, 'output', i_disease)
viz_dir  <- file.path(local_dir, 'viz', i_disease)
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
#source custom functions that are relevant to this module
file.path(my_repo, '_lib', 'pred_fx.R') %>% source
file.path(my_repo, '_lib', 'viz_fx.R') %>% source
#***********************************************************************************************************************

# ---PREP DATA----------------------------------------------------------------------------------------------------------
##combine all the external datasets##
#disease specific parameters database
params_dt <- read_excel(param_path, sheet='database') %>% 
  as.data.table

#default parameters
defaults_dt <- read_excel(param_path, sheet='defaults') %>% 
  as.data.table

#combine param information into a list
params_list <- list(
  'params'=params_dt,
  'defaults'=defaults_dt
)

#read in the country/year specific GDP data
gdp_dt <- file.path(model_data_dir, 'imf_world_economic_outlook.csv') %>% 
  fread %>% 
  .[WEO_Subject_Code=='NGDPDPC'] %>% #usd per person
  setnames(., names(.), names(.) %>% tolower) %>%  #force names to lowercase for consistency 
  .[, c('iso', paste0('x', 1980:2027)), with=F] %>% 
  melt(., id.vars='iso', variable.name='year_id', value.name='gdp') %>% 
  .[year_id=='x2015'] %>% #TODO which year should we be using? this one has no missingness except for SYR
  .[, year_id := NULL] %>% 
  .[, gdp := str_replace_all(gdp, ',', '') %>% as.numeric] %>%  #convert from chr to numeric
  setkey(., iso)  

#read shapefiles for mapping
#iu shapefile
espen_iu_shp <- file.path(shape_dir, "ESPEN_IU_2021.shp") %>%
  st_read %>% 
  subset(ADMIN0 != "Yemen") %>% 
  setNames(names(.) %>% tolower)
#adm0 for borders
espen_admin0_shp <- file.path(shape_dir, "ESPEN_ADM0_2021.shp") %>%
  st_read %>% 
  subset(ADMIN0 != "Yemen") %>% 
  setNames(names(.) %>% tolower)

# afro_admin0_shp <- file.path(shape_dir, "afr_g2014_2013_0.shp") %>%
#   st_read %>% 
#   subset(ADMIN0 != "Yemen") %>% 
#   setNames(names(.) %>% tolower)
#***********************************************************************************************************************

# ---RUN MODEL----------------------------------------------------------------------------------------------------------
##build a new model for IU level predictions
if(rerun_model) { file.path(my_repo, 'model.R') %>% source
} else file.path(model_data_dir, 'benchmark_model.RDS') %>% readRDS %>% list2env(., globalenv())
#***********************************************************************************************************************
 
# ---PREDICT------------------------------------------------------------------------------------------------------------
#make IU level predictions using your custom fx
dt <- predictDeliveryCosts(i_disease,
                           mod=mod,
                           mod_dt=mod_dt,
                           param_list=params_list)
#***********************************************************************************************************************
 
# ---PRED DIAGNOSTICS---------------------------------------------------------------------------------------------------
##diagnostics used to test the results of our predictions
if(examine_predictions) {
  
  #examine some rows and test them against the online shiny tool
  #this line can be used interactively to test different country and IU combinations in a compact manner
  dt[iso=="NGA", c('iso', 'IU_ID', coef_vars, 'cost_iu'), with=F]
  
  #highlight outliers for plots
  dt[, iso_label := ifelse(isOutlier(cost_iu), iso, NA_character_)]
  dt[is.na(iso_label), iso_label:="All Others"]

}
#***********************************************************************************************************************

# ---SCRAP -------------------------------------------------------------------------------------------------------------
##