# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 01/18/2023
# Purpose: Prepare datasets and run COI analysis
# source("/homes/jfrostad/_code/start/ipm/coi/prep.R", echo=T)
#***********************************************************************************************************************

# ----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

#set opts
set.seed(98118)
options(scipen=999) #readability
#use cairo to render instead of quartz (quartz causes big slowdowns with geom_sf)
if(!identical(getOption("bitmapType"), "cairo") && isTRUE(capabilities()[["cairo"]])){
  options(bitmapType = "cairo")
}

#set control flow params
reload <- F #set true if you want to reprep all the central data
disease <- 'RSV'

## Set core_repo locations
user            <- Sys.info()['user']
box_name        <- 'Joseph Frostad' #TODO find out how to pull this relative
local_dir       <- ifelse(Sys.info()["sysname"] == "Linux",
                          file.path('/homes', user, ''),
                          file.path('C:/Users', user, 'Documents/start/ipm/coi/'))
my_repo <- file.path(local_dir, 'repo')
my_dropbox <- file.path('C:/Users', user, 'UW START Dropbox', box_name, 'START COI') 
setwd(my_repo)

#load packages
#TODO only relevant to running in linux on shared cluster
package_lib    <- sprintf('%s_code/_lib/pkg_R', my_repo)
## Load libraries and  MBG project functions.
.libPaths(package_lib)

#TODO categorize and comment packages
pacman::p_load(readxl, janitor, data.table, naniar, dplyr,
               magrittr, scales, ggplot2, ggridges, ggrepel, gridExtra, RColorBrewer, 
               sf, viridis, 
               stargazer,
               #caret, mlbench, randomForest, pls,
               zoo)

#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
#raw data
code_dir <- file.path(my_repo, 'code')
data_dir <- file.path(my_dropbox, 'Deliverables/Default COI Data')
#data_extract_path <- 'ehd_data_v1.xlsx' #TODO rename

###Output###
out_dir <- file.path(local_dir, 'output')
viz_dir  <- file.path(local_dir, 'viz')
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
#source custom functions that are relevant to this module
# file.path(code.dir, '_lib', 'mod_fx.R') %>% source
# file.path(code.dir, '_lib'  , 'prep_fx.R') %>% source
# file.path(code.dir, '_lib', 'viz_fx.R') %>% source

##custom utilities##
#helper function to copy things out of R
writeExcel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

#label outliers statistically
isOutlier <- function(x) {
  quantile(x, 0.25, na.rm=T) - 1.5 * IQR(x, na.rm=T) | x > quantile(x, 0.75, na.rm=T) + 1.5 * IQR(x, na.rm=T)
}

#***********************************************************************************************************************

# ---PREP DATA----------------------------------------------------------------------------------------------------------
##read in and prep datasets for analysis##
if(reload) {
  
  #combine all the core datasets
  
  #IMF Data
  #Used to convert currencies to 2017 USD
  imf_dt <- read_xlsx(file.path(data_dir, 'IMF.XLSX'), skip=1, sheet = "Conversion factor") %>% 
    as.data.table %>% 
    setnames(.,
             c('...1', '...20'),
             c('iso', 'notes')) %>% 
    melt(., id.vars=c('iso', 'notes'), variable.name='year',value.name = 'imf_conv')
  
  
  #UNICEF Data
  #TODO wait for a better formatting of this one
  
  #WHO CHOICE Data
  #Used for cost of services based on facility type
  #TODO will be updated
  choice_dt <- read_xlsx(file.path(data_dir, 'WHO_CHOICE.XLSX'), sheet = "Sheet1") %>% 
    as.data.table %>% 
    setnames(.,
             c('ISO', 
               'Secondary hospital cost per bed day 2010$', 
               'Cost of hospital outpatient visit', 
               'Cost of no bed health center-rural'),
             c('iso', 
               'inpatient',
               'outpatient_hosp',
               'outpatient_clinic')) 
  
  #Custom Literature Values Database
  #TODO build db
  
}