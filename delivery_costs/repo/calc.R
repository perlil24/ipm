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
#use cairo to render instead of quartz (quartz causes big slowdowns with geom_sf)
if(!identical(getOption("bitmapType"), "cairo") && isTRUE(capabilities()[["cairo"]])){
  options(bitmapType = "cairo")
}

#set control flow params
reload <- T #set true if you want to reprep all the data

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
pacman::p_load(readxl, janitor, data.table, naniar, dplyr, stringr, magrittr, #data wrangling
               scales, ggplot2, ggridges, ggrepel, gridExtra, RColorBrewer, viridis, #viz
               plm, lme4, #modelling tools
               stargazer, #sig tables
               #caret, mlbench, randomForest, pls, #ML tools
               zoo)

#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
code_dir <- file.path(my_repo, 'code')
data_dir <- file.path(local_dir, 'data')

#TODO consider which data is universal and which is by disease. How to organize
model_data_dir <- file.path(my_dropbox, '/1 - Data gathering - all diseases/Costs/Delivery costs/')
  coef_path <- "Delivery_cost_regression_coefficients_to_use_7_12_2022.csv" #TODO rename
  
#also load the central model objs received from fitzpatrick
load(file.path(data_dir, 'benchmark.Rdata'), verbose=T)
  
#ihme data
ihme_version <- '2022-09-13'
ihme_data_dir <- file.path(my_dropbox, 'IHME', ihme_version)
  pop_path <- 'IU_pop_dens_all_age.csv'
  
#ntd mc data
mc_version <- '2023-02-24'
mc_data_dir <- file.path(my_dropbox, 'LF/NTD MC')

#TODO for now we are just doing this for LF. in the future should be a function with disease type as an argument
lf_version <- '2022-08-26'
lf_data_dir <- file.path(my_dropbox, paste0('LF/InputData ', lf_version))
  lf_path <- 'MDA_costs_22_08_2022_AM.csv'

  
#also some IU level data for LF
  lf_iu_version <- '2023-02-15'
  lf_iu_data_dir <- file.path(my_dropbox, paste0('LF/InputData ', lf_iu_version))
  lf_iu_path <- 'lf_long_IU_cov.csv' #coverage data at IU scenario level
  lf_iu_dens_path <- 'IU_pop_dens.csv' #population data at IU year level
  lf_iu_int_path <- 'MDA-procurement-cost_LF.xlsx' #disease integration structure
  
  # example_path <- "Example data/Example_data_for_testing_code_7_12_2022.csv"
  # cost_path <- 'Cost_data_23_08_2022_complete_to_share.csv'

###Output###
lf_out_dir <- file.path(my_dropbox, paste0('LF/OutputData ', lf_version))
out_dir <- file.path(local_dir, 'output')
viz_dir  <- file.path(local_dir, 'viz')
#***********************************************************************************************************************

# ---FUNCTIONS----------------------------------------------------------------------------------------------------------
#source custom functions that are relevant to this module
# file.path(code.dir, '_lib', 'mod_fx.R') %>% source

##custom utilities##
#helper function to copy things out of R
writeExcel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

#label outliers statistically
isOutlier <- function(x) {
  return(x < quantile(x, 0.25, na.rm=T) - 1.5 * IQR(x, na.rm=T) | x > quantile(x, 0.75, na.rm=T) + 1.5 * IQR(x, na.rm=T))
}

#***********************************************************************************************************************

# ---PREP DATA----------------------------------------------------------------------------------------------------------
##read in and prep datasets for analysis##
if(reload) {
  
  #combine all the core datasets
  
  #population dataset which will spread things out to be at IUs
  pop_dt <- file.path(ihme_data_dir, pop_path) %>% 
    fread %>% 
    setnames(., names(.), names(.) %>% tolower) %>% 
    setnames(., c('ihme_loc_id', 'dens'), c('iso','pop_density')) %>% 
    .[, espen_loc := paste0(iso, '0', iu_id)]
  
  #ntd mc dataset which we will filter the IUs by
  iu_dt <- file.path(mc_data_dir, mc_version, 'Data') %>% 
    list.files(full.names = T) %>% 
    lapply(., fread, select=c('espen_loc')) %>% 
    rbindlist %>% 
    unique(by='espen_loc')
  
  iu_list <- unique(iu_dt$espen_loc) %>% 
    str_remove_all(., "[^0-9.-]") %>% #remove isos 
    str_remove_all(., "^0+") %>% #remove trailing zeros
    as.integer
  
  #filter the pop dataset to the modelled IUs
  #TODO, the pops file is not unique by IU...so we need to get the year variable added to this file but for now..
  #i will just select the first row of each IU (latest year?)
  #note we could also select the last row using (fromLast=T)
  pop_dt <- pop_dt[iu_id%in%iu_list]%>% 
    unique(by='iu_id')

  
  #lf dataset which is disease/scenario specific
  lf_dt <- file.path(lf_data_dir, lf_path) %>% 
    fread %>% 
    setnames(., names(.), names(.) %>% tolower) %>%  #force names to lowercase for consistency 
    unique(by=c('iso')) %>%  #collapse the scenarios, we will use another datasheet
    .[, scenario := NULL] %>% 
    .[, c('iso', 'gdp', 'delivery_cost_range')] %>%  #this is all we need for now
    #TODO eventually should just retire this dataset as gdp is the only use of it now
    #impute the IMP decision constants
    #TODO, validate these assumptions and eventually put them all into an assumption sheet to merge on?
    .[, eco := 0] %>% 
    .[, vol := 0] %>% 
    #.[, int := 1] %>% #this is now country/cov specific
    .[, rds := 1] %>%  #TODO should be scenario specific but it's not present
    .[, yrs := 5] %>% 
    .[, nat := 0] %>%  #TODO could invert subnat but its the same assumption
    .[, sch := 0] %>% 
    #.[, den := pop_density] %>% #match the naming in fitzpatrick model
    #.[, pop := str_replace_all(pop, ',', '') %>% as.numeric] %>% #convert from chr to numeric
    #.[, pop_treated := str_replace_all(`mda size`, ',', '') %>% as.numeric] %>% #convert from chr to numeric
    .[, gdp := str_replace_all(gdp, ',', '') %>% as.numeric] %>% #convert from chr to numeric
    .[, VUT := 0] %>% #create study level dummies and set to 0
    .[, Kri := 0] %>% #create study level dummies and set to 0
    .[, Fri := 0] %>% #create study level dummies and set to 0
    .[, Mon := 0] #create study level dummies and set to 0
    #TODO not clear what to do with unit percentages...not given as decimals in table 2
    #.[, cov := str_replace(coverage, '%', '') %>% as.numeric] %>% 
    #.[, cov := cov/1] #need to confirm this is the right way to unit the percentages
  
  
  #there are 6 countries some missing pops/mda size/GDPs so i will impute them using world bank figures
  lf_dt[iso=='BDI', gdp:= 221.48]
  lf_dt[iso=='BFA', gdp:= 893.08]
  lf_dt[iso=='MWI', gdp:= 634.84]
  lf_dt[iso=='SLE', gdp:= 480.04]
  lf_dt[iso=='SWZ', gdp:= 3978.4]
  lf_dt[iso=='TZZ', gdp:= 1031] #zanzibar: i didnt see this split out on the world bank site so i used UN figures here:
  #http://data.un.org/Data.aspx?d=SNAAMA&f=grID:101;currID:USD;pcFlag:1;crID:836

  #add quintiles of gdp
  lf_dt[, gdp_quint := cut(gdp, quantile(gdp, probs=0:5/5, na.rm=T), include.lowest=TRUE, labels=FALSE)]
  
  
  #they are also missing pop/pop densities, using the mean by GDP for now
  # lf_dt[, pop := impute_mean(pop), by=gdp_quint] #missing densities for some countries (see below)
  # lf_dt[, den := impute_mean(den), by=gdp_quint] #missing densities for some countries (see below)
  
  #bring in the IU level data and merge it on to expand and calculate pop treated
  #iu/scenario specific coverage
  lf_iu_cov_dt <- file.path(lf_iu_data_dir, lf_iu_path) %>% 
    fread %>% 
    setnames('value', 'cov') %>% 
    .[, ]
  #iu level population data
  lf_dens_dt <- file.path(lf_iu_data_dir, lf_iu_dens_path) %>% 
    fread
  #disease integration data at country level
  lf_int_dt <- file.path(lf_iu_data_dir, lf_iu_int_path) %>% 
    read_excel() %>% 
    as.data.table() %>% 
    .[, .(ISO, `Co-endemic`)] %>% 
    unique() %>% #TODO verify that theres no difference in integration between the different Product/Drug groups (there are 2 diff)
    .[, int := 1] %>% 
    .[`Co-endemic`==1, int := 2]
  
  #expand to the IU level and add coverage/density
  lf_dt <- merge(lf_dt, lf_iu_cov_dt, by.x=c('iso'), by.y=c('ISO'))
  lf_dt <- merge(lf_dt, lf_dens_dt, by=c('IU_ID', 'year_id')) #TODO verify correct year
  lf_dt <- merge(lf_dt, lf_int_dt, by.x=c('iso'), by.y=c('ISO'))
  lf_dt[, pop_treated := cov*adj_pop]#calculate the target pop as cov*IU pop
  lf_dt[, pop_density := dens]
  
}

#***********************************************************************************************************************

# ---PREP MODEL---------------------------------------------------------------------------------------------------------
#regression coefficients which come from this paper:
#https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0005037
#note do we want to extract uncertainty?
coef_dt <- file.path(model_data_dir, coef_path) %>% 
  fread %>% 
  setnames(., names(.), names(.) %>% tolower %>%  #force names to lowercase for consistency
             str_replace(., 'model_', '') %>% #remove extraneous prefixes
             str_replace(., 'log_', '') %>% 
             str_replace(., 'log', '') )

#rename the variables to match those from our cost dataset
setnames(coef_dt, 
         c('pop', 'den'),
         c('pop_treated', 'pop_density'))

#reorder some things to match regression #1
irrelevant_coefs <- c('vut', 'kri', 'fri', 'mon', 'sqrtdis') #remove study dummies and the distance var from reg#2
coef_dt[, (irrelevant_coefs)  := NULL]
setcolorder(coef_dt,neworder='constant') #set constant first to match the model object format

#now use the extracted coefficients to predict using a custom function due to the complexity of the model. 
#TODO we can do it like this for now but i think it's best to get the model object or the var:covar matrix from
#the authors, then predict like a regular lmer
#we could simulate the lmer if necessary using makeLmer but it requires more info about the model output

#here's a hacky way to make a fake lm with the provided coefficients
#first generate a fake dataset to run our lm over
fake_dt <- data.table(eco=rnorm(n = 1000, mean = 0, sd = 1),
                      vol=rnorm(n = 1000, mean = 0, sd = 1),
                      int=rnorm(n = 1000, mean = 0, sd = 1),
                      rds=rnorm(n = 1000, mean = 0, sd = 1),
                      yrs=rnorm(n = 1000, mean = 0, sd = 1),
                      cov=rnorm(n = 1000, mean = 0, sd = 1),
                      nat=rnorm(n = 1000, mean = 0, sd = 1),
                      sch=rnorm(n = 1000, mean = 0, sd = 1),
                      pop_treated=rnorm(n = 1000, mean = 0, sd = 1),
                      pop_density=rnorm(n = 1000, mean = 0, sd = 1),
                      gdp=rnorm(n = 1000, mean = 0, sd = 1),
                      cost=rnorm(n = 1000, mean = 0, sd = 1))

#setup a fake lm model that will store our extracted coefs
fake_mod <- lm(cost~eco+vol+log(int)+log(rds)+
            yrs+cov+nat+sch+log(pop_treated)+log(pop_density)+log(gdp)+
            eco:log(int)+cov:nat+eco:sch+cov:sch+
            nat:log(pop_treated)+nat:log(pop_density)+nat:log(gdp),
          data=fake_dt)

#then inject our literature coefficients
#TODO setup some tests to assert that model formula matches coef dt order
coef_names <- fake_mod$coefficients %>% names #extract the names for clarity because injecting erases them
#also make a vector of the ivs for later examination
coef_vars <- coef_names %>% 
  .[!(.%like%':')] %>% 
  .[!(.%like%'Intercept')] %>% 
  str_remove_all(., "log\\(") %>% 
  str_remove_all(., "\\)")
#inject coefficients from the extraction dt into the fake model obj
fake_mod$coefficients <- unlist(coef_dt[1]) %>% as.numeric() 
names(fake_mod$coefficients) <- coef_names #preserve the names

#start building a new model for IU level predicting
setDT(mda_uc)[, pop_treated := pop] %>%  #set to match our naming convention
  .[, pop_density := den] %>%  #set to match our naming convention
  .[, cov := cov/100] #treat as a percent so we can test logit transforms


#lets examine some of the values

#split out vars so we can melt and compare
dummy_vars <- c('eco', 'vol', 'sch', 'nat')
cont_vars <- c('int', 'rds', 'yrs', 'cov', 'pop_treated', 'pop_density', 'gdp') #we will plot only the continuous vars
plot_dt <- melt(mda_uc,
                measure.vars=cont_vars,
                variable.name = 'dep_var',
                value.name = 'dep_value')

#look at all the distributions of our data separately
#check out the distributions of natl values and then the subnational ones by country
varDistPLot <- function(var, dt, by_var=NULL, trans='none') {
  message(var)
  plot_dt <- copy(dt)
  if(trans=='log') plot_dt[, (var) := var %>% get %>% log]
  if(trans=='logit') plot_dt[, (var) := var %>% get %>% logit]
  plot <-
  ggplot(plot_dt, aes_string(x=var, fill=by_var, group=by_var)) +
    geom_density(alpha=0.4) +
    ggtitle("Variable Distributions", subtitle=paste0('Data Trans: ', trans)) +
    scale_fill_viridis_c() +
    theme_minimal()
 
}

pdf(file.path(viz_dir, 'coef_distributions.pdf'), height=8, width=12)
lapply(cont_vars, varDistPLot, dt=mda_uc, by_var='nat')
lapply(cont_vars, varDistPLot, dt=mda_uc, by_var='nat', trans='log')
varDistPLot(var='cov', dt=mda_uc, by_var='nat', 'logit') %>% print
varDistPLot(var='ucb', dt=mda_uc, by_var='nat') %>% print
varDistPLot(var='ucb', dt=mda_uc, by_var='nat', trans='log') %>% print
dev.off()

#transformations
plot_dt[dep_var %in% c('int', 'rds', 'pop_treated', 'pop_density', 'gdp'), dep_value := dep_value]
plot_dt[, dep_value_trans := dep_value]
plot_dt[dep_var %in% c('int', 'rds', 'pop_treated', 'pop_density', 'gdp'), dep_value_trans := log(dep_value)]
plot_dt[dep_var=='cov', dep_value_trans := logit(dep_value)]

#look at all the variable relationships with our IV
ggplot(plot_dt[ucb<8], aes(x=dep_value, y=ucb, color=nat, group=nat)) +
  geom_point(alpha=.8) +
  facet_wrap(~dep_var) +
  scale_y_log10() +
  scale_color_viridis() +
  theme_minimal()

#look at all the variable relationships with our IV if we transform the vars
ggplot(plot_dt[ucb<8], aes(x=dep_value_trans, y=log(ucb), color=nat, fill=nat, group=nat)) +
  geom_point(alpha=.8) +
  geom_smooth(method='loess') +
  facet_wrap(~dep_var) +
  scale_y_log10() +
  scale_color_viridis() +
  scale_fill_viridis(guide=F) +
  theme_minimal()

#examine some vars more closely
ggplot(mda_uc[ucb<8], aes(x=log(pop_density)*log(pop_treated), y=ucb)) +
  geom_point(aes(alpha=log(pop_treated))) +
  geom_smooth(method='gam', color='darkorange1') +
  facet_wrap(~sch) +
  scale_y_log10() +
  scale_color_viridis() +
  theme_minimal()

#rerun the fitzpatrick model
og_mod <- lmer(log(ucb)~eco+vol+log(int)+log(rds)+
              yrs+cov+nat+sch+log(pop_treated)+log(den)+log(gdp)+
              VUT+Kri+Fri+Mon+
              eco:log(int)+cov:nat+eco:sch+cov:sch+
              nat:log(pop_treated)+nat:log(den)+nat:log(gdp) +(1|study_ref),
            data=mda_uc)

#run our version of the model
mod <- lmer(log(ucb)~eco+vol+log(int)+log(rds)+yrs+logit(cov)+sch+
              log(pop_treated)*log(pop_density)+log(gdp)+
              VUT+Kri+Fri+Mon+
              eco:log(int)+eco:sch+sch:logit(cov)+
              (1|study_ref),
            data=mda_uc[nat==0])

nat_mod <- lmer(log(ucb)~eco+vol+log(int)+log(rds)+yrs+logit(cov)+sch+
              log(pop_treated)+log(den)+log(gdp)+
              VUT+Kri+Fri+Mon+
              eco:log(int)+eco:sch+cov:sch+
              (1|study_ref),
            data=mda_uc[nat==1])

#output the model results
stargazer(mod,
          type='html') %>%
  capture.output(file=file.path(out_dir, 'table_1.html'))

#***********************************************************************************************************************

# ---PREDICT------------------------------------------------------------------------------------------------------------
#tested the injection method, works as long as the coefficients are ordered correctly in the model coef dt
# mod_test <- lm(y~eco+vol+log(int),
#                data=example_dt)
# mod_test$coefficients <- c(1.134, .157, -1.348, -.17)
# example_dt[, cost_test := exp(eco*.157 + vol*-1.348 + log(int)*-.17+1.134)]
#TODO test a few rows manually

#make national predictions
#lf_dt[, cost_natl := predict(mod, newdata =lf_dt, re.form=NA) %>% exp]

#merge with pop density data to breakdown into IUs
#setnames(lf_dt, c('pop_density'), c('pop_density_iso'))
#lf_dt <- merge(lf_dt, pop_dt, by='iso', all.x=T, allow.cartesian = T)
#lf_dt[, den := pop_density] #match the naming conventions in fitzpatrick model
#lf_dt[, den := impute_mean(den), by=gdp_quint] #TODO missing densities for some IUs, use avg by GDP
#setnames(lf_dt, 'adj_pop', 'pop') #TODO confirm that this is the right value

#generate also the IU cost
#should we be changing nat to 0 now that these are subnational sites?
lf_dt[, nat := 0] #if we do it significantly drops the cost to more reasonable values
lf_dt[, sch := 0]
lf_dt[gdp>max(mda_uc$gdp, na.rm=T), gdp := max(mda_uc$gdp, na.rm=T)] #cap GDP to maximum value in dataset
lf_dt[, cost_iu := predict(mod, newdata =lf_dt, re.form=NA) %>% exp]

#output the results
write.csv(lf_dt[, c('iso', 'scenario', 'year_id', 'IU_ID', coef_vars, 'cost_iu'), with=F], 
          file=file.path(lf_out_dir, 'delivery_cost_preds_integrated.csv'))
#***********************************************************************************************************************

# ---DIAGNOSTICS--------------------------------------------------------------------------------------------------------
##examine some rows and test them against the online shiny tool
#this line can be used interactively to test different country and IU combinations in a compact manner
lf_dt[iso=="NGA", c('iso', 'IU_ID', coef_vars, 'cost_iu'), with=F]

#highlight outliers for plots
lf_dt[, iso_label := ifelse(isOutlier(cost_iu), iso, NA_character_)]
lf_dt[is.na(iso_label), iso_label:="All Others"]

#see which variables are driving outliers
plot_dt <- melt(lf_dt,
                measure.vars=cont_vars,
                variable.name = 'dep_var',
                value.name = 'dep_value')
plot_dt[dep_var %in% c('int', 'rds', 'pop_treated', 'pop_density', 'gdp'), dep_value := dep_value]
plot_dt[, dep_value_trans := dep_value]
plot_dt[dep_var %in% c('int', 'rds', 'pop_treated', 'pop_density', 'gdp'), dep_value_trans := log(dep_value)]
plot_dt[dep_var=='cov', dep_value_trans := logit(dep_value)]


#scatter to see the relationships
ggplot(plot_dt[cost_iu>3], aes(x=dep_value_trans, y=cost_iu, color=gdp_quint)) +
  geom_point(alpha=.8) +
  #geom_smooth(method='loess') +
  facet_wrap(~dep_var) +
  scale_y_log10() +
  scale_color_viridis() +
  #scale_fill_viridis(guide=F) +
  theme_minimal()
file.path(viz_dir, 'coef_v_iu_costs.png') %>% ggsave(height=8, width=12)

#scatter the IU preds against the natl preds to look for outliers
  ggplot(lf_dt, aes(cost_iu, cost_natl, color=iso_label, size=gdp)) +
    geom_point(position='jitter') +
    #geom_text_repel(box.padding = 0.5, max.overlaps = Inf)  +
    scale_color_brewer('Outlier Countries', palette='Paired') +
    scale_x_log10() +
    scale_y_log10() +
    ggtitle("Delivery Cost Natl vs IU") +
    theme_minimal()

file.path(viz_dir, 'natl_vs_iu_costs.png') %>% ggsave(height=8, width=12)


##plot the results##
#check out the distributions of natl values and then the subnational ones by country
natlDensPlot <- function(this_iso, var, dt) {
  message(var)
  ggplot(dt[iso%in%this_iso], aes_string(var)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Delivery Cost Distributions") +
  theme_minimal()
}

#natl
natlDensPlot(unique(lf_dt$iso), var='cost_iu', dt=lf_dt)
file.path(viz_dir, 'natl_cost_distributions.png') %>% ggsave(height=8, width=12)

#subnatl, binned by gdp quintiles
subnatlDensPlot <- function(i, var, dt) {
  message(var)
  ggplot(dt[gdp_quint%in%i & year_id %in%seq(2020,2040,5)], aes_string(var, fill='iso')) +
    geom_density(alpha=0.65) +
    scale_x_log10()+
    scale_fill_brewer('Country', palette = 'Set1') +
    facet_wrap(~year_id)+
    ggtitle(paste0("Delivery Cost Distributions for GDP Quintile #", i)) +
    theme_minimal()
}
pdf(file.path(viz_dir, 'subnatl_cost_distributions.pdf'), height=8, width=12)
lapply(unique(lf_dt$gdp_quint) %>% sort, subnatlDensPlot, dt=lf_dt, var='cost_iu')
dev.off()

#check out the relationships between the covariates and cost
coefPlot <- function(var, pred_var, dt) {
  message(var)
  ggplot(dt, aes_string(x=var, y=pred_var) ) +
    geom_point(position='jitter') +
    #geom_hex(bins = 70) +
    ggtitle(var) +
    theme_bw() 
}

pdf(file.path(viz_dir, 'delivery_cost_coef_relationships.pdf'), height=8, width=12)
lapply(coef_names[2:12], coefPlot, dt=lf_dt[cost_iu>100], pred_var='cost_iu')
dev.off()
#***********************************************************************************************************************

# ---SCRAP -------------------------------------------------------------------------------------------------------------
#cost data
#note that this is variable by scenario/disease/idu
# cost_dt <- file.path(data_dir, cost_path) %>% 
#   fread %>% 
#   setnames(., names(.), names(.) %>% tolower) #force names to lowercase for consistency 
#TODO confirm with per that this is the right data  

#example data
# example_dt <- file.path(data_dir, example_path) %>% 
#   fread %>% 
#   setnames(., names(.), names(.) %>% tolower) #force names to lowercase for consistency

# #next lets process the data 
# log_vars <- c('int', 'rds', 'pop', 'den', 'gdp') #some vars need to be converted to logspace
# example_dt[, paste0('log_', log_vars) := lapply(.SD, log), .SDcols=log_vars]
# #TODO some of the log vars did not exist in the cost_dt, only the example (e.g. int)
# example_dt[, sqrt_dis := sqrt(dis)] #one var is sqrt transformed
# #we also need to create some interaction terms
# example_dt[, eco__log_int := eco * log_int]
# example_dt[, cov__nat := cov * nat]
# example_dt[, eco__sch := eco * sch]
# example_dt[, cov__sch := cov * sch]
# example_dt[, nat__log_pop := nat * log_pop]
# example_dt[, nat_log_den := nat * log_den]
# example_dt[, nat_log_gdp := nat * log_gdp]


#aldinas code
#First load in the data with the coefficients THIS IS A DATASET WHICH WILL REMAIN CONSISTENT FOR EVERY CALCULATION OF DELIVERY COSTS 
# library(readr)
# 
# Delivery_cost_regression_coefficients_to_use_7_12_2022 <- read_csv("C:/Users/amesi/OneDrive/Desktop/UW START Dropbox/Aldina Mesic/INTERNAL IPM Vx Valuations/NTD 2022/1 - Data gathering - all diseases/Costs/Delivery costs/Delivery_cost_regression_coefficients_to_use_7_12_2022.csv", 
#                                                                    col_types = cols(model_eco = col_number(), 
#                                                                                     model_vol = col_number(), model_log_int = col_number(), 
#                                                                                     model_log_rds = col_number(), model_yrs = col_number(), 
#                                                                                     model_cov = col_number(), model_nat = col_number(), 
#                                                                                     model_sch = col_number(), model_logpop = col_number(), 
#                                                                                     model_logden = col_number(), model_loggdp = col_number(), 
#                                                                                     model_VUT = col_number(), model_Kri = col_number(), 
#                                                                                     model_Fri = col_number(), model_Mon = col_number(), 
#                                                                                     model_sqrtdis = col_number(), model_eco_logint = col_number(), 
#                                                                                     model_cov_nat = col_number(), model_eco_sch = col_number(), 
#                                                                                     model_cov_sch = col_number(), model_nat_logpop = col_number(), 
#                                                                                     model_nat_logden = col_number(), 
#                                                                                     model_nat_gdp = col_number(), Constant = col_number()))
# View(Delivery_cost_regression_coefficients_to_use_7_12_2022)
# 
# model<- file.path(data_dir, coef_path) %>% 
#   fread 
# 
# ##Next, I will load an example dataset, for the sake of demonstration before making each scenario/iu/disease dataset. This process should be followed by each. 
# #THIS IS THE DATASET WHICH WILL VARY BASED ON EACH SCENARIO FOR EACH DISEASE
# 
# Example_data_for_testing_code_7_12_2022 <- read_csv("C:/Users/amesi/OneDrive/Desktop/UW START Dropbox/Aldina Mesic/INTERNAL IPM Vx Valuations/NTD 2022/1 - Data gathering - all diseases/Costs/Delivery costs/Example data/Example_data_for_testing_code_7_12_2022.csv", 
#                                                     col_types = cols(IU_ID = col_number(), 
#                                                                      Eco = col_number(), Vol = col_number(), 
#                                                                      Int = col_number(), Rds = col_number(), 
#                                                                      Yrs = col_number(), Cov = col_number(), 
#                                                                      Nat = col_number(), Sch = col_number(), 
#                                                                      pop = col_number(), den = col_number(), 
#                                                                      gdp = col_number(), VUT = col_number(), 
#                                                                      Kri = col_number(), Fri = col_number(), 
#                                                                      Mon = col_number(), dis = col_number(), 
#                                                                      `Log(int)` = col_number(), `Log(rds)` = col_number(), 
#                                                                      `Log(pop)` = col_number(), `Log(den)` = col_number(), 
#                                                                      `Log(GDP)` = col_number(), `Sqrt(dis)` = col_number(), 
#                                                                      `Eco:log(int)` = col_number(), `Cov: nat` = col_number(), 
#                                                                      `Eco: sch` = col_number(), `Cov: sch` = col_number(), 
#                                                                      `Nat: log(pop)` = col_number(), `Nat:log(den)` = col_number(), 
#                                                                      `Nat: log(gdp)` = col_number()))
# View(Example_data_for_testing_code_7_12_2022)
# 
# input<-file.path(data_dir, example_path) %>% 
#   fread  
# 
# #next lets process the data 
# 
# input$`Log(int)` <- log(input$Int)
# input$`Log(rds)` <-log(input$Rds)
# input$`Log(pop)` <-log(input$pop)
# input$`Log(den)` <-log(input$den)
# input$`Log(GDP)` <-log(input$gdp)
# input$`Sqrt(dis)` <-sqrt(input$dis)
# input$`Eco:log(int)` <-input$Eco * input$`Log(int)`
# input$`Cov: nat` <- input$Cov * input$Nat
# input$`Eco: sch` <- input$Eco * input$Sch
# input$`Cov: sch` <- input$Cov * input$Sch
# input$`Nat: log(pop)`<- input$Nat * input$`Log(pop)`
# input$`Nat:log(den)` <- input$Nat * input$`Log(den)`
# input$`Nat: log(gdp)` <- input$Nat * input$`Log(GDP)`
# 
# View(input)
# 
# #Next we have to multiply each input variable by the coefficients and then sum all plus the constant to get the delivery cost 
# 
# input$intermediate_eco <- model$model_eco * input$Eco
# input$intermediate_vol <- model$model_vol  * input$Vol
# input$intermediate_log_int <- model$model_log_int  *input$`Log(int)`
# input$intermediate_log_rds <- model$model_log_rds  *input$`Log(rds)`
# input$intermediate_yrs<- model$model_yrs  *input$Yrs
# input$intermediate_cov <- model$model_cov  *input$Cov
# input$intermediate_nat<-  model$model_nat  * input$Nat
# input$intermediate_sch  <- model$model_sch  * input$Sch
# input$intermediate_logpop   <- model$model_logpop  * input$`Log(pop)`
# input$intermediate_logden <- model$model_logden  * input$`Log(den)`
# input$intermediate_loggdp <- model$model_loggdp  * input$`Log(GDP)`
# input$intermediate_VUT <- model$model_VUT  * input$VUT
# input$intermediate_Kri  <- model$model_Kri  *input$Kri
# input$intermediate_Fri  <- model$model_Fri  *input$Fri
# input$intermediate_Mon   <- model$model_Mon  *input$Mon
# input$intermediate_sqrtdis <- model$model_sqrtdis  *input$`Sqrt(dis)`
# input$intermediate_eco_logint  <- model$model_eco_logint  *input$`Eco:log(int)`
# input$intermediate_cov_nat  <- model$model_cov_nat  * input$`Cov: nat`
# input$intermediate_eco_sch <- model$model_eco_sch  *input$`Eco: sch`
# input$intermediate_cov_sch  <- model$model_cov_sch  *input$`Cov: sch`
# input$intermediate_nat_logpop <- model$model_nat_logpop  *input$`Nat: log(pop)`
# input$intermediate_nat_logden  <- model$model_nat_logden  * input$`Nat:log(den)`
# input$intermediate_nat_gdp <- model$model_nat_gdp  * input$`Nat: log(gdp)`
# 
# ##now lets replace all NAs
# library(tidyverse)
# input <-input %>% replace(is.na(.), 0)
# 
# ##making sure all variables are numeric so we can add across columns 
# as.numeric(input$`intermediate_eco`)
# as.numeric(input$`intermediate_vol`)
# as.numeric(input$`intermediate_log_int`)
# as.numeric(input$`intermediate_log_rds`)
# as.numeric(input$`intermediate_yrs`)
# as.numeric(input$`intermediate_cov `)
# as.numeric(input$`intermediate_nat`)
# as.numeric(input$`intermediate_sch`)
# as.numeric(input$`intermediate_logpop`)
# as.numeric(input$`intermediate_logden`)
# as.numeric(input$`intermediate_loggdp`)
# as.numeric(input$`intermediate_VUT`)
# as.numeric(input$`intermediate_Kri`)
# as.numeric(input$`intermediate_Fri`)
# as.numeric(input$`intermediate_Mon`)
# as.numeric(input$`intermediate_sqrtdis`)
# as.numeric(input$`intermediate_eco_logint`)
# as.numeric(input$`intermediate_cov_nat`)
# as.numeric(input$`intermediate_eco_sch`)
# as.numeric(input$`intermediate_cov_sch`)
# as.numeric(input$`intermediate_nat_logpop`)
# as.numeric(input$`intermediate_nat_logden`)
# 
# 
# 
# # now to get the delivery cost
# 
# #first lets make a variable called sum as it will sum all of the 'intermediate' data variables 
# 
# input$sum <- 0 
# 
# #next lets check the column names / numbers to see which to multiply 
# 
# colnames(input)
# input$sum <- rowSums(input[, 33:55 ])
# 
# ##now lets calculate the log delivery cost (this is what the model corresponds to)
# 
# input$logdeliverycost<-input$sum + model$Constant
# 
# ##now lets exponetiate to get the actual delivery cost 
# View(input)
# input$deliverycost<-exp(input$logdeliverycost)
# 
