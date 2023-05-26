# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 01/18/2023
# Purpose: Functions for predicting delivery costs
# source("/homes/jfrostad/_code/start/ipm/delivery_costs/calc.R", echo=T)
#***********************************************************************************************************************

# ---PREP FX------------------------------------------------------------------------------------------------------------
#source custom functions that are relevant to this module
# file.path(code.dir, '_lib', 'mod_fx.R') %>% source
  
#custom fx to read in and prepare the predictor datasets
#TODO for cov data, what is the measure variable meaning? Chemo1Cov vs Chemo2Cov
#TODO Chemo2Cov only applies to one scenario but they aren't mutually exclusive
#TODO maybe pop density should be a central file? not sure its disease scenario specific
paramReadR <- function(i, lookup, dir) {
  
  #read in the file
  message('reading in predictor data for \n', i)
  path <- file.path(dir, lookup[parameter==i, value]) #build path

  if(path %like% 'csv') {
    out <- fread(path, header=T) %>% #read in
    setnames(., names(.), names(.) %>% tolower) #set lowercase
  } else if(path %like% 'xlsx') {
    out <- read_excel(path) %>% #read in
      as.data.table %>% 
      setnames(., names(.), names(.) %>% tolower) #set lowercase
  }
  
  #format long if necessary
  #TODO, this may not be the best way to verify this step is necessary
  if('parameter'%in%names(out)) {
    message('~>predictor data was provided in wide format, reshaping long')
    out <-
      melt(out, id.vars=c('scenario', 'parameter'),
           measure.vars = c(2022:2040) %>% as.character,
           variable.name = 'year_id',
           variable.factor=F) %>% 
      .[, year_id := as.integer(year_id)] %>% 
      dcast(scenario+year_id~parameter,
            value.var='value') %>% 
      setnames(., names(.), names(.) %>% tolower) 
  }

  #keying based on resolution
  #note this step is to provide merge information for later
  reso <- lookup[parameter==i, resolution] %T>%
    message('~~>keying ', i, ' on ', ., '\n\n') %>% 
    str_split_1('\\+') 
  
  out <- setkeyv(out, reso) #read resolution info from params dt and split
  
}
#***********************************************************************************************************************
 
# ---MAIN FX------------------------------------------------------------------------------------------------------------
#master function to combine data and make predictions of delivery cost 
predictDeliveryCosts <- function(i_disease, #the disease we are predicting for
                                 mod, #the model object for our benchmarking model
                                 mod_dt, #the data used to fit that model
                                 param_list, #a list containing our parameter objects (defaults and disease specific)
                                 pred_vars=parent.frame()$pred_vars, #the relevant predictor variables
                                 input_dir=parent.frame()$my_dropbox, #the path to our input data folder
                                 gdp_dt=parent.frame()$gdp_dt, #country-year level gdp data
                                 make_ci=F, #bootstrap the confidence interval
                                 make_plots=T #generate some diagnostic plots
                                 ) { 

  ##combine all the core datasets##
  message('reading in and combining predictor datasets')
  #disease specific parameters database
  params_dt <- param_list$params[disease==i_disease]  #filter by disease

  #setup the output dirs
  out_dir <- file.path(my_dropbox, i_disease, paste0('OutputData ', params_dt[parameter=='iu_version', value])) %T>% 
    dir.create(recursive = T)
  viz_dir <- file.path(out_dir, 'viz') %T>% 
    dir.create(recursive = T)
  
  #extract predictors list
  model_predictors_list <- params_dt[parameter_type=='predictor', parameter]
  model_predictors_vec <- model_predictors_list %>% str_split('\\+') %>% unlist
  
  #default parameters
  defaults_dt <- param_list$defaults[!(parameter%in%model_predictors_vec)] %>% #filter out any disease specific params
    .[, notes:=NULL] %>%  #don't need for modelling
    .[, fakeid := 1] %>% 
    dcast(fakeid~parameter,
          value.var='value')

  #lookup input data dir based on version info and disease
  cost_data_dir <- file.path(input_dir, paste0(i_disease, '/InputData ',  
                                               params_dt[parameter=='iu_version', value]))
 
  #read in and combine our disease specific predictor data using custom fx
  dt <-
    lapply(unique(model_predictors_list), 
           paramReadR,
           lookup=params_dt, dir=cost_data_dir) %>% 
    Reduce(function(...) merge(..., all=FALSE, sort=FALSE), .) #note we set sort to false to keep from rekeying

  #merge on the GDP data
  dt <- merge(dt, gdp_dt, all.x=T)
  
  #merge on the default values for any other predictors that are not disease specific
  dt <- dt %>% 
    .[,fakeid:=1] %>% #TODO there's probably a better way to do this universal join
    merge(defaults_dt, by='fakeid') %>% 
    .[,fakeid:=NULL] #cleanup
  
  #calculate population predictors
  dt[, pop_treated := cov*adj_pop]#calculate the target pop as cov*IU pop
  dt[, pop_density := dens] #TODO best practice to rename this upstream
  
  message('plotting missingness')
  #examine missingness
  country_miss_plot <- gg_miss_fct(x = dt, fct = iso)
  file.path(viz_dir, 'country_missingness.png') %>% ggsave(height=8, width=12)
  
  message('making IU level predictions')
  #make IU level delivery cost predictions
  dt[gdp>max(mod_dt$gdp, na.rm=T), gdp := max(mod_dt$gdp, na.rm=T)] #cap GDP to maximum value in input dataset
  dt[, cost_iu := predict(mod, newdata =dt, re.form=NA) %>% exp]

  if(make_ci) { 
    
    #also make a confidence interval for diagnostics
    dt[, study_ref := 1] #requires the random effect grouping variable to predict
    
    dt_ci <- dt[scenario=='1_KK2'&year_id==2022] %>% 
      cbind(., predictInterval(mod, newdata = ., n.sims = 250, which = "fixed") %>% exp) %>% 
      .[, range := upr-lwr]
    
    #plot a bivariate map to show the relationship between cost and model uncertainty
    dt_ci$cost_bin <- cut(dt_ci$cost_iu, breaks = c(0,.5,1,10,100), include.lowest = TRUE)
    bi_data <- bi_class(dt_ci, x = cost_bin, y = range, style = "quantile", dim = 4)
    bi_map <- 
      cartographeR(shapefile=espen_iu_shp,
                   borders=espen_admin0_shp,
                   dt=bi_data, map_varname = 'bi_class', map_label = 'Uncertainty v Cost',
                   map_title = '',
                   scale_type='bivar', 
                   get_plot = T)

    bi_legend <- bi_legend(pal = "GrPink2",
                           dim = 4,
                           xlab = "Delivery Cost",
                           ylab = "Uncertainty Interval",
                           size = 8)
    
    finalPlot <- ggdraw() +
      draw_plot(bi_map, 0, 0, 1, 1) +
      draw_plot(bi_legend, 0.01, .01, 0.2, 0.2)
    
    file.path(viz_dir, 'uncertainty_map.png') %>% ggsave(finalPlot, height=8, width=12)

  }
  
  message('saving predictions')
  #output the results
  write.csv(dt[, c('iso', 'scenario', 'year_id', 'iu_id', pred_vars, 'cost_iu'), with=F], 
            file=file.path(out_dir, 'delivery_cost_preds.csv'))
  
  if(make_plots){
    
    message('generating diagnostic plots and maps')
    #subnatl
    pdf(file.path(viz_dir, 'subnatl_cost_ridges.pdf'), height=8, width=12)
    lapply(unique(dt$scenario), subnatlRidgePlot, dt=dt, var='cost_iu')
    dev.off()
    
    #mapping
    #create a manual discrete color scale for the continuous costs
    dt$cost_bin <- cut(dt$cost_iu, breaks = c(0,.25,.5,.75,1,2.5,5,100), include.lowest = TRUE)
    bin_colors <- viridis::magma(n = 8)
    names(bin_colors) <- unique(dt$cost_bin)

    #facet by scenario for one year
    cartographeR(shapefile=espen_iu_shp,
                 borders=espen_admin0_shp,
                 dt=dt[year_id==2022], 
                 map_varname = 'cost_bin', map_label = 'Delivery \nCost',
                 map_title = '',
                 scale_type='cont_man', 
                 scale_vals=bin_colors,
                 facet_var = 'scenario')
    
    #facet by year for one scenario
    cartographeR(shapefile=espen_iu_shp,
                 borders=espen_admin0_shp,
                 dt=dt[scenario=='1_KK2'&year_id%in%c(2022,2030,2040)], 
                 map_varname = 'cost_bin', map_label = 'Delivery \nCost',
                 map_title = '',
                 scale_type='cont_man', 
                 scale_vals=bin_colors,
                 facet_var = 'year_id')
    
    #mapping just one scenario
    cartographeR(shapefile=espen_iu_shp,
                 borders=espen_admin0_shp,
                 dt=dt[scenario=='1_KK2'&year_id==2022], 
                 map_varname = 'cost_bin', map_label = 'Delivery \nCost',
                 map_title = '',
                 scale_type='cont_man', 
                 scale_vals=bin_colors)
    
  }
  
  return(dt)
    
}

#***********************************************************************************************************************