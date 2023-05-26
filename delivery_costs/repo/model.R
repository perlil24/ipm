# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 01/18/2023
# Purpose: Run a new version of the benchmarking model
# source("/homes/jfrostad/_code/start/ipm/delivery_costs/calc.R", echo=T)
#***********************************************************************************************************************
 
#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
data_dir <- file.path(local_dir, 'data')
###Output###
model_data_dir <- file.path(my_dropbox, '/1 - Data gathering - all diseases/Costs/Delivery costs/')
#***********************************************************************************************************************
 
# ---RUN MODEL----------------------------------------------------------------------------------------------------------
##build a new model for IU level predictions

#raw central model objs received from fitzpatrick via email
#load(file.path(data_dir, 'benchmark.Rdata'), verbose=T)

#first read in the fitzpatrick data, received from the author as RData and saved by JF into a csv in our dir
mod_dt <- file.path(data_dir, 'benchmark_data.csv') %>% 
    fread %>% 
  .[, pop_treated := pop] %>%  #set to match our naming convention
  .[, pop_density := den] %>%  #set to match our naming convention
  .[, cov := cov/100] #treat as a percent 

#rerun the fitzpatrick model
og_mod <- lmer(log(ucb)~eco+vol+log(int)+log(rds)+
              yrs+cov+nat+sch+log(pop_treated)+log(den)+log(gdp)+
              VUT+Kri+Fri+Mon+
              eco:log(int)+cov:nat+eco:sch+cov:sch+
              nat:log(pop_treated)+nat:log(den)+nat:log(gdp) +(1|study_ref),
            data=mod_dt)

#run our version of the model
mod <- lmer(log(ucb)~eco+vol+log(int)+log(rds)+yrs+car::logit(cov)+sch+
              log(pop_treated)*log(pop_density)+log(gdp)+
              VUT+Kri+Fri+Mon+
              eco:log(int)+eco:sch+sch:car::logit(cov)+
              (1|study_ref),
            data=mod_dt[nat==0]) #decided to only use subnational data since we make subnational preds

#output the model results
stargazer(og_mod, mod,
          type='html') %>%
  capture.output(file=file.path(data_dir, 'table_1.html'))

#output the model object
#also save a csv of the lite data for edmund
out <- list(
  'mod'=mod,
  'mod_dt'=mod_dt
)
saveRDS(out, file=file.path(model_data_dir, 'benchmark_model.RDS'))
#***********************************************************************************************************************

# ---MOD DIAGNOSTICS----------------------------------------------------------------------------------------------------
#diagnostics used to test and build the benchmarking model
if(examine_model) {
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
}