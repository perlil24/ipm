# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 07/26/2022
# Purpose: Store functions used for mapping and visualizations
# source("/homes/jfrostad/_code/ehd_mapsense/_lib/viz_fx.R", echo=T)
#***********************************************************************************************************************

# ----FUNCTIONS---------------------------------------------------------------------------------------------------------
#function to create custom maps 
cartographeR <- function(shapefile, 
                         borders,
                         geo_var='iu_id',
                         dt,
                         map_varname, map_label=NA, map_title=NA, tag=NA, #use tag to label files
                         subset_var=NA, subset_val=F,
                         facet_var=F,
                         filter_geocodes=F,
                         scale_type='cont',
                         scale_vals=NULL,
                         viz_dir=parent.frame()$viz_dir, 
                         get_plot=F) {

  #if necessary, subset data
  if(subset_var %>% is.character) dt <- dt[get(subset_var)==subset_val]
  
  #make variable to plot
  if(scale_type%like%'cont'&!(scale_type%like%'man')) dt[, map_var := get(map_varname)] 
  else dt[, map_var := get(map_varname) %>% as.factor]

  #cleanup geocodes if needed
  if(filter_geocodes %>% is.character) dt <- dt[!(get(geo_var) %in% filter_geocodes)]

  #merge dropouts to shapefile and plot
  shp <- shapefile %>% 
    merge(dt, by=geo_var, allow.cartesian=T) 

  #make plot
  plot <- ggplot() + 
    geom_sf(data = shp, aes(fill = map_var), lwd=0) + 
    geom_sf(data = borders, 
            color = "black",
            linewidth = 0.1,
            fill = NA) +
    theme_void() 
    #geom_sf(data = road_sf, lwd=.25, color = 'gray20', fill = 'gray95', show.legend = 'line') +
    #geom_sf(data = water_sf, lwd=0, color = 'gray70', fill = 'gray95') +
    #TODO seems to make the map tilted even though this is the NAD83 proj?
    #coord_sf(crs=4269) 
  
  #facet by theme if needed
  if(facet_var %>% is.character) plot <- plot + facet_wrap(reformulate(facet_var))

  #add on the colorscale
  #TODO could probably just make this reflexive based on input data

  if(scale_type=='cont_log') { 
    plot <- plot + scale_fill_viridis_c(map_label, option='magma', na.value = "grey75", 
                                        trans = scales::pseudo_log_trans(sigma = 0.001))
  } else if(scale_type=='cont_man') plot <- plot + scale_fill_manual(map_label, values=scale_vals, na.value = "grey75")
  else if(scale_type=='cont') plot <- plot + scale_fill_viridis_c(map_label, option='magma', na.value = "grey75")
  else if(scale_type=='cont_grad') plot <- plot + scale_fill_gradient2(map_label, na.value = "grey75")
  else if(scale_type=='bivar') plot <- plot + bi_scale_fill(pal='GrPink2', guide='none') + bi_theme()
    
  
  #title 
  #TODO add more functionality
  if(map_title %>% is.character & subset_var %>% is.na) plot <- plot + ggtitle(map_title)
  else if(subset_var %>% is.character  & map_title %>% is.na) plot <- plot + ggtitle(subset_name)
  else if(subset_var %>% is.character & map_title %>% is.character) plot <- plot + 
    ggtitle(paste0(map_title, ': \n', subset_name))
  
  #save the plot
  #TODO make it so you can change the output dir?
  file.path(viz_dir, paste0(map_varname, '_',
                            ifelse(tag %>% is.character,
                                   tag,
                                   ''),
                            ifelse(subset_var %>% is.character,
                                   subset_name,
                                   ''),
                            ifelse(facet_var %>% is.character,
                                   paste0('_by_', facet_var),
                                   ''),
                            '_map.png')
  ) %>% ggsave(height=8, width=12)
  
  if(get_plot) return(plot)
  
}

#***********************************************************************************************************************

# ---PLOT FX------------------------------------------------------------------------------------------------------------
#plotting/mapping functions

#check out the distributions of natl values and then the subnational ones by country
natlDensPlot <- function(this_iso, var, fillvar, dt) {
  
  message(this_isoiso, ': plotting density of ', var, '/n by ', fillvar)
  
  plot <-
    ggplot(dt[iso%in%this_iso], aes_string(x=var, fill=fillvar)) +
    geom_density(alpha=0.6) +
    scale_fill_viridis_d(option='viridis') +
    ggtitle("Delivery Cost Distributions") +
    theme_minimal() +
    facet_wrap(~year_id)
  
  print(plot)
  
}

subnatlRidgePlot <- function(this_scenario, var, years=c(2022, 2030, 2040), dt) {
  message(var)
  ggplot(dt[year_id%in%years & scenario==this_scenario], aes(x = .data[[var]], 
                                                             y = forcats::fct_reorder(.data[['iso']],
                                                                                      .data[[var]], 
                                                                                      .fun=median),
                                                             fill = 0.5 - abs(0.5 - stat(ecdf)))) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE)  +
    scale_fill_viridis_c(name = "Tail probability", direction = -1) +
    scale_x_log10()+
    scale_y_discrete('')+
    facet_wrap(~year_id)+
    ggtitle("Delivery Cost Distributions by Country: ", this_scenario) +
    theme_minimal()
}

#***********************************************************************************************************************
 
# ---SCRAP -------------------------------------------------------------------------------------------------------------

#***********************************************************************************************************************