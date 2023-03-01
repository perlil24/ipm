# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: JF
# Date: 02/15/2023
# Purpose: Shiny plot to test the Fitzpatrick model
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
pacman::p_load(shiny, ggplot2)

#***********************************************************************************************************************

# ----IN/OUT------------------------------------------------------------------------------------------------------------
###Input###
code_dir <- file.path(my_repo, 'code')

#TODO consider which data is universal and which is by disease. How to organize
model_data_dir <- file.path(my_dropbox, '/1 - Data gathering - all diseases/Costs/Delivery costs/')
coef_path <- "Delivery_cost_regression_coefficients_to_use_7_12_2022.csv" #TODO rename

#ihme data
ihme_version <- '2022-09-13'
ihme_data_dir <- file.path(my_dropbox, 'IHME', ihme_version)
pop_path <- 'IU_pop_density.csv'


#TODO for now we are just doing this for LF. in the future should be a function with disease type as an argument
lf_version <- '2022-08-26'
lf_data_dir <- file.path(my_dropbox, paste0('LF/InputData ', lf_version))
lf_path <- 'MDA_costs_22_08_2022_AM.csv'

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
  quantile(x, 0.25, na.rm=T) - 1.5 * IQR(x, na.rm=T) | x > quantile(x, 0.75, na.rm=T) + 1.5 * IQR(x, na.rm=T)
}

#***********************************************************************************************************************

# ---PREP DATA----------------------------------------------------------------------------------------------------------
##read in and prep datasets for analysis##
#combine all the core datasets

#population dataset which will spread things out to be at IUs
pop_dt <- file.path(ihme_data_dir, pop_path) %>% 
  fread %>% 
  setnames(., names(.), names(.) %>% tolower)

#lf dataset which is disease/scenario specific
lf_dt <- file.path(lf_data_dir, lf_path) %>% 
  fread %>% 
  setnames(., names(.), names(.) %>% tolower) %>%  #force names to lowercase for consistency 
  #impute the IMP decision constants
  #TODO, validate these assumptions and eventually put them all into an assumption sheet to merge on?
  .[, eco := 0] %>% 
  .[, vol := 0] %>% 
  .[, int := 1] %>% 
  .[, rds := 1] %>%  #TODO should be scenario specific but it's not present
  .[, yrs := 5] %>% 
  .[, nat := 1] %>%  #TODO could invert subnat but its the same assumption
  .[, sch := 1] %>% 
  .[, pop := str_replace_all(pop, ',', '') %>% as.numeric] %>% #convert from chr to numerics
  .[, gdp := str_replace_all(gdp, ',', '') %>% as.numeric] %>% #convert from chr to numerics
  #TODO not clear what to do with unit percentages...not given as decimals in table 2
  .[, cov := str_replace(coverage, '%', '') %>% as.numeric]

#merge with pop data to breakdown into IUs
setnames(lf_dt, c('pop', 'pop_density'), c('pop_iso', 'pop_density_iso'))
lf_dt <- merge(lf_dt, pop_dt, by='iso', all.x=T, allow.cartesian = T)
setnames(lf_dt, 'adj_pop', 'pop') #TODO confirm that this is the right value
#***********************************************************************************************************************

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
         c('den'),
         c('pop_density'))

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
                      pop=rnorm(n = 1000, mean = 0, sd = 1),
                      pop_density=rnorm(n = 1000, mean = 0, sd = 1),
                      gdp=rnorm(n = 1000, mean = 0, sd = 1),
                      cost=rnorm(n = 1000, mean = 0, sd = 1))

#setup a fake lm model that will store our extracted coefs
mod <- lm(cost~eco+vol+log(int)+log(rds)+
            yrs+cov+nat+sch+log(pop)+log(pop_density)+log(gdp)+
            eco:log(int)+cov:nat+eco:sch+cov:sch+
            nat:log(pop)+nat:log(pop_density)+nat:log(gdp),
          data=fake_dt)

#then inject our literature coefficients
#TODO setup some tests to assert that model formula matches coef dt order
coef_names <- mod$coefficients %>% names #extract the names for clarity because injecting erases them
mod$coefficients <- unlist(coef_dt[1]) %>% as.numeric() #inject from the extraction dt
names(mod$coefficients) <- coef_names

#***********************************************************************************************************************

# ---SHINY APP----------------------------------------------------------------------------------------------------------
##
ui <- fluidPage(
  
  titlePanel('Applying the Fitzpatrick Model Coefficients on Simulated Data', 
             windowTitle = 'Delivery Costs Model Sandbox'),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
    radioButtons("eco",
                 label = "Economic",
                 choices = c(0,1),
                 selected = 0,
                 inline = TRUE),
    radioButtons("vol",
                 label = "Volunteers",
                 choices = c(0,1),
                 selected = 0,
                 inline = TRUE),
    radioButtons("int",
                 label = "Number of Diseases Targetted",
                 choices = c(1:5),
                 selected = 1,
                 inline = TRUE),
    radioButtons("rds",
                 label = "Number of Annual Rounds",
                 choices = c(1:5),
                 selected = 1,
                 inline = TRUE),
    radioButtons("yrs",
                 label = "Years of Implementation",
                 choices = c(1:10),
                 selected = 5,
                 inline = TRUE),
    radioButtons("nat",
                 label = "National",
                 choices = c(0,1),
                 selected = 1,
                 inline = TRUE),
    radioButtons("sch",
                 label = "Schoolbased",
                 choices = c(0,1),
                 selected = 1,
                 inline = TRUE),
    sliderInput("cov", "Coverage:",min = 10, max = 100, value = 80, step=10, textOutput("SliderText")),
    sliderInput("pop", "Pop:",
                min = min(lf_dt$pop, na.rm=T) %>% round(2), max = max(lf_dt$pop, na.rm=T) %>% round(2), 
                value = c(100,200), 
                step=.1, textOutput("SliderText")),
    sliderInput("pop_density", "Pop Density:", 
                min = .1, max=100,
                value = c(1,2000), 
                step=.1, textOutput("SliderText")),
    sliderInput("gdp", "GDP:", 
                min = min(lf_dt$gdp, na.rm=T) %>% round(2), max = max(lf_dt$gdp, na.rm=T) %>% round(2), 
                value = c(500,2000),
                step=10, textOutput("SliderText"))
    ),
  
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = "distPlot")
    )
  
))

server <- function(input, output) {
  
  #whenever the sliders change, regenerate the dataset
  toListen <- reactive({
    list(input$eco,input$vol, input$int, input$rds,
         input$yrs,input$cov, input$nat, input$sch,
         input$pop,input$pop_density, input$gdp)
  })

  observeEvent(toListen(), {

    #change the dataset
    pred_dt <- data.table(eco=rep(input$eco %>% as.numeric, 1000),
                          vol=rep(input$vol %>% as.numeric, 1000),
                          int=rep(input$int %>% as.numeric, 1000),
                          rds=rep(input$rds %>% as.numeric, 1000),
                          yrs=rep(input$yrs %>% as.numeric, 1000),
                          cov=rep(input$cov %>% as.numeric, 1000),
                          nat=rep(input$nat %>% as.numeric, 1000),
                          sch=rep(input$sch %>% as.numeric, 1000),
                          pop=runif(1000, input$pop[1], input$pop[2]),
                          pop_density=runif(1000, input$pop_density[1], input$pop_density[2]),
                          gdp=runif(1000, input$gdp[1], input$gdp[2])
                          )
    
    str(pred_dt) %>% print
    
    pred_dt[, cost := predict(mod, newdata=pred_dt) %>% exp]

    # Expression that generates a histogram. The expression is
    # wrapped in a call to renderPlot to indicate that:
    #
    #  1) It is "reactive" and therefore should be automatically
    #     re-executed when inputs change
    #  2) Its output type is a plot
    
    output$distPlot <- renderPlot({
      
      ggplot(pred_dt, aes(x=cost)) +
        geom_density(fill="darkseagreen", color="black", alpha=0.8) +
        ggtitle("Delivery Cost Distributions") +
        scale_x_log10('Delivery Cost', limits=c(0.01, 2000))+
        theme_minimal()
      
    })
    
  })
    
}

shinyApp(ui = ui, server = server,
         options = list(height = 500))