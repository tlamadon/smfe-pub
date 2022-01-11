

# --- SETTING COUNTRY OPTIONS ----

#' get valid suffix set for a given country
#' @export
Do.Country.Suffix <- function(country){

  # defaults
  suffix.set <- list()
  suffix.set$AKM <- c("_connected","_leaveout")
  suffix.set$CRE <- c("_connected_clusters10","_all_clusters10","_leaveout_clusters10")
  suffix.set$TraceHO <- c("_connected","_leaveout")
  suffix.set$TraceHE <- c("_leaveout")
  suffix.set$interacted <- NA # c("_connected_clusters10")

  # US
  if(substr(country,1,3)=='US-'){
    if(country %in% c('US-early','US-late')){
      suffix.set$AKM <- c("_connected","_leaveout")
      #suffix.set$CRE <- c("_connected_clusters10","_all_clusters10",paste0("_connected_clusters",2:5*10))
      suffix.set$CRE <- c("_connected_clusters10","_leaveout_clusters10")
      suffix.set$TraceHO <- NA
      suffix.set$TraceHE <- NA
      suffix.set$interacted <- NA #c("_connected_clusters10")
    }
    if(country %in% c('US-early-noendpoints','US-late-noendpoints')){
      suffix.set$AKM <- c("_connected")
      suffix.set$CRE <- c("_connected_clusters10")
      suffix.set$TraceHO <- NA
      suffix.set$TraceHE <- NA
      suffix.set$interacted <- NA
    }
    if(grepl("minearn",country) | grepl("minfirmsize",country)){
      suffix.set$AKM <- c("_connected")
      suffix.set$CRE <- c("_connected_clusters10")
      suffix.set$TraceHO <- c("_connected")
      suffix.set$TraceHE <- NA
      suffix.set$interacted <- NA
    }
    # for states, use defaults
  }


  # AT, IT, IT_Kline
  if((country=='AT') | (country=='IT') | (country=='IT_Kline')){
    # use defaults
  }

  # Sweden
  if((country=='SW')){
    # use defaults
  }

  # testing code on Brad's laptop
  if(country=='Testing-Brad'){
    # use defaults
  }

  if(grepl('educgender',country)){
    suffix.set$CRE <- c(suffix.set$CRE,"_connected_clusters30")
  }

  return(suffix.set)

}


#' get valid directory paths for a given country
#' @export
Do.Country.Paths <- function(country){

  # defaults
  paths <- list()

  # US--------------------------------------------------
  if (substr(country,1,3)=='US-'){
    paths$raw <- "~/manageCDW/raw_data/"
    if(country %in% c('US-early','US-late','US-early-noendpoints','US-late-noendpoints')){
      library(akm)
      library(paneltools)
      library(parallel)
      dir.create(file.path("~/bhlmms/national"), showWarnings = FALSE)
      paths$data <- sprintf("~/bhlmms/national/%s/jdata_sdata/",country)
      paths$res <- sprintf("~/bhlmms/national/%s/results/",country)
      paths$final <- "~/bhlmms/national/final-output/"
      paths$attrition <- sprintf("~/bhlmms/national/%s/attrition/",country)
      dir.create(file.path(sprintf("~/bhlmms/national/%s",country)), showWarnings = FALSE)
    } else {
      dir.create(file.path("~/bhlmms/states"), showWarnings = FALSE)
      paths$data <- sprintf("~/bhlmms/states/%s/jdata_sdata/",country)
      paths$res <- sprintf("~/bhlmms/states/%s/results/",country)
      paths$final <- "~/bhlmms/states/final-output/"
      paths$attrition <- sprintf("~/bhlmms/states/%s/results/",country)
      dir.create(file.path(sprintf("~/bhlmms/states/%s",country)), showWarnings = FALSE)
    }
  }

  # AT, IT, ES-------------------------------------------------------
  if ((country %in% c('ES')) | ((grepl('AT', country))) | ((grepl('IT', country)))){
      if (!(country %in% c('AT', 'IT'))) {
      setwd(path)
      proj_path=dirname(dirname(path))
      paths$samples  = paste0(proj_path, "/Data_", substr(country,1,2), "/Samples/")
      dir.create(paths$samples, showWarnings = FALSE)
      paths$data <- paste0(paths$samples,'smfe-res-',country, '/')
      paths$res  = paste0(proj_path, "/Results/", substr(country,1,2), "/")
      paths$raw  = paste0(proj_path, "/Data_", substr(country,1,2), "/")
      dir.create(paths$data, showWarnings = FALSE)
    } else {
      setwd(path)
      proj_path=dirname(dirname(path))
      paths$connectivity  = paste0(proj_path, "/Data_", substr(country,1,2), "/Yearly/")
      paths$res  = paste0(proj_path, "/Results/", country, "/")
      paths$data  = paste0(proj_path, "/Data_", country, "/")
      paths$raw  = paste0(proj_path, "/Data_", country, "/")
    }
      if ((grepl('IT', country)) & (grepl('ITKline', country)==0) ){
            paths$raw<-paste0(proj_path, "/Data_IT/1985_2001_veneto_only.dta")
      } else if ((grepl('ITKline', country))) {
           if (grepl('6', country)) {
              paths$raw<-paste0(proj_path, "/Data_IT/All_Veneto_1996and2001_T_greater_2.dta") #
           } else {
            paths$raw<-paste0(proj_path, "/Data_IT/All_Veneto_1999and2001_T_equal_2.dta") #
           }
      } else if ((grepl('AT', country))) {
            paths$raw<-paste0(proj_path, "/Data_AT/Raw/anadata.dta")
      } else if (country =="ES") {
            paths$raw<-paste0(paths$data, "mcvl_cleaned.dta")
      }
    paths$prog  = paste0(proj_path, "/smfe//smfe/R/") #where the programs are in
    paths$call  = paste0(proj_path, "/smfe/smfe/inst/server-scripts/") # where the run script is in
    paths$pkg  = paste0(proj_path, "/Packages/") #where the packages are stored
    paths$files  = paste0(proj_path, "/Files/") #where the packages are being installed in
    paths$final = paste0(proj_path, "/smfe/smfe/inst/local-res/")
    paths$attrition=paths$res
  }

  # IT, tibo working in a different folder --------------------
  if (substr(country,1,2) == 'IY'){
    proj_path = "~/git"
    paths$samples  = paste0(proj_path, "/Data_", substr(country,1,2), "/Samples/")
    paths$res   = paste0(proj_path, "/Results/", substr(country,1,2), "/")
    paths$raw   = paste0("~/shared/smfe/data.dta")
    paths$prog  = paste0(proj_path, "/smfe/smfe/R/") #where the programs are in
    paths$call  = paste0(proj_path, "/smfe/smfe/inst/server-scripts/") # where the run script is in
    paths$final = paste0(proj_path, "/smfe/smfe/inst/local-res/")

    paths$data <- paste0(proj_path,'/smfe-res-',country, '/')
    paths$attrition=paths$res

    dir.create(paths$data, showWarnings = FALSE)
    dir.create(paths$samples, showWarnings = FALSE)
  }

  # SW------------------------------------------------------------------------
  if (substr(country,1,2) == "SW") {
#     library(akm)
    paths$data  <- sprintf("../smfe-res-%s/",country)
    paths$res   <- sprintf("../smfe-res-%s/",country)
    paths$final <- sprintf("../smfe-res-%s/",country)
    paths$attrition   <- "../tmp/"
  }

  # Sweden for attrition
  if (country=='SW-attrition'){
#     library(akm)
    paths$data  <- "../tmp2/data/"
    paths$res   <- "../tmp2/tmp/"
    paths$final <- "../tmp2/final/"
    paths$attrition <- "../tmp2/attrition/"
  }

  # NO------------------------------------------------------------------------
  if (country=='NO'){
    paths$data  <- "/ssb/stamme01/swp/lae/wk48/labor/temp/"
    paths$res   <- "/ssb/stamme01/swp/lae/wk48/labor/estimates/"
    paths$final <- "/ssb/stamme01/swp/lae/wk48/labor/results/"
    paths$attrition <- "/ssb/stamme01/swp/lae/wk48/labor/attrition/"
  }
  if (country=='NO3thresh'){
    dir.create(file.path("/ssb/stamme01/swp/lae/wk48/labor/NO3thresh/"), showWarnings = FALSE)
    paths$data  <- "/ssb/stamme01/swp/lae/wk48/labor/NO3thresh/temp/"
    paths$res   <- "/ssb/stamme01/swp/lae/wk48/labor/NO3thresh/estimates/"
    paths$final <- "/ssb/stamme01/swp/lae/wk48/labor/NO3thresh/results/"
    paths$attrition <- "/ssb/stamme01/swp/lae/wk48/labor/NO3thresh/attrition/"
  }
  if (country=='NO-hourlywage'){
    paths$data  <- "/ssb/stamme01/swp/lae/wk48/labor/hourlywage/temp/"
    paths$res   <- "/ssb/stamme01/swp/lae/wk48/labor/hourlywage/estimates/"
    paths$final <- "/ssb/stamme01/swp/lae/wk48/labor/hourlywage/results/"
    paths$attrition <- "/ssb/stamme01/swp/lae/wk48/labor/hourlywage/attrition/"
  }
  if (country=='NO3thresh-hourlywage'){
    dir.create(file.path("/ssb/stamme01/swp/lae/wk48/labor/NO3thresh_hourlywage/"), showWarnings = FALSE)
    paths$data  <- "/ssb/stamme01/swp/lae/wk48/labor/NO3thresh_hourlywage/temp/"
    paths$res   <- "/ssb/stamme01/swp/lae/wk48/labor/NO3thresh_hourlywage/estimates/"
    paths$final <- "/ssb/stamme01/swp/lae/wk48/labor/NO3thresh_hourlywage/results/"
    paths$attrition <- "/ssb/stamme01/swp/lae/wk48/labor/NO3thresh_hourlywage/attrition/"
  }
  if (country=='NO6thresh-annualearnings'){
    paths$data  <- "/ssb/stamme01/swp/lae/wk48/labor/NO6thresh_annualearnings/temp/"
    paths$res   <- "/ssb/stamme01/swp/lae/wk48/labor/NO6thresh_annualearnings/estimates/"
    paths$final <- "/ssb/stamme01/swp/lae/wk48/labor/NO6thresh_annualearnings/results/"
    paths$attrition <- "/ssb/stamme01/swp/lae/wk48/labor/NO6thresh_annualearnings/attrition/"
  }
  if (country=='NO3thresh-annualearnings'){
    dir.create(file.path("/ssb/stamme01/swp/lae/wk48/labor/NO3thresh_annualearnings/"), showWarnings = FALSE)
    paths$data  <- "/ssb/stamme01/swp/lae/wk48/labor/NO3thresh_annualearnings/temp/"
    paths$res   <- "/ssb/stamme01/swp/lae/wk48/labor/NO3thresh_annualearnings/estimates/"
    paths$final <- "/ssb/stamme01/swp/lae/wk48/labor/NO3thresh_annualearnings/results/"
    paths$attrition <- "/ssb/stamme01/swp/lae/wk48/labor/NO3thresh_annualearnings/attrition/"
  }


  if (country=='NO6brad-annualearnings'){
    paths$data  <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings/jdata_sdata/"
    paths$res   <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings/intermediate_output/"
    paths$final <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings/final_output/"
    paths$attrition <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings/attrition/"
  }

  if (country=='NO6brad-annualearnings-educgender'){
    paths$data  <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_educgender/jdata_sdata/"
    paths$res   <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_educgender/intermediate_output/"
    paths$final <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_educgender/final_output/"
    paths$attrition <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_educgender/attrition/"
  }
  if (country=='NO6brad-annualearnings-educindustry'){
    paths$data  <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_educindustry/jdata_sdata/"
    paths$res   <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_educindustry/intermediate_output/"
    paths$final <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_educindustry/final_output/"
    paths$attrition <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_educindustry/attrition/"
  }

  if (country=='NO6brad-annualearnings-industry'){
    paths$data  <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_industry/jdata_sdata/"
    paths$res   <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_industry/intermediate_output/"
    paths$final <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_industry/final_output/"
    paths$attrition <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_industry/attrition/"
  }
  if (country=='NO6brad-annualearnings-educ'){
    paths$data  <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_educ/jdata_sdata/"
    paths$res   <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_educ/intermediate_output/"
    paths$final <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_educ/final_output/"
    paths$attrition <- "/ssb/stamme01/swp/bjs/wk24/private/BHLMMS/NO6brad_annualearnings_educ/attrition/"
  }





  #DE----------------------------------------------------------------
    if ((country=='DE') | (country=='DE-test')) {
    if (country=='DE'){
    paths$res  = "H:/log/"
    paths$pkg  = "//N0016017/usr/public/R-Packages/3.4/" #local pkg depository
    paths$files  = "H:/prog/"      #where to install the pkgs
    paths$call  = opts$paths$files                     #where the scripts (03_Main.R) are
    paths$prog  = opts$paths$files                     #where the function (other_functions.R) files are
    paths$data  = "H:/data/"
    paths$final=paths$res
    paths$attrition=paths$res
    }
   if (country=='DE-test'){
    setwd(path)
    proj_path=dirname(path)
    paths$res  = paste0(dirname(proj_path), "/Results/")
    paths$pkg  = paste0(dirname(proj_path), "/Packages/")
    paths$files  = paste0(dirname(proj_path), "/Files/") #where the packages are being installed in
    paths$prog  = paste0(proj_path, "/R/")
    paths$data  = paste0(dirname(proj_path), "/Data_DE/")
    paths$final=paths$res
    paths$attrition=paths$res

    }
   }

    # Testing Brad--------------------------------------------
  if (country=='Testing-Brad'|country=='Testing-Claire'){
    paths$data <- "~/Downloads/smfe-temp/jdata_sdata/"
    paths$res <- "~/Downloads/smfe-temp/results/"
    paths$final <- "~/Downloads/smfe-temp/final-output/"
    paths$attrition <- "~/Downloads/smfe-temp/attrition/"
    dir.create(file.path("~/Downloads/smfe-temp"), showWarnings = FALSE)
  }


  # make sure directories exist
  dir.create(file.path(paths$data), showWarnings = FALSE)
  dir.create(file.path(paths$res), showWarnings = FALSE)
  dir.create(file.path(paths$final), showWarnings = FALSE)
  dir.create(file.path(paths$attrition), showWarnings = FALSE)

  return(paths)

}


#' set loading parameters by country
#' @export
Do.Country.Loading <- function(country){

  # defaults
  loading <- list(data_type = 'rds',separate = FALSE)

  return(loading)

}



#' get attrition exercise options
#' @export
Do.Country.Attrition <- function(country){

  # defaults
  attrition.exercise=list()
  attrition.exercise <- list(keep.shares=1:9/10,draws=1:10,moversperfirm=15,samefirms=FALSE)
  attrition.exercise$methods=list()
  attrition.exercise$methods$connected <- c('AKM','CRE','TraceHO')
  attrition.exercise$methods$leaveout <- c('AKM','CRE','TraceHO','TraceHE')

  # US
  if(country=='US-early'|country=='US-late'){
    #default
  }


  # DE
  if(country=='DE'|country=='DE-test'){
    #default
  }

  # AT, IT
  if (country=='AT'| country=='IT' ){
    #default
  }

  # Sweden
  if (country=='SW-attrition'){
    # default
  }

  # Testing
  if(country=='Testing-Brad'){
    attrition.exercise$keep.shares <- 3/4
    attrition.exercise$draws <- 1:2
  }

  return(attrition.exercise)

}


#' get miscellaneous options
#' @export
Do.Country.Misc <- function(country){

  # defaults
  misc <- list(ncores=1,
               nstart=100,
               preserve.estimates=TRUE,
               preserve.initdata=TRUE,
               preserve.rawdata=TRUE,
               cre.sub=FALSE,
               posterior_var=TRUE,
               mover_cluster_max_share=0.1,
               data_only=FALSE,
               connectivity=FALSE,
               cluster_bothyears=FALSE,
               grouping_var='wages'
               )

  # US
  if (substr(country,1,3)=='US-'){
    if(country %in% c('US-early','US-late','US-early-noendpoints','US-late-noendpoints')){
      misc$ncores <- 4
      misc$nstart <- 30
      misc$cre.sub <- TRUE
      misc$posterior_var <- FALSE
    } else {

    }
  }

  # Testing
  if(country=='Testing-Brad'){
    misc$ncores <- 4
    misc$posterior_var <- TRUE
  }

  if(grepl('select',country)){
     year_set = sub(".*select_", "", country)
     misc$begin_year = as.integer(substr(year_set,1,4))
     misc$end_year = as.integer(substr(year_set,6,9))
  }


  if(grepl('educ',country)){
    misc$grouping_var='educ'
  }
  if(grepl('industry',country)){
    misc$grouping_var='industry'
  }
  if(grepl('educindustry',country)){
    misc$grouping_var='educindustry'
  }
  if(grepl('educgender',country)){
    misc$grouping_var='educgender'
  }


  return(misc)

}


#' initialize the options
#' @export
#' @param country Country
Do.Init.Opts <- function(country){

  opts <- list()
  opts$country <- country
  opts$paths <- Do.Country.Paths(country)
  opts$loading <- Do.Country.Loading(country)
  opts$suffix.set <- Do.Country.Suffix(country)
  opts$attrition.exercise <- Do.Country.Attrition(country)
  opts$misc <- Do.Country.Misc(country)

  return(opts)
}



