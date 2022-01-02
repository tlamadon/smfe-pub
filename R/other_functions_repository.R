#This file includes all wrapper functions called during the Estimation from Estimate_Data.R


# --- INSTALLING AND CALLING PACKAGES ----

#' Get package dependencies
#' @param packs A string vector of package names
#' @return A string vector with packs together with names of any dependencies
#' @export
Do.Dependencies <- function(packs){
  dependencyNames <- unlist(
    tools::package_dependencies(packages = packs, db = available.packages(),
                                which = c("Depends", "Imports"),
                                recursive = TRUE))
  packageNames <- union(packs, dependencyNames)
  packageNames
}



#' Make package repository
#' @param directory_Pkg A string vector of name directory_Pkg: where packages are to be stored
#' @return A string vector with packs
#' @export
Do.pkg.repository <- function(directory_Pkg) {
  #Script to download all packages to a local repository to install it later

  # Calculate dependencies
  sysd<-.Platform$pkgType

  pkgs1<-c("spData", "e1071", "classInt", "labelled",  "questionr", "devtools", "futile.logger", "knitr", "kableExtra", "data.table", "reshape2", "plyr", "stringr", "ggm", "FactoClass", "questionr", "gtools", "sparsesvd", "SparseM", "SDMTools", "classInt", "questionr") # pkgs1<-c("devtools", "kableExtra", "roxygen2", "knitr")
  packages_rblm=c("Matrix","SparseM", "gtools", "reshape", "splines","quantreg",
                  "ggplot2", "nnls", "data.table", "FactoClass", "SDMTools", "lpSolve",
                  "reshape2", "stringr", "limSolve", "MASS", "quadprog",
                  "gridExtra", "foreign", "futile.logger", "Hmisc",
                  "plyr", "Rmpi", "snow","tidyr")
  #On Linux, xclip required for questionr
  if (sysd=="source") {
    pkgsl<-c(pkgs1, "xclip")
  }

  packages_all=c(pkgs1, packages_rblm)
  packages_all=unique(packages_all)
  packages <- Do.Dependencies(packages_all)

  #Download the packages  and save csv File that contains name of them
  setwd(directory_Pkg)
  pkgInfo <- download.packages(pkgs = packages, destdir = getwd(), type = sysd)
  write.csv(file = "pkgFilenames.csv", basename(pkgInfo[, 2]), row.names = FALSE)
}

#' Read names of required packages: used whenever csv file cannot be read
#'
#' @return A string vector with packs
#' @export
Do.names.pkgs<-function() {
  pks<-c("devtools_1.13.5.zip","futile.logger_1.4.3.zip","knitr_1.20.zip","kableExtra_0.9.0.zip","data.table_1.11.4.zip",
         "reshape2_1.4.3.zip","plyr_1.8.4.zip","stringr_1.3.1.zip","ggm_2.3.zip","FactoClass_1.2.4.zip","questionr_0.6.2.zip",
         "gtools_3.5.0.zip","sparsesvd_0.1-4.zip","SparseM_1.77.zip","SDMTools_1.1-221.zip","Matrix_1.2-14.zip","reshape_0.8.7.zip",
         "quantreg_5.36.zip","ggplot2_2.2.1.zip","nnls_1.4.zip","lpSolve_5.6.13.zip","limSolve_1.5.5.3.zip","MASS_7.3-49.zip",
         "quadprog_1.5-5.zip","gridExtra_2.3.zip","foreign_0.8-70.zip","Hmisc_4.1-1.zip","snow_0.4-2.zip",
         "httr_1.3.1.zip","memoise_1.1.0.zip","whisker_0.3-2.zip","digest_0.6.15.zip","rstudioapi_0.7.zip","jsonlite_1.5.zip",
         "git2r_0.21.0.zip","withr_2.1.2.zip","mime_0.5.zip","curl_3.2.zip","openssl_1.0.1.zip","R6_2.2.2.zip","lambda.r_1.2.3.zip",
         "futile.options_1.0.1.zip","formatR_1.5.zip","evaluate_0.10.1.zip","highr_0.6.zip","markdown_0.8.zip","yaml_2.1.19.zip",
         "glue_1.2.0.zip","magrittr_1.5.zip","stringi_1.1.7.zip","xml2_1.2.0.zip","rvest_0.3.2.zip","rmarkdown_1.9.zip",
         "readr_1.1.1.zip","scales_0.5.0.zip","viridisLite_0.3.0.zip","htmltools_0.3.6.zip","Rcpp_0.12.17.zip","tibble_1.4.2.zip",
         "hms_0.4.2.zip","base64enc_0.1-3.zip","rprojroot_1.3-2.zip","selectr_0.4-1.zip","RColorBrewer_1.1-2.zip","dichromat_2.0-0.zip",
         "munsell_0.4.3.zip","labeling_0.3.zip","pkgconfig_2.0.1.zip","rlang_0.2.1.zip","colorspace_1.3-2.zip","backports_1.1.2.zip",
         "cli_1.0.0.zip","crayon_1.3.4.zip","pillar_1.2.3.zip","assertthat_0.2.0.zip","utf8_1.1.4.zip","igraph_1.2.1.zip",
         "lattice_0.20-35.zip","ade4_1.7-11.zip","ggrepel_0.8.0.zip","xtable_1.8-2.zip","scatterplot3d_0.3-41.zip",
         "KernSmooth_2.23-15.zip","gtable_0.2.0.zip","lazyeval_0.2.1.zip","shiny_1.1.0.zip","miniUI_0.1.1.1.zip",
         "classInt_0.2-3.zip","labelled_1.1.0.zip","spData_0.2.8.3.zip","e1071_1.6-8.zip","class_7.3-14.zip","haven_1.1.1.zip",
         "dplyr_0.7.5.zip","httpuv_1.4.3.zip","sourcetools_0.1.7.zip","later_0.7.2.zip","promises_1.0.1.zip",
         "bindrcpp_0.2.2.zip","tidyselect_0.2.4.zip","forcats_0.3.0.zip","bindr_0.1.1.zip","purrr_0.2.5.zip",
         "R.utils_2.6.0.zip","R.oo_1.22.0.zip","R.methodsS3_1.7.1.zip","MatrixModels_0.4-1.zip","survival_2.41-3.zip",
         "Formula_1.2-3.zip","latticeExtra_0.6-28.zip","cluster_2.0.7-1.zip","rpart_4.1-13.zip","nnet_7.3-12.zip",
         "acepack_1.4.1.zip","htmlTable_1.12.zip","viridis_0.5.1.zip","checkmate_1.8.5.zip","htmlwidgets_1.2.zip",
         "withr_2.1.0.zip", "igraph_1.1.2.zip", "ade4_1.7-8.zip", "tidyr_0.8.1.zip", "zoo_1.8-6.tar.gz", "sandwich_2.5-1.tar.gz",
         "lfe_2.8-3.tar.gz", "mltools_0.3.5.tar.gz", "akm_1.0.tar.gz","sparseutils_1.0.tar.gz", "haven1.1.0.tar.gz", "RSpectra_0.16-0.tar.gz" )
  #"Rmpi_0.6-7.zip"
  #
}

#' Install packages locally
#' @param directory_Temp Unpacking directory
#' @param directory_Pkg A string vector of name directory_Pkg
#' @param directory_Lib Package Library folder
#' @param with_file 1: use csv file with pkgs name, otherwise read from list
#' @export
Do.install.from.local<-function(directory_Temp, directory_Pkg, directory_Lib,  with_file ) {
  sysd<-.Platform$pkgType

  # Set working directory to the location of the package files
  Sys.setenv(TMPDIR=directory_Temp)

  # Read the package filenames and install
  if (with_file==1) {
    #from csv file
    pks <- read.csv("pkgFilenames.csv", stringsAsFactors = FALSE)[, 1]
  } else if (with_file==0) {
    #from R code with package names, to avoid writing to file  as advised
    pks<-Do.names.pkgs()
  }

  #Get the current version of the package in the local depository
  #get pks name without version #
  pks_short<-substr(pks,1, regexpr("_",pks)-1)
  setwd(directory_Pkg)
  pks_vers<-list.files(directory_Pkg)
  pks_vers_short<-substr(pks_vers,1, regexpr("_",pks_vers)-1)
  #find version number in directory for each package
  pks_in<-pks_short[1]
  pks_ni<-1
  for (pks_n in 1:length(pks_short) ) {
    if (is.na(match(pks_short[pks_n],pks_vers_short ))==0)  {
      pks_in[pks_ni]<-pks_vers[match(pks_short[pks_n],pks_vers_short )]
      pks_ni<-pks_ni+1
    }
  }
  pkgFilenames<-paste0(directory_Pkg, pks_in)

  #Install
  temp_com<-paste0("TMPDIR=", directory_Temp)
  install.packages(pkgFilenames, repos = NULL, type = sysd, configure.vars=temp_com, lib=directory_Lib)

}

#' Loading packages
#' @param directory_Prog Program directory
#' @param do_cluster Using clusters
#' @param directory_Lib Package Library folder
#' @export
Do.loading.pkgs<-function(directory_Prog,directory_Pkg, directory_Lib,do_cluster ) {

  #Loading Packages
  pks<-Do.names.pkgs()

  pks_short<-substr(pks,1, regexpr("_",pks)-1)
  setwd(directory_Pkg)
  pks_vers<-list.files(directory_Pkg)
  pks_vers_short<-substr(pks_vers,1, regexpr("_",pks_vers)-1)
  #find version number in directory for each package
  pks_in<-pks_short[1]
  pks_ni<-1
  for (pks_n in 1:length(pks_short) ) {
    if (is.na(match(pks_short[pks_n],pks_vers_short ))==0)  {
      pks_in[pks_ni]<-pks_vers[match(pks_short[pks_n],pks_vers_short )]
      pks_ni<-pks_ni+1
    }
  }

  #Load
  sapply(pks_short, require, character.only = TRUE,lib.loc=directory_Lib )

  #Loading BLM by sourcing code
  setwd(directory_Prog)
  file.sources = list.files(pattern="*.R")
  file.sources.other = list.files(pattern="0")
  file.sources=file.sources[!(file.sources %in% file.sources.other) & !(file.sources %in% c("formatR", "Iabbp_Read.do"))]
  file.sources=c(file.sources, "m2-mixt.r")
  file.other=list.files(pattern="master")
  file.sources=file.sources[!(file.sources %in% file.other)]
  sapply(file.sources,source,.GlobalEnv)

  #  setwd(directory_Prog)
  #  file.sources = list.files(pattern="*.r")
  #  sapply(file.sources,source,.GlobalEnv)

    require(parallel, lib.loc=directory_Lib)
  if (do_cluster==1) {
    library(Rmpi) }

  #in DE sometimes probelamtic
  require(ggm, lib.loc=directory_Lib) # problem in DE comp
  require(FactoClass, lib.loc=directory_Lib) # problem in DE comp
  require(futile.logger, lib.loc=directory_Lib) # problem in DE comp
  #require(akm, lib.loc=directory_Lib)  #not possible in DE
  require(tidyr, lib.loc=directory_Lib)
}

# --- READ AND SUMMARIZE DATA ----

#' Read data
#' @param directory_Data Folder with data
#' @param filtering if put floor on wages and ceiling on abs. wage growth
#' @param data_suffix "" if standard analysis "_all" if time analysis
#' @export
Do.reading.data <- function(directory_Data, filtering=0, data_suffix="",data_type="csv", other_vars=0, floor=opts$loading$floor) {
  #Read in data

  if (data_type=="csv") {
    sdata <- fread(paste0(directory_Data, "sdata", data_suffix, ".csv"), colClasses="numeric")
    jdata <- fread(paste0(directory_Data, "jdata", data_suffix, ".csv"), colClasses="numeric")
  } else {
    load(paste0(directory_Data, "sdata", data_suffix, ".dat"))
    load(paste0(directory_Data, "jdata", data_suffix, ".dat"))
  }


  #Additions and Changes
  sdata$cod_pgr<-as.character(sdata$cod_pgr) #Worker TYpe
  sdata$f1<-as.character(sdata$f1) #Firm TYpe
  sdata$f2 <- (sdata$f1)
  jdata$cod_pgr<-as.character(jdata$cod_pgr) #Worker TYpe
  jdata$f1<-as.character(jdata$f1) #Firm TYpe
  jdata$f2<-as.character(jdata$f2) #Firm TYpe
  sdata$x<-1
  jdata$x=1

  if (other_vars==0) {
    jdata = jdata[,list(cod_pgr,anno,y1,y2,f1,f2,age,serv,manuf,ret_trade,cons,edu_belowsecondary,edu_secondary,edu_tertiary)]
    sdata = sdata[,list(cod_pgr,anno,y1,y2,f1,f2,age,serv,manuf,ret_trade,cons,edu_belowsecondary,edu_secondary,edu_tertiary)]
  }

  #Firm Size
  tdata<-data.table(rbind(sdata,jdata, fill=TRUE))
  if ("N_j" %in% colnames(tdata) ){
    tdata<-  tdata[,!"N_j",with=FALSE]
    sdata<-  sdata[,!"N_j",with=FALSE]
    jdata<-  jdata[,!"N_j",with=FALSE]
  }
  Ns_dat<-tdata[,list(N_j=.N),by=.(f1, anno)]
  setkeyv(Ns_dat, c("f1", "anno"))
  jdata[Ns_dat,N_j:=i.N_j, on=c("f1","anno")]
  sdata[Ns_dat,N_j:=i.N_j, on=c("f1", "anno")]

  if (filtering==1) {
    #2) Filtering: 5 EUR
    sdata<-sdata[y1>floor][y2>floor]
    jdata<-jdata[y1>floor][y2>floor]
    #Wage change more than 1 in abs
    #    sdata<-sdata[abs(y2-y1)<1]
    #    jdata<-jdata[abs(y2-y1)<1]
  }

  #In case Do.residualize is not performed (otherwise overwritten)
  sdata$y1_l<-0
  jdata$y1_l<-0

  #Size Data Set
  Ns<-nrow(sdata)
  Nm<-nrow(jdata)
  stayer_share = sum(Ns)/(sum(Ns)+sum(Nm))

  flog.info("reading data Ns=%i, Nm=%i , stayer_share=%2f",Ns,Nm,stayer_share);

  # combine the movers and stayers, ad stands for all data:
  ad = list(sdata=sdata,jdata=jdata)



}

# Program to residualize data
#' @param ad Data
#' @param residualize If to residualize or not
#' @export
Do.residualize<-function(ad, residualize=1) {

  if (residualize==1) {

    ad$sdata$age2<-ad$sdata$age*ad$sdata$age
    ad$jdata$age2<-ad$jdata$age*ad$jdata$age
    # ad$sdata$ten2<-ad$sdata$ten*ad$sdata$ten
    #    ad$jdata$ten2<-ad$jdata$ten*ad$jdata$ten

    if (length(unique(ad$sdata$anno))==1) {
      #First Period
      lM <- lm(y1 ~ 1+age+age2, data=ad$sdata)
      ad$sdata$ry1<-ad$sdata$y1-predict(lM)
      ad$jdata$ry1<-ad$jdata$y1-predict(lM, ad$jdata)
      ad$jdata$panno<-ad$jdata$anno+1
      ad$sdata$panno<-ad$sdata$anno+1
      #Second period
      lM <- lm(y2 ~ 1+age+age2, data=ad$sdata)
      ad$sdata$ry2<-ad$sdata$y2-predict(lM)
      ad$jdata$ry2<-ad$jdata$y2-predict(lM, ad$jdata)
    } else if (length(unique(ad$sdata$anno))>1) {
      #First Period
      lM <- lm(y1 ~ factor(anno)+age+age2, data=ad$sdata)
      ad$sdata$ry1<-ad$sdata$y1-predict(lM)
      ad$jdata$ry1<-ad$jdata$y1-predict(lM, ad$jdata)
      ad$jdata$panno<-ad$jdata$anno+1
      ad$sdata$panno<-ad$sdata$anno+1
      #Second period
      lM <- lm(y2 ~ factor(anno)+age+age2, data=ad$sdata)
      ad$sdata$ry2<-ad$sdata$y2-predict(lM)
      ad$jdata$ry2<-ad$jdata$y2-predict(lM, ad$jdata)
    }
    #Name adjustement
    ad$sdata$y1_l<-ad$sdata$y1
    ad$jdata$y1_l<-ad$jdata$y1
    ad$jdata$y1<-ad$jdata$ry1
    ad$jdata$y2<-ad$jdata$ry2
    ad$sdata$y1<-ad$sdata$ry1
    ad$sdata$y2<-ad$sdata$ry2
  } else if (residualize==0) {
    ad$sdata$y1_l<-0
    ad$jdata$y1_l<-0
  }
  return(ad)
}


#' Do Event Study
#'
#' @param ad Data
#' @param reporting reporting wether to save to file or print only on screen ( & into Log file)
#' @export
Do.event.study<-function(ad, reporting, opts_min=opts$minN){
  #Event Type

  #mean wage by firm
  invisible(ad$jdata[,m_l1:=mean(y1),by=f1])
  invisible(ad$jdata[,m_l2:=mean(y2),by=f1])

  #quartiles
  ms_q    = grouping.getMeasures(ad,"ecdf",Nw=20,y_var = "y1")
  grps_q  = grouping.classify.once(ms_q,k = 4,nstart = 1000,iter.max = 200,step=250)
  ad   = grouping.append(ad,grps_q$best_cluster,drop=T)

  #summaries
  ms<-mean(rbind(ad$sdata$y1_l, ad$jdata$y1_l)) #adding left-out mean

  if (reporting==0) {
    ss2<-ad$jdata[,list("w_1"=mean(m_l1)+ms, "w_2"=mean(m_l2)+ms, "N_w"=length(unique(cod_pgr, na.rm=T)),"N_f"=length(unique(f1, na.rm=T))),by=.(j1,j2)]
  } else if (reporting!=0) {
    ss2<-ad$jdata[,list("w_1"=mean(m_l1)+ms, "w_2"=mean(m_l2)+ms),by=.(j1,j2)]
  }

  s2<-data.table(j1=rep(c(1,4),2), j2=sort(rep(c(1,4),2)))
  setkeyv(ss2,c("j1","j2"))
  setkeyv(s2,c("j1","j2"))
  s2[ss2,w_1:=i.w_1]
  s2[ss2,w_2:=i.w_2]
  if (reporting==0) {
    s2[ss2,N_f:=i.N_f]
    s2[ss2,N_w:=i.N_w]
  }
  s2$name<-paste0(s2$j1, " to ", s2$j2)
  #Deleting if less than the required number of observations for confidentiality
  if (reporting==0) {
    s2$w_1[s2$N_w<opts_min]<-NaN
    s2$w_2[s2$N_w<opts_min]<-NaN
  }
  s2<-t(s2[,3:ncol(s2)])

  return(s2)


}

#' Do Summary Kline
#'
#' @param sim Data
#' @param reporting wether to save to file or print only on screen ( & into Log file)
#' @param country name to save results
#' @export
Do.summary.Kline<-function(sim, reporting, country) {

  #1) Summary  Table for Connected Set

  # extract connected set
  f0s   = get.largest.conset.fid(sim$jdata)
  sim$jdata = sim$jdata[f1%in%f0s][f2%in%f0s]
  sim$sdata = sim$sdata[f1%in%f0s]

  # Reshape as single data vector
  y1_all<-c(sim$jdata$y1, sim$sdata$y1,sim$jdata$y2, sim$sdata$y2 )
  f1_all<-c(sim$jdata$f1, sim$sdata$f1,sim$jdata$f2, sim$sdata$f2 )
  Bel_all<-c(sim$jdata$Bel1, sim$sdata$Bel1, sim$jdata$Bel2, sim$sdata$Bel2 )
  cod_all <-c(sim$jdata$cod_pgr, sim$sdata$cod_pgr, sim$jdata$cod_pgr, sim$sdata$cod_pgr )
  fullt_all<-c(sim$jdata$fullt, sim$sdata$fullt, sim$jdata$fullt, sim$sdata$fullt)
  fullt_all[is.na(fullt_all)]<-0
  mov_all<-c(((sim$jdata$y1)*0+1),(sim$sdata$y1*0),((sim$jdata$y2)*0+1),(sim$sdata$y2*0) )
  sms = list(y1_all=y1_all, f1_all=f1_all, Bel_all=Bel_all, cod_all=cod_all, mov_all=mov_all, fullt_all=fullt_all)

  setDT(sms)
  ss<-t(sms[,list("number of observations"=length(y1_all),
                  "number of movers"=length(unique(cod_all[mov_all==1])),
                  "number of firms"=length(unique(f1_all)),
                  "share of full-time observations"=round(mean(fullt_all)*100*10^2,digits=0)/10^2,
                  "mean log-earnings"=round(mean(y1_all)*10^4, digits=0)/10^4,
                  "variance of log-earnings"=round(sd(y1_all)^2*10^4, digits=0)/10^4),
            by=Bel_all][order(Bel_all)])
  sa<-t(sms[,list("numbder of observations"=length(y1_all),
                  "number of movers"=length(unique(cod_all[mov_all==1])),
                  "number of firms"=length(unique(f1_all, na.rm=T)),
                  "share of full-time observations"=round(mean(fullt_all)*100*10^2,digits=0)/10^2,
                  "mean log-earnings"=round(mean(y1_all)*10^4, digits=0)/10^4,
                  "variance of log-earnings"=round(sd(y1_all)^2*10^4, digits=0)/10^4)])
  ss<-ss[2:(length(sa)+1),]
  sx<-cbind(ss,sa)

  #2) Summary Table for Leave-out Set
  # extract leave out firms
  jids_con = get.largest.leaveoutset.fid(sim$jdata)
  jidscon<-unique(c(jids_con$f1,jids_con$f2))
  sim$jdata<-sim$jdata[( (sim$jdata$f1 %in% jidscon)) ]
  sim$sdata<-sim$sdata[( (sim$sdata$f1 %in% jidscon)) ]

  # Reshape as single data vector
  y1_all<-c(sim$jdata$y1, sim$sdata$y1,sim$jdata$y2, sim$sdata$y2 )
  f1_all<-c(sim$jdata$f1, sim$sdata$f1,sim$jdata$f2, sim$sdata$f2 )
  Bel_all<-c(sim$jdata$Bel1, sim$sdata$Bel1, sim$jdata$Bel2, sim$sdata$Bel2 )
  cod_all <-c(sim$jdata$cod_pgr, sim$sdata$cod_pgr, sim$jdata$cod_pgr, sim$sdata$cod_pgr )
  fullt_all<-c(sim$jdata$fullt, sim$sdata$fullt, sim$jdata$fullt, sim$sdata$fullt)
  fullt_all[is.na(fullt_all)]<-0
  mov_all<-c(((sim$jdata$y1)*0+1),(sim$sdata$y1*0),((sim$jdata$y2)*0+1),(sim$sdata$y2*0) )
  sms = list(y1_all=y1_all, f1_all=f1_all, Bel_all=Bel_all, cod_all=cod_all, mov_all=mov_all, fullt_all=fullt_all)

  setDT(sms)
  ss<-t(sms[,list("number of observations"=length(y1_all),
                  "number of movers"=length(unique(cod_all[mov_all==1])),
                  "number of firms"=length(unique(f1_all)),
                  "share of full-time observations"=round(mean(fullt_all)*100*10^2,digits=0)/10^2,
                  "mean log-earnings"=round(mean(y1_all)*10^4, digits=0)/10^4,
                  "variance of log-earnings"=round(sd(y1_all)^2*10^4, digits=0)/10^4),
            by=Bel_all][order(Bel_all)])
  sa<-t(sms[,list("numbder of observations"=length(y1_all),
                  "number of movers"=length(unique(cod_all[mov_all==1])),
                  "number of firms"=length(unique(f1_all, na.rm=T)),
                  "share of full-time observations"=round(mean(fullt_all)*100*10^2,digits=0)/10^2,
                  "mean log-earnings"=round(mean(y1_all)*10^4, digits=0)/10^4,
                  "variance of log-earnings"=round(sd(y1_all)^2*10^4, digits=0)/10^4)])
  ss<-ss[2:(length(sa)+1),]
  sx_L<-cbind(ss,sa)

  #join both tables
  sT<-rbind(sx, sx_L)

  #Formatting and exporting of table
  #thousands separator
  sepdat<-matrix(c(rep(1,(3*1)), rep(0, (3*1)),rep(1,(3*1)), rep(0, (3*1))  ))
  spdat<-matrix(sepdat,nrow=12,ncol=ncol(sT))
  sT[spdat==1]<-prettyNum(sT[spdat==1], big.mark = ",")
  colnames(sT)<-cbind("Rovigo", "Belluno","Rovigo-Belluno")
  return(sT)
}

#' Do Summary Stats
#'
#' @param ad Data
#' @param reporting wether to save to file or print only on screen ( & into Log file)
#' @param country name to save results
#' @export
Do.summary.stats<-function(ad, reporting, opts_min=opts$minN) {

  sx<-Do.compute.stats(ad, groupi="j1", reporting, opts_min)

  #Exporting
  colnames(sx)<-cbind("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "all")
  return(sx)
}

#' Do Compare Leave-out and Left-out Sets
#'
#' @param sim Data
#' @param directory_Res directory for Results
#' @param country name to save results
#' @export
Do.compare.sets<-function(sim, reporting, opts_min=opts$minN){
  sim.copy<-copy(sim)
  if (reporting==0){
    invisible(Info_S<-"This produces the summary statistics by leave-out and left-out set and for all firms combined.
              Table 1): Describtive Statistics
              The rows describe the contained information, where the first row describes the set (leave-out or left-out).
              FOR CONFIDENTIALITY TEST:
              In this table, results computed with less than the required min. number of obs. are denoted as NaN for confidentiality.")

    flog.info(Info_S)
  }

  #1)full data summary
  sim$jdata$mov<-1
  sim$sdata$mov<-0
  sx_full<-Do.compute.stats(sim, groupi="mov", reporting, opts_min)

  #2) connected set
  # extract connected set
  f0s   = get.largest.conset.fid(sim$jdata)
  sim$jdata = sim$jdata[f1%in%f0s][f2%in%f0s]
  sim$sdata = sim$sdata[f1%in%f0s]
  sim$tdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)
  sx_connect<-Do.compute.stats(sim, groupi="mov", reporting, opts_min)

  #3) leave out firms
  sim<-copy(sim.copy)
  sim$jdata$mov<-1
  sim$sdata$mov<-0
  jids_con = get.largest.leaveoutset.fid(sim$jdata)
  jidscon<-unique(c(jids_con$f1,jids_con$f2))
  sim$jdata = sim$jdata[f1%in%jidscon][f2%in%jidscon]
  sim$sdata = sim$sdata[f1%in%jidscon]
  sx_leave_out<-Do.compute.stats(sim, groupi="mov", reporting, opts_min)

  colnames(sx_full)<-c(  "Stayers", "Movers", "All")
  colnames(sx_connect)<-c("Stayers", "Movers", "All")
  colnames(sx_leave_out)<-c("Stayers", "Movers", "All")

  sx<-list()
  sx$full<-sx_full
  sx$connect<-sx_connect
  sx$leave_out<-sx_leave_out
  return(sx)
}

#' Do Compare Leave-out and Left-out Sets
#'
#' @param sim Data
#' @param directory_Res directory for Results
#' @param country name to save results
#' @export
Do.compare.sets.movers<-function(sim, reporting, opts_min=opts$minN){

  sim.copy<-copy(sim)
  if (reporting==0){
    invisible(Info_S<-"This producesmovers statistics by leave-out and left-out set and for all firms combined.
              FOR CONFIDENTIALITY TEST:
              In this table, results computed with less than the required min. number of obs. are denoted as NaN for confidentiality.")

    flog.info(Info_S)
  }

  #1)full data summary
  sx_full<-Do.analyze.distr(sim)

  #2) connected set
  # extract connected set
  f0s   = get.largest.conset.fid(sim$jdata)
  sim$jdata = sim$jdata[f1%in%f0s][f2%in%f0s]
  sim$sdata = sim$sdata[f1%in%f0s]
  sim$tdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)
  sx_connect<-Do.analyze.distr(sim)

  #3) leave out firms
  sim<-copy(sim.copy)
  jids_con = get.largest.leaveoutset.fid(sim$jdata)
  jidscon<-unique(c(jids_con$f1,jids_con$f2))
  sim$jdata = sim$jdata[f1%in%jidscon][f2%in%jidscon]
  sim$sdata = sim$sdata[f1%in%jidscon]
  sx_leave_out<-Do.analyze.distr(sim)

  sx<-list()
  sx$full<-sx_full
  sx$connect<-sx_connect
  sx$leave_out<-sx_leave_out
  return(sx)
}


#' @param ad Data
#' @export
Do.analyze.distr<-function(ad) {
  tt<-ad$jdata[,list(f1=c(f1,f2))][,.N,f1][N<=10]
  cum.prob<-tt[,list(N_f=.N),by=(N)][order(N)]
  cum.prob$cdf_N_f<-cumsum(cum.prob$N_f)/sum(cum.prob$N_f)
  return(cum.prob)
}

#' @export
Do.analyze.wage<-function(ad){
  tdata<-data.table(rbind(ad$sdata,ad$jdata, fill=TRUE))
  data_w<-hist(tdata$y1, breaks=15)
  distr_wage<-data.table(wage=data_w$mids, cdf_wage=cumsum(data_w$counts)/sum(data_w$counts), N=data_w$counts)
  return(distr_wage)

}

#' @param sim Data
#' @param group according to which group to summarize
#' @export
Do.compute.stats<-function(sim, groupi, reporting=0, opts_min=opts$minN){

  sim$tdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)

  invisible(sim$tdata[,sv:=sd(as.numeric(y1_l), na.rm=T),by=f1])
  group=eval(parse(text=((paste0("sim$tdata$",groupi)))))
  sa<-sim$tdata[, list("number of workers"=length(unique(as.character(cod_pgr), na.rm=T)),
                       "number of firms"=length(unique(as.character(f1), na.rm=T)),
                       "mean observed firm size"=mean(as.numeric(N_j), na.rm=T),
                       "number firms $\\geq$ 10"=ifelse(length(unique(as.character(f1[na.omit(N_j)>=10])))==0,0L,length(unique(as.character(f1[na.omit(N_j)>=10])))),
                       "number firms $\\geq$ 50"=ifelse(length(unique(as.character(f1[na.omit(N_j)>=50])))==0,0L,length(unique(as.character(f1[na.omit(N_j)>=50])))),
                       "% less than secondary education"=mean(as.numeric(edu_belowsecondary), na.rm=T),
                       "% secondary education"=mean(as.numeric(edu_secondary), na.rm=T),
                       "% tertiary education"=mean(as.numeric(edu_tertiary), na.rm=T),
                       "% workers younger than 30"=mean(as.numeric(age)<30)*100,
                       "% workers between 31 and 50"=mean(as.numeric(age)<50 & as.numeric(age)>=31)*100,
                       "% workers older than 50"=mean(as.numeric(age)>=50)*100,
                       "% workers in manufacturing"=mean(as.numeric(manuf), na.rm=T)*100,
                       "% workers in services"=mean(as.numeric(serv), na.rm=T)*100,
                       "% workers in retail and trade"=mean(as.numeric(ret_trade), na.rm=T)*100,
                       "% workers in construction"=mean(as.numeric(cons), na.rm=T)*100,
                       "mean log-earnings"=mean(as.numeric(y1_l), na.rm=T),
                       "variance of log-earnings"=sd(as.numeric(y1_l), na.rm=T)^2,
                       "between firm variance of log-earnings"=sd(sv, na.rm=T)^2),by=eval(group)][order(eval(group))]
  ss<-sim$tdata[, list("number of workers"=length(unique(as.character(cod_pgr), na.rm=T)),
                       "number of firms"=length(unique(as.character(f1), na.rm=T)),
                       "mean observed firm size"=mean(as.numeric(N_j), na.rm=T),
                       "number firms $\\geq$ 10"=ifelse(length(unique(as.character(f1[na.omit(N_j)>=10])))==0,0L,length(unique(as.character(f1[na.omit(N_j)>=10])))),
                       "number firms $\\geq$ 50"=ifelse(length(unique(as.character(f1[na.omit(N_j)>=50])))==0,0L,length(unique(as.character(f1[na.omit(N_j)>=50])))),
                       "% less than secondary education"=mean(as.numeric(edu_belowsecondary), na.rm=T),
                       "% secondary education"=mean(as.numeric(edu_secondary), na.rm=T),
                       "% tertiary education"=mean(as.numeric(edu_tertiary), na.rm=T),
                       "% workers younger than 30"=mean(as.numeric(age)<30)*100,
                       "% workers between 31 and 50"=mean(as.numeric(age)<50 & as.numeric(age)>=31)*100,
                       "% workers older than 50"=mean(as.numeric(age)>=50)*100,
                       "% workers in manufacturing"=mean(as.numeric(manuf), na.rm=T)*100,
                       "% workers in services"=mean(as.numeric(serv), na.rm=T)*100,
                       "% workers in retail and trade"=mean(as.numeric(ret_trade), na.rm=T)*100,
                       "% workers in construction"=mean(as.numeric(cons), na.rm=T)*100,
                       "mean log-earnings"=mean(as.numeric(y1_l), na.rm=T),
                       "variance of log-earnings"=sd(as.numeric(y1_l), na.rm=T)^2,
                       "between firm variance of log-earnings"=sd(sv, na.rm=T)^2)]
  #Filter out means with less than the required min number of observations for confidentiality
  if (reporting==0){
    indx_more20_ss<-matrix((ss[,1]<opts_min),nrow=nrow(ss),(ncol=ncol(ss)))
    indx_more20_sa<-matrix((sa[,2]<opts_min),nrow=nrow(sa),(ncol=ncol(sa)))
    ss[indx_more20_ss]<-NaN
    sa[indx_more20_sa]<-NaN
  }

  ss<-t(ss)
  sa<-t(sa)

  # #filter out missing info (this is country specific)
  sa<-sa[2:(nrow(sa)),]
  sx<-cbind(sa,ss)
  indx_i=complete.cases(sx)
  sx<-sx[indx_i,]
  #
  # #Formatting
  # #N of digits matrix
  # mmdat <- matrix(c(rep(10^0, (5*1)), rep(10, (10*1)), rep(10^2,1), rep(10^3,(2*1))), nrow = 18, ncol=1, byrow=TRUE)
  # mdat<-matrix(mmdat,nrow=nrow(mmdat),(ncol=ncol(sx)))
  # mdat<-mdat[indx_i,]
  # sx<-round(sx*mdat, digits=0)/mdat
  # #matrix for adding %
  # ppdat<-matrix(c(rep("",(5*1)), rep("%", (10*1)), rep("", (3*1)) ))
  # pdat<-matrix(ppdat,nrow=nrow(mmdat),ncol=ncol(sx))
  # pdat<-pdat[indx_i,]
  # sx[,1:ncol(sx)]<-paste(sx[,1:ncol(sx)], pdat[,1:ncol(sx)], sep="", collapse=NULL)
  # #thousands separator
  # sepdat<-matrix(c(rep(1,(5*1)), rep(0, (13*1)) ))
  # spdat<-matrix(sepdat,nrow=nrow(mmdat),ncol=ncol(sx))
  # spdat<-spdat[indx_i,]
  # sx[spdat==1]<-prettyNum(sx[spdat==1], big.mark = ",")


  return(sx)

}

# --- COMPUTING STATS ----
#' Do Compute wage groups
#'
#' @param res2 object from estimation
#' @param ms mean data
#' @export
Do.compute.groups.w<-function(res2, ms) {

  res_ql<-res2
  model<-res2
  qt<-6
  rr = data.table(l=1:length(model$A1),Em = model$Em,Esd = model$Esd,
                  N = model$Ns,A1=model$A1,B1=model$B1,A2=model$A2,B2=model$B2)

  alpha_m  = rr[, wtd.mean(Em,N)]
  alpha_sd = sqrt(rr[, wtd.mean(Esd^2,N) + wtd.var(Em,N) ])

  qts = qnorm( (1:qt)/(qt+1))

  rr2 = rr[, list( y1= (qts*alpha_sd + alpha_m)* B1+ A1 ,y2= (qts*alpha_sd + alpha_m)* B2+ A2, k=1:qt),l ]
  rr2 = melt(rr2,id.vars = c('l','k'))
  rr2$value<-rr2$value+ms
  return(rr2)

}


#' Sparse colSums
#' @export
sColSums <- function(M) {
  return(as.numeric((rep(1,dim(M)[1]) %*% M)))
}

#' Sparse rowSums
#' @export
sRowSums <- function(M) {
  return(as.numeric(M %*% rep(1,dim(M)[2])))

}


#' @export
Do.hybrid.leaveout<-function(sizeM=15, nm_list=c(seq(1,0.1,l=10),0.05), leaveoutN=1, data_type, directory_Data, filtering, residualize, cl=NaN) {

  #read in data: as muti-year data required
  ad<-Do.reading.data(directory_Data, filtering, data_suffix="_all", data_type)
  #keep three years of data
  ad$sdata<-ad$sdata[anno>=(max(anno)-2)]
  ad$jdata<-ad$jdata[anno>=(max(anno)-2)]
  #residualize
  ad<-Do.residualize(ad, residualize)

  # get firms with at least nm movers
  fids = ad$jdata[,list(f1=c(f1,f2))][,.N,f1][N>=sizeM,f1]

  # only keep these firms
  ad$sdata = ad$sdata[f1 %in% fids]
  ad$jdata = ad$jdata[f1 %in% fids][f2 %in% fids]

  reshyb<-list()
  set.seed(324313)

  for (itN in 1:leaveoutN){
    flog.info("[Hybrid-leave-out] Iteration=%i", itN)

    res_akm_blm_leave_out<-Do.leaveout.wrap(ad, nm_list, itN)

    if (itN==1) {
      reshyb<-res_akm_blm_leave_out
    } else if (itN>1){
      reshyb<-rbind(reshyb, res_akm_blm_leave_out)
    }
  }
  res_hybrid=list()
  res_hybrid$mean<-setDT(reshyb)[,lapply(.SD, mean, na.rm=TRUE), by=.(share_mover_keep,hetero) ]
  res_hybrid$sd<-setDT(reshyb)[,lapply(.SD, sd, na.rm=TRUE), by=.(share_mover_keep,hetero) ]



  # # use cluster if available
  # if (!any(is.na(cl))) {
  #   flog.info("cluster -- exporting objects to nodes")
  #   # export environment to nodes
  #   clusterExport(cl,c("ad","nm_list", "leaveoutN"),environment())
  #   mylapply <- function(...) parLapply(cl,...)
  #   nnodes=length(cl)
  # } else {
  #   mylapply <- function(...) lapply(...)
  #   nnodes=1
  # }
  #
  #
  # flog.info("starting repetitions with %i nodes",nnodes)
  # reshyb  = mylapply(1:leaveoutN, function(ij) {
  #   res_hyb = list()
  #   tryCatch({
  #   res_hyb=Do.leaveout.core2(ad,nm_list)
  #   }, error = function(e) {catf("error in rep %i!\n",ij);print(e);})
  #   flog.info("done with reptitions %i/%i",ij,leaveoutN)
  #   res_hyb$it=ij
  #   res_hyb
  # })
  #
  #
  #  temp=melt(reshyb) #L2: variable, L1: it
  #  res_hybrid=list()
  #  res_hybrid$mean<-setDT(temp)[,lapply(.SD, mean, na.rm=TRUE), by=.(L2) ]
  #  res_hybrid$sd<-setDT(temp)[,lapply(.SD, sd, na.rm=TRUE), by=.(L2) ]


  return(res_hybrid)
}



#' @export
#'
Do.leaveout.wrap<-function(sim, nm_list, itN){
  #Do.leaveout.wrap<-function(sim, nm_list){

  resall=data.frame()
  # run AKM on this set of firms
  rr = m2.trace.estimate(sim,hetero=FALSE)
  with(rr$stats,psi_var/set_logwage_var)
  rr$stats$share_mover_keep = 100
  rr$stats$itN = itN
  resall=rbind(resall,rr$stats)

  for (nm_i in nm_list) { #list of worker leave outs
    triali <- 1
    errori=1

    res_n=data.frame()
    while ( (errori==1) & (triali <= 10) ) {
      triali <- triali + 1
      errori=0
      res_n<-try(Do.leaveout.core(sim, nm_i, itN))
      #      res_n<-try(Do.leaveout.core(sim, nm_i))
      if (is(res_n,  "try-error")){
        errori=1
      }

    }
    resall<-rbind(resall,res_n)

  }
  return(resall)
}



#' @export
#Do.leaveout.core<-function(sim, share_mover_keep) {
Do.leaveout.core<-function(sim, share_mover_keep, itN) {
  #Core of estimation for each share of movers kept
  rrr = data.frame()
  flog.info("[Hybrid-leave-out] Share of movers kept=%2f", share_mover_keep)
  sim$jdata = sim$jdata[rank(runif(.N))/.N<=share_mover_keep]
  rr = m2.trace.estimate(sim,hetero=FALSE)
  rr$stats$share_mover_keep = share_mover_keep
  rr$stats$itN = itN
  rrr = rbind(rrr,data.frame(rr$stats))
  rr = m2.trace.estimate(sim,hetero=TRUE)
  rr$stats$share_mover_keep = share_mover_keep
  rr$stats$itN = itN
  rrr = rbind(rrr,data.frame(rr$stats))
  return(rrr)
}

#' @export
Do.leaveout.core2<-function(sim, nm_list) {
  # Do.leaveout.core<-function(sim, share_mover_keep, itN) {
  resall=data.frame()
  # run AKM on this set of firms
  rr = m2.trace.estimate(sim,hetero=FALSE)
  with(rr$stats,psi_var/set_logwage_var)
  rr$stats$share_mover_keep = 100
  #    rr$stats$itN = itN
  resall=rbind(resall,rr$stats)

  for (share_mover_keep in nm_list) { #list of worker leave outs

    #Core of estimation for each share of movers kept
    rrr = data.frame()
    flog.info("[Hybrid-leave-out] Share of movers kept=%2f", share_mover_keep)
    sim$jdata = sim$jdata[rank(runif(.N))/.N<=share_mover_keep]
    rr = m2.trace.estimate(sim,hetero=FALSE)
    rr$stats$share_mover_keep = share_mover_keep
    #        rr$stats$itN = itN
    rrr = rbind(rrr,data.frame(rr$stats))
    rr = m2.trace.estimate(sim,hetero=TRUE)
    rr$stats$share_mover_keep = share_mover_keep
    #        rr$stats$itN = itN
    rrr = rbind(rrr,data.frame(rr$stats))
    #return(rrr)
  }
  resall=rbind(resall,rrr)
  return(resall)
}




##---COMMENTS FOR CONFIDENTIALITY CHECK DE----
#'@param reporting
#'@export
Do.info.DE.project<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"This file estimates variance decompositions of log wages by correcting for mobility bias.
              FOR CONFIDENTIALITY CHECK:
              The file gives out estimation steps with additional estimation based
              information. Data related output will be explained in a note right before the table
              All output will be produced in two consecutive tables: one for the confidentiality check and
              one for us researchers to copy paste into a csv file.
              Raw data is used for all estimations but the output is most closely related to case numbers in
              D 4) Event Study
              D 5) Summary Stats
              D 6) Summary of Distributions
              where the case numbers are reported within the tables. In the estimation
              D 2) Trace estimation
              D 7) Estimating model
              the output are Variance decompositions.The case number of observation used to compute
              those are given within the table.
              D 9) Is a composite command that combines D 6), D 2) and part of D 7). Not all
              results are requested with each run.")
    flog.warn(Info_T)
  }
}
#'@param reporting
#'@export
Do.info.DE.project.new<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"This file estimates variance decompositions of log wages by correcting for mobility bias under different settings.
              FOR CONFIDENTIALITY CHECK:
              Data related output will be explained in a note right before the table.
              All output will be produced in two consecutive tables: one for the confidentiality check and
              one for us researchers to copy paste into a csv file.
              Raw data is used for all parameter estimations. However most output is obtained through simulations
              after parameter estimates have been obtained with the raw data. Hence, most results are simulation-
              based and a case number/#of firms is not strictly necessary according to the FDZ guidelines.
              We will explain which results use the raw data in turn and which are purely simulation based.
              ---------------------------------------------------------------------------------------------")
    flog.warn(Info_T)
  }
}

#'@param reporting
#'@export
Do.info.DE.project.running<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"This file shows the progression stes/intermediate steps taken during the estimation.
                       It is helpful for debugging when the code stopped and no final output file was created.
              ---------------------------------------------------------------------------------------------")
    flog.warn(Info_T)
  }
}

#'Hints and notes for confidentiality check in Germany
#'@param reporting
#'@export
Do.info.DE.trace<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"The following estimation computes the correction to the variance decomposition and does this for
              D. 2a) for different assumptions about the homoskedasticity of errors
              D. 2b) different samples (full sample, connected set of firms (cf. AKM), leave-out xset of firms (cf. Kline et al 2018).
              D. 2c) for different assumptions about the inclusion of movers in the estimation.")
    flog.warn(Info_T)
  }
}

#'Hints and notes for confidentiality check in Germany
#'@param reporting
#'@export
Do.info.DE.event<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"The following estimation computes an event study in which we compute the average wage of workers before a move
              and the average wage of workers after a move for 4 firm typen, defined as in BLM (2018) or Kline (2018) through
              average wages. That is, firms with a similar cdf of average worker wages will be grouped together in one group.")
    flog.warn(Info_T)
  }
}

#'Hints and notes for confidentiality check in Germany
#'@param reporting
#'@export
Do.info.DE.sets<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"The following estimation computes summary statistics for the different sets used
              (full sample, connected set of firms (cf. AKM), leave-out xset of firms (cf. Kline et al 2018)).")
    flog.warn(Info_T)
  }
}

#'Hints and notes for confidentiality check in Germany
#'@param reporting
#'@export
Do.info.DE.stats<-function(reporting){
  if (reporting==0){
    invisible(Info_S<-"This estimation the summary statistics by firm cluster 1 to 10 and for all firms combined. Firm clusters
              are defined through average worker wages as described in BLM 2018
              Describtive Statistics
              The rows describe the contained information, where the first row describes the firm cluster  number
              FOR CONFIDENTIALITY TEST:
              In this table, results computed with less than the required min. number of obs. are denoted as NaN for confidentiality.")

    flog.warn(Info_S)
  }
}

#'Hints and notes for confidentiality check in Germany
#'@param reporting
#'@export
Do.info.DE.wages<-function(reporting){
  if (reporting==0){
    invisible(Info_S<-"This estimation the summary statistics by firm cluster 1 to 10 and for all firms combined. Firm clusters
              are defined through average worker wages as described in BLM 2018
              Wage differences
              This table shows the average wage difference of workers moving between firms of type x/y (row/column).
              FOR CONFIDENTIALITY TEST:
              Wage differences for cases with less than the required min. number of obs. have been removed and are displaced as NaN.")

    flog.warn(Info_S)
  }
}

#'Hints and notes for confidentiality check in Germany
#'@param reporting
#'@export
Do.info.DE.stats.mobility<-function(reporting){
  if (reporting==0){
    invisible(Info_S<-"This estimation the summary statistics by firm cluster 1 to 10 and for all firms combined. Firm clusters
              are defined through average worker wages as described in BLM 2018
              Moving patterns
              This table shows the number of workers moving between firms of type x/y (row/column)")

    flog.warn(Info_S)
  }
}

#'Hints and notes for confidentiality check in Germany
#'@param reporting
#'@export
Do.info.DE.sets.table<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"The following table reports summary statistics for the two sets.")
    flog.warn(Info_T)
  }
}

#'Hints and notes for confidentiality check in Germany
#'@param reporting
#'@export
Do.info.DE.event.table<-function(reporting){
  if (reporting==0){
    invisible(Info_ES<-"The following table reports:

              - first row: mean wage of workers in period -1 starting at firm group x
              - second row: mean wage of workers in period 1 ending up at firm group y
              - third row: # of firms in cell
              - fourth row: # of workers in cell
              - fifth row: mobility from group of firms x to group of firm y (e.g. 1 to 1, 1 to 4 etc)
              FOR CONFIDENTIALITY TEST:
              In this table, results computed with less than the min number of required obs. are denoted as NaN (missing) for confidentiality.")

    flog.warn(Info_ES)
  }
}


#'Hints and notes for confidentiality check in Germany
#'@param reporting
#'@export
Do.info.DE.trace.table<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"The following table reports the wage variance contributions when controlling for biases
              under certain assumptions on the error variance.
              FOR CONFIDENTIALITY TEST:
              This table is only shown if results were computed with more than the required
              min number of obs. (cf. Do.NaN.trace just before)
              The table contains for the confidentiality check
              - total number of firms (second column)
              - total number of stayers and total number of workers: number of
              workers (4th and 5th column)
              Caution: 'set number of firms=10' just means that we group firms
              into ten groups, not # of firms!")
    flog.warn(Info_T)
  }
}

#'@param reporting
#'@export
Do.info.DE.estimate<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"The following estimates the variance decomposition of wages using the BLM model. It hence estimates the
              model and then SIMULATES data to estimate the variance decomposition.Different estimators are used for this.
              a) LL estimator
              b) LP estimator
              c) QL estimator
              d) MX estimator.
              After each of these estimations, we put out a covariance matrix of wage components due to workers and firms.
              After all desired estimators have been computed we further put out a table with variance decompositions across all stimators.")
    flog.warn(Info_T)
  }
}

#'@param reporting
#'@export
Do.info.DE.final_interactedEEm<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"The following  table shows the expected, estimated wage for workers of different type (rows)
                       and firms of different type(columns) estimated through the interaction algorithm. These are outputs of simulated data
                       given the estimated parameters of the model. Hence no data is reported here.")
    flog.warn(Info_T)
  }
}

#'@param reporting
#'@export
Do.info.DE.final_interactedquantiles<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"The following  table shows the expected, estimated wage (3rd column) for workers of different type (first column)
                       and firms of different type (second columns) estimated through the interaction algorithm. Within the
                      firm and worker types we further distinguish according to the distribution of
                      productivity of the worker type (before last column). These are outputs of simulated data
                       given the estimated parameters of the model. Hence no data is reported here.")
    flog.warn(Info_T)
  }
}

#'@param reporting
#'@export
Do.info.DE.final_randomattrition<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"The following shows the variance decomposition of wages using the BLM model. It hence estimates the
              model and then SIMULATES data to estimate the variance decomposition.Different estimators are used for this.
              a) AKM
              b) CRE
              c) CRE-0
              d) Interacted CRE
              e) TraceHE
              f) TRaceHO
              All of these estimators make different assumptions about the DGP.
              Furthermore, in this estimation we restrict attention to a given number of workers (# of workers in column 15)  )
              that are drawn randomly (number of draw is in the last column).
              These are outputs of simulated data given the estimated parameters of the model. Hence no data is reported here.")
    flog.warn(Info_T)
  }
}


#'@param reporting
#'@export
Do.info.DE.final_samplesize<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"This table shows the case number for each set (column 1) that we compute in the model.
                      The second column shows the number of firms, the third the number of workers.")
    flog.warn(Info_T)
  }
}

Do.explanations<-function(filename){
  reporting=0
  if ((filename=='final_interactedEEm_DE-test') | (filename=='final_interactedEEm_DE')) {
                Do.info.DE.final_interactedEEm(reporting)
  } else if  ((filename=='final_interactedquantiles_DE-test')| (filename=='final_interactedquantiles_DE')) {
                Do.info.DE.final_interactedquantiles(reporting)
  } else if  ((filename=='final_output_DE-test')| (filename=='final_output_DE')) {
                Do.info.DE.final_output(reporting)
  } else if  ((filename=='final_randomattrition_DE-test') |(filename=='final_randomattrition_DE')) {
                Do.info.DE.final_randomattrition(reporting)
  } else if  ((filename=='final_samplesize_DE-test')|(filename=='final_samplesize_DE')) {
                Do.info.DE.final_samplesize(reporting)
  } else if  ((filename=='final_sortingstats_DE')|(filename=='final_sortingstats_DE-test')) {
              Do.info.DE.final_sortingstats(reporting)
  } else if ((filename=='final_clusterstats_DE-test')|(filename=='final_clusterstats_DE')) {
              Do.info.DE.final_clusterstats(reporting)
  } else if ((filename=='final_interactedcoefs_DE-test')| (filename=='final_interactedcoefs_DE')){
              Do.info.DE.final_interactedcoefs(reporting)
  }

}





#'@param reporting
#'@export
Do.info.DE.final_interactedcoefs<-function(reporting){
  if (reporting==0){

    invisible(Info_T<-"The following table reports the results of the estimation
              of the interacted model, where A and B are the two cluster-specific parameters.")
    flog.warn(Info_T)
  }
}

#'@param reporting
#'@export
Do.info.DE.final_clusterstats<-function(reporting){
  if (reporting==0){

    invisible(Info_T<-"The following table reports the distribution of wages within clusters.
              The table contains for the confidentiality check
              - the cluster ID (first column)
              - the number of firms (second column)
              - the mean size (3rd column)
              - the number of firms with more than 10 or 50 movers (4th and 5th)
              - number of workers in cell (6th column)
              - the remaining columns: wage statistics: wage mean, variance, skewness, kurtosis")
    flog.warn(Info_T)
  }
}

#'@param reporting
#'@export
Do.info.DE.final_sortingstats<-function(reporting){
  if (reporting==0){

    invisible(Info_T<-"The following table reports the distribution of wages.
              The table contains for the confidentiality check
              - wages: log wage
              - N: the number of observations in each bin of the wage distribution
              this is done fr each worker and firm cluster combination (first two columns).")
    flog.warn(Info_T)
  }
}



#'@param reporting
#'@export
Do.info.DE.final_output<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"The following shows the variance decomposition of wages using the BLM model. It hence estimates the
              model and then SIMULATES data to estimate the variance decomposition.Different estimators are used for this.
              a) AKM
              b) CRE
              c) CRE-0
              d) Interacted CRE
              e) TraceHE
              f) TRaceHO
              All of these estimators make different assumptions about the DGP.These are outputs of simulated data
                       given the estimated parameters of the model. Hence no data is reported here.")
    flog.warn(Info_T)
  }
}


#'@param reporting
#'@export
Do.info.DE.estimate.decomp<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"This table shows the variance decomposition for all four estimation types
              - cor_kl : correlation firm and worker part of wages
              - cov_kl : covariance firm and worker part of wages
              - var_k : variance firm part wages
              - var_l : variance worker part wages
              - rsq: RSQ of estimation model
              - N_s: Number of observations of stayers
              - N_j: Number of observations for movers
              - Share: Share of movers in total observations.
              FOR CONFIDENTIALITY TEST:
              This table is only shown if results were computed with more than the required min number of obs.")
    flog.warn(Info_T)
  }
}

#'@param reporting
#'@export
Do.info.DE.estimate.window<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"The following estimates the variance decomposition of wages using the BLM model. It hence estimates the
              model and then SIMULATES data to estimate the variance decomposition.Different estimators are used for this.
              a) LL estimator
              b) LP estimator
              c) QL estimator
              d) MX estimator.
              After all desired estimators have been computed we further put out a table with variance decompositions across all stimators.")
    flog.warn(Info_T)
  }
}

#'@param reporting
#'@export
Do.info.DE.dist<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"The following table reports the distribution of the mobility patterns across firms.
              The table contains for the confidentiality check
              - N: number of movers in a firm
              - N_f: number of firms with N movers
              - cdf_N_f: cdf of this number of movers.
              Please note: N=1 does NOT mean that there is only one observation, hence this does not
              need to be deleted.
              e.g.   N=2  N_f=1045   cdf_N_f=.51   means that there are 1045 firms with 2 movers
              and that 51 % of all observations of movers are in this case (The number of observations
              in each cell is therefore N_f*N.)")

    flog.warn(Info_T)
  }
}

#'@param reporting
#'@export
Do.info.DE.dist.wage<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"The following table reports the distribution of wages.
              The table contains for the confidentiality check
              - wage: log wage
              - cdf_wage: cdf of this wage
              - N: the number of observations in each bin of the wage distribution
              (taken from hist).")

    flog.warn(Info_T)
  }
}

#'@param reporting
#'@export
Do.info.DE.dist.all<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"Below we analyze the distribution of movers within firms and the distribution of wages.")

    flog.warn(Info_T)
  }
}

#'@param reporting
#'@export
Do.info.DE.composite<-function(reporting){
  if (reporting==0){
    invisible(Info_T<-"This analysis conducts several estimations at once
              A) Summary of data subsets
              B) AKM estimation
              C) Trace estimation
              D) BLM trace estimation
              E) Clustering and estimation of BLM model.")

    flog.warn(Info_T)
  }
}

##---EXPORTING, NICE TABLES----

#' Write csv table to log file, for DE
#'
#' @export
Do.log.table<-function(opts, filename, print_row_name=FALSE) {
    #Read output file
    data <- fread(paste0(opts$paths$final, filename, ".csv"))
    flog.info("Output as Table")
    print(data, row.names = FALSE, topn=nrow(data))
    flog.info("Output as CSV Table")
    cat(sprintf("####### @CSVBEGIN %s\n",filename))
    write.csv.log(data)
    cat(sprintf("####### @CSVEND %s\n",filename))
}

#' Export a data.frame either to the log file or directly to disk
#'
#' @param type 0 to save to disk, 1 to send to the log
#' @export
Do.export.table<-function(data, filename, type=0, print_row_name=FALSE) {
  # save to file
  if (type==0) {
    flog.info("[saving] Saving results to %s",filename)
    write.csv(data,filename,row.names = print_row_name)

    # print in console in CSV format, wrap into tags
  } else {
    flog.info("Output as Table")
    print(data, row.names = FALSE, topn=nrow(data))
    flog.info("Output as CSV Table")
    cat(sprintf("####### @CSVBEGIN %s\n",filename))
    write.csv.log(data)
    cat(sprintf("####### @CSVEND %s\n",filename))
  }

}

#' @export
write.csv.log <- function(d) {
  tc <- textConnection("NewLog", "w") #"a" to append, "w" to write new
  write.csv(d,file=tc)
  close(tc)
  cat(paste0(NewLog,"\n"))
}

#' @export
Do.export.event<-function(s2, directory_Res, country, reporting){

  file_id=paste0(directory_Res, "res_event_", country, ".eps")
  if (reporting==1){
    setEPS()
    postscript(file_id)
    matplot(c(-1,1),s2[1:2,1:4], type="b", xlab="Time (0=move)", ylab="Mean Log Wage of Movers",pch=15:19,        xaxp  = c(-1, 1, 2) , lwd=2)
    names<-c("1 to 1", "1 to 4",               "4 to 1", "4 to 4")
    legend("bottom", inset=1, legend=names, col=c(1:5),pch=15:19,
           bg= ("white"), xpd = TRUE, horiz = TRUE, bty = "n",  cex = 2)

    dev.off()
  } else if (reporting==0) {
    s2
    s2[]
    print("Event Study")
    Do.export.results(s2, reporting, file_id)
  }


}
#Function creates table with summary of different estimators for BLM var decomp
#' @export
Do.decomp.table<-function(res_N, num_obs, Ns, Nm, country, directory_Res, reporting){

  share<-Ns/(Ns+Nm)
  X_N=data.table(N_s=rep(Ns,ncol(res_N)), N_j=rep(Nm,ncol(res_N)), Share=rep(share, ncol(res_N)))
  cdt=data.table(Country=rep(country,ncol(res_N)))
  edt=data.table(Est=c(( if ("ll" %in% opts$do) "LL" else {}), ( if (("lp" %in% opts$do)) "LP" else {}), ( if (("ql" %in% opts$do)) "QL" else {}), ( if (("mx" %in% opts$do)) "MX" else {})))
  dd=cbind(cdt, edt, t(res_N), rbind(X_N))

  if (reporting==0){
    invisible(Info_T<-"This table shows the variance decomposition for all four estimation types
              - cor_kl : correlation firm and worker part of wages
              - cov_kl : covariance firm and worker part of wages
              - var_k : variance firm part wages
              - var_l : variance worker part wages
              - rsq: RSQ of estimation model
              - N_s: Number of observations of stayers
              - N_j: Number of observations for movers
              - Share: Share of movers in total observations.
              FOR CONFIDENTIALITY TEST:
              This table is only shown if results were computed with more than the required min number of obs.")
    flog.info(Info_T)
  }
  return(dd)
}

#Function computes group averages
#' @export
Do.export.groups<-function(rr2,file_id, reporting ){

  try({
    if (reporting==1) {
      ggplot(rr2,aes(x=l,y=value,color=factor(k))) + geom_line() + geom_point() + theme_bw()
      ggsave(file_id)
    } else if (reporting==0) {
      flog.warn("Average wage for workers of type l at firm type k. These Results are based on SIMULATIONS, not on data.
                Hence there are no case numbers reported!")
      flog.warn("Worker and firm types are estimated within the BLM framework. Firm types
                represent types as defined by average worker wages per firm. Worker types are
                estimated as in BLM (2018). ")
      Do.export.table(data=rr2,filename=file_id, type=opts$export_type, print_row_name=TRUE)
    }})
}


#' Export Latex Files of Results
#'
#' @param data Data
#' @param reporting reporting wether to save to file or print only on screen ( & into Log file)
#' @param file_id name for File to save
#' @param align_options options for latex table
#' @export
Do.export.results<-function(data,reporting, file_id, digitsx=NULL, align_option={}){
  if (missing(align_option)){
    if (reporting==1){
      print(xtable(data, type = "latex", digits=digitsx), file = file_id, floating = FALSE)
    } else if (reporting==0) {
      flog.info("Output as Table")
      print(data, row.names = FALSE, topn=nrow(data))
      flog.info("Output as Latex Table")
      print(xtable(data, type = "latex", digits=digitsx), file = "",  floating = FALSE)
    }
  } else {
    if (reporting==1){
      print(xtable(data, type = "latex", digits=digitsx,align=align_option), file = file_id,  floating = FALSE)
    } else if (reporting==0) {
      flog.info("Output as Table")
      print(data, row.names = FALSE, topn=nrow(data))
      flog.info("Output as Latex Table")
      print(xtable(data, type = "latex", digits=digitsx, align=align_option), file = "",  floating = FALSE)
    }
  }
}


#This function removes results computed with less observations than the minim required number (set as option)
#for the Hybrid Tables
#' @export
Do.NaN.Hybrid<-function(res_Hybrid_connected, opts_min=opts$minN) {
  setDT(res_Hybrid_connected)
  x_nm<-res_Hybrid_connected$nm[(res_Hybrid_connected$variable=="nw" & res_Hybrid_connected$value< opts$minN)]
  x_set<-res_Hybrid_connected$set[(res_Hybrid_connected$variable=="nw" & res_Hybrid_connected$value< opts$minN)]
  res_Hybrid_connected$value[((res_Hybrid_connected$nm %in% x_nm)& (res_Hybrid_connected$set %in% x_set))]<-NaN
  return(res_Hybrid_connected)
}

#This function removes results computed with less observations than the minim required number (set as option)
#for the trace tables
#' @export
Do.NaN.trace<-function(res_Hybrid_connected, opts_min=opts$minN) {
  res_Hybrid_connected[((res_Hybrid_connected$total_number_of_movers+res_Hybrid_connected$total_number_of_stayers)< opts$minN)]<-NaN
  return(res_Hybrid_connected)
}


#This function exports tables to csv files or as .tex tables to the log file
#' @export
Do.generate.table <- function(res, nm_list, leave_out=0, sds={}) {
  res = data.table(res)
  res = res[nm %in% nm_list]

  nc = nrow(res)+1

  if (leave_out==0){
    tt =
      tt_text_row("min number of movers")  %:% tt_numeric_row(res$nm,dec=0)

    tt=tt+tt_text_row("n firms with own type") %:% tt_numeric_row(res$firms_with_own_type,dec=0) +
      tt_spacer_row(7)

    tt = tt +
      tt_text_row("")  %:% tt_text_row("Connected set",nrow(res),"c") + tt_rule_mid_partial(list(c(2,nc))) +
      tt_text_row("n workers")     %:% tt_numeric_row(res$set0_nw,dec=0) +
      tt_text_row("n firms")       %:% tt_numeric_row(res$set0_nf,dec=0)

    tt = tt+tt_spacer_row(7)
    tt = tt +
      tt_text_row("")  %:% tt_text_row("AKM, firms with few movers clustered, Connected set",nrow(res),"c") + tt_rule_mid_partial(list(c(2,nc))) +
      tt_text_row("n workers")            %:% tt_numeric_row(res$set1_nw,dec=0) +
      tt_text_row("n firms")              %:% tt_numeric_row(res$set1_nf,dec=0) +
      tt_text_row("$Var(\\psi)$")         %:% tt_numeric_row(res$set1_psi_var,big.mark="") +
      tt_text_row("$Var(\\psi)$ share")   %:% tt_numeric_row(100*res$set1_psi_var/res$set1_wage_var,big.mark="",percentage=T,dec=1) +
      tt_text_row("$Var(\\psi)$ cor")     %:% tt_numeric_row(res$set1_psi_var-res$set1_trace,big.mark="") +
      tt_text_row("$Var(\\psi)$ cor share")%:% tt_numeric_row(100*(res$set1_psi_var-res$set1_trace)/res$set1_wage_var,big.mark="",percentage=T,dec=1) +
      tt_text_row("$Cov(\\psi,alpha_i)$") %:% tt_numeric_row(res$set1_cov,big.mark="")+
      tt_text_row("Within cluster firm variance") %:% tt_numeric_row(res$set1_omega_var,big.mark="")+
      tt_spacer_row(7)

    tt = tt +
      tt_text_row("")  %:% tt_text_row("AKM, Leave out connected set",nc-1,"c") + tt_rule_mid_partial(list(c(2,nc))) +
      tt_text_row("n workers")            %:% tt_numeric_row(res$set2_nw,dec=0) +
      tt_text_row("n firms")              %:% tt_numeric_row(res$set2_nf,dec=0) +
      tt_text_row("$Var(\\psi)$")         %:% tt_numeric_row(res$set2_psi_var,big.mark="") +
      tt_text_row("$Var(\\psi)$ share")   %:% tt_numeric_row(100*res$set2_psi_var/res$set2_wage_var,big.mark="",percentage=T,dec=1) +
      tt_text_row("$Var(\\psi)$ cor")     %:% tt_numeric_row(res$set2_psi_var - res$set2_trace,big.mark="") +
      tt_text_row("$Var(\\psi)$ cor share")   %:% tt_numeric_row(100*(res$set2_psi_var - res$set2_trace)/res$set2_wage_var,big.mark="",percentage=T,dec=1) +
      tt_text_row("$Cov(\\psi,alpha_i)$") %:% tt_numeric_row(res$set2_cov,big.mark="")+
      tt_spacer_row(7)

    tt = tt +
      tt_text_row("")  %:% tt_text_row("AKM - Homo -  firms with own type only",nrow(res),"c") + tt_rule_mid_partial(list(c(2,nc))) +
      tt_text_row("n workers")            %:% tt_numeric_row(res$set3_nw,dec=0) +
      tt_text_row("n firms")              %:% tt_numeric_row(res$set3_nf,dec=0) +
      tt_text_row("$Var(\\psi)$")         %:% tt_numeric_row(res$set3_psi_var,big.mark="") +
      tt_text_row("$Var(\\psi)$ share")   %:% tt_numeric_row(100*res$set3_psi_var/res$set3_wage_var,big.mark="",percentage=T,dec=1) +
      tt_text_row("$Cov(\\psi,alpha_i)$") %:% tt_numeric_row(res$set3_cov,big.mark="")+
      tt_spacer_row(4)

    tt = tt +
      tt_text_row("")  %:% tt_text_row("AKM - Hetero - firms with own type only",nrow(res),"c") + tt_rule_mid_partial(list(c(2,nc))) +
      tt_text_row("n workers")            %:% tt_numeric_row(res$set4_nw,dec=0) +
      tt_text_row("n firms")              %:% tt_numeric_row(res$set4_nf,dec=0) +
      tt_text_row("$Var(\\psi)$")         %:% tt_numeric_row(res$set4_psi_var,big.mark="") +
      tt_text_row("$Var(\\psi)$ share")   %:% tt_numeric_row(100*res$set4_psi_var/res$set4_wage_var,big.mark="",percentage=T,dec=1) +
      tt_text_row("$Cov(\\psi,alpha_i)$") %:% tt_numeric_row(res$set4_cov,big.mark="")+
      tt_spacer_row(4)
  } else if (leave_out==1){
    tt =
      tt_text_row("number of movers left-out")  %:% tt_numeric_row(res$nm,dec=0)

    tt = tt+tt_spacer_row(7)
    tt = tt +
      tt_text_row("")  %:% tt_text_row("AKM, firms with few movers clustered, Connected set",nrow(res),"c") + tt_rule_mid_partial(list(c(2,nc))) +
      tt_text_row("n workers")            %:% tt_numeric_row(res$set1_nw,dec=0) +
      tt_text_row("n firms")              %:% tt_numeric_row(res$set1_nf,dec=0) +
      tt_text_row("$Var(\\psi)$")         %:% tt_numeric_row(res$set1_psi_var,big.mark="") +
      tt_text_row("")                     %:% tt_numeric_row(sds$set1_psi_var,big.mark="") +
      # tt_text_row("$Var(\\psi)$ share")   %:% tt_numeric_row(100*res$set1_psi_var/res$set1_wage_var,big.mark="",percentage=T,dec=1) +
      #
      # tt_text_row("$Var(\\psi)$ cor")     %:% tt_numeric_row(res$set1_psi_var-res$set1_trace,big.mark="") +
      # tt_text_row("$Var(\\psi)$ cor share")%:% tt_numeric_row(100*(res$set1_psi_var-res$set1_trace)/res$set1_wage_var,big.mark="",percentage=T,dec=1) +
      # tt_text_row("$Cov(\\psi,alpha_i)$") %:% tt_numeric_row(res$set1_cov,big.mark="")+
      # tt_text_row("Within cluster firm variance") %:% tt_numeric_row(res$set1_omega_var,big.mark="")+
      tt_spacer_row(7)

    tt = tt +
      tt_text_row("")  %:% tt_text_row("AKM, Leave out connected set",nc-1,"c") + tt_rule_mid_partial(list(c(2,nc))) +
      tt_text_row("n workers")            %:% tt_numeric_row(res$set2_nw,dec=0) +
      tt_text_row("n firms")              %:% tt_numeric_row(res$set2_nf,dec=0) +
      tt_text_row("$Var(\\psi)$")         %:% tt_numeric_row(res$set2_psi_var,big.mark="") +
      tt_text_row("")                     %:% tt_numeric_row(sds$set2_psi_var,big.mark="") +
      # tt_text_row("$Var(\\psi)$ share")   %:% tt_numeric_row(100*res$set2_psi_var/res$set2_wage_var,big.mark="",percentage=T,dec=1) +
      # tt_text_row("$Var(\\psi)$ cor")     %:% tt_numeric_row(res$set2_psi_var - res$set2_trace,big.mark="") +
      # tt_text_row("$Var(\\psi)$ cor share")   %:% tt_numeric_row(100*(res$set2_psi_var - res$set2_trace)/res$set2_wage_var,big.mark="",percentage=T,dec=1) +
      # tt_text_row("$Cov(\\psi,alpha_i)$") %:% tt_numeric_row(res$set2_cov,big.mark="")+
      tt_spacer_row(7)

    tt = tt +
      tt_text_row("")  %:% tt_text_row("AKM - Homo -  firms with own type only",nrow(res),"c") + tt_rule_mid_partial(list(c(2,nc))) +
      tt_text_row("n workers")            %:% tt_numeric_row(res$set3_nw,dec=0) +
      tt_text_row("n firms")              %:% tt_numeric_row(res$set3_nf,dec=0) +
      tt_text_row("$Var(\\psi)$")         %:% tt_numeric_row(res$set3_psi_var,big.mark="") +
      tt_text_row("")                     %:% tt_numeric_row(sds$set3_psi_var,big.mark="") +
      # tt_text_row("$Var(\\psi)$ share")   %:% tt_numeric_row(100*res$set3_psi_var/res$set3_wage_var,big.mark="",percentage=T,dec=1) +
      # tt_text_row("$Cov(\\psi,alpha_i)$") %:% tt_numeric_row(res$set3_cov,big.mark="")+
      tt_spacer_row(4)

    tt = tt +
      tt_text_row("")  %:% tt_text_row("AKM - Hetero - firms with own type only",nrow(res),"c") + tt_rule_mid_partial(list(c(2,nc))) +
      tt_text_row("n workers")            %:% tt_numeric_row(res$set4_nw,dec=0) +
      tt_text_row("n firms")              %:% tt_numeric_row(res$set4_nf,dec=0) +
      tt_text_row("$Var(\\psi)$")         %:% tt_numeric_row(res$set4_psi_var,big.mark="") +
      tt_text_row("")                     %:% tt_numeric_row(sds$set4_psi_var,big.mark="") +
      # tt_text_row("$Var(\\psi)$ share")   %:% tt_numeric_row(100*res$set4_psi_var/res$set4_wage_var,big.mark="",percentage=T,dec=1) +
      # tt_text_row("$Cov(\\psi,alpha_i)$") %:% tt_numeric_row(res$set4_cov,big.mark="")+
      tt_spacer_row(4)
  }

  tt
}

##---COMPARATIVE RESULTS----
#' Do Compute stats for sub-groups
#'
#' @param ad Data
#' @param all_coworkers 1:compute sample split for size of firm
#' @export
Do.groups<-function(ad,all_coworkers=0, opts_min=opts$minN) {

  stats<-data.frame()

  for (speci in 1:14){

    #Initialize
    stat<-list()
    sim<-ad

    if (speci==1) {
      stat$spec<-"young"
      sim$tdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)
      mean_age<-mean(sim$tdata$age,na.rm=TRUE)
      #Only for young workers
      sim$sdata<-sim$sdata[age<=mean_age,]
      sim$jdata<-sim$jdata[age<=mean_age,]
    } else if (speci==2) {
      stat$spec<-"old"
      sim$tdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)
      mean_age<-mean(sim$tdata$age,na.rm=TRUE)
      #Only for young workers
      sim$sdata<-sim$sdata[age>mean_age,]
      sim$jdata<-sim$jdata[age>mean_age,]
    } else if (speci==3) {
      stat$spec<-"manuf"
      #Only manuf
      sim$sdata<-sim$sdata[manuf==1,]
      sim$jdata<-sim$jdata[manuf==1,]
    } else if (speci==4) {
      stat$spec<-"No manuf"
      #Only manuf
      sim$sdata<-sim$sdata[manuf==0,]
      sim$jdata<-sim$jdata[manuf==0,]
    } else if ((speci==5) & (all_coworkers==1)) {
      stat$spec<-"large"
      #Firm Size
      sim$tdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)
      sim$tdata[,Ij:=rowid(f1)]
      sim$tdata[,mw:=.N,by=f1]
      setkey(sim$tdata,f1)
      sim$jdata[sim$tdata,mw:=i.mw]
      sim$sdata[sim$tdata,mw:=i.mw]
      mean_size<-mean(sim$tdata$mw[sim$tdata$Ij==1],na.rm=TRUE)
      sim$sdata<-sim$sdata[N_j>mean_size,]
      sim$jdata<-sim$jdata[N_j>mean_size,]
    } else if ((speci==6) & (all_coworkers==1)) {
      stat$spec<-"small"
      #Firm Size
      sim$tdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)
      sim$tdata[,Ij:=rowid(f1)]
      sim$tdata[,mw:=.N,by=f1]
      setkey(sim$tdata,f1)
      sim$jdata[sim$tdata,mw:=i.mw]
      sim$sdata[sim$tdata,mw:=i.mw]
      mean_size<-mean(sim$tdata$mw[sim$tdata$Ij==1],na.rm=TRUE)
      sim$sdata<-sim$sdata[N_j<=mean_size,]
      sim$jdata<-sim$jdata[N_j<=mean_size,]
    } else if (speci==7){
      #Avaerge Firm quality
      stat$spec<-"high W."
      sim$tdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)
      ts<-sim$tdata[,list(mw=mean(y1, na.rm=T), N_j=max(N_j)),by=f1]
      setkey(ts,f1)
      sim$jdata[ts,mw:=i.mw, on=("f1")]
      sim$sdata[ts,mw:=i.mw, on=("f1")]
      mean_w<-weighted.mean(ts$mw,ts$N_j, na.rm=TRUE)
      sim$sdata<-sim$sdata[mw>mean_w,]
      sim$jdata<-sim$jdata[mw>mean_w,]
    } else if (speci==8){
      #Avaerge Firm quality
      stat$spec<-"Low W."
      sim$tdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)
      ts<-sim$tdata[,list(mw=mean(y1, na.rm=T), N_j=max(N_j)),by=f1]
      setkey(ts,f1)
      sim$jdata[ts,mw:=i.mw, on=("f1")]
      sim$sdata[ts,mw:=i.mw, on=("f1")]
      mean_w<-weighted.mean(ts$mw,ts$N_j, na.rm=TRUE)
      sim$sdata<-sim$sdata[mw<=mean_w,]
      sim$jdata<-sim$jdata[mw<=mean_w,]
    } else if (speci==9){
      # Firm Age
      stat$spec<-"Young Firms"
      sim$tdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)
      ts<-sim$tdata[,list(mw=mean(firm_age, na.rm=T), N_j=max(N_j)),by=f1]
      setkey(ts,f1)
      sim$jdata[ts,mw:=i.mw, on=("f1")]
      sim$sdata[ts,mw:=i.mw, on=("f1")]
      mean_w<-mean(ts$mw, na.rm=TRUE)
      sim$sdata<-sim$sdata[mw<=mean_w,]
      sim$jdata<-sim$jdata[mw<=mean_w,]
    }else if (speci==10){
      # Firm Age
      stat$spec<-"Old Firms"
      sim$tdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)
      ts<-sim$tdata[,list(mw=mean(firm_age, na.rm=T), N_j=max(N_j)),by=f1]
      setkey(ts,f1)
      sim$jdata[ts,mw:=i.mw, on=("f1")]
      sim$sdata[ts,mw:=i.mw, on=("f1")]
      mean_w<-mean(ts$mw, na.rm=TRUE)
      sim$sdata<-sim$sdata[mw>mean_w,]
      sim$jdata<-sim$jdata[mw>mean_w,]
    } else if (speci==11) {
      stat$spec<-"low W, old"
      sim$txdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)
      mean_age<-mean(sim$txdata$age,na.rm=TRUE)
      sim$tdata<-rbind(sim$sdata[age>mean_age], sim$jdata[age>mean_age], fill=TRUE)
      ts<-sim$tdata[,list(mw=mean(y1, na.rm=T), N_j=max(N_j)),by=f1]
      setkey(ts,f1)
      sim$jdata[ts,mw:=i.mw, on=("f1")]
      sim$sdata[ts,mw:=i.mw, on=("f1")]
      mean_w<-weighted.mean(ts$mw,ts$N_j, na.rm=TRUE)
      sim$sdata<-sim$sdata[mw<=mean_w,]
      sim$jdata<-sim$jdata[mw<=mean_w,]
    } else if (speci==12) {
      stat$spec<-"high W, old"
      sim$txdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)
      mean_age<-mean(sim$txdata$age,na.rm=TRUE)
      sim$tdata<-rbind(sim$sdata[age>mean_age], sim$jdata[age>mean_age], fill=TRUE)
      ts<-sim$tdata[,list(mw=mean(y1, na.rm=T), N_j=max(N_j)),by=f1]
      setkey(ts,f1)
      sim$jdata[ts,mw:=i.mw, on=("f1")]
      sim$sdata[ts,mw:=i.mw, on=("f1")]
      mean_w<-weighted.mean(ts$mw,ts$N_j, na.rm=TRUE)
      sim$sdata<-sim$sdata[mw>mean_w,]
      sim$jdata<-sim$jdata[mw>mean_w,]
    } else if (speci==13) {
      stat$spec<-"low W,  old firms"
      sim$txdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)
      firm_d<-sim$txdata[, list(mw=mean(firm_age)), by=f1]
      mean_age<-mean(firm_d$mw,na.rm=TRUE)
      sim$tdata<-rbind(sim$sdata[firm_age>mean_age], sim$jdata[firm_age>mean_age], fill=TRUE)
      ts<-sim$tdata[,list(mw=mean(y1, na.rm=T), N_j=max(N_j)),by=f1]
      setkey(ts,f1)
      sim$jdata[ts,mw:=i.mw, on=("f1")]
      sim$sdata[ts,mw:=i.mw, on=("f1")]
      mean_w<-weighted.mean(ts$mw,ts$N_j, na.rm=TRUE)
      sim$sdata<-sim$sdata[mw<=mean_w,]
      sim$jdata<-sim$jdata[mw<=mean_w,]
    } else if (speci==14) {
      stat$spec<-"high W, old firms"
      sim$txdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)
      firm_d<-sim$txdata[, list(mw=mean(firm_age)), by=f1]
      mean_age<-mean(firm_d$mw,na.rm=TRUE)
      sim$tdata<-rbind(sim$sdata[firm_age>mean_age], sim$jdata[firm_age>mean_age], fill=TRUE)
      ts<-sim$tdata[,list(mw=mean(y1, na.rm=T), N_j=max(N_j)),by=f1]
      setkey(ts,f1)
      sim$jdata[ts,mw:=i.mw, on=("f1")]
      sim$sdata[ts,mw:=i.mw, on=("f1")]
      mean_w<-weighted.mean(ts$mw,ts$N_j, na.rm=TRUE)
      sim$sdata<-sim$sdata[mw>mean_w,]
      sim$jdata<-sim$jdata[mw>mean_w,]
    }

    flog.info("[compare groups] Specification: %s",stat$spec)

    if ((speci!=6 & speci!=5) | ((speci==6) & (all_coworkers==1)) |  ((speci==5) & (all_coworkers==1))) {
      #Cluster anew
      #measure for clustering
      ms    = grouping.getMeasures(sim,"ecdf",Nw=20,y_var = "y1")
      # then we group we choose k=10
      grps  = grouping.classify.once(ms,k = 10,nstart = 1000,iter.max = 200,step=250)
      # finally we append the results to adata
      sim   = grouping.append(sim,grps$best_cluster,drop=T)


      #Stats
      stat$age<-mean(c(sim$sdata$age, sim$jdata$age), na.rm=TRUE)
      stat$manuf<-mean(c(sim$sdata$manuf, sim$jdata$manuf), na.rm=TRUE)
      sim$tdata<-rbind(sim$sdata, sim$jdata, fill=TRUE)
      sim$tdata[,Ij:=rowid(f1)]
      sim$tdata[,mw:=mean(firm_age, na.rm=T),by=f1]
      sim$tdata[,msz:=.N,by=f1]
      stat$mw<-mean(sim$tdata$mw[sim$tdata$Ij==1],na.rm=TRUE)
      stat$N_j<-mean(sim$tdata$msz[sim$tdata$Ij==1],na.rm=TRUE)

      #Estimate Model
      mstats = sim$jdata[,list(m1=mean(y1),sd1=sd(y1),
                               m2=mean(y2),sd2=sd(y2),
                               v12 = cov(y1,y2),.N),list(j1,j2)]
      cstats = sim$sdata[,list(m1=mean(y1),sd1=sd(y1),
                               m2=mean(y2),sd2=sd(y2),
                               v12 = cov(y1,y2),.N),list(j1)]
      res_ql = m2.minirc.estimate(cstats,mstats,method = 2)

      #Save Results
      stat$rsq<-res_ql$vdec$rsq1
      stat$sig_psi<-res_ql$vdec$stats$var_l
      stat$sig_alph<-res_ql$vdec$stats$var_k
      stat$cor<-res_ql$vdec$stats$cor_kl
      stat$cov<-res_ql$vdec$stats$cov_kl

      #Size
      stat$Ns<-nrow(sim$sdata)
      stat$Nm<-nrow(sim$jdata)
      stat$share<-stat$Nm/(stat$Nm+stat$Ns)


      stats<-rbind(stats,stat)
    }
    if (speci==1){
      stats$spec<-as.character(stats$spec)
    }
  }

  flog.info("Only Results with required min obs printed")
  indx_more20_ss<-matrix((stats$Nm<opts_min),nrow=nrow(stats),(ncol=ncol(stats)))
  stats[indx_more20_ss]<-NaN


  return(stats)

}



#' @export
#'
Do.comparison.time<-function(directory_Data, data_type, filtering, residualize=1, opts_min=opts$minN) {

  #Note: this analysis requires a file sdata_all.csv and jdata_all.csv

  #Read in the data with information for all years
  ad<-Do.reading.data(directory_Data, filtering, data_suffix="_all", data_type)

  stats<-data.table()

  for (year in (min(ad$sdata$anno)):(max(ad$sdata$anno)-1)){

    #Group firms for this year
    ad1<-list(sdata=ad$sdata[(ad$sdata$anno==year)], jdata=ad$jdata[(ad$jdata$anno==year)])
    #Residualize data
    ad1<-Do.residualize(ad1, residualize)
    ad1.copy<-ad1

    for (speci in 1:3) {
      ad1<-ad1.copy

      stat=list()
      flog.info("[compare across time] Year: %i",year)
      stat$years<-year

      stat$spec="all"
      if (speci==2){
        #Avaerge Firm quality
        stat$spec<-"high W."
        ad1$tdata<-rbind(ad1$sdata, ad1$jdata, fill=TRUE)
        ts<-ad1$tdata[,list(mw=mean(y1, na.rm=T), N_j=max(N_j)),by=f1]
        setkey(ts,f1)
        ad1$jdata[ts,mw:=i.mw, on=("f1")]
        ad1$sdata[ts,mw:=i.mw, on=("f1")]
        mean_w<-weighted.mean(ts$mw,ts$N_j, na.rm=TRUE)
        ad1$sdata<-ad1$sdata[mw>mean_w,]
        ad1$jdata<-ad1$jdata[mw>mean_w,]
      } else if (speci==3){
        stat$spec<-"low W."
        ad1$tdata<-rbind(ad1$sdata, ad1$jdata, fill=TRUE)
        ts<-ad1$tdata[,list(mw=mean(y1, na.rm=T), N_j=max(N_j)),by=f1]
        setkey(ts,f1)
        ad1$jdata[ts,mw:=i.mw, on=("f1")]
        ad1$sdata[ts,mw:=i.mw, on=("f1")]
        mean_w<-weighted.mean(ts$mw,ts$N_j, na.rm=TRUE)
        ad1$sdata<-ad1$sdata[mw<=mean_w,]
        ad1$jdata<-ad1$jdata[mw<=mean_w,]
      }
      flog.info("[compare groups] Specification: %s",stat$spec)
      stat$sd<-sd(c(ad1$sdata$y1_l, ad1$jdata$y1_l), na.rm=TRUE)^2

      #Cluster
      ms    = grouping.getMeasures(ad1,"ecdf",Nw=20,y_var = "y1")
      # then we group we choose k=10
      grps  = grouping.classify.once(ms,k = 10,nstart = 1000,iter.max = 200,step=250)
      # finally we append the results to adata
      ad1   = grouping.append(ad1,grps$best_cluster,drop=T)
      stat$sd2<-sd(c(ad1$sdata$y1_l, ad1$jdata$y1_l), na.rm=TRUE)^2

      #Estimate
      mstats = ad1$jdata[,list(m1=mean(y1),sd1=sd(y1),
                               m2=mean(y2),sd2=sd(y2),
                               v12 = cov(y1,y2),.N),list(j1,j2)]
      cstats = ad1$sdata[,list(m1=mean(y1),sd1=sd(y1),
                               m2=mean(y2),sd2=sd(y2),
                               v12 = cov(y1,y2),.N),list(j1)]
      res_ql = m2.minirc.estimate(cstats,mstats,method = 2)

      stat$rsq<-res_ql$vdec$rsq1
      stat$sig_psi<-res_ql$vdec$stats$var_l
      stat$sig_alph<-res_ql$vdec$stats$var_k
      stat$cor<-res_ql$vdec$stats$cor_kl
      stat$cov<-res_ql$vdec$stats$cov_kl

      #Stats
      stat$Ns<-nrow(ad1$sdata)
      stat$Nm<-nrow(ad1$jdata)
      stat$share<-stat$Nm/(stat$Nm+stat$Ns)
      #Combine
      stats<-rbind(stats,stat)

      if (speci==1){
        stats$spec<-as.character(stats$spec)
      }

    }
  }

  flog.info("Only Results with required min obs printed")
  indx_more20_ss<-matrix((stats$Nm<opts_min),nrow=nrow(stats),(ncol=ncol(stats)))
  stats[indx_more20_ss]<-NaN

  return(stats)
}

#' @export
#'
Do.long_data<-function(sim){
  # Reshape as single data vector
  y1<-c(sim$jdata$y1, sim$sdata$y1,sim$jdata$y2, sim$sdata$y2 )
  firm_ID<-as.numeric(c(sim$jdata$f1, sim$sdata$f1,sim$jdata$f2, sim$sdata$f2 ))
  worker_ID <-as.numeric(c(sim$jdata$cod_pgr, sim$sdata$cod_pgr, sim$jdata$cod_pgr, sim$sdata$cod_pgr ))

  sms = list(y1=y1, firm_ID=firm_ID, worker_ID=worker_ID)
  return(sms)
}


#' @export
#'
Do.comparison.window.trace<-function(window_vec, year_vec, directory_Data, data_type, filtering, residualize=1, opts_min=opts$minN, res_path=opts$paths$res, opts_country=opts$country, reporting=opts$reporting) {

  #Note: this analysis requires a file sdata_all.csv and jdata_all.csv

  #Read in the data with information for all years
  ad<-Do.reading.data(directory_Data, filtering, data_suffix="_all", data_type)

  stats<-data.table()

  #Get the set of years for which to perform analysis
  if (year_vec==""){
    year_vec<-unique(ad$sdata$anno)
  }


  for (year in year_vec){

    for (window in window_vec){ #how many windows of data

      #Get data for this year
      ad1<-list(sdata=ad$sdata[anno>=(year-window+1)& anno <= year], jdata=ad$jdata[anno>=(year-window+1) & anno <= year])

      #Residualize data
      ad1<-Do.residualize(ad1, residualize)

      stat=list()
      flog.warn("[compare across time] Year: %i",year)
      stat$years<-year

      #A) Summary Stats
      flog.info("Summary leave-in and leave-out set")
      Do.info.DE.sets(opts$reporting)
      res_sets<-Do.compare.sets(ad1, reporting=opts$reporting, opts_min=opts$minN)
      flog.info("Export Leave-out comparison")
      file_id = paste0(res_path,"res_sets_full_", opts_country,"-",window,"-",year ,".csv")
      Do.info.DE.sets.table(opts$reporting)
      Do.export.table(data=res_sets$full, filename=file_id, type=opts$export_type, print_row_name=TRUE)
      file_id = paste0(res_path,"res_sets_connect_", opts_country,"-",window,"-",year ,".csv")
      Do.info.DE.sets.table(opts$reporting)
      Do.export.table(data=res_sets$connect, filename=file_id, type=opts$export_type, print_row_name=TRUE)
      file_id = paste0(res_path,"res_sets_leave_out_", opts_country,"-",window,"-",year ,".csv")
      Do.info.DE.sets.table(opts$reporting)
      Do.export.table(data=res_sets$leave_out, filename=file_id, type=opts$export_type, print_row_name=TRUE)

      # #B) AKM estimation
      # long_data<-Do.long_data(ad1)
      # setDT(long_data)
      # # find the connected set of firms, keep only workers in that subset
      # connected_set_of_firms <- akm.estimate.connected_set(long_data,ncat=100)
      # long_data <- long_data[firm_ID %in% connected_set_of_firms$largest]
      # # estimate AKM firm effects
      # firm_effects <- akm.estimate.zig_zag(long_data, outcome_var = "y1")
      # # computes AKM worker effects, given by average deviation from firm effect
      # long_data[, akm_worker_effect := mean(y1 - akm_firm_effect), by = "worker_ID"]
      # long_data[, akm_idiosyncratic := y1 - akm_worker_effect - akm_firm_effect]
      # # extract statistics
      # akm_statistics <- akm.estimate.extract_statistics(long_data, outcome_var = "y1")
      # file_id = paste0(res_path,"res_akm_",  opts_country,"-",window,"-",year ,".csv")
      # Do.export.table(data=akm_statistics, filename=file_id, type=opts$export_type)

      #C) Estimate Trace
      Do.info.DE.trace(opts$reporting)
      #C i) Trace with fixed effects
      res_het<-m2.trace.estimate(ad1,hetero=T)
      res_homo<-m2.trace.estimate(ad1,hetero=F)
      res_trace<-rbind(res_het$stats, res_homo$stats)
      flog.info("#Set NaN for results computed with less than min # of obs")
      res_trace<-Do.NaN.trace(res_trace, opts_min=opts$minN)
      file_id = paste0(res_path,"res_trace_",  opts_country,"-",window,"-",year ,".csv")
      Do.info.DE.trace.table(opts$reporting)
      Do.export.table(data=res_trace, filename=file_id, type=opts$export_type)

      #C ii) Trace with BLM with different sets
      res_blm0<-m2.trace.blm(ad1,set=0, subsample=0)
      res_blm1<-m2.trace.blm(ad1,set=1, subsample=0)
      res_blm2<-m2.trace.blm(ad1,set=2, subsample=0)
      res_trace_blm0<-rbind(res_blm0$stats, res_blm1$stats,res_blm2$stats)
      flog.info("#Set NaN for results computed with less than min # of obs")
      res_trace_blm0<-Do.NaN.trace(res_trace_blm0, opts_min=opts$minN)
      file_id = paste0(res_path,"res_trblm0_", opts_country,"-",window,"-",year ,".csv")
      Do.info.DE.trace.table(opts$reporting)
      Do.export.table(data=res_trace_blm0, filename=file_id, type=opts$export_type)

      #C ii) Trace with BLM for different subsamples
      res_blm0<-m2.trace.blm(ad1,set=0, subsample=2)
      res_blm1<-m2.trace.blm(ad1,set=1, subsample=2)
      res_blm2<-m2.trace.blm(ad1,set=2, subsample=2)
      res_trace_blm2<-rbind(res_blm0$stats, res_blm1$stats,res_blm2$stats)
      flog.info("#Set NaN for results computed with less than min # of obs")
      res_trace_blm2<-Do.NaN.trace(res_trace_blm2, opts_min=opts$minN)
      file_id = paste0(res_path,"res_trblm2_", opts_country,"-",window,"-",year ,".csv")
      Do.info.DE.trace.table(opts$reporting)
      Do.export.table(data=res_trace_blm2, filename=file_id, type=opts$export_type)

      #E) Cluster and estimate
      flog.warn("Here we estimate firm types for future use as in BLM (2018)")
      ms    = grouping.getMeasures(ad1,"ecdf",Nw=20,y_var = "y1")
      # then we group we choose k=10
      grps  = grouping.classify.once(ms,k = 10,nstart = 1000,iter.max = 200,step=250)
      # finally we append the results to adata
      ad1   = grouping.append(ad1,grps$best_cluster,drop=T)
      Do.info.DE.estimate.window(opts$reporting)
      #Estimate LL
      res_ll=m2.mini.estimate(ad1$jdata, ad1$sdata, norm=1,model0=c(), method="linear", withx=FALSE)
      stat$rsq<-res_ll$vdec$rsq1

      invisible(capture.output(stat$var_l<-res_ll$vdec$stats$var_l))
      invisible(capture.output(stat$var_k<-res_ll$vdec$stats$var_k))
      invisible(capture.output(stat$cor_kl<-res_ll$vdec$stats$cor_kl))
      invisible(capture.output(stat$cov_kl<-res_ll$vdec$stats$cov_kl))
      #Stats
      invisible(capture.output(stat$N_s<-nrow(ad1$sdata)))
      invisible(capture.output(stat$N_j<-nrow(ad1$jdata)))
      invisible(capture.output(stat$share<-stat$N_j/(stat$N_j+stat$N_s)))

      #Combine
      setDT(stat)
      file_id = paste0(res_path,"res_ll_", opts_country,"-",window,"-",year ,".csv")
      flog.warn("Combine Table with var decomp")
      Do.info.DE.estimate.decomp(opts$reporting)
      Do.export.table(data=stat, filename=file_id, type=opts$export_type, print_row_name=TRUE)

    }
  }
}

