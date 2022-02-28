

# ---COMBINED DATA CONSTRUCTION CALLS ----

#' Read jdata/sdata
#' @param directory_Data Directory where the data is located
#' @param suffix Additional specifications of the dataset to use
#' @param data_type csv or rds
#' @param opts  Options
#' @export
Do.reading.jdata_sdata <- function(directory_Data, suffix = "", data_type = "csv", opts) {
  flog.info("reading jdata/sdata with suffix %s", suffix)

  # file is in .rds format
  if (opts$loading$separate) {
    # jdata and sdata are separate
    jdata <- readRDS(paste0(directory_Data, "jdata", suffix, ".rds"))
    sdata <- readRDS(paste0(directory_Data, "sdata", suffix, ".rds"))
    ad <- list(sdata = sdata, jdata = data)
  } else {
    # jdata and sdata are NOT separate
    ad <- readRDS(paste0(directory_Data, "bothdata", suffix, ".rds"))
  }

  flog.info("finished: reading jdata/sdata with suffix %s", suffix)

  return(ad)
}


#' export jdata/sdata
#' @param directory_Data Directory where the data is located
#' @param suffix Additional specifications of the dataset to use
#' @param data_type csv or rds
#' @param separate jdata and sdata are separate (TRUE), or already in a list together (FALSE)
#' @export
Do.export.jdata_sdata <- function(suffix = "", opts) {
  flog.info("exporting jdata/sdata with suffix %s", suffix)

  # file is in .rds format
  if (opts$loading$separate) {
    # jdata and sdata are separate
    jdata <- readRDS(paste0(opts$paths$data, "jdata", suffix, ".rds"))
    sdata <- readRDS(paste0(opts$paths$data, "sdata", suffix, ".rds"))
  } else {
    # jdata and sdata are NOT separate
    flog.info("reading %s", paste0(opts$paths$data, "bothdata", suffix, ".rds"))
    ad <- readRDS(paste0(opts$paths$data, "bothdata", suffix, ".rds"))
    jdata <- ad$jdata
    sdata <- ad$sdata
  }

  # convert firms to integers
  f1s <- jdata[, unique(c(f1, f2))]
  fids <- data.table(f1 = f1s, nfid = 1:length(f1s))
  setkey(fids, f1)
  setkey(jdata, f1)
  jdata[, f1i := fids[jdata, nfid]]
  setkey(sdata, f1)
  sdata[, f1i := fids[sdata, nfid]]
  setkey(jdata, f2)
  jdata[, f2i := fids[jdata, nfid]]

  # convert to data.frame
  jdata <- as.data.frame(jdata)
  sdata <- as.data.frame(sdata)

  # save the sdata/jdata separately
  flog.info("exporting sdata to %s", paste0(opts$paths$data, "sdata", suffix, "_py.rds"))
  saveRDS(sdata, file = paste0(opts$paths$data, "sdata", suffix, "_py.rds"), compress = F)
  flog.info("exporting jdata to %s", paste0(opts$paths$data, "jdata", suffix, "_py.rds"))
  saveRDS(jdata, file = paste0(opts$paths$data, "jdata", suffix, "_py.rds"), compress = F)
  flog.info("finished exporting sdata and jdata")
}


#' Write jdata/sdata
#' @param directory_Data Directory where the data is located
#' @param suffix Additional specifications of the dataset to use
#' @param data_type csv or rds
#' @param separate jdata and sdata are separate (TRUE), or already in a list together (FALSE)
#' @export
Do.writing.jdata_sdata <- function(ad, directory_Data, suffix = "", data_type = "csv", opts) {
  if (opts$loading$separate) {
    # jdata and sdata are separate
    jdata <- ad$jdata
    sdata <- ad$sdata
    flog.info("writing jdata/sdata with suffix %s (%s)", suffix, paste0(directory_Data, "jdata", suffix, ".rds"))
    saveRDS(jdata, paste0(directory_Data, "jdata", suffix, ".rds"), compress = F)
    flog.info("writing jdata/sdata with suffix %s", suffix, paste0(directory_Data, "sdata", suffix, ".rds"))
    saveRDS(sdata, paste0(directory_Data, "sdata", suffix, ".rds"), compress = F)
  } else {
    # jdata and sdata are NOT separate
    flog.info("writing jdata/sdata with suffix %s (%s)", suffix, paste0(directory_Data, "bothdata", suffix, ".rds"))
    saveRDS(ad, file = paste0(directory_Data, "bothdata", suffix, ".rds"), compress = F)
  }

  flog.info("finished: writing jdata/sdata with suffix %s", suffix)
  return(ad)
}

#' Check for existence of jdata/sdata
#' @param directory_Data Directory where the data is located
#' @param suffix Additional specifications of the dataset to use
#' @param data_type csv or rds
#' @param separate jdata and sdata are separate (TRUE), or already in a list together (FALSE)
#' @export
Do.exists.jdata_sdata <- function(directory_Data, suffix = "", data_type = "csv", opts) {
  exists.jdata_sdata <- FALSE

  if (opts$loading$separate) {
    # jdata and sdata are separate
    exists.jdata_sdata <- file.exists(paste0(directory_Data, "jdata", suffix, ".rds"))
  } else {
    # jdata and sdata are NOT separate
    exists.jdata_sdata <- file.exists(paste0(directory_Data, "bothdata", suffix, ".rds"))
  }

  return(exists.jdata_sdata)
}


#' Assign a unique (worker,firm) spell ID for consecutive years
#' @param dd Input data
#' @export
Do.spell.ID <- function(dd) {

  # sort by (worker,year) then convert to vectors
  dd <- dd[order(worker_ID, year)]
  worker_ID_vec <- dd[, worker_ID]
  firm_ID_vec <- dd[, firm_ID]
  year_vec <- dd[, year]
  nn <- length(worker_ID_vec)

  # loop to create a spell identifier, start new spell ID when either the worker or firm changes
  spell_ID <- rep(1, nn)
  for (ii in 2:nn) {
    if ((worker_ID_vec[ii] == worker_ID_vec[ii - 1]) & (firm_ID_vec[ii] == firm_ID_vec[ii - 1]) & (year_vec[ii] == year_vec[ii - 1] + 1)) {
      spell_ID[ii] <- spell_ID[ii - 1]
    } else {
      spell_ID[ii] <- spell_ID[ii - 1] + 1
    }
    if (ii == ceiling(ii / 1e4) * 1e4) {
      flog.info("assigned %s/%s spell IDs", ii, nn)
    }
  }

  dd$spell_ID <- spell_ID
  return(dd)
}


#' lfe::felm-based function to residualize out covariates from a given dataset.
#'
#' @description
#'  This function regresses an outcome on a given set of covariates and
#'  obtains and stores the residual for each observation.
#'
#' @param input_data The data containing the outcome and covariates (data.table).
#' @param residualize_var The name of the outcome variable (character).
#' @param discrete_vars A vector of discrete variable names (character).
#' @param continuous_vars A vector of continuous variable names (character).
#' @param return_new_vars If TRUE, returns new variables called _residual and _predicted.
#'  If FALSE, overwrites the residualize_var with its residuals.
#'
#' @export
Do.lfe.residualize <- function(input_data,
                               residualize_var,
                               discrete_vars = NULL,
                               continuous_vars = NULL,
                               return_new_vars = F) {

  # various checks
  assertDataTable(input_data)
  assertCharacter(residualize_var, len = 1)
  assertFlag(return_new_vars)
  if (is.null(discrete_vars) & is.null(continuous_vars)) {
    stop(print("At least one of `discrete_vars' and `continuous_vars' must not be NULL."))
  }
  if (!is.null(discrete_vars)) {
    for (dv in discrete_vars) {
      if (input_data[, sum(is.na(get(dv)))] > 0) {
        stop(sprintf("There are missing values in discrete variable %s", dv))
      }
    }
  }
  if (!is.null(continuous_vars)) {
    for (cv in continuous_vars) {
      if (input_data[, sum(is.na(get(cv)))] > 0) {
        stop(sprintf("There are missing values in continuous variable %s", cv))
      }
    }
  }

  # formula and model construction
  felm_formula <- paste(c(
    residualize_var,
    "~",
    paste(continuous_vars, collapse = " + "),
    "|",
    paste(discrete_vars, collapse = " + ")
  ),
  collapse = " "
  )

  flog.info("lfe::felm residualization formula: \n%s", felm_formula)

  felm_model <- felm(formula = as.formula(felm_formula), data = input_data)

  # run fast regression based on .lm.fit
  output_vector <- felm_model$residuals
  if (return_new_vars) {
    flog.info("Finished regression; putting results in %s_residual and %s_predict.", residualize_var, residualize_var)
    input_data[, (paste0(residualize_var, "_residual")) := output_vector]
    input_data[, (paste0(residualize_var, "_predict")) := get(residualize_var) - output_vector]
  } else {
    flog.info("Finished regression; overwriting %s with its residuals.", residualize_var)
    input_data[, (residualize_var) := output_vector]
  }

  return(input_data)
}


#' This simulates MWE panel data with movers, stayers, and correlated firm and worker effects
#' @param workers Number of workers to simulate
#' @param firms Number of firms to simulate
#' @export
Do.simulate.test.data <- function(workers = 2000, firms = 1000) {

  # build stayers data
  test.sdata <- data.table(worker_ID = (workers + 1):(2 * workers), year = 2001)
  test.sdata[, firm_ID := sample(1:firms, 1), worker_ID]

  # build movers data (for simplicity: everyone moves in year 3, 1 mover per firm)
  test.jdata <- data.table(expand.grid(worker_ID = 1:workers, year = 2001:2002))
  test.jdata[, firm_ID := sample(1:firms, 1), worker_ID]
  test.jdata[, firm_ID2 := sample(1:firms, 1), worker_ID]
  test.jdata[year >= 2002, firm_ID := firm_ID2]
  test.jdata[, firm_ID2 := NULL]
  test.jdata <- test.jdata[order(worker_ID, year)]

  # combine movers and stayers
  test.data <- rbindlist(list(test.jdata, test.sdata), use.names = T, fill = T)

  # draw FE and WE with correlation
  FE_WE <- MASS::mvrnorm(n = nrow(test.data), mu = c(0, 0), Sigma = matrix(c(1, .6, .6, 1), 2, 2))
  test.data[, FE_base := FE_WE[, 1]]
  test.data[, WE_base := FE_WE[, 2]]
  test.data[, true_FE := mean(FE_base), firm_ID]
  test.data[, true_WE := mean(WE_base), worker_ID]
  test.data[, FE_base := NULL]
  test.data[, WE_base := NULL]

  # build wages = firm effect + worker effect + noise
  test.data[, y := true_FE + true_WE + rnorm(nrow(test.data))]

  # add a random year of birth for completeness
  test.data[, yob := sample(1970:1975, 1), worker_ID]

  # finish
  test.data <- test.data[, list(worker_ID, year, firm_ID, age = year - yob, wages = y)]

  return(test.data)
}


#' Import raw data, which is hard-coded and very country-specific
#' @param opts Options, edit run-opts.R to modify country-specific options
#' @export
Do.load.raw.data <- function(opts) {
  wdata <- data.table()

  # Austria (AT) ---------------
  if ((grepl("AT", opts$country))) {
    # Loading data
    dta <- read_dta(opts$paths$raw)
    df <- setDT(dta)
    df <- df[, list(days_worked = df$days_worked, worker_ID = df$cod_pgr, year = df$anno, firm_ID = df$matr_az, age = df$age, wages = df$lwage)]
    wdata %<>% rbind(df)
    wdata <- wdata[(wdata$age >= 25) & (wdata$age <= 60)] # age limits as in US
    wdata$earnings <- exp(wdata$wages) * wdata$days_worked
    # Restrictions
    # Duration
    if (grepl("6", country)) {
      wdata <- wdata[(wdata$year >= 2010) & (wdata$year <= 2015)] # 6 years
    } else if (grepl("3", country)) {
      wdata <- wdata[(wdata$year >= 2013) & (wdata$year <= 2015)] # 3 years
    }
    #Full-Year Work
    if (grepl('full', country)) {
      wdata=wdata[wdata$days_worked>=365]
    }
    floor <- mean(wdata$earnings, na.rm = TRUE) * 0.325
    # Earnings Threshold
    if (grepl("thresh", country)) {
      wdata <- wdata[wdata$earnings > floor] # same restriction as US
    }
    wdata <- wdata[!((wdata$wages == "NA") | (wdata$wages <= -Inf) | (wdata$age == "NA") | (wdata$year == "NA") | (wdata$worker_ID == "NA") | (wdata$firm_ID == "NA")), ] # delete missing obs
    wdata$firm_ID <- as.numeric(factor(rank(wdata$firm_ID))) # convert due to large ID
  }

  # Italy (IT) ---------------
  if ((grepl("IT", opts$country)) & (grepl("ITKline", opts$country) == 0)) {
    # Loading data
    dta <- read_dta(opts$paths$raw)
    df <- setDT(dta)
    df <- df[, list(days_worked = df$gior_r, worker_ID = df$id, year = df$year, firm_ID = df$firmid, age = df$age, wages = log(df$daily_wage))]
    wdata %<>% rbind(df)
    wdata <- wdata[(wdata$age >= 25) & (wdata$age <= 60)] # age restriction
    wdata <- wdata[(wdata$days_worked <= 366)] # cleaning data
    wdata$earnings <- exp(wdata$wages) * wdata$days_worked
    # Restrictions
    # Duration
    if (grepl("6", country)) {
      wdata <- wdata[(wdata$year >= 1996)] # 6 years
    } else if (grepl("3", country)) {
      wdata <- wdata[(wdata$year >= 1999)] # 3 years
    }
    #Full-Year Work
    #Note: 312 days modal value
    if (grepl('full', country)) {
          wdata=wdata[wdata$days_worked>=312]
    }
    floor <- mean(wdata$earnings, na.rm = TRUE) * 0.325
    # Earnings Threshold
    if (grepl("thresh", country)) {
      wdata <- wdata[wdata$earnings > floor] # same restriction as US
    }
    wdata$firm_ID <- as.numeric(factor(rank(wdata$firm_ID))) # convert due to large ID
    wdata <- wdata[!((wdata$wages == "NA") | (wdata$wages <= -Inf) | (wdata$age == "NA") | (wdata$year == "NA") | (wdata$worker_ID == "NA") | (wdata$firm_ID == "NA")), ] # delete missing obs
  }

  # Italy for Kline et al sample (ITKline) ---------------
  if (grepl("ITKline", opts$country)) {
    # Loading data
    dta <- read_dta(opts$paths$raw)
    # Keep only certain vars but keep data as is otherwise
    df <- dta[, (names(dta) %in% c("log_dailywages", "year", "id", "firmidnew", "age"))]
    df <- setDT(df)
    df <- df[, list(worker_ID = df$id, year = df$year, firm_ID = df$firmidnew, age = df$age, wages = (df$log_dailywages))]
    wdata %<>% rbind(df)
    if (grepl("ITKline2", opts$country)) {
      lM <- lm(wages ~ factor(year), data = wdata) # residualization only year effects
      wdata[, wages_residual := wages - predict(lM)]
      wdata[, wages_predict := wages - (wages - predict(lM))]
    }
    wdata <- wdata[!((wdata$wages == "NA") | (wdata$wages <= -Inf) | (wdata$age == "NA") | (wdata$year == "NA") | (wdata$worker_ID == "NA") | (wdata$firm_ID == "NA")), ] # delete missing obs
  }

  # Norway (NO) -----------------------
  if (opts$country %in% c("NO", "NO3thresh")) {
    wdata <- setDT(readRDS(file = "/ssb/stamme01/swp/lae/wk48/labor/data/data_0926.rds"))
    if (opts$country == "NO3thresh") {
      wdata <- wdata[year %in% c(2, 3, 4)]
    }
  }
  if (opts$country %in% c("NO-hourlywage", "NO3thresh-hourlywage")) {
    wdata <- setDT(readRDS(file = "/ssb/stamme01/swp/lae/wk48/labor/data/data_0926_hourlywage.rds"))
    if (opts$country == "NO3thresh-hourlywage") {
      wdata <- wdata[year %in% c(2, 3, 4)]
    }
  }
  if (opts$country %in% c("NO6thresh-annualearnings", "NO3thresh-annualearnings")) {
    wdata <- setDT(readRDS(file = "/ssb/stamme01/swp/lae/wk48/labor/data/data_0926_annualearnings.rds"))
    if (opts$country == "NO3thresh-annualearnings") {
      wdata <- wdata[year %in% c(2, 3, 4)]
    }
  }

  # Sweden (full sample) ---------------
  if (substr(opts$country, 1, 2) == "SW") {
    load("L:\\Tibo\\qtrdata\\smfe-data-all.dat")
    wdata <- rdata
    rm(rdata)
    wdata <- wdata[!((wdata$wages == "NA") | (wdata$age == "NA") | (wdata$year == "NA") | (wdata$worker_ID == "NA") | (wdata$firm_ID == "NA")), ] # delete missing obs
    wdata$firm_ID <- as.numeric(factor(rank(wdata$firm_ID))) # convert due to large ID
  }

  # Sweden (attrition sample)
  if (opts$country == "SW-attrition") {
    flog.info("loading swedish data for SW-attrition")
    load("L:\\Tibo\\qtrdata\\smfe-data-all.dat")
    flog.info("done loading data.")
    wdata <- rdata
    rm(rdata)

    # Restrictions
    wdata <- wdata[(year >= 2001) & (year <= 2005)] # only one year keep
    wdata <- wdata[monthsworked == 12] # full time work
    wdata[, firm_ID2 := .GRP, firm_ID]
    wdata$firm_ID <- NULL
    wdata[, firm_ID := firm_ID2]
    wdata$firm_ID2 <- NULL
    flog.info("done preparing data.")
  }


  # US ---------------
  if (substr(opts$country, 1, 3) == "US-") {
    # set the year range
    if (str_detect(opts$country, "late")) {
      year_set <- 2010:2015 # these are the "late" years in the sample
    }
    if (str_detect(opts$country, "early")) {
      year_set <- 2001:2006 # these are the "early" years in the sample
    }
    if (str_detect(opts$country, "select")) {
      year_set <- opts$misc$begin_year:opts$misc$end_year # these are the custom "select" years in the sample
    }
    # set the minimum earnings threshold
    minearn <- 15000 # this is the default minimum earnings threshold, in 2015 USD
    if (str_detect(opts$country, "minearn")) {
      startindex <- str_locate(opts$country, "minearn")[2]
      endindex <- nchar(opts$country)
      minearn <- readr::parse_number(substr(opts$country, startindex + 1, endindex)) # this is the custom minimum earnings threshold, if specified
    }
    # set the firm size threshold
    minfirmsize <- 0 # this is the default firm size threshold, in number of worker observations per firm
    if (str_detect(opts$country, "minfirmsize")) {
      startindex <- str_locate(opts$country, "minfirmsize")[2]
      endindex <- nchar(opts$country)
      minfirmsize <- readr::parse_number(substr(opts$country, startindex + 1, endindex)) # this is the custom minimum firm size threshold, if specified
    }
    # load the year of birth (ssa_yob), which came from a separate SSA file
    yob_data <- readRDS(sprintf("%s/Demographic/demographic_%s.rds", opts$paths$raw, 2010))[!is.na(ssa_yob) & ssa_yob > 1925 & ssa_yob < 2005][, list(worker_ID, yob = ssa_yob)]
    # load the firm and worker data
    for (yr in year_set) {
      flog.info("loading %s", yr)
      # get the set of private sector firms in this year
      firms_year <- rbindlist(list(
        readRDS(file = sprintf("%sF1120/f1120_%s.rds", opts$paths$raw, yr)),
        readRDS(file = sprintf("%sF1120s/f1120s_%s.rds", opts$paths$raw, yr)),
        readRDS(file = sprintf("%sF1065/f1065_%s.rds", opts$paths$raw, yr))
      ), use.names = T, fill = T)
      private_sector_firms <- unique(firms_year$firm_ID)
      firms_year <- NULL
      gc()
      # load all wage records then restrict to private sector and keep only relevant variables
      wage_data <- setDT(readRDS(file = sprintf("%sW2/w2_%s.rds", opts$paths$raw, yr))) # note: these are CPI-adjusted wages in 2015 USD
      wage_data <- wage_data[wages > minearn][firm_ID %in% private_sector_firms]
      gc() # impose the minimum earnings threshold
      wage_data %<>% merge(yob_data, by = "worker_ID") # merge the ssa_yob to the worker records
      wage_data <- wage_data[, list(worker_ID, year = yr, firm_ID, birth_year = yob, wages = log(wages), state)]
      gc() # keep only the needed variables and convert wages to log units
      # restrict to a state, if specified
      unique_states <- wage_data[, unique(state)]
      possible_state <- substr(opts$country, 4, 5) # this is the custom state restriction, if specified
      if (possible_state %in% unique_states) { # if a custom state is specified
        flog.info("restricting to state %s", possible_state)
        wage_data <- wage_data[state == possible_state]
        gc() # restrict the wage data to the specified state
      }
      wage_data[, state := NULL] # drop the state variable now that it has been used to restrict the sample, if specified
      # firm size
      if (minfirmsize > 0) { # impose the minimum firm size restriction, if specified
        wage_data[, firmsize := .N, firm_ID] # count the number of observations at the firm
        wage_data <- wage_data[firmsize >= minfirmsize]
        wage_data[, firmsize := NULL]
        gc()
      }
      # finish
      wdata %<>% rbind(wage_data)
      wage_data <- NULL
      private_sector_firms <- NULL
      gc()
    }
    wdata[, age := year - birth_year]
    wdata <- na.omit(wdata)
  }


  # Test data -----------------
  if (substr(opts$country, 1, 8) == "Testing-") {
    wdata <- Do.simulate.test.data(workers = 10000, firms = 1000)
  }

  # make sure data loaded
  if (nrow(wdata) == 0) {
    stop(sprintf("Data failed to load for country %s. Please check Do.load.raw.data for mistakes.", opts$country))
  }

  wdata_names <- names(wdata)
  required_names <- c("worker_ID", "firm_ID", "year", "age", "wages")
  for (vv in required_names) {
    if (!(vv %in% wdata_names)) {
      stop(sprintf("Raw data is missing required variable: %s", vv))
    }
  }

  # Force variable types
  wdata$firm_ID <- as.integer(wdata$firm_ID)
  wdata$worker_ID <- as.integer(wdata$worker_ID)
  wdata$year <- as.integer(wdata$year)

  return(wdata)
}


#' Take care of any miscellaneous country-specific sample construction while the wdata is available in memory
#' @export
Do.misc.sample.construction <- function(wdata, opts) {

  # noendpoints case
  if (str_detect(opts$country, "noendpoints")) {

    # construct the mean wages for spells of at least length 3, dropping first and last year (3-year stayers)
    spells_noendpoints <- wdata[year > spell_begin & year < spell_end][, list(log_wage = mean(wages), begin = spell_begin[1], end = spell_end[1], spell_length = .N), list(worker_ID, spell_ID, firm_ID)]
    saveRDS(spells_noendpoints, file = paste0(opts$paths$data, "base.rds"), compress = F)

    # keep those with 6-year employment, who are either 6-year stayers or 3-and-3 year movers
    wdata[, active_years := .N, list(worker_ID)]
    employed6 <- wdata[active_years == 6 & (spell_end - spell_begin) %in% c(2, 5)]
    employed6[, spells_per_worker := length(unique(spell_ID)), list(worker_ID)]
    employed6 <- employed6[spells_per_worker <= 2]
    gc()
    saveRDS(employed6, file = paste0(opts$paths$data, "employed6years.rds"), compress = F)
  }

  saveRDS(wdata, file = paste0(opts$paths$data, "yearly.rds"), compress = F)

  return(NULL)
}


#' Import raw data and convert to spell data
#' @export
Do.sample.init <- function(opts, data_filter_function = function(d) d) {

  # check if output already exists
  output_exists <- file.exists(paste0(opts$paths$data, "base.rds"))
  if (output_exists) {
    if (opts$misc$preserve.rawdata) {
      flog.info("Do.sample.init: Output already exists. misc$preserve.rawdata==TRUE so skipping.")
      return(NULL)
    } else {
      flog.info("Do.sample.init: Output already exists. misc$preserve.rawdata==FALSE so writing over it.")
    }
  }

  # load country-specific raw data
  wdata <- Do.load.raw.data(opts)
  wdata <- data_filter_function(wdata)

  if (!(opts$misc$grouping_var == "wages")) {
    setnames(wdata, opts$misc$grouping_var, "grouping_var")
  }

  # residualization
  flatage <- 40
  wdata[, `:=`(ones = 1, agesq = (age - flatage)^2, agecu = (age - flatage)^3)]
  if (grepl("ITKline2", opts$country) == F) { # only ITKline 2 years no age residualization
    wdata <- Do.lfe.residualize(
      input_data = wdata, residualize_var = "wages", discrete_vars = "year",
      continuous_vars = c("ones", "agesq", "agecu"), return_new_vars = T
    )
  }
  wdata[, `:=`(ones = NULL, agesq = NULL, agecu = NULL)]
  gc()
  wdata[, wages := NULL]
  setnames(wdata, "wages_residual", "wages")

  # find spell ID
  setorderv(wdata, cols = c("worker_ID", "year"))
  if ("paneltools" %in% .packages()) {
    flog.info("package paneltools found. Finding spell ID with paneltools::computeSpellID.")
    wdata[, spell_ID := paneltools::computeSpellID(worker_ID, year, firm_ID)]
  } else {
    flog.info("package paneltools NOT found. Finding spell ID with smfe::Do.spell.ID which may be slower.")
    wdata <- Do.spell.ID(wdata)
  }
  wdata[, spell_begin := min(year), list(spell_ID)]
  wdata[, spell_end := max(year), list(spell_ID)]

  flog.info("saving spells")
  # construct the mean wages for full spells (unrestricted stayers)
  if (opts$misc$grouping_var == "wages") {
    spells_full <- wdata[, list(log_wage = mean(wages), begin = spell_begin[1], end = spell_end[1], spell_length = .N), list(worker_ID, spell_ID, firm_ID)]
  } else {
    spells_full <- wdata[, list(log_wage = mean(wages), begin = spell_begin[1], end = spell_end[1], spell_length = .N, grouping_var = grouping_var[1]), list(worker_ID, spell_ID, firm_ID)]
  }
  saveRDS(spells_full, file = paste0(opts$paths$data, "base.rds"), compress = F)

  # take care of any miscellaneous country-specific sample construction issues while the wdata is available in memory
  Do.misc.sample.construction(wdata, opts)

  wdata <- NULL
  gc() # help manage memory
  return(NULL)
}


#' Import spell data, convert to jdata_all and sdata_all
#' @export
Do.jdata_sdata.make <- function(opts) {

  # check if output already exists
  output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$data, suffix = "_all", data_type = opts$loading$data_type, opts)
  if (output_exists) {
    if (opts$misc$preserve.initdata) {
      flog.info("Do.jdata_sdata.make: Output already exists. misc$preserve.initdata==TRUE so skipping.")
      return(NULL)
    } else {
      flog.info("Do.jdata_sdata.make: Output already exists. misc$preserve.initdata==FALSE so writing over it.")
    }
  }

  spells <- setDT(readRDS(file = paste0(opts$paths$data, "base.rds")))

  spells[, unique_spells := .N, list(worker_ID)]

  if (opts$misc$grouping_var == "wages") {
    # construct sdata
    sdata <- spells[unique_spells == 1][, list(wid = worker_ID, f1 = firm_ID, f2 = firm_ID, y1 = log_wage, y2 = log_wage, spell_length)]
    # construct jdata (use the next spell in order as the f2,y2 for a given f1,y1 by wid)
    jdata <- spells[unique_spells > 1][, list(wid = worker_ID, f1 = firm_ID, y1 = log_wage, begin, spell_length)][order(wid, begin)][, begin := NULL]
  } else {
    # construct sdata
    sdata <- spells[unique_spells == 1][, list(wid = worker_ID, f1 = firm_ID, f2 = firm_ID, y1 = log_wage, y2 = log_wage, spell_length, grouping_var)]
    # construct jdata (use the next spell in order as the f2,y2 for a given f1,y1 by wid)
    jdata <- spells[unique_spells > 1][, list(wid = worker_ID, f1 = firm_ID, y1 = log_wage, begin, spell_length, grouping_var)][order(wid, begin)][, begin := NULL]
  }

  jdata[, spell_index := 1:.N, list(wid)] # not necessary for _full files since spells should be in order, but necessary for _noendpoints case
  jdata2 <- copy(jdata)
  jdata2[, spell_index := spell_index - 1]
  jdata2[, grouping_var := NULL]
  setnames(jdata2, c("f1", "y1"), c("f2", "y2"))
  setnames(jdata2, "spell_length", "spell_length2")
  jdata <- merge(jdata, jdata2, by = c("wid", "spell_index"))[, spell_index := NULL]

  jdata_samefirm <- jdata[f1 == f2]
  jdata <- jdata[f1 != f2]

  if (opts$misc$grouping_var == "wages") {
    jdata_samefirm1 <- jdata_samefirm[, list(wid, f1 = f1, f2 = f1, y1 = y1, y2 = y1, spell_length = spell_length)]
    jdata_samefirm2 <- jdata_samefirm[, list(wid, f1 = f2, f2 = f2, y1 = y2, y2 = y2, spell_length = spell_length2)]
  } else {
    jdata_samefirm1 <- jdata_samefirm[, list(wid, f1 = f1, f2 = f1, y1 = y1, y2 = y1, spell_length = spell_length, grouping_var)]
    jdata_samefirm2 <- jdata_samefirm[, list(wid, f1 = f2, f2 = f2, y1 = y2, y2 = y2, spell_length = spell_length2, grouping_var)]
  }
  jdata_samefirm <- rbind(jdata_samefirm1, jdata_samefirm2)

  sdata <- rbind(sdata, jdata_samefirm)

  # export as list
  ad <- list(sdata = sdata, jdata = jdata)
  ad$sdata$f1 <- as.character(ad$sdata$f1)
  ad$sdata$f2 <- as.character(ad$sdata$f2)
  ad$jdata$f1 <- as.character(ad$jdata$f1)
  ad$jdata$f2 <- as.character(ad$jdata$f2)
  Do.writing.jdata_sdata(ad, directory_Data = opts$paths$data, suffix = "_all", data_type = opts$loading$data_type, opts)
}


#' execute all the set restrictions
#' @export
Do.jdata_sdata.restrict_set.execute <- function(opts) {

  # use suffixes to determine if connected set or leave-one-out set is required for this opts
  all_suffixes <- unique(unlist(opts$suffix.set, recursive = FALSE))
  any_connected <- max(str_detect(all_suffixes, "_connected"), na.rm = T)
  any_leaveout <- max(str_detect(all_suffixes, "_leaveout"), na.rm = T)

  # execute the set restrictions
  if (any_connected) {
    Do.jdata_sdata.restrict_set(leaveout = FALSE, opts)
  }
  if (any_leaveout) {
    Do.jdata_sdata.restrict_set(leaveout = TRUE, opts)
  }
  if (!any_connected & !any_leaveout) {
    flog.info("Do.jdata_sdata.restrict_set.execute: No set restrictions to run.")
  }
}


#' execute all the clustering
#' @export
Do.jdata_sdata.cluster.execute <- function(opts) {

  # use suffixes determine which clusters need to be computed
  all_suffixes <- unique(unlist(opts$suffix.set, recursive = FALSE))
  all_suffixes <- all_suffixes[str_detect(all_suffixes, "_clusters")]
  connected_clusters <- all_suffixes[str_detect(all_suffixes, "_connected")]
  all_clusters <- all_suffixes[str_detect(all_suffixes, "_all")]
  leaveout_clusters <- all_suffixes[str_detect(all_suffixes, "_leaveout")]

  # for those that need clustering, infer the clusters needed, then run them
  if (length(connected_clusters) == 0 & length(all_clusters) == 0 & length(leaveout_clusters)) {
    stop("Suffix list does not have any cluster-related suffixes.")
  }
  if (length(connected_clusters) > 0) {
    cluster.set <- readr::parse_number(sub(".*clusters", "", connected_clusters))
    cluster.set <- cluster.set[!is.na(cluster.set)]
    flog.info("clustering suffix %s with clusters %s", "_connected", paste(cluster.set, collapse = ","))
    Do.jdata_sdata.cluster.run_multiple(cluster.set, suffix = "_connected", opts = opts)
  }
  if (length(all_clusters) > 0) {
    cluster.set <- readr::parse_number(sub(".*clusters", "", all_clusters))
    cluster.set <- cluster.set[!is.na(cluster.set)]
    flog.info("clustering suffix %s with clusters %s", "_all", paste(cluster.set, collapse = ","))
    Do.jdata_sdata.cluster.run_multiple(cluster.set, suffix = "_all", opts = opts)
  }
  if (length(leaveout_clusters) > 0) {
    cluster.set <- readr::parse_number(sub(".*clusters", "", leaveout_clusters))
    cluster.set <- cluster.set[!is.na(cluster.set)]
    flog.info("clustering suffix %s with clusters %s", "_leaveout", paste(cluster.set, collapse = ","))
    Do.jdata_sdata.cluster.run_multiple(cluster.set, suffix = "_leaveout", opts = opts)
  }
}


#' Restrict jdata/sdata to the connected set
#' @export
Do.jdata_sdata.connected <- function(ad) {
  f0s <- get.largest.conset.fid(ad$jdata)
  ad$jdata <- ad$jdata[f1 %in% f0s][f2 %in% f0s]
  ad$sdata <- ad$sdata[f1 %in% f0s]
  return(ad)
}

#' Restrict jdata/sdata to the leave-one-out connected set
#' @export
Do.jdata_sdata.leaveout <- function(ad) {
  number.of.moves <- nrow(ad$jdata)
  ad$jdata <- get.largest.leaveoutset.fid2(ad$jdata)
  f1s <- ad$jdata[, unique(c(f1, f2))]
  ad$sdata <- ad$sdata[f1 %in% f1s]
  return(ad)
}

#' Restrict jdata/sdata
#' @param leaveout If TRUE, leave-one-out connected set. If FALSE, connected set.
#' @export
Do.jdata_sdata.restrict_set <- function(leaveout = FALSE, opts) {

  # check if output already exists
  output_exists <- FALSE
  if (leaveout) {
    output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$data, suffix = "_leaveout", data_type = opts$loading$data_type, opts)
  } else {
    output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$data, suffix = "_connected", data_type = opts$loading$data_type, opts)
  }
  if (output_exists) {
    if (opts$misc$preserve.initdata) {
      flog.info("Do.jdata_sdata.restrict_set: Output already exists. misc$preserve.initdata==TRUE so skipping.")
      return(NULL)
    } else {
      flog.info("Do.jdata_sdata.restrict_set: Output already exists. misc$preserve.initdata==FALSE so writing over it.")
    }
  }

  # importantly, suffix='all' here for the initial ad, prior to any set restrictions
  ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$data, suffix = "_all", data_type = opts$loading$data_type, opts)

  # perform set restriction
  if (leaveout) {
    ad <- Do.jdata_sdata.leaveout(ad)
    Do.writing.jdata_sdata(ad, directory_Data = opts$paths$data, suffix = "_leaveout", data_type = opts$loading$data_type, opts)
    Do.export.jdata_sdata(suffix = "_leaveout", opts)
  } else {
    ad <- Do.jdata_sdata.connected(ad)
    Do.writing.jdata_sdata(ad, directory_Data = opts$paths$data, suffix = "_connected", data_type = opts$loading$data_type, opts)
    Do.export.jdata_sdata(suffix = "_connected", opts)
  }

  return(NULL)
}


#' Restrict jdata/sdata
#' @param leaveout If TRUE, leave-one-out connected set. If FALSE, connected set.
#' @param nclus Number of clusters
#' @export
Do.jdata_sdata.cluster <- function(nclus = 10, suffix, opts) {
  suffix_out <- sprintf("%s_clusters%s", suffix, nclus)

  # check if output already exists
  output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$data, suffix = suffix_out, data_type = opts$loading$data_type, opts)
  if (output_exists) {
    if (opts$misc$preserve.estimates) {
      flog.info("Do.jdata_sdata.cluster for suffix %s and %s clusters: Output already exists. misc$preserve.estimates==TRUE so skipping.", suffix, nclus)
      return(NULL)
    } else {
      flog.info("Do.jdata_sdata.cluster for suffix %s and %s clusters: Output already exists. misc$preserve.estimates==FALSE so writing over it.", suffix, nclus)
    }
  }

  ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$data, suffix = suffix, data_type = opts$loading$data_type, opts)

  flog.info("Estimating %s clusters for suffix %s", nclus, suffix)
  if (opts$misc$grouping_var == "wages") {
    if (opts$misc$cluster_bothyears) {
      combined_data <- rbindlist(list(ad$jdata[, list(wid, f1, y1)], ad$jdata[, list(wid, f1 = f2, y1 = y2)], ad$sdata[, list(wid, f1, y1)]))
    } else {
      combined_data <- rbindlist(list(ad$jdata[, list(wid, f1, y1)], ad$sdata[, list(wid, f1, y1)]))
    }
    ms <- grouping.getMeasures(list(sdata = combined_data, jdata = ad$jdata), "ecdf", Nw = 20, y_var = "y1")
  } else {
    combined_vars <- c("wid", "f1", "y1", "grouping_var")
    combined_data <- rbindlist(list(ad$jdata[, .SD, .SDcols = combined_vars], ad$sdata[, .SD, .SDcols = combined_vars]))
    ms <- grouping.getMeasures(list(sdata = combined_data, jdata = ad$jdata), "frequency", y_var = "grouping_var")
  }

  grps <- grouping.classify.once(ms, k = nclus, nstart = opts$misc$nstart, iter.max = 100, step = 1)
  ad <- grouping.append(list(sdata = ad$sdata, jdata = ad$jdata), grps$best_cluster, drop = T)
  ms <- NULL
  grps <- NULL
  gc()

  Do.writing.jdata_sdata(ad, directory_Data = opts$paths$data, suffix = suffix_out, data_type = opts$loading$data_type, opts)
  Do.export.jdata_sdata(suffix = suffix_out, opts)
}

#' Restrict jdata/sdata, skip the input/output architecture
#' @param ad jdata and sdata
#' @param nclus Number of clusters
#' @export
Do.jdata_sdata.cluster.noIO <- function(ad, nclus = 10, nstart = 30) {
  combined_data <- rbindlist(list(ad$jdata[, list(wid, f1, y1)], ad$sdata[, list(wid, f1, y1)]))
  ms <- grouping.getMeasures(list(sdata = combined_data, jdata = ad$jdata), "ecdf", Nw = 20, y_var = "y1")
  grps <- grouping.classify.once(ms, k = nclus, nstart = nstart, iter.max = 100, step = 1)

  ad <- grouping.append(list(sdata = ad$sdata, jdata = ad$jdata), grps$best_cluster, drop = T)
  ms <- NULL
  grps <- NULL
  gc()

  return(ad)
}


#' handle multiple cluster numbers with parallel-option
#' @export
Do.jdata_sdata.cluster.run_multiple <- function(cluster.set, suffix, opts) {
  ncores <- opts$misc$ncores
  if (ncores > 1) {
    parallel::mclapply(cluster.set, Do.jdata_sdata.cluster, mc.cores = ncores, suffix = suffix, opts = opts)
  } else {
    lapply(cluster.set, Do.jdata_sdata.cluster, suffix = suffix, opts = opts)
  }
}


# ---COMBINED ESTIMATION CALLS ----

#' execute all the CRE suffixes
#' @export
Do.CRE.execute <- function(opts) {
  flog.info("[Do.CRE] Beginning CRE execution")
  suffix.set <- opts$suffix.set$CRE
  any_suffixes <- any(!is.na(suffix.set))
  ncores <- opts$misc$ncores
  if (any_suffixes) {
    if (ncores > 1) {
      parallel::mclapply(suffix.set, Do.CRE.estimate, mc.cores = ncores, opts = opts)
      parallel::mclapply(suffix.set, Do.CRE.decomp, mc.cores = ncores, opts = opts)
    } else {
      lapply(suffix.set, Do.CRE.estimate, opts = opts)
      lapply(suffix.set, Do.CRE.decomp, opts = opts)
    }
    flog.info("[Do.CRE] Finished CRE execution")
  } else {
    flog.info("[Do.CRE] Do.CRE.execute skipped: there are no suffixes to run on")
  }
}

#' execute all the AKM suffixes
#' @export
Do.AKM.execute <- function(opts) {
  flog.info("[Do.AKM] Beginning AKM execution")
  suffix.set <- opts$suffix.set$AKM
  any_suffixes <- any(!is.na(suffix.set))
  ncores <- opts$misc$ncores
  if (any_suffixes) {
    if (ncores > 1) {
      parallel::mclapply(suffix.set, Do.AKM.estimate, mc.cores = ncores, opts = opts)
      parallel::mclapply(suffix.set, Do.AKM.decomp, mc.cores = ncores, opts = opts)
    } else {
      lapply(suffix.set, Do.AKM.estimate, opts = opts)
      lapply(suffix.set, Do.AKM.decomp, opts = opts)
    }
    flog.info("[Do.AKM] Finished AKM execution")
  } else {
    flog.info("[Do.AKM] Do.AKM.execute skipped: there are no suffixes to run on")
  }
}

#' execute all the TraceHO suffixes
#' @export
Do.TraceHO.execute <- function(opts, diff = FALSE) {
  if (diff == FALSE) {
    if ("sparseutils" %in% .packages()) {
      flog.info("[Do.TraceHO] Beginning Trace-HO execution with diff=%s", diff)
    } else {
      flog.info("[Do.TraceHO] Required package sparseutils is missing. Skipping Trace-HO execution with diff=%s.", diff)
      return(NULL)
    }
  }
  suffix.set <- opts$suffix.set$TraceHO
  any_suffixes <- any(!is.na(suffix.set))
  ncores <- opts$misc$ncores
  if (any_suffixes) {
    if (ncores > 1) {
      parallel::mclapply(suffix.set, Do.TraceHO.estimate, mc.cores = ncores, opts = opts, diff = diff)
      parallel::mclapply(suffix.set, Do.TraceHO.decomp, mc.cores = ncores, opts = opts, diff = diff)
    } else {
      lapply(suffix.set, Do.TraceHO.estimate, opts = opts, diff = diff)
      lapply(suffix.set, Do.TraceHO.decomp, opts = opts, diff = diff)
    }
    flog.info("[Do.TraceHO] Finished Trace-HO execution with diff=%s", diff)
  } else {
    flog.info("[Do.TraceHO] Do.TraceHO.execute skipped: there are no suffixes to run on")
  }
}

#' execute all the TraceHE suffixes
#' @export
Do.TraceHE.execute <- function(opts, diff = FALSE) {
  flog.info("[Do.TraceHE] Beginning Trace-HE execution with diff=%s", diff)
  if (diff == FALSE) {
    if ("sparseutils" %in% .packages()) {
      flog.info("[Do.TraceHE] Beginning Trace-HE execution with diff=%s", diff)
    } else {
      flog.info("[Do.TraceHE] Required package sparseutils is missing. Skipping Trace-HE execution with diff=%s.", diff)
      return(NULL)
    }
  }
  suffix.set <- opts$suffix.set$TraceHE
  any_suffixes <- any(!is.na(suffix.set))
  ncores <- opts$misc$ncores
  if (any_suffixes) {
    if (ncores > 1) {
      parallel::mclapply(suffix.set, Do.TraceHE.estimate, mc.cores = ncores, opts = opts, diff = diff)
      parallel::mclapply(suffix.set, Do.TraceHE.decomp, mc.cores = ncores, opts = opts, diff = diff)
    } else {
      lapply(suffix.set, Do.TraceHE.estimate, opts = opts, diff = diff)
      lapply(suffix.set, Do.TraceHE.decomp, opts = opts, diff = diff)
    }
    flog.info("[Do.TraceHE] Finished Trace-HE execution with diff=%s", diff)
  } else {
    flog.info("[Do.TraceHE] Do.TraceHE.execute skipped: there are no suffixes to run on")
  }
}

#' execute all the interacted suffixes
#' @export
Do.interacted.execute <- function(opts) {
  flog.info("[Do.interacted] Beginning BLM-interacted execution")
  suffix.set <- opts$suffix.set$interacted
  any_suffixes <- any(!is.na(suffix.set))
  ncores <- opts$misc$ncores
  if (any_suffixes) {
    if (ncores > 1) {
      parallel::mclapply(suffix.set, Do.interacted.estimate, mc.cores = ncores, opts = opts)
      parallel::mclapply(suffix.set, Do.interacted.decomp, mc.cores = ncores, opts = opts)
    } else {
      lapply(suffix.set, Do.interacted.estimate, opts = opts)
      lapply(suffix.set, Do.interacted.decomp, opts = opts)
    }
    flog.info("[Do.interacted] Finished BLM-interacted execution")
  } else {
    flog.info("[Do.interacted] Do.interacted.execute skipped: there are no suffixes to run on")
  }
}


#' run AKM without the input/output architecture
#' @export
Do.AKM.estimate.noIO <- function(ad) {

  # reshape data
  long_data <- rbindlist(list(
    ad$jdata[, list(worker_ID = wid, firm_ID = f1, wages_pt = y1)],
    ad$jdata[, list(worker_ID = wid, firm_ID = f2, wages_pt = y2)]
  ))

  if ("akm" %in% .packages()) {
    flog.info("[Do.AKM] Package akm found. Estimating AKM on the jdata with akm.estimate.zig_zag.")
    firm_effects <- akm.estimate.zig_zag(long_data, outcome_var = "wages_pt", tol = 0, max_iterations = 100, report_frequency = 10)$firms_with_fixedeffects
  } else {
    flog.info("[Do.AKM] Package akm NOT found. Estimating AKM on the jdata with lfe::getfe, which may be slower.")
    felm_res <- felm(as.formula("wages_pt ~ 1 | worker_ID + firm_ID"), data = long_data)
    firm_effects <- setDT(getfe(felm_res))[fe == "firm_ID"][, list(firm_ID = as.character(idx), akm_firm_effect = effect)]
  }

  flog.info("[Do.AKM] Injecting AKM firm effects back in to the rbind(jdata,sdata) and estimating AKM worker effects")
  long_data <- rbindlist(list(
    ad$jdata[, list(worker_ID = wid, firm_ID = f1, wages_pt = y1)],
    ad$jdata[, list(worker_ID = wid, firm_ID = f2, wages_pt = y2)],
    ad$sdata[, list(worker_ID = wid, firm_ID = f1, wages_pt = y1)]
  ))
  long_data <- merge(long_data, firm_effects, by = "firm_ID")
  long_data[, akm_worker_effect := mean(wages_pt - akm_firm_effect), list(worker_ID)]

  return(long_data)
}


#' run AKM
#' @export
Do.AKM.estimate <- function(suffix, opts) {

  # check if output already exists
  output_exists <- file.exists(sprintf("%sakm_%s.rds", opts$paths$res, suffix))
  if (output_exists) {
    if (opts$misc$preserve.estimates) {
      flog.info("[Do.AKM] Do.AKM.estimate: Output already exists. misc$preserve.estimates==TRUE so skipping.")
      return(NULL)
    } else {
      flog.info("[Do.AKM] Do.AKM.estimate: Output already exists. misc$preserve.estimates==FALSE so writing over it.")
    }
  }

  # prepare data
  ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$data, suffix = suffix, data_type = opts$loading$data_type, opts)

  # get AKM results
  long_data <- Do.AKM.estimate.noIO(ad)

  flog.info("[Do.AKM] export results for %s", suffix)
  saveRDS(long_data, file = sprintf("%sakm_%s.rds", opts$paths$res, suffix), compress = F)

  return(NULL)
}


#' run Trace estimation
#' @export
Do.TraceHO.estimate <- function(suffix, opts, diff = FALSE) {

  # check if output already exists
  if (diff) {
    ff <- sprintf("%straceHOdiff_%s.rds", opts$paths$res, suffix)
  } else {
    ff <- sprintf("%straceHOlevel_%s.rds", opts$paths$res, suffix)
  }

  output_exists <- file.exists(ff)
  if (output_exists) {
    if (opts$misc$preserve.estimates) {
      flog.info("[Do.TraceHO] Do.TraceHO.estimate: Output already exists. misc$preserve.estimates==TRUE so skipping.")
      return(NULL)
    } else {
      flog.info("[Do.TraceHO] Do.TraceHO.estimate: Output already exists. misc$preserve.estimates==FALSE so writing over it.")
    }
  }

  # prepare data
  ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$data, suffix = suffix, data_type = opts$loading$data_type, opts)

  flog.info("[Do.TraceHO] running TraceHO for %s with diff=%s", suffix, diff)
  if (diff) {
    trace_res <- m2.trace.estimate(ad, hetero = FALSE, check_set = FALSE, var_psi_only = TRUE)
  } else {
    trace_res <- m2.trace.estimate.full(ad, hetero = FALSE, check_set = FALSE, impose_leaveout = FALSE, make_each_row_one_wid = TRUE)
  }

  flog.info("[Do.TraceHO] exporting TraceHO results for %s with diff=%s", suffix, diff)
  saveRDS(trace_res, file = ff, compress = F)

  return(NULL)
}


#' run Trace estimation
#' @export
Do.TraceHE.estimate <- function(suffix, opts, diff = FALSE) {

  # check if output already exists
  if (diff) {
    ff <- sprintf("%straceHEdiff_%s.rds", opts$paths$res, suffix)
  } else {
    ff <- sprintf("%straceHElevel_%s.rds", opts$paths$res, suffix)
  }

  output_exists <- file.exists(ff)
  if (output_exists) {
    if (opts$misc$preserve.estimates) {
      flog.info("[Do.TraceHE] Do.TraceHE.estimate: Output already exists. misc$preserve.estimates==TRUE so skipping.")
      return(NULL)
    } else {
      flog.info("[Do.TraceHE] Do.TraceHE.estimate: Output already exists. misc$preserve.estimates==FALSE so writing over it.")
    }
  }

  # prepare data
  ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$data, suffix = suffix, data_type = opts$loading$data_type, opts)

  flog.info("[Do.TraceHE] running TraceHE for %s with diff=%s", suffix, diff)
  if (diff) {
    trace_res <- m2.trace.estimate(ad, hetero = TRUE, check_set = FALSE, var_psi_only = TRUE)
  } else {
    trace_res <- m2.trace.estimate.full(ad, hetero = TRUE, check_set = FALSE, impose_leaveout = FALSE, make_each_row_one_wid = TRUE)
  }

  flog.info("[Do.TraceHE] exporting TraceHE results for %s with diff=%s", suffix, diff)
  saveRDS(trace_res, file = ff, compress = F)

  return(NULL)
}


#' run CRE/CRE-0
#' @export
Do.CRE.estimate <- function(suffix = "", opts) {

  # check if output already exists
  output_exists <- file.exists(sprintf("%scre_%s.rds", opts$paths$res, suffix))
  if (output_exists) {
    if (opts$misc$preserve.estimates) {
      flog.info("[Do.CRE] Do.CRE.estimate: Output already exists. misc$preserve.estimates==TRUE so skipping.")
      return(NULL)
    } else {
      flog.info("[Do.CRE] Do.CRE.estimate: Output already exists. misc$preserve.estimates==FALSE so writing over it.")
    }
  }

  ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$data, suffix = suffix, data_type = opts$loading$data_type, opts)

  flog.info("[Do.CRE] running linear.cre for suffix %s", suffix)
  cre_res <- m2.cre.estimate(ad)
  filename <- sprintf("%scre_%s.rds", opts$paths$res, suffix)
  flog.info("[Do.CRE] exporting linear.cre results for %s (%s)", suffix, filename)
  saveRDS(cre_res, file = filename, compress = F)

  return(NULL)
}

#' run interacted BLM
#' @export
Do.interacted.estimate <- function(suffix = "", opts) {

  # check if output already exists
  output_exists <- file.exists(sprintf("%sinteracted_%s.rds", opts$paths$res, suffix))
  if (output_exists) {
    if (opts$misc$preserve.estimates) {
      flog.info("[Do.interacted] Do.interacted.estimate: Output already exists. misc$preserve.estimates==TRUE so skipping.")
      return(NULL)
    } else {
      flog.info("[Do.interacted] Do.interacted.estimate: Output already exists. misc$preserve.estimates==FALSE so writing over it.")
    }
  }

  ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$data, suffix = suffix, data_type = opts$loading$data_type, opts)

  blm_res <- m2.mini.estimate(jdata = ad$jdata, sdata = ad$sdata, method = "prof")

  flog.info("[Do.interacted] exporting interacted results for %s", suffix)
  saveRDS(blm_res, file = sprintf("%sinteracted_%s.rds", opts$paths$res, suffix), compress = F)

  return(NULL)
}


#' AKM stats
#' @export
Do.AKM.stats <- function(long_data, suffix) {
  long_data[, x_mean := mean(akm_worker_effect), firm_ID]
  long_data[, w_mean := mean(wages_pt), firm_ID]
  stats <- data.table(method = "AKM", total_var = long_data[, var(wages_pt)])
  stats$x_var <- long_data[, var(akm_worker_effect)]
  stats$psi_var <- long_data[, var(akm_firm_effect)]
  stats$psi_x_cov <- long_data[, cov(akm_firm_effect, akm_worker_effect)]
  stats$psi_x_cor <- with(stats, psi_x_cov / (sqrt(x_var * psi_var)))
  stats$segregation_var <- long_data[, var(x_mean)]
  stats$total_between_var <- long_data[, var(w_mean)]
  stats$residual_var <- with(stats, total_var - x_var - psi_var - 2 * psi_x_cov)
  stats[, x_within_var := x_var - segregation_var]
  stats[, total_within_var := total_var - total_between_var]
  stats$suffix <- suffix
  return(stats)
}


#' CRE stats
#' @export
Do.CRE.stats <- function(all) {

  r <- data.table(
    method          = "CRE",
    psi_var         = all$vdec$var_psi,
    psi_x_cov       = all$vdec$cov_alpha_psi
  )

  return(r)
}

#' CRE-0 stats
#' @export
Do.CRE0.stats <- function(res) {
  all <- res$cre$all
  all[, x_var := res$Esd[j1]^2, j1]
  all[, eps_var := res$eps1_sd[j1]^2, j1]

  r <- all[, list(
    method = "CRE-0",
    x_within_var = wt.mean(x_var, N),
    segregation_var = wt.var(Em, N),
    x_var = wt.var(Em, N) + wt.mean(x_var, N),
    psi_var = wt.var(psi1, N),
    psi_x_cov = wt.cov(psi1, Em, N),
    residual_var = wt.mean(eps_var, N)
  )]

  r$psi_x_cor <- with(r, psi_x_cov / (sqrt(psi_var * x_var)))
  r$total_between_var <- with(r, psi_var + 2 * psi_x_cov + segregation_var)
  r$total_within_var <- with(r, x_within_var + residual_var)
  r$total_var <- with(r, total_between_var + total_within_var)
  return(r)
}


#' gather CRE-0 and CRE stats
#' @export
Do.CRE_both.stats <- function(cre_both_res, suffix, posterior_var = opts$misc$posterior_var) {
  stats <- Do.CRE.stats(cre_both_res)
  stats$suffix <- suffix
  return(stats)
}


#' gather CRE-0 and CRE output
#' @export
Do.CRE.decomp <- function(suffix, opts) {
  flog.info("[Do.CRE] importing CRE estimates for %s", suffix)
  cre_both_res <- readRDS(file = sprintf("%scre_%s.rds", opts$paths$res, suffix))

  flog.info("[Do.CRE] gathering CRE stats for %s", suffix)
  stats <- Do.CRE_both.stats(cre_both_res, suffix, posterior_var = opts$misc$posterior_var)

  flog.info("[Do.CRE] exporting CRE stats for %s", suffix)
  write.csv(stats, file = sprintf("%scre_results_%s.rds", opts$paths$res, suffix), row.names = F)

  return(NULL)
}


#' gather TraceHO stats
#' @export
Do.TraceHO.decomp <- function(suffix, opts, diff = FALSE) {
  if (diff) {
    ff <- sprintf("%straceHOdiff_%s.rds", opts$paths$res, suffix)
    ffout <- sprintf("%straceHOdiff_results_%s.rds", opts$paths$res, suffix)
  } else {
    ff <- sprintf("%straceHOlevel_%s.rds", opts$paths$res, suffix)
    ffout <- sprintf("%straceHOlevel_results_%s.rds", opts$paths$res, suffix)
  }

  flog.info("[Do.TraceHO] importing TraceHO estimates for %s", suffix)
  trace_res <- readRDS(ff)

  # get stats
  stats <- Do.TraceHO.stats(trace_res, suffix, diff = diff)

  flog.info("[Do.TraceHO] exporting TraceHO stats for %s", suffix)
  write.csv(stats, file = ffout, row.names = F)
}

#' compute TraceHO stats
#' @export
Do.TraceHO.stats <- function(trace_res, suffix, diff = FALSE) {
  if (diff) {
    stats <- data.table(total_var = trace_res$stats$total_logwage_var, total_between_var = trace_res$stats$set_btw_firm)
    stats$psi_var <- trace_res$stats$psi_var - trace_res$stats$trace_term_homo_Q
    stats$method <- "traceHOdiff"
  } else {
    stats <- data.table(
      total_var           = trace_res$stats$total_logwage_var,
      total_between_var   = trace_res$stats$set_btw_firm,
      psi_var             = trace_res$stats$full_dec_psi_var_bc,
      x_var               = trace_res$stats$full_dec_alpha_var_bc,
      psi_x_cov           = trace_res$stats$full_dec_cov_bc
    )
    stats[, psi_x_cor := psi_x_cov / sqrt(psi_var * x_var)]
    stats$method <- "traceHOlevel"
  }
  stats$suffix <- suffix
  return(stats)
}

#' compute TraceHE stats
#' @export
Do.TraceHE.stats <- function(trace_res, suffix, diff = F) {
  if (diff) {
    stats <- data.table(total_var = trace_res$stats$total_logwage_var, total_between_var = trace_res$stats$set_btw_firm)
    stats$psi_var <- trace_res$stats$psi_var - trace_res$stats$trace_term_hetero
    stats$method <- "traceHEdiff"
  } else {
    stats <- data.table(
      total_var           = trace_res$stats$total_logwage_var,
      total_between_var   = trace_res$stats$set_btw_firm,
      psi_var             = trace_res$stats$full_dec_psi_var_bc,
      x_var               = trace_res$stats$full_dec_alpha_var_bc,
      psi_x_cov           = trace_res$stats$full_dec_cov_bc
    )
    stats[, psi_x_cor := psi_x_cov / sqrt(psi_var * x_var)]
    stats$method <- "traceHElevel"
  }
  stats$suffix <- suffix
  return(stats)
}


#' gather TraceHE stats
#' @export
Do.TraceHE.decomp <- function(suffix, opts, diff = FALSE) {
  if (diff) {
    ff <- sprintf("%straceHEdiff_%s.rds", opts$paths$res, suffix)
    ffout <- sprintf("%straceHEdiff_results_%s.rds", opts$paths$res, suffix)
  } else {
    ff <- sprintf("%straceHElevel_%s.rds", opts$paths$res, suffix)
    ffout <- sprintf("%straceHElevel_results_%s.rds", opts$paths$res, suffix)
  }

  flog.info("[Do.TraceHE] importing TraceHE estimates for %s", suffix)
  trace_res <- readRDS(ff)

  # get stats
  stats <- Do.TraceHE.stats(trace_res, suffix, diff = diff)

  flog.info("[Do.TraceHE] exporting TraceHE stats for %s", suffix)
  write.csv(stats, file = ffout, row.names = F)
}


#' gather AKM stats
#' @export
Do.AKM.decomp <- function(suffix, opts) {
  flog.info("[Do.AKM] loading AKM estimates for %s", suffix)
  long_data <- readRDS(file = sprintf("%sakm_%s.rds", opts$paths$res, suffix))

  flog.info("[Do.AKM] gathering AKM stats for %s", suffix)
  stats <- Do.AKM.stats(long_data, suffix)

  flog.info("[Do.AKM] export AKM stats for %s", suffix)
  write.csv(stats, file = sprintf("%sakm_results_%s.rds", opts$paths$res, suffix), row.names = F)

  return(stats)
}


#' do AKM without spell collapse (no intermediate files are created)
#' @export
Do.AKM.without.Spell.Collapse <- function(opts) {


  # check if output already exists
  output_exists <- file.exists(sprintf("%sakm_results_%s.rds", opts$paths$res, "_nospells"))
  if (output_exists) {
    if (opts$misc$preserve.estimates) {
      flog.info("[Do.AKM] Do.AKM.without.Spell.Collapse: Output already exists. misc$preserve.estimates==TRUE so skipping.")
      return(NULL)
    } else {
      flog.info("[Do.AKM] Do.AKM.without.Spell.Collapse: Output already exists. misc$preserve.estimates==FALSE so writing over it.")
    }
  }


  # load country-specific raw data
  wdata <- readRDS(file = paste0(opts$paths$data, "yearly.rds"))

  # find connected set of movers
  wdata[, max_firm_ID := max(firm_ID)]
  wdata[, min_firm_ID := min(firm_ID)]
  movers <- wdata[min_firm_ID != max_firm_ID]
  f0s <- get.connected_set(movers, ncat = 100)$largest
  movers <- movers[firm_ID %in% f0s]
  wdata <- wdata[firm_ID %in% f0s]
  wdata[, firm_ID := as.character(firm_ID)]

  # estimate AKM
  long_data <- wdata[, list(worker_ID, firm_ID, year, wages_pt = wages)]
  gc()
  if ("akm" %in% .packages()) {
    flog.info("[Do.AKM] Package akm found. Estimating AKM on the jdata with akm.estimate.zig_zag.")
    firm_effects <- akm.estimate.zig_zag(long_data, outcome_var = "wages_pt", tol = 0, max_iterations = 100, report_frequency = 10)$firms_with_fixedeffects
  } else {
    flog.info("[Do.AKM] Package akm NOT found. Estimating AKM on the jdata with lfe::getfe, which may be slower.")
    felm_res <- felm(as.formula("wages_pt ~ 1 | worker_ID + firm_ID"), data = long_data)
    firm_effects <- setDT(getfe(felm_res))[fe == "firm_ID"][, list(firm_ID = as.character(idx), akm_firm_effect = effect)]
  }

  flog.info("[Do.AKM] Injecting AKM firm effects back in to the rbind(jdata,sdata) and estimating AKM worker effects")
  long_data <- wdata[, list(worker_ID, firm_ID, year, wages_pt = wages)]
  gc()
  long_data <- merge(long_data, firm_effects, by = "firm_ID")
  long_data[, akm_worker_effect := mean(wages_pt - akm_firm_effect), list(worker_ID)]

  stats <- Do.AKM.stats(long_data, "_nospells")
  write.csv(stats, file = sprintf("%sakm_results_%s.rds", opts$paths$res, "_nospells"), row.names = F)
}




#' gather interacted output
#' @export
Do.interacted.decomp <- function(suffix, opts) {
  blm_nonlinear <- readRDS(file = sprintf("%sinteracted_%s.rds", opts$paths$res, suffix))

  ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$data, suffix = suffix, data_type = opts$loading$data_type, opts)

  full_data <- rbindlist(list(
    ad$jdata[, list(worker_ID = wid, firm_ID = f1, cluster = j1, wages_pt = y1)],
    ad$jdata[, list(worker_ID = wid, firm_ID = f2, cluster = j2, wages_pt = y2)],
    ad$sdata[, list(worker_ID = wid, firm_ID = f1, cluster = j1, wages_pt = y1)]
  ))

  nclus <- ad$jdata[, length(unique(c(j1, j2)))]

  firmeffects_nonlinear <- data.table(cluster = 1:nclus, FEnl = blm_nonlinear$A1, gamma = blm_nonlinear$B1)

  full_data <- merge(full_data, firmeffects_nonlinear, by = "cluster")[order(firm_ID)]
  full_data[, gamma_bar := mean(gamma)]

  full_data[, xnl := mean((wages_pt - FEnl) / gamma), list(worker_ID)]
  full_data[, WEbarnl := xnl * gamma_bar]
  full_data[, WEnl := xnl * gamma]
  full_data[, predictablenl := WEnl + FEnl]

  full_data[, xbar := mean(xnl)]
  full_data[, psi_tilde := FEnl + gamma * xbar]
  full_data[, x_tilde := gamma_bar * (xnl - xbar)]
  full_data[, tau := (gamma - gamma_bar) * (xnl - xbar)]
  full_data[, x_between := mean(x_tilde), list(firm_ID)]
  full_data[, w_between := mean(wages_pt), list(firm_ID)]

  decomp <- with(full_data, data.table(
    psi_var = var(psi_tilde),
    x_var = var(x_tilde),
    psi_x_cov = cov(psi_tilde, x_tilde),
    psi_x_cor = cor(psi_tilde, x_tilde),
    segregation_var = var(x_between),
    total_var = var(wages_pt),
    total_between_var = var(w_between),
    interacted_var = var(tau) + 2 * cov(tau, x_tilde + psi_tilde)
  ))
  decomp[, x_within_var := x_var - segregation_var]
  decomp[, total_within_var := total_var - total_between_var]
  decomp[, residual_var := total_var - x_var - psi_var - 2 * psi_x_cov - interacted_var]

  decomp$suffix <- suffix
  decomp$method <- "interacted"

  flog.info("[Do.interacted] exporting interacted stats for %s", suffix)
  write.csv(decomp, file = sprintf("%sinteracted_results_%s.rds", opts$paths$res, suffix), row.names = F)
}

#' delete intermediate output
#' @export
Do.delete <- function(delete_this, opts, attrition = FALSE) {
  valid_inputs <- c("restricted_sets", "clusters", "AKM", "CRE", "interacted", "TraceHOdiff", "TraceHEdiff", "TraceHOlevel", "TraceHElevel")
  if (!(delete_this %in% valid_inputs)) {
    stop(sprintf("Do.delete: input must belong to %s", paste(valid_inputs, collapse = ",")))
  }

  path <- opts$paths$res
  if (attrition) {
    path <- opts$paths$attrition
  }

  if (delete_this == "AKM") {
    files <- list.files(path = path, pattern = "akm")
  }
  if (delete_this == "CRE") {
    files <- list.files(path = path, pattern = "cre")
  }
  if (delete_this == "TraceHOdiff") {
    files <- list.files(path = path, pattern = "traceHOdiff")
  }
  if (delete_this == "TraceHEdiff") {
    files <- list.files(path = path, pattern = "traceHEdiff")
  }
  if (delete_this == "TraceHOlevel") {
    files <- list.files(path = path, pattern = "traceHOlevel")
  }
  if (delete_this == "TraceHElevel") {
    files <- list.files(path = path, pattern = "traceHElevel")
  }
  if (delete_this == "interacted") {
    files <- list.files(path = path, pattern = "interacted")
  }
  if (delete_this == "clusters") {
    path <- opts$paths$data
    files <- list.files(path = opts$paths$data, pattern = "clusters")
  }
  if (delete_this == "restricted_sets") {
    path <- opts$paths$data
    files <- c(list.files(path = opts$paths$data, pattern = "leaveout"), list.files(path = opts$paths$data, pattern = "connected"))
  }

  if (length(files) > 0) {
    files <- paste0(path, files)
  } else {
    flog.info("Do.delete: No results files exist in the expected location for %s; skipping.", opts$country)
    return(NULL)
  }

  for (ff in files) {
    flog.info("[Do.delete] Now deleting file %s", ff)
    file.remove(ff)
  }

  return(NULL)
}



# ---COMBINED FINAL OUTPUT CALLS ----


#' export all the _results files
#' @export
Do.Final.Output <- function(opts) {

  # get files of results
  files <- list.files(path = opts$paths$res, pattern = "cre_results|akm_results|traceHOdiff_results|traceHOlevel_results|traceHEdiff_results|traceHElevel_results")
  if (length(files) > 0) {
    files <- paste0(opts$paths$res, files)
  } else {
    stop("Do.Final.Output: No results files exist in the expected location.")
  }

  # bind them together
  final_output <- NULL
  for (ff in files) {
    dd <- setDT(read.csv(file = ff))
    final_output <- rbindlist(list(final_output, dd), use.names = T, fill = T)
  }

  # export
  write.csv(final_output, file = paste0(opts$paths$final, "final_output_", opts$country, ".csv"), row.names = F)
  flog.info("Do.Final.Output: Exported final output decompositions.")

  return(NULL)
}


#' get the counts for all samples
#' @export
Do.Sample.Stats <- function(ad, suffix) {

  # main sample counts
  stayers <- ad$sdata[, length(unique(wid)) / 1e3]
  movers <- ad$jdata[, length(unique(wid)) / 1e3]
  firms <- ad$jdata[, length(unique(c(f1, f2))) / 1e3]
  desc <- data.table(suffix = suffix, firms = firms, stayers = stayers, movers = movers)

  # distribution of movers across firms
  movers_by_firm <- rbind(ad$jdata[, list(wid, firm_ID = f1)], ad$jdata[, list(wid, firm_ID = f2)])[, list(movers = length(unique(wid))), firm_ID]
  desc$mean_movers <- movers_by_firm[, mean(movers)]
  desc$q10_movers <- movers_by_firm[, quantile(movers, .1)]
  desc$q20_movers <- movers_by_firm[, quantile(movers, .2)]
  desc$q30_movers <- movers_by_firm[, quantile(movers, .3)]
  desc$q40_movers <- movers_by_firm[, quantile(movers, .4)]
  desc$q50_movers <- movers_by_firm[, quantile(movers, .5)]
  desc$q60_movers <- movers_by_firm[, quantile(movers, .6)]
  desc$q70_movers <- movers_by_firm[, quantile(movers, .7)]
  desc$q80_movers <- movers_by_firm[, quantile(movers, .8)]
  desc$q90_movers <- movers_by_firm[, quantile(movers, .9)]

  return(desc)
}

#' get the counts for all samples
#' @export
Do.Sample.Stats.extended <- function(ad, suffix) {

  # main sample counts
  stayers <- ad$sdata[, length(unique(wid))]
  movers <- ad$jdata[, length(unique(wid))]
  workers <- length(unique(c(ad$sdata$wid, ad$jdata$wid)))
  firms <- length(unique(c(ad$jdata$f1, ad$jdata$f2, ad$sdata$f1)))
  variance <- var(c(ad$sdata$y1, ad$jdata$y1, ad$jdata$y2))
  obs <- (nrow(ad$sdata) + 2 * nrow(ad$jdata))
  desc <- data.table(suffix = suffix, workers = workers, firms = firms, stayers = stayers, movers = movers, obs = obs, var = variance)

  return(desc)
}


#' computes connectedeness measure following Weidner and Jochmann on movers
#' @export
Do.connectedness_jdata <- function(jdata) {

  # On Firms:
  jdata2 <- rbind(jdata[, list(f1, f2)], jdata[, list(f1 = f2, f2 = f1)])
  AD <- acast(jdata2[, .N, list(f1, f2)], value.var = "N", f1 ~ f2, drop = FALSE, fill = 0)

  # Degree matrix
  diag_D <- rowSums(AD)
  D <- diag(nrow(AD)) * diag_D
  # Laplacian
  L <- D - AD
  #  S=diag(nrow(AD))-diag_D_inv*AD*diag_D_inv
  # Normalized Laplacian
  D_inv <- (diag_D)^(-1 / 2)
  diag_D_inv <- diag(nrow(AD)) * D_inv
  S <- diag_D_inv %*% L %*% diag_D_inv

  # smallest and largest
  y <- list()
  #  eig_min=eigs_sym(S, 2, which='SM', opts = list(retvec = FALSE))
  eig_min <- eigs(S, 2, which = "SM", opts = list(retvec = FALSE))
  y$eig_min <- sort(eig_min$values)[2]
  eig_max <- eigs(S, 1, which = "LM", opts = list(retvec = FALSE))
  y$eig_max <- eig_max$values
  return(y)
}

#' computes connectedeness measure following Weidner and Jochmann on movers
#' @export
Do.connectedness_year <- function(opts, year) {
  flog.info("Connectedness for year %s ", year)
  path <- paste0(opts$paths$connectivity, substr(country, 1, 2), "_", year, "/")
  results <- list()
  res <- Do.connectedness_set(opts, set = "_connected_clusters10", path)
  results <- rbind(results, res)
  res <- Do.connectedness_set(opts, set = "_leaveout", path)
  results <- rbind(results, res)
  res <- Do.connectedness_set(opts, set = "_connected", path)
  results <- rbind(results, res)
  results <- cbind(results, year)
  results <- as.data.table(results)
  return(results)
}

#' computes connectedeness measure following Weidner and Jochmann on movers
#' @export
Do.connectedness_set <- function(opts, set, path) {
  flog.info("Connectedness for set %s", set)
  ad <- Do.reading.jdata_sdata(directory_Data = path, suffix = set, data_type = opts$loading$data_type, opts)
  jdata <- ad$jdata
  res <- Do.connectedness_jdata(jdata)
  res$set <- set
  return(res)
}

#' computes connectedeness measure following Weidner and Jochmann on movers
#' uses a rolling window of years
#' @export
Do.connectedness <- function(opts) {
  require(RSpectra)
  flog.info("Determine years of data")
  opts$misc$connectivity <- TRUE
  wdata <- Do.load.raw.data(opts)
  years <- sort(unique(wdata$year))
  years <- years[2]:years[length(years) - 1] # years are the middle years between y1 and y2
  countries <- paste0(opts$country, "_", years) # name of folders for annual data
  # Create annual data sets
  flog.info("Create annual data sets")
  lapply(countries, Do.run.all, preserve.estimates = TRUE, preserve.initdata = TRUE, preserve.rawdata = TRUE, data_only = TRUE, connectivity = TRUE)
  flog.info("Compute Connectivity")
  # Compute Connectivity for all years
  cons <- bind_rows(lapply(years, Do.connectedness_year, opts = opts))
  # export
  flog.info("Export Connectivity Results")
  cons <- apply(cons, 2, as.character)
  write.csv(cons, file = paste0(opts$paths$final, "final_connectedness_", opts$country, ".csv"), row.names = F)
  # Delete data sets with annual data
  unlink(opts$paths$connectivity, recursive = TRUE)
}


#' gather the counts for all samples
#' @export
Do.Sample.Counts <- function(opts) {
  all_suffixes <- c("_all", unique(unlist(opts$suffix.set, recursive = FALSE)))
  all_suffixes <- all_suffixes[!is.na(all_suffixes)]

  desc_all <- data.table()

  for (suffix in all_suffixes) {

    # load data
    ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$data, suffix = suffix, data_type = opts$loading$data_type, opts)

    # main descriptives
    desc <- Do.Sample.Stats.extended(ad, suffix)

    desc_all <- rbind(desc_all, desc)
  }

  # export
  write.csv(desc_all, file = paste0(opts$paths$final, "final_samplesize_", opts$country, ".csv"), row.names = F)
  return(desc_all)
}


#' descriptives about clusters
#' @export
Do.Sample.Clusters <- function(opts) {
  CRE_suffixes <- opts$suffix.set$CRE
  CRE_suffixes <- CRE_suffixes[str_detect(CRE_suffixes, "10")]

  cluster_stats_all <- data.table()
  sorting_stats_all <- data.table()

  for (suffix in CRE_suffixes) {

    # load data
    ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$data, suffix = suffix, data_type = opts$loading$data_type, opts)

    # shape data long
    full_data <- rbindlist(list(
      ad$jdata[, list(worker_ID = wid, firm_ID = f1, cluster = j1, y = y1)],
      ad$jdata[, list(worker_ID = wid, firm_ID = f2, cluster = j2, y = y2)],
      ad$sdata[, list(worker_ID = wid, firm_ID = f1, cluster = j1, y = y1)]
    ))

    # collect cluster count
    nclus <- ad$jdata[, length(unique(c(j1, j2)))]

    # collapse to firm size, keep the cluster info
    firms <- full_data[, list(size = length(unique(worker_ID))), list(firm_ID, cluster)]

    # collect firm size distribution across clusters
    firm_stats <- firms[, list(firms = .N, mean_size = mean(size), firms10 = sum(size >= 10), firms50 = sum(size >= 50)), cluster][order(cluster)]

    # collect wage moments across clusters
    wage_moments <- full_data[, list(workers = length(unique(worker_ID)), wmean = mean(y), wvar = var(y), wskew = skewness(y), wkurt = kurtosis(y)), cluster]

    cluster_stats <- merge(firm_stats, wage_moments, by = "cluster")
    cluster_stats$suffix <- suffix
    cluster_stats_all <- rbind(cluster_stats_all, cluster_stats)


    # read CRE results
    cre_both_res <- readRDS(file = sprintf("%scre_%s.rds", opts$paths$res, suffix))

    # inject the FEs into the full data in long format, find implied WEs
    firmeffects <- data.table(cluster = 1:nclus, FE = cre_both_res$A1)
    full_data <- merge(full_data, firmeffects, by = "cluster")[order(firm_ID)]
    full_data[, WE := mean(y - FE), list(worker_ID)]

    # discretize the WEs
    full_data[, WEq := ecdf(WE)(WE)]
    full_data[, WEgroup := 1 + as.numeric(WEq >= .1) + as.numeric(WEq >= .2) + as.numeric(WEq >= .3) + as.numeric(WEq >= .4) +
      as.numeric(WEq >= .5) + as.numeric(WEq >= .6) + as.numeric(WEq >= .7) + as.numeric(WEq >= .8) + as.numeric(WEq >= .9)]

    sorting_stats <- full_data[, list(N = .N, wages = mean(y)), list(WEgroup, cluster)][order(cluster, WEgroup)]
    sorting_stats$suffix <- suffix

    sorting_stats_all <- rbind(sorting_stats_all, sorting_stats)
  }

  # export
  write.csv(apply(cluster_stats_all, 2, as.character), file = paste0(opts$paths$final, "final_clusterstats_", opts$country, ".csv"), row.names = F)
  write.csv(apply(sorting_stats_all, 2, as.character), file = paste0(opts$paths$final, "final_sortingstats_", opts$country, ".csv"), row.names = F)
}




#' descriptives about clusters with interactions
#' @export
Do.Interacted.Sorting <- function(opts) {
  interacted_suffixes <- opts$suffix.set$interacted
  interacted_suffixes <- interacted_suffixes[str_detect(interacted_suffixes, "10")]

  sorting_stats_all <- data.table()

  for (suffix in interacted_suffixes) {
    blm_nonlinear <- readRDS(file = sprintf("%sinteracted_%s.rds", opts$paths$res, suffix))

    ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$data, suffix = suffix, data_type = opts$loading$data_type, opts)

    full_data <- rbindlist(list(
      ad$jdata[, list(worker_ID = wid, firm_ID = f1, cluster = j1, wages_pt = y1)],
      ad$jdata[, list(worker_ID = wid, firm_ID = f2, cluster = j2, wages_pt = y2)],
      ad$sdata[, list(worker_ID = wid, firm_ID = f1, cluster = j1, wages_pt = y1)]
    ))

    nclus <- ad$jdata[, length(unique(c(j1, j2)))]

    firmeffects_nonlinear <- data.table(cluster = 1:nclus, FEnl = blm_nonlinear$A1, gamma = blm_nonlinear$B1)

    flog.info("injecting firm and interaction effects to the full data")
    full_data <- merge(full_data, firmeffects_nonlinear, by = "cluster")[order(firm_ID)]
    full_data[, WE := mean((wages_pt - FEnl) / gamma), list(worker_ID)]

    flog.info("discretize the WEs")
    full_data[, WEq := ecdf(WE)(WE)]
    full_data[, WEgroup := 1 + as.numeric(WEq >= .1) + as.numeric(WEq >= .2) + as.numeric(WEq >= .3) + as.numeric(WEq >= .4) +
      as.numeric(WEq >= .5) + as.numeric(WEq >= .6) + as.numeric(WEq >= .7) + as.numeric(WEq >= .8) + as.numeric(WEq >= .9)]

    flog.info("extracting statistics")
    sorting_stats <- full_data[, list(N = .N, wages = mean(wages_pt)), list(WEgroup, cluster)][order(cluster, WEgroup)]
    sorting_stats$suffix <- suffix

    sorting_stats_all <- rbind(sorting_stats_all, sorting_stats)
  }

  # export
  write.csv(apply(sorting_stats_all, 2, as.character), file = paste0(opts$paths$final, "final_interactedsortingstats_", opts$country, ".csv"), row.names = F)
}


#' quantiles from the interacted model
#' @export
Do.Quantiles.Interacted <- function(opts) {
  interacted_suffixes <- opts$suffix.set$interacted

  interactedquantiles_all <- data.table()
  EEm_all <- data.table()
  rawcoefs_all <- data.table()

  for (suffix in interacted_suffixes) {

    # import interacted model
    model <- readRDS(file = sprintf("%sinteracted_%s.rds", opts$paths$res, suffix))

    # collect the raw coefficients
    rawcoefs <- data.table(cluster = 1:length(model$A1), A1 = model$A1, B1 = model$B1)
    rawcoefs_all <- rbind(rawcoefs_all, rawcoefs)

    # prepare quantiles
    qt <- 19
    qvals <- (1:qt) / (qt + 1)
    qts <- qnorm(qvals)

    # extract info from the interacted model
    rr <- data.table(l = 1:length(model$A1), Em = model$Em, Esd = model$Esd, N = model$Ns, A1 = model$A1, B1 = model$B1, A2 = model$A2, B2 = model$B2)
    alpha_m <- rr[, wtd.mean(Em, N)]
    alpha_sd <- sqrt(rr[, wtd.mean(Esd^2, N) + wtd.var(Em, N)])
    rr2 <- rr[, list(y1 = (qts * alpha_sd + alpha_m) * B1 + A1, y2 = (qts * alpha_sd + alpha_m) * B2 + A2, k = 1:qt), l]

    # clean-up
    rr2 <- melt(rr2, id.vars = c("l", "k"))
    rr2 <- rr2[variable == "y1"]
    rr2[, variable := NULL]
    qmat <- data.table(k = 1:qt, qk = qvals)
    rr2 <- merge(rr2, qmat, by = "k", all.x = T)
    setnames(rr2, c("l", "k", "qk"), c("FirmCluster", "WorkerEffect", "WorkerEffectQ"))

    # combine
    rr2$suffix <- suffix
    EEm <- as.data.table(model$EEm)
    EEm$suffix <- suffix
    interactedquantiles_all <- rbind(interactedquantiles_all, rr2)
    EEm_all <- rbind(EEm_all, EEm)
  }

  # export results
  write.csv(interactedquantiles_all, file = paste0(opts$paths$final, "final_interactedquantiles_", opts$country, ".csv"), row.names = F)
  write.csv(EEm_all, file = paste0(opts$paths$final, "final_interactedEEm_", opts$country, ".csv"), row.names = F)
  write.csv(rawcoefs_all, file = paste0(opts$paths$final, "final_interactedcoefs_", opts$country, ".csv"), row.names = F)
}

#' do the 6-year Card-Kline event study (currently only defined for US because of the 6-year requirement)
#' @export
Do.EventStudy.6year <- function(opts) {

  # load yearly data (has spell info, but not collapsed by spell)
  wdata <- readRDS(file = paste0(opts$paths$data, "yearly.rds"))
  pre_move_year <- wdata[, min(year)] + 1
  post_move_year <- pre_move_year + 3

  # define if the worker satisfies the "ideal mover" structure of (2/3 years at firm A) then (2/3 years at firm B)
  wdata[, movers := 0]
  wdata[(spell_begin <= pre_move_year & spell_end == (pre_move_year + 1)) | (spell_end >= post_move_year & spell_begin == (post_move_year - 1)), movers := 1]
  wdata[, unique_spells := length(unique(spell_begin)), list(worker_ID)]
  wdata[unique_spells == 1, movers := 0]
  wdata[, sum_movers := sum(movers), worker_ID]
  wdata[, ideal_mover := 0]
  wdata[sum_movers >= 4, ideal_mover := 1]
  wdata <- wdata[, .(worker_ID, firm_ID, year, wages, wages_predict, ideal_mover)]
  gc()

  # use non-ideal movers to construct the firm mean wage, attach to ideal movers subsample
  firm_mean_wage <- wdata[ideal_mover == 0][, list(firm_mean_wages = mean(wages)), firm_ID]
  wdata <- merge(wdata[ideal_mover == 1], firm_mean_wage, by = "firm_ID")
  wdata[, firm_quantile := ecdf(firm_mean_wages)(firm_mean_wages)]

  # merge this to the ideal movers, form 4 equally-sized quantile groups, assign to the pre/post move year
  wdata[, firm_group := 1]
  wdata[firm_quantile >= .25 & firm_quantile < .5, firm_group := 2]
  wdata[firm_quantile >= .5 & firm_quantile < .75, firm_group := 3]
  wdata[firm_quantile >= .75, firm_group := 4]
  wdata[, firm_group_pre := firm_group[year == pre_move_year], worker_ID]
  wdata[, firm_group_post := firm_group[year == post_move_year], worker_ID]
  wdata <- wdata[!is.na(firm_group_pre) & !is.na(firm_group_post)]

  # finish
  stats <- wdata[, list(wages = mean(wages), wagesraw = mean(wages + wages_predict), N = .N), list(firm_group_pre, firm_group_post, year)][order(firm_group_pre, firm_group_post, year)]
  write.csv(stats, file = sprintf("%sres_%s_CHK-events.csv", opts$paths$final, opts$country), row.names = F)
}



#' do the 5-year Card-Kline event study
#' @export
Do.EventStudy.5year <- function(opts) {

  # load yearly data (has spell info, but not collapsed by spell)
  wdata <- readRDS(file = paste0(opts$paths$data, "yearly.rds"))
  pre_move_year <- wdata[, min(year)] + 1
  post_move_year <- pre_move_year + 2
  wdata <- wdata[year >= (pre_move_year - 1) & year <= (post_move_year + 1)]

  # define if the worker satisfies the "ideal mover" structure of (1/2 years at firm A) then (dropped year) then (1/2 years at firm B)
  wdata[, movers := 0]
  wdata[(spell_end == (pre_move_year)) | (spell_begin == (post_move_year)), movers := 1]
  wdata[, unique_spells := length(unique(spell_begin)), list(worker_ID)]
  wdata[unique_spells == 1, movers := 0]
  wdata[, sum_movers := sum(movers), worker_ID]
  wdata[, ideal_mover := 0]
  wdata[sum_movers >= 2, ideal_mover := 1]
  wdata <- wdata[, .(worker_ID, firm_ID, year, wages, wages_predict, ideal_mover)]
  gc()

  # use non-ideal movers to construct the firm mean wage, attach to ideal movers subsample
  firm_mean_wage <- wdata[ideal_mover == 0][, list(firm_mean_wages = mean(wages)), firm_ID]
  wdata <- merge(wdata[ideal_mover == 1], firm_mean_wage, by = "firm_ID")
  wdata[, firm_quantile := ecdf(firm_mean_wages)(firm_mean_wages)]

  # merge this to the ideal movers, form 4 equally-sized quantile groups, assign to the pre/post move year
  wdata[, firm_group := 1]
  wdata[firm_quantile >= .25 & firm_quantile < .5, firm_group := 2]
  wdata[firm_quantile >= .5 & firm_quantile < .75, firm_group := 3]
  wdata[firm_quantile >= .75, firm_group := 4]
  wdata[, firm_group_pre := firm_group[year == pre_move_year], worker_ID]
  wdata[, firm_group_post := firm_group[year == post_move_year], worker_ID]
  wdata <- wdata[!is.na(firm_group_pre) & !is.na(firm_group_post)]

  # finish
  stats <- wdata[, list(wages = mean(wages), wagesraw = mean(wages + wages_predict), N = .N), list(firm_group_pre, firm_group_post, year)][order(firm_group_pre, firm_group_post, year)]
  write.csv(stats, file = sprintf("%sres_%s_CHK-events.csv", opts$paths$final, opts$country), row.names = F)
}



#' get the descriptives for all samples
#' @export
Do.Sample.Descriptives <- function(opts) {

  # get sample counts
  flog.info("Collecting sample counts")
  Do.Sample.Counts(opts)

  # get Card-Kline 6-year event study (currently only defined for US because of the 6-year requirement)
  if (str_detect(opts$country, "noendpoints")) {
    flog.info("Collecting 6-year event study")
    Do.EventStudy.6year(opts)
  }
}





#' runs all the commands, given 'country' as input
#' requires to adjust Do.simple.init and setting country options
#' in Do.Init.Opts and subroutines
#' @export
Do.run.all <- function(country, preserve.estimates = TRUE, preserve.initdata = TRUE, preserve.rawdata = TRUE, posterior_var = TRUE, data_only = FALSE, connectivity = FALSE) {

  flog.info("1. [Do.run.all] set up with country-specific options for country %s", country)

  # set up the options
  opts <- Do.Init.Opts(country)
  opts$misc$preserve.rawdata <- preserve.rawdata
  opts$misc$preserve.initdata <- preserve.initdata
  opts$misc$preserve.estimates <- preserve.estimates
  opts$misc$posterior_var <- posterior_var
  opts$misc$data_only <- data_only
  opts$misc$connectivity < connectivity

  # convert the raw data into prepared data that can be used directly by the estimators

  flog.info("2. [Do.run.all] Data Processing")

  flog.info("2a. [Do.run.all] Reading the raw data and saving sample for country %s", country)
  Do.sample.init(opts)
  Do.jdata_sdata.make(opts)

  flog.info("2b. [Do.run.all] Prepare connected and leave-one-out connected sets for the jdata/sdata for country %s", country)
  Do.jdata_sdata.restrict_set.execute(opts)

  flog.info("2c. [Do.run.all] Add clusters to the jdata/sdata for country %s", country)
  Do.jdata_sdata.cluster.execute(opts)

  # if data_only is TRUE, we are done.

  if (opts$misc$data_only) {
    flog.info("[Do.run.all] DONE for country %s", country)
    return(NULL)
  }

  # if data_only is FALSE, we run the specified estimation routines.

  flog.info("3. [Do.run.all] Estimation for country %s", country)

  flog.info("3a. [Do.run.all] Estimate CRE, then collect decompositions for country %s", country)
  Do.CRE.execute(opts)

  flog.info("3b. [Do.run.all] Estimate AKM, then collect decompositions for country %s", country)
  Do.AKM.execute(opts)
  if (opts$country %in% c("US-early", "US-late", "Testing-Brad")) {
    Do.AKM.without.Spell.Collapse(opts)
  }

  flog.info("3c. [Do.run.all] Estimate TraceHO, then collect decompositions for country %s", country)
  Do.TraceHO.execute(opts, diff = FALSE)

  flog.info("3d. [Do.run.all] Estimate TraceHE, then collect decompositions for country %s", country)
  Do.TraceHE.execute(opts, diff = FALSE)

  # having finished the estimation routines, we now prepare them in clean output.
  flog.info("4. [Do.run.all] Finish for country %s", country)

  flog.info("4a. [Do.run.all] Collect all the decompositions into one final output file for country %s", country)
  Do.Final.Output(opts)

  flog.info("4b. [Do.run.all] Collect various descriptive statistics for country %s", country)
  Do.Sample.Descriptives(opts)

  flog.info("[Do.run.all] DONE for country %s", country)
}

#' runs only the data preparation commands, given 'country' as input
#' requires to adjust Do.simple.init and setting country options
#' in Do.Init.Opts and subroutines
#' @export
Do.prepare.samples <- function(country, data_filter_function = function(d) d) {

  flog.info("1. [Do.run.all] set up with country-specific options for country %s", country)
  opts <- Do.Init.Opts(country)
  opts$misc$preserve.rawdata <- FALSE
  opts$misc$preserve.initdata <- FALSE
  opts$misc$preserve.estimates <- FALSE

  flog.info("2. [Do.run.all] Data Processing")
  flog.info("2a. [Do.run.all] Reading the raw data and saving sample for country %s", country)
  Do.sample.init(opts, data_filter_function = data_filter_function)
  Do.jdata_sdata.make(opts)

  flog.info("2b. [Do.run.all] Prepare connected and leave-one-out connected sets for the jdata/sdata for country %s", country)
  Do.jdata_sdata.restrict_set.execute(opts)

  flog.info("2c. [Do.run.all] Add clusters to the jdata/sdata for country %s", country)
  Do.jdata_sdata.cluster.execute(opts)
  return(opts)
}
