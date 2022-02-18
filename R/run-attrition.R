

#' Initialize attrition exercise data
#' @param opts The options
#' @export
Do.attrition.init <- function(opts) {

  # check if output already exists
  output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = "_attrition_all", data_type = opts$loading$data_type, opts)
  if (output_exists) {
    if (opts$misc$preserve.estimates) {
      flog.info("Do.attrition.init: Output already exists. misc$preserve.estimates==TRUE so skipping.")
      return(NULL)
    } else {
      flog.info("Do.attrition.init: Output already exists. misc$preserve.estimates==FALSE so writing over it.")
    }
  }

  moversperfirm <- opts$attrition.exercise$moversperfirm

  # initial jdata/sdata
  flog.info("[attrition] loading all data, then restricting to firms with at least %s movers.", moversperfirm)
  ad_init <- Do.reading.jdata_sdata(directory_Data = opts$paths$data, suffix = "_all", data_type = opts$loading$data_type, opts)

  # restrict initial set to firms with at least 15 movers
  movers_by_firm <- rbind(ad_init$jdata[, list(wid, firm_ID = f1)], ad_init$jdata[, list(wid, firm_ID = f2)])[, list(movers = length(unique(wid))), firm_ID]
  all_firms <- movers_by_firm[, unique(firm_ID)]
  firms_with_15_movers <- movers_by_firm[movers >= moversperfirm, unique(firm_ID)]
  ad_init$jdata <- ad_init$jdata[f1 %in% firms_with_15_movers & f2 %in% firms_with_15_movers]
  ad_init$sdata <- ad_init$sdata[f1 %in% firms_with_15_movers]
  ad_init <- Do.jdata_sdata.connected(ad_init)
  flog.info("[attrition] kept %s out of %s firms because they have at least %s movers. Saving initial data.", length(firms_with_15_movers), length(all_firms), moversperfirm)

  Do.writing.jdata_sdata(ad_init, directory_Data = opts$paths$attrition, suffix = "_attrition_all", data_type = opts$loading$data_type, opts)
}


#' Define an exporter to python
#' @param ad List with sdata/jdata
#' @param directory_Data Directory where the data is located
#' @param suffix Additional specifications of the dataset to use
#' @export
Do.python.export.jdata_sdata <- function(ad, directory_Data, suffix = "") {
  jdata <- ad$jdata
  sdata <- ad$sdata

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

  jdata <- as.data.frame(jdata)
  sdata <- as.data.frame(sdata)

  flog.info("exporting sdata to %s", paste0(directory_Data, "sdata", suffix, "_py.rds"))
  saveRDS(sdata, file = paste0(directory_Data, "sdata", suffix, "_py.rds"), compress = F)
  flog.info("exporting jdata to %s", paste0(directory_Data, "jdata", suffix, "_py.rds"))
  saveRDS(jdata, file = paste0(directory_Data, "jdata", suffix, "_py.rds"), compress = F)

  flog.info("finished exporting sdata and jdata")
}


#' Run connected sets for attrition
#' @param opts The options
#' @export
Do.attrition.draws.connected <- function(opts) {
  keep.shares <- opts$attrition.exercise$keep.shares
  if (max(keep.shares) >= 1 | min(keep.shares) <= 0) {
    stop("[attrition] Do.attrition.draws.connected: keep.shares must be strictly less than 1 and greater than 0. Note that keep.share=1 is included by defaultif draw==1.")
  }
  draws <- unique(opts$attrition.exercise$draws)
  draw.counter <<- 1
  num.draws <- length(unique(draws))
  ncores <- opts$misc$ncores

  # check if input already exists
  output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = "_attrition_all", data_type = opts$loading$data_type, opts)
  if (!output_exists) {
    stop("[attrition] Missing the initial data for attrition. You should have run Do.attrition.init(opts) first.")
  }

  ad_init <- Do.reading.jdata_sdata(directory_Data = opts$paths$attrition, suffix = "_attrition_all", data_type = opts$loading$data_type, opts)
  flog.info("[attrition] for each random draw: looping over keep.shares, saving connected set for each.")

  for (draw in draws) {
    flog.info("[attrition] beginning subsample data construction for draw %s/%s", draw.counter, num.draws)

    ad_draw <- copy(ad_init)

    # randomly assign which movers to drop
    set.seed(as.integer(sprintf("1010%s", draw)))
    ad_draw$jdata[, random_val := runif(1), list(f1, wid)]
    ad_draw$jdata[, random_val := rank(random_val) / .N, list(f1)]

    if (opts$attrition.exercise$samefirms) {
      keep.share <- min(keep.shares)
      flog.info("[attrition] same firms: keeping only firms with at least the minimum keep.share which is %s", keep.share)

      # find the connected set for the minimum share in keep.shares
      initial.firm.count <- ad_draw$jdata[, length(unique(c(f1, f2)))]
      ad <- copy(ad_draw)
      ad$jdata[, drop_this_mover := (random_val > keep.share)]

      # drop the movers and firms that no longer have any movers
      movers_to_drop <- ad$jdata[drop_this_mover == 1, unique(wid)]
      ad$jdata <- ad$jdata[drop_this_mover == 0]
      remaining_firms <- ad$jdata[, unique(c(f1, f2))]
      ad$sdata <- ad$sdata[(f1 %in% remaining_firms)]
      gc()

      # find connected set and only keep those firms
      ad <- Do.jdata_sdata.connected(ad)
      remaining_firms <- ad$jdata[, unique(c(f1, f2))]
      ad_draw$jdata <- ad_draw$jdata[f1 %in% remaining_firms & f2 %in% remaining_firms]
      gc()
      ad_draw$sdata <- ad_draw$sdata[(f1 %in% remaining_firms)]
      gc()
      final.firm.count <- ad_draw$jdata[, length(unique(c(f1, f2)))]

      flog.info("[attrition] same firms: initial firm count was %s, final firm count is %s", initial.firm.count, final.firm.count)
    }


    run_connected_sets <- function(keep.share) {
      suffix <- sprintf("_connected_draw%s_keep%s", draw, keep.share)
      if (opts$attrition.exercise$samefirms) {
        suffix <- sprintf("_connected_samefirms_draw%s_keep%s", draw, keep.share)
      }

      # check if output already exists
      output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix, data_type = opts$loading$data_type, opts)
      if (output_exists & opts$misc$preserve.estimates) {
        flog.info("[attrition] for draw %s/%s: connected set for keep.share %s already exists, so skipping it.", draw.counter, num.draws, keep.share)
      } else {
        flog.info("[attrition] for draw %s/%s: getting connected set for keep.share %s", draw.counter, num.draws, keep.share)

        ad <- copy(ad_draw)
        ad$jdata[, drop_this_mover := (random_val > keep.share)]

        # drop the movers and firms that no longer have any movers
        movers_to_drop <- ad$jdata[drop_this_mover == 1, unique(wid)]
        ad$jdata <- ad$jdata[drop_this_mover == 0]
        remaining_firms <- ad$jdata[, unique(c(f1, f2))]
        ad$sdata <- ad$sdata[(f1 %in% remaining_firms)]
        gc()

        # find connected set and save
        ad <- Do.jdata_sdata.connected(ad)
        Do.writing.jdata_sdata(ad, directory_Data = opts$paths$attrition, suffix = suffix, data_type = opts$loading$data_type, opts)
        Do.python.export.jdata_sdata(ad, directory_Data = opts$paths$attrition, suffix = suffix)
        flog.info("[attrition] for draw %s/%s: saved connected set for keep.share %s", draw.counter, num.draws, keep.share)
      }
    }

    keep.shares.set <- copy(keep.shares)
    if (draw == 1) {
      keep.shares.set <- c(keep.shares.set, 1)
    }

    if (ncores == 1) {
      lapply(keep.shares.set, run_connected_sets)
    } else {
      parallel::mclapply(keep.shares.set, run_connected_sets, mc.cores = ncores)
    }


    draw.counter <<- draw.counter + 1
  }

  flog.info("[attrition] DONE with connected sets.")
  return(NULL)
}


#' Run leaveout sets for attrition
#' @param opts The options
#' @export
Do.attrition.draws.leaveout <- function(opts) {
  keep.shares <- opts$attrition.exercise$keep.shares
  if (max(keep.shares) >= 1 | min(keep.shares) <= 0) {
    stop("[attrition] Do.attrition.draws.leaveout: keep.shares must be strictly less than 1 and greater than 0. Note that keep.share=1 is included by default if draw==1.")
  }
  draws <- unique(opts$attrition.exercise$draws)
  draw.counter <<- 1
  num.draws <- length(unique(draws))
  ncores <- opts$misc$ncores

  methods.leaveout <- opts$attrition.exercise$methods$leaveout
  any_leaveout <- any(!is.na(methods.leaveout))

  if (!any_leaveout) {
    flog.info("[attrition] No leaveout methods specified. Skipping the construction of leaveout sets.")
    return(NULL)
  }

  flog.info("[attrition] beginning leaveout set data construction, which loops through all %s draws", num.draws)

  run_leaveout <- function(ii) {
    keep.share <- specifications[ii]$keep.share
    draw <- specifications[ii]$draw

    suffix_con <- sprintf("_connected_draw%s_keep%s", draw, keep.share)
    suffix_lo <- sprintf("_leaveout_draw%s_keep%s", draw, keep.share)
    if (opts$attrition.exercise$samefirms) {
      suffix_con <- sprintf("_connected_samefirms_draw%s_keep%s", draw, keep.share)
      suffix_lo <- sprintf("_leaveout_samefirms_draw%s_keep%s", draw, keep.share)
    }

    # check if input already exists
    input_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_con, data_type = opts$loading$data_type, opts)
    if (!input_exists) {
      stop("[attrition] for draw %s: connected set for keep.share %s does not exist. You should have first run Do.attrition.draws.connected(opts).", draw, keep.share)
    }

    # check if output already exists
    output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_lo, data_type = opts$loading$data_type, opts)
    if (output_exists & opts$misc$preserve.estimates) {
      flog.info("[attrition] for draw %s: leaveout set for keep.share %s already exists, so skipping it.", draw, keep.share)
    } else {
      flog.info("[attrition] for draw %s: getting leaveout set for keep.share %s, which may be slow.", draw, keep.share)
      ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_con, data_type = opts$loading$data_type, opts)

      if (opts$attrition.exercise$samefirms) {

        # find the connected set for the minimum share in keep.shares
        min.keep.share <- min(keep.shares)
        flog.info("[attrition] same firms: keeping only firms with at least the minimum keep.share which is %s", keep.share)
        initial.firm.count <- ad$jdata[, length(unique(c(f1, f2)))]
        ad_temp <- copy(ad)
        ad_temp$jdata[, drop_this_mover := (random_val > min.keep.share)]

        # drop the movers and firms that no longer have any movers
        movers_to_drop <- ad_temp$jdata[drop_this_mover == 1, unique(wid)]
        ad_temp$jdata <- ad_temp$jdata[drop_this_mover == 0]
        remaining_firms <- ad_temp$jdata[, unique(c(f1, f2))]
        ad_temp$sdata <- ad_temp$sdata[(f1 %in% remaining_firms)]
        gc()

        # find connected set and only keep those firms
        ad_temp <- Do.jdata_sdata.leaveout(ad_temp)
        remaining_firms <- ad_temp$jdata[, unique(c(f1, f2))]
        ad$jdata <- ad$jdata[f1 %in% remaining_firms & f2 %in% remaining_firms]
        gc()
        ad$sdata <- ad$sdata[(f1 %in% remaining_firms)]
        gc()
        final.firm.count <- ad$jdata[, length(unique(c(f1, f2)))]

        flog.info("[attrition] same firms: initial firm count was %s, final firm count is %s", initial.firm.count, final.firm.count)
      }

      ad <- Do.jdata_sdata.leaveout(ad)
      Do.writing.jdata_sdata(ad, directory_Data = opts$paths$attrition, suffix = suffix_lo, data_type = opts$loading$data_type, opts)
      Do.python.export.jdata_sdata(ad, directory_Data = opts$paths$attrition, suffix = suffix_lo)
      flog.info("[attrition] for draw %s: saved leaveout set for keep.share %s", draw, keep.share)
    }
  }

  specifications <- as.data.table(expand.grid(keep.share = keep.shares, draw = draws))
  if (1 %in% draws) {
    specifications <- rbind(specifications, data.table(keep.share = 1, draw = 1))
  }

  if (ncores == 1) {
    lapply(1:nrow(specifications), run_leaveout)
  } else {
    parallel::mclapply(1:nrow(specifications), run_leaveout, mc.cores = ncores)
  }

  flog.info("[attrition] DONE with leaveout sets.")
  return(NULL)
}


#' Run attrition cluster estimation
#' @param opts The options
#' @export
Do.attrition.draws.clusters <- function(opts) {
  keep.shares <- opts$attrition.exercise$keep.shares
  if (max(keep.shares) >= 1 | min(keep.shares) <= 0) {
    stop("[attrition] Do.attrition.clusters: keep.shares must be strictly less than 1 and greater than 0. Note that keep.share=1 is included by default if draw==1.")
  }
  draws <- unique(opts$attrition.exercise$draws)
  draw.counter <<- 1
  num.draws <- length(unique(draws))
  ncores <- opts$misc$ncores

  flog.info("[attrition] Begin clustering (we only consider 10 clusters in the attrition exercise).")
  methods.connected <- opts$attrition.exercise$methods$connected
  methods.leaveout <- opts$attrition.exercise$methods$leaveout

  run_clusters <- function(ii) {
    keep.share <- specifications[ii]$keep.share
    draw <- specifications[ii]$draw

    suffix_con <- sprintf("_connected_draw%s_keep%s", draw, keep.share)
    suffix_lo <- sprintf("_leaveout_draw%s_keep%s", draw, keep.share)
    suffix_con_clusters10 <- sprintf("_connected_clusters10_draw%s_keep%s", draw, keep.share)
    suffix_lo_clusters10 <- sprintf("_leaveout_clusters10_draw%s_keep%s", draw, keep.share)
    if (opts$attrition.exercise$samefirms) {
      suffix_con <- sprintf("_connected_samefirms_draw%s_keep%s", draw, keep.share)
      suffix_lo <- sprintf("_leaveout_samefirms_draw%s_keep%s", draw, keep.share)
      suffix_con_clusters10 <- sprintf("_connected_samefirms_clusters10_draw%s_keep%s", draw, keep.share)
      suffix_lo_clusters10 <- sprintf("_leaveout_samefirms_clusters10_draw%s_keep%s", draw, keep.share)
    }

    if ("CRE" %in% methods.connected) {

      # check if input already exists
      input_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_con, data_type = opts$loading$data_type, opts)
      if (!input_exists) {
        stop("[attrition] for draw %s: connected set for keep.share %s does not exist. You should have first run Do.attrition.draws.connected(opts).", draw, keep.share)
      }

      # check if output already exists. If not, run it.
      output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_con_clusters10, data_type = opts$loading$data_type, opts)
      if (output_exists & opts$misc$preserve.estimates) {
        flog.info("[attrition] for draw %s: clusters for connected set for keep.share %s already exists, so skipping it.", draw, keep.share)
      } else {
        flog.info("[attrition] for draw %s: start estimating clusters for connected set for keep.share %s", draw, keep.share)
        ad_con <- Do.reading.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_con, data_type = opts$loading$data_type, opts)
        ad_con <- Do.jdata_sdata.cluster.noIO(ad_con, nclus = 10, nstart = 30)
        Do.writing.jdata_sdata(ad_con, directory_Data = opts$paths$attrition, suffix = suffix_con_clusters10, data_type = opts$loading$data_type, opts)
        Do.python.export.jdata_sdata(ad_con, directory_Data = opts$paths$attrition, suffix = suffix_con_clusters10)
        flog.info("[attrition] for draw %s: done estimating clusters for connected set for keep.share %s", draw, keep.share)
      }
    }

    if ("CRE" %in% methods.leaveout) {

      # check if input already exists
      input_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_lo, data_type = opts$loading$data_type, opts)
      if (!input_exists) {
        stop("[attrition] for draw %s: leaveout set for keep.share %s does not exist. You should have first run Do.attrition.draws.leaveout(opts).", draw, keep.share)
      }

      # check if output already exists. If not, run it.
      output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_lo_clusters10, data_type = opts$loading$data_type, opts)
      if (output_exists & opts$misc$preserve.estimates) {
        flog.info("[attrition] for draw %s: clusters for leaveout set for keep.share %s already exists, so skipping it.", draw, keep.share)
      } else {
        flog.info("[attrition] for draw %s: start estimating clusters for leaveout set for keep.share %s", draw, keep.share)
        ad_lo <- Do.reading.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_lo, data_type = opts$loading$data_type, opts)
        ad_lo <- Do.jdata_sdata.cluster.noIO(ad_lo, nclus = 10, nstart = 30)
        Do.writing.jdata_sdata(ad_lo, directory_Data = opts$paths$attrition, suffix = suffix_lo_clusters10, data_type = opts$loading$data_type, opts)
        Do.python.export.jdata_sdata(ad_lo, directory_Data = opts$paths$attrition, suffix = suffix_lo_clusters10)
        flog.info("[attrition] for draw %s: done estimating clusters for leaveout set for keep.share %s", draw, keep.share)
      }
    }
  }

  specifications <- as.data.table(expand.grid(keep.share = keep.shares, draw = draws))
  if (1 %in% draws) {
    specifications <- rbind(specifications, data.table(keep.share = 1, draw = 1))
  }

  if (ncores == 1) {
    lapply(1:nrow(specifications), run_clusters)
  } else {
    parallel::mclapply(1:nrow(specifications), run_clusters, mc.cores = ncores)
  }

  flog.info("[attrition] DONE clustering.")
  return(NULL)
}


#' Run attrition estimation
#' @param opts The options
#' @export
Do.attrition.estimation <- function(opts) {
  keep.shares <- opts$attrition.exercise$keep.shares
  if (max(keep.shares) >= 1 | min(keep.shares) <= 0) {
    stop("[attrition] Do.attrition.clusters: keep.shares must be strictly less than 1 and greater than 0. Note that keep.share=1 is included by default.")
  }
  num.draws <- opts$attrition.exercise$num.draws
  ncores <- opts$misc$ncores

  flog.info("[attrition] Begin all estimation.")
  methods.connected <- opts$attrition.exercise$methods$connected
  methods.leaveout <- opts$attrition.exercise$methods$leaveout
  any_connected <- any(!is.na(methods.connected))
  any_leaveout <- any(!is.na(methods.leaveout))

  run_estimators <- function(ii) {
    keep.share <- specifications[ii]$keep.share
    draw <- specifications[ii]$draw

    flog.info("[attrition] for draw %s/%s: begin all estimation for keep.share %s", draw, num.draws, keep.share)

    suffix_con <- sprintf("_connected_draw%s_keep%s", draw, keep.share)
    suffix_lo <- sprintf("_leaveout_draw%s_keep%s", draw, keep.share)
    suffix_con_clusters10 <- sprintf("_connected_clusters10_draw%s_keep%s", draw, keep.share)
    suffix_lo_clusters10 <- sprintf("_leaveout_clusters10_draw%s_keep%s", draw, keep.share)

    res <- data.table()

    # connected set estimation
    if (any_connected) {
      if ("AKM" %in% methods.connected) {
        output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = paste0(suffix_con, "_akm"), data_type = opts$loading$data_type, opts)
        if (output_exists & opts$misc$preserve.estimates) {
          flog.info("[attrition] for draw %s/%s: AKM for connected set for keep.share %s already exists, so skipping it.", draw, num.draws, keep.share)
        } else {
          flog.info("[attrition] con-set AKM")
          ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_con, data_type = opts$loading$data_type, opts)
          sample <- Do.Sample.Stats(copy(ad), suffix = "_connected")
          res <- Do.AKM.stats(long_data = Do.AKM.estimate.noIO(ad), "_connected")
          res <- merge(res, sample, by = "suffix")
          res[, keep_share := keep.share]
          res[, draw := draw]
          Do.writing.jdata_sdata(res, directory_Data = opts$paths$attrition, suffix = paste0(suffix_con, "_akm"), data_type = opts$loading$data_type, opts)
        }
      }
      if ("CRE" %in% methods.connected) {
        output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = paste0(suffix_con_clusters10, "_cre"), data_type = opts$loading$data_type, opts)
        if (output_exists & opts$misc$preserve.estimates) {
          flog.info("[attrition] for draw %s/%s: CRE for connected set for keep.share %s already exists, so skipping it.", draw, num.draws, keep.share)
        } else {
          flog.info("[attrition] con-set CRE")
          ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_con_clusters10, data_type = opts$loading$data_type, opts)
          sample <- Do.Sample.Stats(copy(ad), suffix = "_connected")
          res <- Do.CRE_both.stats(m2.cre.estimate(ad), suffix = "_connected")
          res <- merge(res, sample, by = "suffix")
          res[, keep_share := keep.share]
          res[, draw := draw]
          Do.writing.jdata_sdata(res, directory_Data = opts$paths$attrition, suffix = paste0(suffix_con_clusters10, "_cre"), data_type = opts$loading$data_type, opts)
        }
      }
      if ("TraceHO" %in% methods.connected) {
        output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = paste0(suffix_con, "_traceHO"), data_type = opts$loading$data_type, opts)
        if (output_exists & opts$misc$preserve.estimates) {
          flog.info("[attrition] for draw %s/%s: CRE for connected set for keep.share %s already exists, so skipping it.", draw, num.draws, keep.share)
        } else {
          flog.info("[attrition] con-set traceHO")
          ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_con, data_type = opts$loading$data_type, opts)
          sample <- Do.Sample.Stats(copy(ad), suffix = "_connected")
          res <- Do.TraceHO.stats(m2.trace.estimate.full(ad, hetero = FALSE, check_set = FALSE, impose_leaveout = FALSE), suffix = "_connected")
          res <- merge(res, sample, by = "suffix")
          res[, keep_share := keep.share]
          res[, draw := draw]
          Do.writing.jdata_sdata(res, directory_Data = opts$paths$attrition, suffix = paste0(suffix_con, "_traceHO"), data_type = opts$loading$data_type, opts)
        }
      }
    }

    # leave-one-out set estimation
    if (any_leaveout) {
      if ("AKM" %in% methods.leaveout) {
        output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = paste0(suffix_lo, "_akm"), data_type = opts$loading$data_type, opts)
        if (output_exists & opts$misc$preserve.estimates) {
          flog.info("[attrition] for draw %s/%s: AKM for leaveout set for keep.share %s already exists, so skipping it.", draw, num.draws, keep.share)
        } else {
          flog.info("[attrition] leavoeut-set AKM")
          ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_lo, data_type = opts$loading$data_type, opts)
          sample <- Do.Sample.Stats(copy(ad), suffix = "_leaveout")
          res <- Do.AKM.stats(long_data = Do.AKM.estimate.noIO(ad), "_leaveout")
          res <- merge(res, sample, by = "suffix")
          res[, keep_share := keep.share]
          res[, draw := draw]
          Do.writing.jdata_sdata(res, directory_Data = opts$paths$attrition, suffix = paste0(suffix_lo, "_akm"), data_type = opts$loading$data_type, opts)
        }
      }
      if ("CRE" %in% methods.leaveout) {
        output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = paste0(suffix_lo_clusters10, "_cre"), data_type = opts$loading$data_type, opts)
        if (output_exists & opts$misc$preserve.estimates) {
          flog.info("[attrition] for draw %s/%s: CRE for leaveout set for keep.share %s already exists, so skipping it.", draw, num.draws, keep.share)
        } else {
          flog.info("[attrition] leaveout-set CRE")
          ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_lo_clusters10, data_type = opts$loading$data_type, opts)
          sample <- Do.Sample.Stats(copy(ad), suffix = "_leaveout")
          res <- Do.CRE_both.stats(m2.cre.estimate(ad), suffix = "_leaveout")
          res <- merge(res, sample, by = "suffix")
          res[, keep_share := keep.share]
          res[, draw := draw]
          Do.writing.jdata_sdata(res, directory_Data = opts$paths$attrition, suffix = paste0(suffix_lo_clusters10, "_cre"), data_type = opts$loading$data_type, opts)
        }
      }
      if ("TraceHO" %in% methods.leaveout) {
        output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = paste0(suffix_lo, "_traceHO"), data_type = opts$loading$data_type, opts)
        if (output_exists & opts$misc$preserve.estimates) {
          flog.info("[attrition] for draw %s/%s: traceHO for leaveout set for keep.share %s already exists, so skipping it.", draw, num.draws, keep.share)
        } else {
          flog.info("[attrition] leaveout-set traceHO")
          ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_lo, data_type = opts$loading$data_type, opts)
          sample <- Do.Sample.Stats(copy(ad), suffix = "_leaveout")
          res <- Do.TraceHO.stats(m2.trace.estimate.full(ad, hetero = FALSE, check_set = FALSE, impose_leaveout = FALSE, make_each_row_one_wid = TRUE), suffix = "_leaveout", diff = F)
          res <- merge(res, sample, by = "suffix")
          res[, keep_share := keep.share]
          res[, draw := draw]
          Do.writing.jdata_sdata(res, directory_Data = opts$paths$attrition, suffix = paste0(suffix_lo, "_traceHO"), data_type = opts$loading$data_type, opts)
        }
      }
      if ("TraceHE" %in% methods.leaveout) {
        output_exists <- Do.exists.jdata_sdata(directory_Data = opts$paths$attrition, suffix = paste0(suffix_lo, "_traceHE"), data_type = opts$loading$data_type, opts)
        if (output_exists & opts$misc$preserve.estimates) {
          flog.info("[attrition] for draw %s/%s: traceHE for leaveout set for keep.share %s already exists, so skipping it.", draw, num.draws, keep.share)
        } else {
          flog.info("[attrition] leaveout-set traceHE")
          ad <- Do.reading.jdata_sdata(directory_Data = opts$paths$attrition, suffix = suffix_lo, data_type = opts$loading$data_type, opts)
          sample <- Do.Sample.Stats(copy(ad), suffix = "_leaveout")
          res <- Do.TraceHE.stats(m2.trace.estimate.full(ad, hetero = TRUE, check_set = FALSE, impose_leaveout = FALSE, make_each_row_one_wid = TRUE), suffix = "_leaveout", diff = F)
          res <- merge(res, sample, by = "suffix")
          res[, keep_share := keep.share]
          res[, draw := draw]
          Do.writing.jdata_sdata(res, directory_Data = opts$paths$attrition, suffix = paste0(suffix_lo, "_traceHE"), data_type = opts$loading$data_type, opts)
        }
      }
    }
    flog.info("[attrition] for draw %s/%s: DONE all estimation for keep.share %s", draw, num.draws, keep.share)
  }

  specifications <- as.data.table(expand.grid(keep.share = keep.shares, draw = 1:num.draws))
  specifications <- rbind(specifications, data.table(keep.share = 1, draw = 1))

  if (ncores == 1) {
    lapply(1:nrow(specifications), run_estimators)
  } else {
    parallel::mclapply(1:nrow(specifications), run_estimators, mc.cores = ncores)
  }

  flog.info("[attrition] DONE all estimation.")

  return(NULL)
}


#' Run attrition estimation
#' @param opts The options
#' @export
Do.attrition.collect.output <- function(opts) {
  keep.shares <- opts$attrition.exercise$keep.shares
  if (max(keep.shares) >= 1 | min(keep.shares) <= 0) {
    stop("[attrition] Do.attrition.clusters: keep.shares must be strictly less than 1 and greater than 0. Note that keep.share=1 is included by default.")
  }
  num.draws <- opts$attrition.exercise$num.draws

  flog.info("[attrition] Begin collecting all estimation.")
  methods.connected <- opts$attrition.exercise$methods$connected
  methods.leaveout <- opts$attrition.exercise$methods$leaveout
  any_connected <- any(!is.na(methods.connected))
  any_leaveout <- any(!is.na(methods.leaveout))

  res_all <- data.table()

  for (draw in 1:num.draws) {
    keep.shares.set <- copy(keep.shares)
    if (draw == 1) {
      keep.shares.set <- c(1, keep.shares.set)
    }
    for (keep.share in keep.shares.set) {
      flog.info("[attrition] for draw %s/%s: collect all estimation for keep.share %s", draw, num.draws, keep.share)

      suffix_con <- sprintf("_connected_draw%s_keep%s", draw, keep.share)
      suffix_lo <- sprintf("_leaveout_draw%s_keep%s", draw, keep.share)
      suffix_con_clusters10 <- sprintf("_connected_clusters10_draw%s_keep%s", draw, keep.share)
      suffix_lo_clusters10 <- sprintf("_leaveout_clusters10_draw%s_keep%s", draw, keep.share)

      res <- data.table()

      # connected set results
      if (any_connected) {
        if ("AKM" %in% methods.connected) {
          suffix <- paste0(suffix_con, "_akm")
          this_res <- setDT(readRDS(paste0(opts$paths$attrition, "bothdata", suffix, ".rds")))
          res <- rbindlist(list(res, this_res), use.names = T, fill = T)
        }
        if ("CRE" %in% methods.connected) {
          suffix <- paste0(suffix_con_clusters10, "_cre")
          this_res <- setDT(readRDS(paste0(opts$paths$attrition, "bothdata", suffix, ".rds")))
          res <- rbindlist(list(res, this_res), use.names = T, fill = T)
        }
        if ("TraceHO" %in% methods.connected) {
          suffix <- paste0(suffix_con, "_traceHO")
          this_res <- setDT(readRDS(paste0(opts$paths$attrition, "bothdata", suffix, ".rds")))
          res <- rbindlist(list(res, this_res), use.names = T, fill = T)
        }
      }

      # leaveout set results
      if (any_leaveout) {
        if ("AKM" %in% methods.leaveout) {
          suffix <- paste0(suffix_lo, "_akm")
          this_res <- setDT(readRDS(paste0(opts$paths$attrition, "bothdata", suffix, ".rds")))
          res <- rbindlist(list(res, this_res), use.names = T, fill = T)
        }
        if ("CRE" %in% methods.leaveout) {
          suffix <- paste0(suffix_lo_clusters10, "_cre")
          this_res <- setDT(readRDS(paste0(opts$paths$attrition, "bothdata", suffix, ".rds")))
          res <- rbindlist(list(res, this_res), use.names = T, fill = T)
        }
        if ("TraceHO" %in% methods.leaveout) {
          suffix <- paste0(suffix_lo, "_traceHO")
          this_res <- setDT(readRDS(paste0(opts$paths$attrition, "bothdata", suffix, ".rds")))
          res <- rbindlist(list(res, this_res), use.names = T, fill = T)
        }
        if ("TraceHE" %in% methods.leaveout) {
          suffix <- paste0(suffix_lo, "_traceHE")
          this_res <- setDT(readRDS(paste0(opts$paths$attrition, "bothdata", suffix, ".rds")))
          res <- rbindlist(list(res, this_res), use.names = T, fill = T)
        }
      }

      res_all <- rbind(res_all, res)
    }
  }

  res_all <- cbind(res_all[, .(draw, keep_share, method, suffix)], res_all[, !c("draw", "keep_share", "method", "suffix")])

  # export
  write.csv(res_all, file = paste0(opts$paths$final, "final_randomattrition_", opts$country, ".csv"), row.names = F)

  flog.info("[attrition] DONE collecting all estimation.")
}


#' runs all the attrition commands, given 'country' as input
#' @param country
#' @param preserve.rawdata If TRUE, do not overwrite the existing raw data setup
#' @param preserve.estimates If TRUE, do not overwrite the existing estimates
#' @param preserve.initdata If TRUE, do not overwrite the existing data initialization
#' @param data_only If TRUE, only set up the data for estimation, do not actually run any estimation
#' @param draws Number of random draws to use for each given attrition share
#' @param moversperfirm Number of movers to require per firm in the initial sample
#' @param cluster_bothyears If TRUE, estimate the clusters using pre-move and post-move years
#' @param samefirms If TRUE, impose that the set of firms is held the same across the various attrition shares
#' @export
Do.run.attrition <- function(country,
                             preserve.rawdata = TRUE,
                             preserve.estimates = TRUE,
                             preserve.initdata = TRUE,
                             data_only = TRUE,
                             draws = NA,
                             moversperfirm = NA,
                             cluster_bothyears = FALSE,
                             samefirms = FALSE) {
  flog.info("1. [Do.run.attrition] set up options and verify raw data for country %s", country)
  flog.info("1a. [Do.run.attrition] set up with country-specific options for country %s", country)
  opts <- Do.Init.Opts(country)
  opts$misc$preserve.rawdata <- preserve.rawdata
  opts$misc$preserve.initdata <- preserve.initdata
  opts$misc$preserve.estimates <- preserve.estimates
  opts$misc$data_only <- data_only
  opts$misc$cluster_bothyears <- cluster_bothyears
  opts$attrition.exercise$samefirms <- samefirms

  if (!is.na(draws)) {
    opts$attrition.exercise$draws <- draws
  }
  if (!is.na(moversperfirm)) {
    opts$attrition.exercise$moversperfirm <- moversperfirm
  }

  flog.info("1b. [Do.run.attrition] If it does not exist yet, reading the raw data and saving spells for country %s", country)
  Do.sample.init(opts)
  Do.jdata_sdata.make(opts)

  flog.info("2. [Do.run.attrition] set up random draws for country %s", country)
  flog.info("2a. [Do.run.attrition] initialize data with 15 movers for country %s", country)
  Do.attrition.init(opts)

  flog.info("2b. [Do.run.attrition] make random draws, find connected sets for country %s", country)
  Do.attrition.draws.connected(opts)

  flog.info("2c. [Do.run.attrition] for each random draw, find leaveout sets for country %s", country)
  Do.attrition.draws.leaveout(opts)

  flog.info("2d. [Do.run.attrition] for each random draw, find clusters for country %s", country)
  Do.attrition.draws.clusters(opts)

  if (data_only) {
    flog.info("2e. [Do.run.attrition] for each random draw, finished all data construction for country %s", country)
    return(NULL)
  }

  flog.info("3. [Do.run.attrition] for each random draw, run estimation for country %s", country)
  Do.attrition.estimation(opts)

  flog.info("4. [Do.run.attrition] collecting all estimation into final output for country %s", country)
  Do.attrition.collect.output(opts)

  flog.info("[Do.run.attrition] DONE for country %s", country)
}
