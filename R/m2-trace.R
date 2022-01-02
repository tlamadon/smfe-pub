#' some work on trace formula

# --- COMPUTING CONNECTED SETS ----


#' Extracts the largest connected set from data using f1,f2
#' movers
#' @export
get.largest.conset.fid.old <- function(jdata) {
  # combine th 2 directions
  jdata2 = rbind(jdata[,list(f1,f2)],jdata[,list(f1=f2,f2=f1)])

  AD = pmin(acast(jdata2[,.N,list(f1,f2)],value.var = "N",f1~f2,drop=FALSE,fill=0),1)
  AD = AD[,rownames(AD)] # enforce same order on row and columns, not sure why acast does not do that!

  # compute connected sets
  cs = conComp(AD,2)
  # extract largest
  cs = data.table(f1=names(cs),set=as.numeric(cs))
  cs.size = cs[,.N,set][order(-N)]
  return(cs[  set == cs.size$set[1]  , f1])
}


#' Extract the largest connected set from the data
#' @export
get.connected_set <- function(data, ncat = 1, threshold = 0.9) {
  setkey(data, worker_ID)
  data[, t := 1:.N, worker_ID]
  setkey(data, worker_ID, t)
  data[, firm_ID2 := data[J(worker_ID, t - 1), firm_ID]]

  dadj <- unique(data[!is.na(firm_ID2), list(f1 = firm_ID, f2 = firm_ID2)])

  dadj <- dadj[f1 != f2]
  dadj <- rbind(dadj, dadj[, list(f1 = f2, f2 = f1)])
  dadj <- dadj[f1 != f2]
  flog.info("[get-connected-set] number of firms connections via movers: %i", nrow(dadj))

  # compute connected set
  setkey(dadj, f1)
  dadj[, grp := 0]
  dadj[, proc := 0]
  # start with first obs

  # cur_worker_ID = data[1,worker_ID]
  cur_grp <- 1
  cur_firm_ID <- dadj[1, f1]

  cur_firm_ID <- dadj[, .N, f1][.N > 50][, sample(dadj$f1, 1)]
  dadj[, grp := 0]

  i <- 0
  while (TRUE) {
    i <- i + 1

    # get all siblings of cur_firm_ID
    firm_IDs <- dadj[f1 %in% cur_firm_ID, unique(f2)]

    # extract the one that are new (we need to get their sibblings)
    cur_firm_ID <- dadj[f1 %in% firm_IDs][grp == 0, unique(f1)]
    if (length(cur_firm_ID) == 0) {
      cur_grp <- cur_grp + 1
      # cur_firm_ID = dadj[grp==0,f1[1]]
      cur_firm_ID <- dadj[grp == 0][, .N, f1][order(-N)][, f1[1]]

      if (dadj[, mean(grp > 0)] > threshold) {
        flog.info("[get-connected-set] passed threshold, we stop.")
        break
      }
      if ((length(cur_firm_ID) == 0) | (is.na(cur_firm_ID))) break # we are done here
    }

    # set these firms to cur_group
    dadj[f1 %in% cur_firm_ID, grp := cur_grp]
    perc <- dadj[, mean(grp > 0)]

    if ((i %% ncat) == 0) flog.info("[get-connected-set][%i] %f%% done, cur_grp=%i, nfirms_grp=%i", i, 100 * perc, cur_grp, length(cur_firm_ID))
  }

  # extract firms in largest connected set
  large_grp <- dadj[, .N, grp][order(-N)][1, grp]
  fids <- dadj[grp == large_grp, unique(c(f1, f2))]

  # compute share of workers
  share <- data[, mean(firm_ID %in% fids)]

  flog.info("[get-connected-set] done")
  return(list(largest = fids, share = share, all = dadj))
}




#' Extracts the largest connected set from data using f1,f2
#' movers
#' @export
get.largest.conset.fid <- function(jdata) {

  ldata = rbind(jdata[,list(worker_ID=1:.N,firm_ID=f1)],jdata[,list(worker_ID=1:.N,firm_ID=f2)])

  res = get.connected_set(ldata,ncat=100)
  return(res$largest)
}


#' Extracts the largest connected set from data using f1,f2
#' movers
#' @export
get.largest.conset.clus <- function(jdata) {
  # combine th 2 directions
  jdata2 = rbind(jdata[,list(j1,j2)],jdata[,list(j1=j2,j2=j1)])
  AD = pmin(acast(jdata2[,.N,list(j1,j2)],value.var = "N",j1~j2,drop=FALSE,fill=0),1)
  AD = AD[,rownames(AD)] # enforce same order on row and columns, not sure why acast does not do that!

  # compute connected sets
  cs = conComp(AD,2)
  # extract largest
  cs = data.table(j1=as.integer(names(cs)),set=as.numeric(cs))
  cs.size = cs[,.N,set][order(-N)]
  return(cs[  set == cs.size$set[1]  , j1])
}


#' Extracts the largest leave-out connected set from data using f1,f2
#' movers
#' @export
get.largest.leaveoutset.fid <- function(jdata,method=0) {

  jdata = data.table(jdata)

  # we loop multiple times
  for (rep in 1:20) {

    # get the connected set
    flog.info("[get-leaveout-set][%i] extract connected set",rep)
    f1s   = get.largest.conset.fid(jdata)
    jdata = jdata[f1%in%f1s][f2%in%f1s]

    # remove firms with 1 mover
    for (i in 1:10) {
      f0s = data.table(rbind(jdata[,list(fid=f1,0)],jdata[,list(fid=f2,0)]))[,.N,fid][N==1,fid]
      if (length(f0s)==0)  break;
      jdata = jdata[!f1%in%f0s][!f2%in%f0s]

      if (nrow(jdata)==0) return(jdata)
    }

    # extract articulation firms
    flog.info("[get-leaveout-set][iter %i] find articulation points in network",rep)
    G   = graph(c(jdata[,rbind(f1,f2)]),directed = F)
    L   = articulation.points(G)
    #if (!is.null(names(V(G)))) {
    #  L   = names(V(G))[L]
    #}
    if (typeof(jdata$f1) == "character") {
      L = names(L)
    }
    flog.info("[get-leaveout-set][iter %i] found %i articulation firms",rep,length(L))

    if (method==0) {
      # for each articulation firm, check removing movers
      bad_movers = c()
      for (fi in L) {
        # find edges for this firm
        #II1 = jdata[,list(1:.N,f1)][f1==fi,V1]
        #II2 = jdata[,list(1:.N,f2)][f2==fi,V1]
        #II = union(II1,II2)

        move_candidates = rbind(jdata[,list(1:.N,fa=f1,fb=f2)],jdata[,list(1:.N,fa=f2,fb=f1)])[fa==fi]
        move_candidates[,m:=.N,fb]
        II = move_candidates[m==1,V1]
        II = setdiff(II,bad_movers)

        for (i in II) {
          Gsub = delete.edges(G, i)
          ng = length( decompose.graph(Gsub) )
          if (ng>1) bad_movers = c(bad_movers,i);
        }
      }
    } else {
      # METHOD 2
      # we simply take movers that are unique movers between
      # two articulation firms
      afirms = jdata[,list(wid = 1:.N,f1,f2)][f1 %in% L][f2 %in% L]
      afirms[, firm_pair_movers := .N, list(f1,f2)]
      afirms = afirms[firm_pair_movers==1]
      bad_movers = afirms[,wid]
      flog.info("[get-leaveout-set][iter %i] method 2, found %i bad movers",rep,length(bad_movers))
    }

    if (length(bad_movers)==0) break;
    jdata = jdata[setdiff(1:.N,bad_movers)]
  }

  flog.info("[get-leaveout-set] done, %i firms in final set", jdata[,length(unique(c(f1,f2)))])
  return(jdata)
}

#' Extracts the largest leave-out connected set from data using f1,f2
#' movers. Does it by create the bi-partite network
#' @export
get.largest.leaveoutset.fid2 <- function(jdata) {

  jdata = data.table(jdata)

  # createing uinque string identifiers
  jdata[,f1str := paste(f1)]
  jdata[,f2str := paste(f2)]
  jdata[,midstr:= paste0("MO",1:.N)]

  # we loop multiple times
  for (rep in 1:20) {

    # get the connected set
    flog.info("[get-leaveout-set][%i] extract connected set",rep)
    f1s   = get.largest.conset.fid(jdata)
    jdata = jdata[f1%in%f1s][f2%in%f1s]

    # remove firms with 1 mover
    for (i in 1:10) {
      f0s = data.table(rbind(jdata[,list(fid=f1,0)],jdata[,list(fid=f2,0)]))[,.N,fid][N==1,fid]
      if (length(f0s)==0)  break;
      jdata = jdata[!f1%in%f0s][!f2%in%f0s]

      if (nrow(jdata)==0) return(jdata)
    }

    # create bi-partite network
    flog.info("[get-leaveout-set][iter %i] find articulation points in bi-partite network",rep)
    G   = graph(c(jdata[,rbind(midstr,f1)],jdata[,rbind(midstr,f2)]),directed = F)
    L   = articulation.points(G)
    L   = names(L)
    L = L[str_detect(L,"MO")]
    flog.info("[get-leaveout-set][iter %i] found %i articulation movers",rep,length(L))

    if (length(L)==0) break;
    jdata = jdata[!(midstr %in% L)]
  }

  flog.info("[get-leaveout-set] done, %i firms in final set", jdata[,length(unique(c(f1,f2)))])
  return(jdata)
}


#' Extracts the largest leave-out connected set from data using f1,f2
#' movers
#' @export
get.largest.leaveoutset.clus <- function(jdata) {

  # we loop multiple times
  for (rep in 1:20) {
    flog.debug("rep %i in computing connected set on clusters",rep,name="compute_sets")

    # get the connected set
    j1s   = get.largest.conset.clus(jdata)
    jdata = jdata[j1%in%j1s][j2%in%j1s]

    # remove firms with 1 mover
    for (i in 1:10) {
      j0s   = jdata[,list(j1=c(j1,j2),.N)][,.N,j1][N==1,j1]
      if (length(j0s)==0)  break;
      jdata = jdata[!j1%in%j0s][!j2%in%j0s]
    }

    # extract articulation firms
    G   = graph(c(jdata[,rbind(j1,j2)]),directed = F)
    L   = articulation.points(G)
    #if (!is.null(names(V(G)))) {
    #  L   = names(V(G))[L]
    #}
    if (typeof(jdata$j1) == "character") {
      L = names(L)
    }
    flog.debug("found %i firms to check",length(L),name="compute_sets")

    # for each articulation firm, check removing movers
    bad_movers = c()
    fc=0
    for (ji in L) {
      # find edges for this firm
      # II1 = jdata[,list(1:.N,j1)][j1==ji,V1]
      # II2 = jdata[,list(1:.N,j2)][j2==ji,V1]
      # II = union(II1,II2)
      # II = setdiff(II,bad_movers)
      fc = fc+1

      move_candidates = rbind(jdata[,list(1:.N,ja=j1,jb=j2)],jdata[,list(1:.N,ja=j2,jb=j1)])[ja==ji]
      move_candidates[,m:=.N,jb]
      II = move_candidates[m==1,V1]
      II = setdiff(II,bad_movers)
      flog.debug("found %i movers to check for current firm %i",length(II),fc,name="compute_sets")

      for (i in II) {
        Gsub = delete.edges(G, i)
        ng = length( decompose.graph(Gsub) )
        if (ng>1) bad_movers = c(bad_movers,i);
      }
    }
    flog.debug("found %i bad movers",length(bad_movers),name="compute_sets")

    if (length(bad_movers)==0) break;
    jdata = jdata[setdiff(1:.N,bad_movers)]
  }

  return(jdata)
}

# ------- ADDITIONAL FUCNTIONS ---------

#' transform event study data to panel data
#' @export
mt.from.m2 <- function(sim,make_each_row_one_wid=FALSE,include_stayer_period1=TRUE, attach_clusters=FALSE) {

  if (make_each_row_one_wid) {
    sim$sdata[,wid2 := 1:.N ]
    wid_stayer_max = sim$sdata[,max(wid2)]
    sim$jdata[, wid2 := (1:.N) + wid_stayer_max]
  } else {
    sim$sdatasdata[, wid2 := wid]
    sim$jdata[, wid2 := wid]
  }

  if (attach_clusters==TRUE) {
    adata = rbind(
      sim$jdata[, list(wid=wid2,fid=f1,y=y1,t=1,m=1,j=j1,j1=j1,j2=j2)],
      sim$jdata[, list(wid=wid2,fid=f2,y=y2,t=2,m=1,j=j2,j1=j1,j2=j2)])
  } else {
    adata = rbind(
      sim$jdata[, list(wid=wid2,fid=f1,y=y1,t=1,m=1)],
      sim$jdata[, list(wid=wid2,fid=f2,y=y2,t=2,m=1)])
  }

  # append period 2 stayers
  if (include_stayer_period1) {
    # adata = rbind(adata,sim$sdata[f1 %in% fids_con, list(wid,fid=f1,y=y1,t=year)])
    if (attach_clusters==TRUE) {
      adata = rbind(adata,sim$sdata[, list(wid=wid2,fid=f1,y=y1,t=1,m=0,j=j1,j1=j1,j2=j1)])
    } else {
      adata = rbind(adata,sim$sdata[, list(wid=wid2,fid=f1,y=y1,t=1,m=0)])
    }
  }

  return(adata)
}

#' Hucthinkson approximative trace computation
#'
#' This function takes in two matrices, computes their inverse, and computes the trace or approximates it
#' via the Hutchinson Trace Approximator approach. Returns approximation of trace( A^-1 Q)

#' @param A mobility matrix
#' @param K weighting matrix (Q) -- or -- fimrs vector S
#' @param ndraws number of columns to draw in approximation
#' @param new 0 or 1 -- if new = 1, use firm vector S not Q; otheriwse use Q, the weighting matrix
#' @return approximation of trace( A^-1 Q)
trapprox <- function(A, K, ndraws, new = 1){
  M <- SparseM::solve(A,nnzlmax=1e8,tmpmax=1e8,nsubmax=1e8)
  # drawing Radamcher matrix
  X <- matrix(rbinom(n = ndraws * nrow(M), size = 1, prob = 0.5), nrow = nrow(M), ncol = ndraws) # create R matrix
  X[X == 0] <- -1
  if(new == 1){
    T = sum(K)
    Q_by_X = vector()
    for(i in 1:ncol(X)){
      V = X[,i]
      V1 = (K/T) * (V - 1/T * sum(V * K))
      V2 = V1 - K/T * sum(V1)
      Q_by_X = cbind(Q_by_X, V2)
    }
    S = mean(SparseM::diag(t(X) %*% M %*% Q_by_X ))
  } else {
    S = mean(SparseM::diag( t(X) %*% M %*% K %*% X ))
  }
  return(S)
}

#' @param A mobility matrix
#' @param K weighting matrix (Q) -- or -- fimrs vector S
#' @param ndraws number of columns to draw in approximation
#' @param new 0 or 1 -- if new = 1, use firm vector S not Q; otheriwse use Q, the weighting matrix
#' @return approximation of trace( A^-1 Q)
m2.trace.tr_approx <- function(M, S, ndraws){

  # this will be eventually replaced with a linear solver instead of inversion
  # M <- SparseM::solve(A,nnzlmax=1e8,tmpmax=1e8,nsubmax=1e8)

  flog.info("[twfe-firm] running trace approximation with ndrwas = %i",ndraws)

  # drawing Radamcher matrix
  X <- matrix(rbinom(n = ndraws * nrow(M), size = 1, prob = 0.5), nrow = nrow(M), ncol = ndraws) # create R matrix
  X[X == 0] <- -1

  # compute Q %*% X in an efficient way
  T = sum(S)
  Q_by_X = vector()
  vals = rep(0,ndraws)
  for(i in 1:ncol(X)){
    V = X[,i]
    V1 = (S/T) * (V - 1/T * sum(V * S))
    V2 = V1 - S/T * sum(V1)
    Q_by_X = cbind(Q_by_X, V2)

    # solve the system
    flog.info("[twfe-firm] computing  %i/%i ",i,ndraws)
    V3 = Matrix::solve(M,Q_by_X,sparse=TRUE,tol=1e-7)
    vals[i] = sum(V3 * V)
  }

  flog.info("[twfe-firm] done with trace approximation m=%4.4f s=%4.4f",mean(vals),sd(vals))

  #tr_approx = mean(SparseM::diag(t(X) %*% M %*% Q_by_X ))
  return( mean(vals))
}

#' function which extracts all bias correction
#' in particular it gets corrected cov and var(alpha)
#'
#' the input adata should have the following columns: fid, wid, m, cs
#'
#' @param var_e variance of the residual
#' @param adata panel data on movers and stayers
#' @export
m2.get_all_bc <- function(data) {

  res = list()

  data[, fid2 := .GRP, fid]
  data[, wid2 := .GRP, wid]
  data[, fid := fid2]
  data[, wid := wid2]

  # 1) construct J,W
  nn = data[,.N]
  nw = data[,max(wid)]
  nf = data[,max(fid)]
  J  <- sparseMatrix(1:nn, data$fid, x = 1,dims = c(nn,nf))
  J  <- J[,1:(nf-1)] # here we want to drop the last firm as a normalization
  W  <- sparseMatrix(1:nn, data$wid, x = 1,dims = c(nn,nw))
  Dw <- data[, .N, wid][order(wid),N]
  Df <- data[, .N, fid][order(fid),N]
  Dinv = Diagonal(nw,1/Dw)
  Y = data$y

  # 0) create the Jq,Wq matrices
  nnq = data[cs==1,.N]
  Jq  <- sparseMatrix(1:nnq, data[cs==1]$fid, x = 1,dims = c(nnq,nf))
  Jq  <- Jq[,1:(nf-1)] # here we want to drop the last firm as a normalization
  Wq  <- sparseMatrix(1:nnq, data[cs==1]$wid, x = 1,dims = c(nnq,nw))

  t = Matrix:::t

  # 2) construct sub-matrices
  M1  = t(J) %*% J - t(J) %*% W %*% Dinv %*% t(W) %*% J
  RHS = as.numeric(t(J) %*% (Diagonal(nn) - W %*% Dinv %*% t(W)) %*% Y)
  flog.info("[full andrews] non zeros in M1 %i (%f perc.) size=%i, getting inverse...",nnzero(M1),100*nnzero(M1)/cumprod(dim(M1))[-1],dim(M1)[1])
  M1inv = Matrix::solve(M1)
  flog.info("[full andrews] done")

  # 3) get alph and psi
  psi_hat    = as.numeric(Matrix::solve(M1, RHS))
  alpha_hat  = as.numeric(Dinv %*% t(W) %*% ( Y- J %*% psi_hat))
  # attach them
  data$psi_hat   = c(psi_hat,0)[data$fid]
  data$alpha_hat = alpha_hat[data$wid]

  # 4) get cov(y,psi)
  res$cov_ypsi_all_fe = data[cs==1,cov(y,psi_hat)]
  res$cov_ypsi_movers_fe  = data[(cs==1) & (m==1),cov(y,psi_hat)]
  res$cov_ypsi_stayers_fe = data[(cs==1) & (m==0),cov(y,psi_hat)]
  res$posterior_movers_share = data[(cs==1) ,mean(m==1)]
  flog.info("[full andrews] fixed-effect Cov(y1,psi): all=%4.4f movers=%4.4f stayers=%4.4f share-mover=%4.4f",res$cov_ypsi_all_fe,res$cov_ypsi_movers_fe,res$cov_ypsi_stayers_fe,res$posterior_movers_share)

  flog.info("[full andrews] computing variance of epsilon")
  # 4) extract the variance of epsilon using the movers only
  # Z = data[,m]
  # var_e = 1/( sum(Z) - nf +1)*sum(Z * Y * (Y - as.numeric(J %*% psi_hat + W %*% alpha_hat)))
  var_e = 1/( nn - nw - nf +1)*sum(Y * (Y - as.numeric(J %*% psi_hat + W %*% alpha_hat)))
  flog.info("[full andrews] variance of epsilon =%4.3f",var_e)

  # computing corrections
  # var(psy)
  # compute full inverse....
  flog.info("[full andrews] computing corrections")
  iota  = rep(1,nnq)
  tr2   = as.numeric(nnq^(-2)*(iota %*% Jq) %*% (M1inv %*% (t(Jq) %*% iota)))
  tr1   = as.numeric(nnq^(-1)* sum( Matrix::diag( (t(Jq) %*% Jq) %*% M1inv )))
  tr    = tr1 - tr2

  flog.info("[full andrews]  psi: fe=%4.3f bc=%4.3f ",
            data[cs==1,var(psi_hat)],data[cs==1,var(psi_hat)] - var_e * tr)

  res$psi_var_fe = data[cs==1,var(psi_hat)]
  res$psi_var_bc = data[cs==1,var(psi_hat)] - var_e * tr

  # ------- COVARIANCE TERM ------ #
  # we have to use J M J' W Dw^-1 W'
  tmp = M1inv %*% ( t(J) %*% ( W %*% (Dinv %*% (t(Wq) %*% iota))))
  tr2 = as.numeric( nnq^(-2) * (iota %*% Jq) %*% tmp)
  tr1 = as.numeric( nnq^(-1) * sum( Matrix::diag(  M1inv %*% ( t(J) %*% W %*%  Dinv %*% t(Wq) %*% Jq   ) )))
  tr = tr1 - tr2
  flog.info("[full andrews]  cov: fe=%4.3f bc=%4.3f ",
            data[cs==1,cov(psi_hat,alpha_hat)],data[cs==1,cov(psi_hat,alpha_hat)] + var_e * tr)
  res$cov_fe = data[cs==1,cov(psi_hat,alpha_hat)]
  res$cov_bc = data[cs==1,cov(psi_hat,alpha_hat)] + var_e * tr

  # ------- VARIANCE OF X ------ #
  # we have to use W Dw^-1 W' + W Dw^-1 W' J M J' W Dw^-1 W'
  p1 = iota %*% (Wq %*% (Dinv %*% (t(Wq) %*% iota)))
  p2 = t(J) %*% ( W %*% (Dinv %*% (t(Wq) %*% iota)))
  p2 = t(p2) %*% (M1inv %*% p2)
  tr2 = nnq^(-2) * as.numeric(p1 + p2)
  # we have to use tr(I +  M J' W Dw^-1 Wq'Jq)
  tr1 = nnq^(-1) * sum(Matrix::diag(Dinv %*% (t(Wq) %*% Wq))) + nn^(-1) * sum(Matrix::diag( M1inv %*% (t(J) %*% W %*% Dinv %*% t(Wq) %*% Jq)))
  tr = tr1 - tr2
  flog.info("[full andrews] alpha fe=%4.3f bc=%4.3f ",
            data[cs==1,var(alpha_hat)],data[cs==1,var(alpha_hat)] - var_e * tr)

  res$alpha_var_fe = data[cs==1,var(alpha_hat)]
  res$alpha_var_bc = data[cs==1,var(alpha_hat)] - var_e * tr

  return(res)
}

#' function which extracts all bias correction
#' in particular it gets corrected cov and var(alpha)
#'
#' @param var_e variance of the residual
#' @param adata panel data on movers and stayers
#' @export
m2.get_all_bc_hetero_bis <- function(data) {

  res = list()

  # mark workers with 1 spell only
  wids = data[,.N,wid][N==1,wid]
  data[,m:=1]
  data[(wid %in% wids),m := 0]
  flog.info("[full tr-hetero] number of workers with only 1 observation %i",data[m==0,.N])

  data[, fid2 := .GRP, fid]
  data[, wid2 := .GRP, wid]
  data[, fid := fid2]
  data[, wid := wid2]

  # 1) construct J,W
  nn = data[,.N]
  nw = data[,max(wid)]
  nf = data[,max(fid)]
  J  <- sparseMatrix(1:nn, data$fid, x = 1,dims = c(nn,nf))
  J  <- J[,1:(nf-1)] # here we want to drop the last firm as a normalization
  W  <- sparseMatrix(1:nn, data$wid, x = 1,dims = c(nn,nw))
  Dw <- data[, .N, wid][order(wid),N]
  Df <- data[, .N, fid][order(fid),N]
  Dinv = Diagonal(nw,1/Dw)
  Y = data$y

  # 0) create the Jq,Wq matrices
  nnq = data[cs==1,.N]
  Jq  <- sparseMatrix(1:nnq, data[cs==1]$fid, x = 1,dims = c(nnq,nf))
  Jq  <- Jq[,1:(nf-1)] # here we want to drop the last firm as a normalization
  Wq  <- sparseMatrix(1:nnq, data[cs==1]$wid, x = 1,dims = c(nnq,nw))

  t = Matrix:::t

  # 2) construct sub-matrices
  M1  = t(J) %*% J - t(J) %*% W %*% Dinv %*% t(W) %*% J
  RHS = as.numeric(t(J) %*% (Diagonal(nn) - W %*% Dinv %*% t(W)) %*% Y)
  flog.info("[full tr-hetero] percentage of non zeros in M1 %f, getting inverse...",nnzero(M1)/cumprod(dim(M1))[-1])

  # 3) get alph and psi
  psi_hat    = as.numeric(Matrix::solve(M1, RHS))
  alpha_hat  = as.numeric(Dinv %*% t(W) %*% ( Y- J %*% psi_hat))
  # attach them
  data$psi_hat   = c(psi_hat,0)[data$fid]
  data$alpha_hat = alpha_hat[data$wid]

  # 4) get cov(y,psi)
  res$cov_ypsi_all_fe = data[cs==1,cov(y,psi_hat)]
  res$cov_ypsi_movers_fe  = data[(cs==1) & (m==1),cov(y,psi_hat)]
  res$cov_ypsi_stayers_fe = data[(cs==1) & (m==0),cov(y,psi_hat)]
  res$posterior_movers_share = data[(cs==1) ,mean(m==1)]
  flog.info("[full tr-hetero] fixed-effect Cov(y1,psi): all=%4.4f movers=%4.4f stayers=%4.4f share-mover=%4.4f",res$cov_ypsi_all_fe,res$cov_ypsi_movers_fe,res$cov_ypsi_stayers_fe,res$posterior_movers_share)

  # computing corrections
  # var(psy)
  # compute full inverse....
  M1inv = Matrix::solve(M1)
  flog.info("[full tr-hetero] done")

  flog.info("[full tr-hetero] prepare covariance and leverage matrix")
  # P = colSums( A .* (A'A)^-1 A' )
  # in the AKM case, for observation (it) it is M[i,i] + M[j,j] + 2 * M[i,j]
  M1inv_dense =  as.matrix(M1inv)
  Pi = sparseDiagCross( t(J) ,M1inv_dense , t(J), nn) -
       2*sparseDiagCross( t(J) , M1inv_dense , t(J) %*% W %*% Dinv %*% t(W), nn) +
       sparseDiagDot(t(W),1/Dw,t(W),nn) +
       sparseDiagCross( t(J) %*% W %*% Dinv %*% t(W) , M1inv_dense , t(J) %*% W %*% Dinv %*% t(W), nn)
  Si = Y * (Y - as.numeric(J %*% psi_hat + W %*% alpha_hat))

  flog.info("[full tr-hetero] filling stayers with estimated variance from movers")
  # We replace the estimate for the non-movers with sigma at the firm level
  data[, s_i := Si][, p_i := Pi]
  flog.info("[full tr-hetero] leverage: min=%4.4f max=%4.4f",data[m==1,min(p_i)],data[m==1,max(p_i)])

  data[m==1, s_i:= s_i/(1-p_i)]
  data[, s_j := mean(s_i[m==1]/(1-p_i[m==1])), fid]
  data[m==0, s_i:=s_j]
  Si = data$s_i

  flog.info("[full tr-hetero] mean variance of residuals hetero = %4.4f",mean(Si))

  # ----- var(psi) correction ------ #
  # compute the right hand side of \mathb{M}
  Rt =    + t(J) %*% Diagonal(nn,Si) %*% J
  Rt = Rt - t(J) %*% W %*% Dinv %*% t(W) %*% Diagonal(nn,Si) %*% J
  Rt = Rt - t(J) %*% Diagonal(nn,Si) %*%W %*% Dinv %*% t(W) %*% J
  Rt = Rt + t(J) %*% W %*% Dinv %*% t(W) %*% Diagonal(nn,Si) %*% W %*% Dinv %*% t(W) %*% J
  iota  = rep(1,nnq)

  #tr1 = nn^-1 * sum(diag( as.matrix(M1inv %*% Rt)))
  tr1 = nnq^-1 * denseTraceProd( as.matrix(M1inv_dense %*% (t(Jq) %*% Jq )),
                                as.matrix(M1inv_dense %*% Rt) )
  tr2 = nnq^-2 * sum( as.numeric( ((iota %*% Jq) %*% M1inv) %*% Rt) * as.numeric( ((iota %*% Jq) %*% M1inv)) )
  tr = tr1 - tr2

  flog.info("[full tr-hetero]  psi: fe=%4.3f bc=%4.3f ",
            data[cs==1,var(psi_hat)],data[cs==1,var(psi_hat)] - tr)
  res$psi_var_fe = data[cs==1,var(psi_hat)]
  res$psi_var_bc = data[cs==1,var(psi_hat)] -  tr

  # ----- cov(alpha,psi) correction ------ #
  # compute tr( Mcal %*% R )
  # first part of U
  # Mc1 = M1inv %*% (t(J) %*% W %*% Dinv %*% t(W) %*% J) %*% M1inv
  # tr11 = nn^-1 * sum(diag( as.matrix(Mc1 %*% Rt)))
  tr11 = - nnq^-1 * denseTraceProd( as.matrix(M1inv_dense %*% (t(J) %*% W %*% Dinv %*% t(Wq) %*% Jq)),
                                     as.matrix(M1inv_dense %*% Rt) )

  # second part of U
  tr12 = - nnq^-2 * sum(  as.numeric( M1inv %*% (t(J) %*% (W %*% (Dinv %*% (t(Wq) %*% iota))))) *
                         as.numeric( ((iota %*% Jq) %*% M1inv) %*% Rt) )

  # we then compute the second part
  Rt1  =       t(J) %*% Diagonal(nn,Si) %*%W %*% Dinv %*% t(Wq) %*% Jq
  Rt1  = Rt1 - t(J) %*% W %*% Dinv %*% t(W) %*% Diagonal(nn,Si) %*% W %*% Dinv %*% t(Wq) %*% Jq

  tr21 = nnq^-1 * sum(diag( as.matrix(M1inv %*% Rt1)))
  tr22 = nnq^-2 * sum(  as.numeric( M1inv %*% ( t(J) %*% (W %*% (Dinv %*% (t(W) %*% (Diagonal(nn,Si) %*% (W %*% (Dinv %*% (t(Wq) %*% iota))))))))) *
                       as.numeric( iota %*% Jq))
  tr23 = nnq^-2 * sum(  as.numeric( M1inv %*% (t(J) %*% (Diagonal(nn,Si) %*% (W %*% (Dinv %*% (t(Wq) %*% iota)))))) *
                       as.numeric( iota %*% Jq) )
  tr = tr11 - tr12 + tr21 + tr22 - tr23

  if (FALSE) {
    homo_tr = nn^-1 * sum( diag(as.matrix( M1inv %*% (t(J) %*% W %*% Dinv %*% t(W) %*% J  )))) -
              nn^-2 * sum(  as.numeric(M1inv %*% (t(J) %*% W %*% Dinv %*% t(W) %*% iota)) * as.numeric( iota %*% J))
  }

  flog.info("[full tr-hetero]  cov: fe=%4.3f bc=%4.3f ",
            data[cs==1,cov(psi_hat,alpha_hat)],data[cs==1,cov(psi_hat,alpha_hat)] - tr)
  res$cov_fe = data[cs==1,cov(psi_hat,alpha_hat)]
  res$cov_bc = data[cs==1,cov(psi_hat,alpha_hat)] - tr

  # ------- VARIANCE OF X ------ #
  # we have to use W Dw^-1 W' + W Dw^-1 W' J M J' W Dw^-1 W'
  tr11 =  nnq^-1 * denseTraceProd( as.matrix(M1inv_dense %*% (t(J) %*% W %*% Dinv %*% t(Wq) %*% Wq %*% Dinv %*% t(W) %*% J)),
                                    as.matrix(M1inv_dense %*% Rt) )

  # second part of U
  tr12 = nnq^-2 * sum(  as.numeric( M1inv %*% (t(J) %*% (W %*% (Dinv %*% (t(Wq) %*% iota))))) *
                        as.numeric( Rt %*% M1inv %*% (t(J) %*% (W %*% (Dinv %*% (t(Wq) %*% iota)))))   )
  tr21 = nnq^-1 * sum( Matrix::diag( W %*% (Dinv %*% (t(W) %*% Diagonal(nn,Si) ) )))
  tr22 = nnq^-2 * sum( Si * as.numeric(W %*% (Dinv %*% (t(Wq) %*% iota))) ^2  )

  tr31 = nnq^-1 * sum( Matrix::diag( M1inv %*% ( t(J) %*% W %*% Dinv %*% t(W) %*%  Diagonal(nn,Si) %*% J) ))
  tr41 = nnq^-1 * sum( Matrix::diag( M1inv %*% ( t(J) %*% W %*% Dinv %*% t(W) %*%  Diagonal(nn,Si) %*% W %*% Dinv %*% t(W) %*% J) ))

  tr32 = nnq^-2 * sum(  as.numeric( M1inv %*% (t(J) %*% (W %*% (Dinv %*% (t(Wq) %*% iota))))) *
                        as.numeric( t(J)  %*% (Diagonal(nn,Si) %*% (W %*% (Dinv %*% (t(Wq) %*% iota))))) )
  tr42 = nnq^-2 * sum(  as.numeric( M1inv %*% (t(J) %*% (W %*% (Dinv %*% (t(Wq) %*% iota))))) *
                        as.numeric( t(J)  %*% (W %*% (Dinv %*% (t(W) %*% (Diagonal(nn,Si)  %*% (W %*% (Dinv %*% ( t(Wq) %*% iota)))))))) )

  tr = (tr11 - tr12) + (tr21 - tr22) - 2* (tr31 - tr32) + 2*(tr41 - tr42)
  flog.info("[full tr-hetero] alpha fe=%4.3f bc=%4.3f ",
            data[cs==1,var(alpha_hat)],data[cs==1,var(alpha_hat)] - tr)

  res$alpha_var_fe = data[cs==1,var(alpha_hat)]
  res$alpha_var_bc = data[cs==1,var(alpha_hat)] - tr

  return(res)
}

#' function which extracts all bias correction
#' in particular it gets corrected cov and var(alpha)
#'
#' @param var_e variance of the residual
#' @param adata panel data on movers and stayers
#' @export
m2.get_all_bc_hetero <- function(data) {

  res = list()

  # remove workers with only 1 spell
  wids = data[,.N,wid][N==1,wid]

  if (length(wids)>0) {
    data = data[!(wid %in% wids)]
    flog.info("[full tr-hetero] dropping %i wids with only 1 spell",length(wids))
  }

  data[, fid2 := .GRP, fid]
  data[, wid2 := .GRP, wid]
  data[, fid := fid2]
  data[, wid := wid2]

  # 1) construct J,W
  nn = data[,.N]
  nw = data[,max(wid)]
  nf = data[,max(fid)]
  J  <- sparseMatrix(1:nn, data$fid, x = 1,dims = c(nn,nf))
  J  <- J[,1:(nf-1)] # here we want to drop the last firm as a normalization
  W  <- sparseMatrix(1:nn, data$wid, x = 1,dims = c(nn,nw))
  Dw <- data[, .N, wid][order(wid),N]
  Df <- data[, .N, fid][order(fid),N]
  Dinv = Diagonal(nw,1/Dw)
  Y = data$y

  t = Matrix:::t

  # 2) construct sub-matrices
  M1  = t(J) %*% J - t(J) %*% W %*% Dinv %*% t(W) %*% J
  RHS = as.numeric(t(J) %*% (Diagonal(nn) - W %*% Dinv %*% t(W)) %*% Y)
  flog.info("[full tr-hetero] percentage of non zeros in M1 %f, getting inverse...",nnzero(M1)/cumprod(dim(M1))[-1])

  # 3) get alph and psi
  psi_hat    = as.numeric(Matrix::solve(M1, RHS))
  alpha_hat  = as.numeric(Dinv %*% t(W) %*% ( Y- J %*% psi_hat))
  # attach them
  data$psi_hat   = c(psi_hat,0)[data$fid]
  data$alpha_hat = alpha_hat[data$wid]

  # computing corrections
  # var(psy)
  # compute full inverse....
  M1inv = Matrix::solve(M1)
  flog.info("[full tr-hetero] done")

  flog.info("[full tr-hetero] prepare covariance and leverage matrix")
  # P = colSums( A .* (A'A)^-1 A' )
  # in the AKM case, for observation (it) it is M[i,i] + M[j,j] + 2 * M[i,j]
  M1inv_dense =  as.matrix(M1inv)
  Pi = sparseDiagCross( t(J) ,M1inv_dense , t(J), nn) -
    2*sparseDiagCross( t(J) , M1inv_dense , t(J) %*% W %*% Dinv %*% t(W), nn) +
    sparseDiagDot(t(W),1/Dw,t(W),nn) +
    sparseDiagCross( t(J) %*% W %*% Dinv %*% t(W) , M1inv_dense , t(J) %*% W %*% Dinv %*% t(W), nn)
  Si = Y * (Y - as.numeric(J %*% psi_hat + W %*% alpha_hat))/(1-Pi)

  flog.info("[full tr-hetero] leverage: min=%4.4f max=%4.4f",min(Pi),max(Pi))

  # ----- var(psi) correction ------ #
  # compute the right hand side of \mathb{M}
  Rt =    + t(J) %*% Diagonal(nn,Si) %*% J
  Rt = Rt - t(J) %*% W %*% Dinv %*% t(W) %*% Diagonal(nn,Si) %*% J
  Rt = Rt - t(J) %*% Diagonal(nn,Si) %*%W %*% Dinv %*% t(W) %*% J
  Rt = Rt + t(J) %*% W %*% Dinv %*% t(W) %*% Diagonal(nn,Si) %*% W %*% Dinv %*% t(W) %*% J
  iota  = rep(1,nn)

  #tr1 = nn^-1 * sum(diag( as.matrix(M1inv %*% Rt)))
  tr1 = nn^-1 * denseTraceProd( as.matrix(M1inv_dense %*% (t(J) %*% J )),
                                as.matrix(M1inv_dense %*% Rt) )
  tr2 = nn^-2 * sum( as.numeric( ((iota %*% J) %*% M1inv) %*% Rt) * as.numeric( ((iota %*% J) %*% M1inv)) )
  tr = tr1 - tr2

  flog.info("[full tr-hetero]  psi: fe=%4.3f bc=%4.3f ",
            data[,var(psi_hat)],data[,var(psi_hat)] - tr)
  res$psi_var_fe = data[,var(psi_hat)]
  res$psi_var_bc = data[,var(psi_hat)] -  tr

  # ----- cov(alpha,psi) correction ------ #
  # compute tr( Mcal %*% R )
  # first part of U
  # Mc1 = M1inv %*% (t(J) %*% W %*% Dinv %*% t(W) %*% J) %*% M1inv
  # tr11 = nn^-1 * sum(diag( as.matrix(Mc1 %*% Rt)))
  tr11 = - nn^-1 * denseTraceProd( as.matrix(M1inv_dense %*% (t(J) %*% W %*% Dinv %*% t(W) %*% J)),
                                   as.matrix(M1inv_dense %*% Rt) )

  # second part of U
  tr12 = - nn^-2 * sum(  as.numeric(M1inv %*% (t(J) %*% (W %*% (Dinv %*% (t(W) %*% iota))))) *
                           as.numeric( ((iota %*% J) %*% M1inv) %*% Rt) )

  # we then compute the second part
  Rt1  =       t(J) %*% Diagonal(nn,Si) %*%W %*% Dinv %*% t(W) %*% J
  Rt1  = Rt1 - t(J) %*% W %*% Dinv %*% t(W) %*% Diagonal(nn,Si) %*% W %*% Dinv %*% t(W) %*% J
  tr21 = nn^-1 * sum(diag( as.matrix(M1inv %*% Rt1)))
  tr22 = nn^-2 * sum(  as.numeric( M1inv %*% ( t(J) %*% (W %*% (Dinv %*% (t(W) %*% (Diagonal(nn,Si) %*% (W %*% (Dinv %*% (t(W) %*% iota))))))))) *
                         as.numeric( iota %*% J))
  tr23 = nn^-2 * sum(  as.numeric( M1inv %*% (t(J) %*% (Diagonal(nn,Si) %*% (W %*% (Dinv %*% (t(W) %*% iota)))))) *
                         as.numeric( iota %*% J) )
  tr = tr11 - tr12 + tr21 + tr22 - tr23

  if (FALSE) {
    homo_tr = nn^-1 * sum( diag(as.matrix( M1inv %*% (t(J) %*% W %*% Dinv %*% t(W) %*% J  )))) -
      nn^-2 * sum(  as.numeric(M1inv %*% (t(J) %*% W %*% Dinv %*% t(W) %*% iota)) * as.numeric( iota %*% J))
  }

  flog.info("[full tr-hetero]  cov: fe=%4.3f bc=%4.3f ",
            data[,cov(psi_hat,alpha_hat)],data[,cov(psi_hat,alpha_hat)] - tr)
  res$cov_fe = data[,cov(psi_hat,alpha_hat)]
  res$cov_bc = data[,cov(psi_hat,alpha_hat)] - tr

  # ------- VARIANCE OF X ------ #
  # we have to use W Dw^-1 W' + W Dw^-1 W' J M J' W Dw^-1 W'
  tr11 = -tr11

  # second part of U
  tr12 = nn^-2 * sum(  as.numeric( M1inv %*% (t(J) %*% (W %*% (Dinv %*% (t(W) %*% iota))))) *
                         as.numeric( Rt %*% M1inv %*% (t(J) %*% (W %*% (Dinv %*% (t(W) %*% iota)))))   )
  tr21 = nn^-1 * sum( Matrix::diag( W %*% (Dinv %*% (t(W) %*% Diagonal(nn,Si) ) )))
  tr22 = nn^-2 * sum( Si * as.numeric(W %*% (Dinv %*% (t(W) %*% iota))) ^2  )

  tr31 = nn^-1 * sum( Matrix::diag( M1inv %*% ( t(J) %*% W %*% Dinv %*% t(W) %*%  Diagonal(nn,Si) %*% J) ))
  tr41 = nn^-1 * sum( Matrix::diag( M1inv %*% ( t(J) %*% W %*% Dinv %*% t(W) %*%  Diagonal(nn,Si) %*% W %*% Dinv %*% t(W) %*% J) ))

  tr32 = nn^-2 * sum(  as.numeric( M1inv %*% (t(J) %*% (W %*% (Dinv %*% (t(W) %*% iota))))) *
                         as.numeric( t(J)  %*% (Diagonal(nn,Si) %*% (W %*% (Dinv %*% (t(W) %*% iota))))) )
  tr42 = nn^-2 * sum(  as.numeric( M1inv %*% (t(J) %*% (W %*% (Dinv %*% (t(W) %*% iota))))) *
                         as.numeric( t(J)  %*% (W %*% (Dinv %*% (t(W) %*% (Diagonal(nn,Si)  %*% (W %*% (Dinv %*% ( t(W) %*% iota)))))))) )

  tr = (tr11 - tr12) + (tr21 - tr22) - 2* (tr31 - tr32) + 2*(tr41 - tr42)
  flog.info("[full tr-hetero] alpha fe=%4.3f bc=%4.3f ",
            data[,var(alpha_hat)],data[,var(alpha_hat)] - tr)

  res$alpha_var_fe = data[,var(alpha_hat)]
  res$alpha_var_bc = data[,var(alpha_hat)] - tr

  return(res)
}

#' Overview of a data set
#' @export
m2.getstats <- function(adata) {

  nm = adata[,list(N=length(unique(fid))),list(wid)][N>=2,.N]
  nw = adata[,length(unique(wid))]

  flog.info('nobs=%i firms:%i workers:%i movers=%i (%4.4f)',
            adata[,.N],
            adata[,length(unique(fid))],
            nw,
            nm,nm/nw)

  # are there spell of length >= 2
  count = adata[, .N,list(wid,fid)][N>1,.N]
  flog.info("%i spells of length >=2",count)

  # workers with more than 2 employers
  count = adata[,list(N=length(unique(fid))),list(wid)][N>2,.N]
  flog.info("%i workers with more than 2 firms",count)

  # checking observations to include in
}

#' compute the posterior variance for an estimated CRE model
#'
#' @export
m2.cre_posterior_var <- function(sim,res) {

  flog.info("[m2.cre_posterior_var] Begin posterior variance estimation for CRE")

  # index firms with integers
  jdata = sim$jdata
  sdata = sim$sdata
  f1s = jdata[,unique(c(f1,f2))]
  fids = data.table(f1=f1s,nfid=1:length(f1s))
  setkey(fids,f1)
  setkey(jdata,f1)
  jdata[,f1i := fids[jdata,nfid]]
  setkey(sdata,f1)
  sdata[,f1i := fids[sim$sdata,nfid]]
  setkey(jdata,f2)
  jdata[,f2i := fids[jdata,nfid]]

  sdata <- sdata[!is.na(f1i)] # this is in case there are firms with stayers but no movers

  # extract size in stayers, and mean
  fsize = rbind(sim$sdata[,list(y1,f1)],sim$jdata[,list(y1,f1)])[,list(N=.N,mw=mean(y1)),f1]
  setkey(fids,f1)
  setkey(fsize,f1)
  fids[,size := fsize[fids,N]]
  fids[,mw   := fsize[fids,mw]]
  fids[is.na(size),size:=0] # pads missing size with 0
  fids[size==0, mw:=0] # pads missing size with 0

  # CONSTRUCT SPARSE DESIGN MATRIX - no need for normalization here
  nf = pmax(max(jdata$f1i),max(jdata$f2i))
  dd = jdata[,list(m1 = y2 - y1,y1=y1,f1i,f2i,j1,j2)]
  N = nrow(dd)
  dd[,v1:=-1][,v2:=1]
  dd[,c1:=f1i][,c2:=f2i]
  dd[N,v1:=0]
  JJ  = sparseMatrix(1:N,dd$c1,x=dd$v1,dims=c(N,nf)) + sparseMatrix(1:N,dd$c2,x=dd$v2,dims = c(N,nf))
  S   = fids[order(nfid),size]

  t = Matrix:::t

  # CONSTRUCT WEITHING MATRICES
  error_sd = res$cre$params$eps_sd
  if (any(is.na(error_sd))) flog.warn("some epsilon sd are NAs")
  error_sd[is.na(error_sd)] = 0.001
  if (any(error_sd < .001)) flog.warn("some epsilon sd are <0.001, setting them to 0.001")
  error_sd[error_sd < .001] = 0.001

  findex = unique(rbind( jdata[,list(f1i,j1)],jdata[,list(f1i=f2i,j1=j2)]))[order(f1i)]
  Or = res$cre$params$psi_sd[findex$j1]^2  # this number of firms, correspong psi_sd
  Zr = error_sd[dd$j1]^2 + error_sd[dd$j2]^2 # this number move, correspong eps_sd
  Mu = as.numeric(res$A1[findex$j1])

  # Construct matrix to inverse
  flog.info("[m2.cre_posterior_var] Begin inverting matrix")
  M1 = Diagonal(nf) +  t(JJ) %*% Diagonal(N,1/Zr) %*% JJ %*% Diagonal(nf,Or)
  M1inv = Matrix::solve(M1)
  flog.info("[m2.cre_posterior_var] Done inverting matrix")

  # Construct objects for Q matrix
  ddq = data.table(rbind( jdata[,list(f1i,j1,y1,m=1)], sdata[,list(f1i,j1,y1,m=0)]))
  Nq  = nrow(ddq)
  JJq = sparseMatrix(1:Nq,ddq$f1i,x=rep(1,Nq),dims=c(Nq,nf))

  # Finally construct the trace term
  iota_q = rep(1,Nq)
  tr1 = Nq^-1 * sum(Matrix::diag(  M1inv  %*%  (t(JJq) %*% JJq %*%  Diagonal(nf,Or))  ))
  tr2 = Nq^-2 * sum( as.numeric( Diagonal(nf,Or) %*% (M1inv %*% (t(JJq) %*% iota_q) )) *
                       as.numeric(iota_q %*% JJq))
  tr = tr1 - tr2

  # And we construct the main term
  V = Mu + as.numeric( Diagonal(nf,Or) %*% t(JJ) %*% Diagonal(N,1/Zr) %*% dd$m1)
  V = as.numeric( JJq %*% (t(M1inv) %*% V))

  res = list()

  res$posterior_var = tr + var(V)
  flog.info("[m2.cre_posterior_var] Done posterior variance estimation for CRE")

  # we construct the posterior cov(y,psi)
  ddq$psi_tmp = V
  res$posterior_cov_ypsi_all     = ddq[,cov(psi_tmp,y1)]
  res$posterior_cov_ypsi_movers  = ddq[m==1,cov(psi_tmp,y1)]
  res$posterior_cov_ypsi_stayers = ddq[m==0,cov(psi_tmp,y1)]
  res$posterior_movers_share = ddq[,mean(m==1)]

  return(res)
}

#' compute the posterior variance for an estimated CRE model
#'
#' @export
m2.cre_posterior_full <- function(sim,res) {

  flog.info("[m2.cre_posterior_full] Begin posterior construction  for CRE")
  pdata = mt.from.m2(sim,make_each_row_one_wid = TRUE,attach_clusters = TRUE)

  flog.info("[m2.cre_posterior_full] input matrices")
  pdata[, fid2 := .GRP, fid]
  pdata[, wid2 := .GRP, wid]
  pdata[, fid := fid2]
  pdata[, wid := wid2]

  flog.info("[m2.cre_posterior_full] append model information")
  error_sd = res$cre$params$eps_sd
  if (any(is.na(error_sd))) flog.warn("some epsilon sd are NAs")
  error_sd[is.na(error_sd)] = 0.001
  if (any(error_sd < .001)) flog.warn("some epsilon sd are <0.001, setting them to 0.001")
  error_sd[error_sd < .001] = 0.001

  # attach variance of epsilon
  pdata$eps_sd = 0
  pdata[m==0, eps_sd := error_sd[j1], j1]
  pdata[(m==1) & (t==1), eps_sd := error_sd[j1], j1]
  pdata[(m==1) & (t==2), eps_sd := error_sd[j2], j2]

  # attach mean of psi
  pdata[, psi_m := as.numeric(res$A1)[j], j]

  # attach variance of psi within
  pdata[, psi_sd := res$cre$params$psi_sd[j], j]

  # attach variance of alpha
  pdata$alpha_sd = 0
  pdata[m==0, alpha_sd := res$cre$params$Esd[j], j]
  pdata[m==1, alpha_sd := res$cre$params$EEsd[j1,j2], list(j1,j2)]

  # attach mean of alpha
  pdata$alpha_m = 0
  pdata[m==0, alpha_m := res$Em[j], j]
  pdata[m==1, alpha_m := res$EEm[j1,j2], list(j1,j2)]

  # 1) construct J,W
  nn = pdata[,.N]
  nw = pdata[,max(wid)]
  nf = pdata[,max(fid)]
  J  <- sparseMatrix(1:nn, pdata$fid, x = 1,dims = c(nn,nf))
  J  <- J[,1:(nf-1)] # here we want to drop the last firm as a normalization
  W  <- sparseMatrix(1:nn, pdata$wid, x = 1,dims = c(nn,nw))
  Dw <- pdata[, .N, wid][order(wid),N]
  Df <- pdata[, .N, fid][order(fid),N]
  Dwinv = Diagonal(nw,1/Dw)
  Y = pdata$y

  # 0) create the Jq,Wq matrices
  ddq = pdata[t==1]
  nnq = ddq[,.N]
  Jq  <- sparseMatrix(1:nnq, ddq$fid, x = 1,dims = c(nnq,nf))
  Jq  <- Jq[,1:(nf-1)] # here we want to drop the last firm as a normalization
  Wq  <- sparseMatrix(1:nnq, ddq$wid, x = 1,dims = c(nnq,nw))

  # 0) creating weighting matrices
  findex = unique(pdata[fid<nf,list(fid,j,psi_sd,psi_m)])[order(fid)]
  Muf    = findex$psi_m        # this number of firms,   correspong cluster mean firm effect
  Omf    = findex$psi_sd^2 + 1.e-10    # this number of firms,   correspong psi_sd

  windex = unique(pdata[t==1,list(wid,j,alpha_sd,alpha_m)])[order(wid)]
  Muw = windex$alpha_m         # this number of workers, correspong mean worker effect
  Omw = windex$alpha_sd^2  + 1.e-10    # this number of workers, correspong alpha_sd

  # creating tilde matrices
  Zr = pdata$eps_sd^2          # this is the size of the data

  Jt = J %*% Diagonal(nf-1,sqrt(Omf))
  Wt = W %*% Diagonal(nw,sqrt(Omw))

  t = Matrix:::t
  Dtw    = 1 + Matrix::diag( t(Wt) %*% Diagonal(nn,1/Zr) %*% Wt)
  Dtwinv = Diagonal(nw, 1/Dtw)

  # Construct matrix to inverse
  flog.info("[m2.cre_posterior_full] Begin inverting matrix")
  M1 = Diagonal(nf-1) +  t(Jt) %*% Diagonal(nn,1/Zr) %*% Jt
  M1 = M1 - t(Jt) %*% Diagonal(nn,1/Zr) %*% Wt %*% Dtwinv %*% t(Wt) %*%  Diagonal(nn,1/Zr) %*% Jt
  M1inv = Matrix::solve(M1)
  flog.info("[m2.cre_posterior_full] Done inverting matrix")

  # And we construct the main term
  Vf = Muf*sqrt(Omf)^-1 + as.numeric( t(Jt) %*% (Diagonal(nn,1/Zr) %*% Y))
  Vf = sqrt(Omf) * as.numeric( M1inv %*% Vf)

  Vw = Muw*sqrt(Omw)^-1 + as.numeric( t(Wt) %*% Diagonal(nn,1/Zr) %*% Y)
  Vw = sqrt(Omf) * as.numeric( M1inv %*% (t(Jt) %*% (Diagonal(nn,1/Zr) %*% (Wt %*% (Dtwinv %*% Vw )))))

  V = as.numeric(Jq %*% (Vf - Vw))

  # we construct the posterior cov(y,psi)
  ddq$psi_tmp = V
  res$posterior_cov_ypsi_all     = ddq[,cov(psi_tmp,y)]
  res$posterior_cov_ypsi_movers  = ddq[m==1,cov(psi_tmp,y)]
  res$posterior_cov_ypsi_stayers = ddq[m==0,cov(psi_tmp,y)]
  res$posterior_movers_share = ddq[,mean(m==1)]
  flog.info("[m2.cre_posterior_full] posterior cov_y_psi = %4.4f",res$posterior_cov_ypsi_all)

  # Finally compute the posterior variance in the cross-section
  iota_q = rep(1,nnq)
  # tr1 = Nq^-1 * sum(Matrix::diag(  M1inv  %*%  (t(JJq) %*% JJq %*%  Diagonal(nf,Or))  ))
  # tr2 = Nq^-2 * sum( as.numeric( Diagonal(nf,Or) %*% (M1inv %*% (t(JJq) %*% iota_q) )) *
  #                      as.numeric(iota_q %*% JJq))
  # tr = tr1 - tr2

  return(res)
}


#' compute the posterior variance for an estimated CRE model
#'
#' @export
m2.woodcock_posterior_var <- function(sim) {

  jdata = sim$jdata
  m2.trace.reExt.all(sim$sdata,sim$jdata,subsample=0,subsample.firms=0,include_covY1_terms=FALSE)


  # we start by computing the variance of sigma

  # moers coming to the firm
  firm_in_grp = jdata[,.N,f1][N>=2]$f1
  flog.info("[WC] processing %i firms",length(firm_in_grp))
  if (length(firm_in_grp)>1) {
    count = 0
    last_time = as.numeric(Sys.time())
    for (firm_cur in firm_in_grp) {
      count = count+1
      dt = jdata[f1==firm_cur,list(f2,f1,y1,y2,psi1,psi2)]

      # create all pairs, make sure they move to different firms
      F2 = spread(dt$f2,2,nrow(dt))
      YY = spread(dt$y2 - dt$psi2 - dt$y1+dt$psi1,2,nrow(dt))

      # add this to the measure of variance!
      num = num + sum((F2 != t(F2)) * YY * t(YY))/2
      den = den + sum((F2 != t(F2)))/2
      if (as.numeric(Sys.time()) >= last_time + 10) {
        flog.info("[CRE] done with %i firms out of %i for j1=%i",count,length(firm_in_grp),l1)
        last_time = as.numeric(Sys.time())
      }
      rm(dt)
    }
  }
  firm_in_grp = jdata[l1==j2,.N,f2][N>=2]
  flog.info("[CRE] found %i firms for j1=%i",nrow(firm_in_grp),l1)
  if ((subsample.firms>0) & (nrow(firm_in_grp)>1)) firm_in_grp = firm_in_grp[sample.int(.N,pmin(.N,subsample.firms),prob=firm_in_grp$N)];
  firm_in_grp = firm_in_grp$f2
  flog.info("[CRE] processing %i firms for j1=%i",length(firm_in_grp),l1)
  if (length(firm_in_grp)>1) {
    count = 0
    last_time = as.numeric(Sys.time())
    for (firm_cur in firm_in_grp) {
      count = count+1
      dt = jdata[f2==firm_cur]
      if (subsample>0) dt = dt[sample.int(.N,pmin(.N,subsample))];

      # create all pairs, make sure they move to different firms
      F1 = spread(dt$f1,2,nrow(dt))
      YY = spread(dt$y2 - dt$psi2 - dt$y1+dt$psi1,2,nrow(dt))

      # add this to the measure of variance!
      num = num + sum((F1 != t(F1)) * YY * t(YY))/2
      den = den + sum((F1 != t(F1)))/2
      rm(dt)
      if (as.numeric(Sys.time()) >= last_time + 10) {
        flog.info("[CRE] done with %i firms out of %i for j2=%i",count,length(firm_in_grp),l1)
        last_time = as.numeric(Sys.time())
      }
    }
  }
  if (den>0) cov_dYm_dYmc[l1] = num/den;



  # index firms with integers
  jdata = sim$jdata
  sdata = sim$sdata
  f1s = jdata[,unique(c(f1,f2))]
  fids = data.table(f1=f1s,nfid=1:length(f1s))
  setkey(fids,f1)
  setkey(jdata,f1)
  jdata[,f1i := fids[jdata,nfid]]
  setkey(sdata,f1)
  sdata[,f1i := fids[sim$sdata,nfid]]
  setkey(jdata,f2)
  jdata[,f2i := fids[jdata,nfid]]

  # extract size in stayers, and mean
  fsize = rbind(sim$sdata[,list(y1,f1)],sim$jdata[,list(y1,f1)])[,list(N=.N,mw=mean(y1)),f1]
  setkey(fids,f1)
  setkey(fsize,f1)
  fids[,size := fsize[fids,N]]
  fids[,mw   := fsize[fids,mw]]
  fids[is.na(size),size:=0] # pads missing size with 0
  fids[size==0, mw:=0] # pads missing size with 0

  # CONSTRUCT SPARSE DESIGN MATRIX - no need for normalization here
  nf = pmax(max(jdata$f1i),max(jdata$f2i))
  dd = jdata[,list(m1 = y2 - y1,f1i,f2i,j1,j2)]
  N = nrow(dd)
  dd[,v1:=-1][,v2:=1]
  dd[,c1:=f1i][,c2:=f2i]
  dd[N,v1:=0]
  JJ = sparseMatrix(1:N,dd$c1,x=dd$v1,dims=c(N,nf)) + sparseMatrix(1:N,dd$c2,x=dd$v2,dims = c(N,nf))
  S  = fids[order(nfid),size]

  t = Matrix:::t

  # CONSTRUCT WEITHING MATRICES
  findex = unique(rbind( jdata[,list(f1i,j1)],jdata[,list(f1i=f2i,j1=j2)]))[order(f1i)]
  Or = res$cre$params$psi_sd[findex$j1]^2  # this number of firms, correspong psi_sd
  Zr = res$cre$params$eps_sd[dd$j1]^2 + res$cre$params$eps_sd[dd$j2]^2 # this number move, correspong eps_sd
  Mu = as.numeric(res$A1[findex$j1])

  # Construct matrix to inverse
  M1 = Diagonal(nf) +  t(JJ) %*% Diagonal(N,1/Zr) %*% JJ %*% Diagonal(nf,Or)
  M1inv = Matrix::solve(M1)

  # Construct objects for Q matrix
  ddq = rbind( jdata[,list(f1i,j1)], sdata[,list(f1i,j1)])
  Nq  = nrow(ddq)
  JJq = sparseMatrix(1:Nq,ddq$f1i,x=rep(1,Nq),dims=c(Nq,nf))

  # Finally construct the trace term
  iota_q = rep(1,Nq)
  tr1 = Nq^-1 * sum(Matrix::diag(  M1inv  %*%  (t(JJq) %*% JJq %*%  Diagonal(nf,Or))  ))
  tr2 = Nq^-2 * sum( as.numeric( Diagonal(nf,Or) %*% (M1inv %*% (t(JJq) %*% iota_q) )) *
                       as.numeric(iota_q %*% JJq))
  tr = tr1 - tr2

  # And we construct the main term
  V = Mu + as.numeric( Diagonal(nf,Or) %*% t(JJ) %*% Diagonal(N,1/Zr) %*% dd$m1)
  V = as.numeric( JJq %*% (M1inv %*% V))

  posterior_var = tr + var(V)
  return(posterior_var)
}

# ------- SIMULATING DATA ---------

#' create a model for testing trace estimation
#' @export
m2.trace.new <- function(nf = 200 , nm = 10000, eps_sd = 1.5) {
  p = list(dsize = c(exp(3.199292), 1+exp(-1.247662)) , nf = nf , nm = nm, eps_sd = eps_sd  )
  S = rpareto(p$nf,p$dsize[1],p$dsize[2])
  psi = rnorm(p$nf)

  model     = copy(p)
  model$S   = S
  model$psi = psi

  return(model)
}

#' simulates for trace estimation
#' @export
m2.trace.simulate.old <- function(model) {
  JJ = array(0,c(model$nm,model$nf))
  AA = array(0,c(model$nf,model$nf))
  F1 = rep(0,model$nm)
  F2 = rep(0,model$nm)
  for (i in 1:model$nm) {
    ii = sample.int(model$nf,2,prob = model$S)
    JJ[i,ii[1]] = 1
    JJ[i,ii[2]] = -1
    AA[ii[1],ii[2]]=1
    AA[ii[2],ii[1]]=1
    F1[i] = ii[1]
    F2[i] = ii[2]
  }
  colnames(AA) = 1:model$nf
  rownames(AA) = 1:model$nf

  D = JJ %*% model$psi + rnorm(model$nm)*model$eps_sd

  return(list(JJ=JJ,AA=AA,D=D,F1=F1,F2=F2))
}

#' simulates for trace estimation
#' @export
m2.trace.simulate <- function(model) {
  F1   = rep(0,model$nm)
  F2   = rep(0,model$nm)
  psi1 = rep(0,model$nm)
  psi2 = rep(0,model$nm)
  for (i in 1:model$nm) {
    ii = sample.int(model$nf,2,prob = model$S)
    F1[i] = ii[1]
    F2[i] = ii[2]
    psi1[i] = model$psi[ii[1]]
    psi2[i] = model$psi[ii[2]]
  }

  jdata = data.table(f1=paste(F1),f2=paste(F2),psi1=psi1,psi2=psi2)
  jdata[, y1 := psi1 + rnorm(.N)*model$eps_sd/sqrt(2)]
  jdata[, y2 := psi2 + rnorm(.N)*model$eps_sd/sqrt(2)]

  sdata = data.table(f1=paste(1:model$nf),psi1=model$psi,size=model$S)
  sdata = sdata[,list(y1 = psi1 + rnorm(ceiling(size))*model$eps_sd/sqrt(2),
                      y2 = psi1 + rnorm(ceiling(size))*model$eps_sd/sqrt(2),f2=f1), list(f1,size,psi1) ]
  sdata$x=1

  return(list(sdata=sdata,jdata=jdata))
}

#' simulates a panel
#' @export
m2.trace.simulate.panel <- function(
  nt = 5,
  ni = 100000,
  firm_size = 10,
  w_sigma = 0.8,
  hetero_g = 0,
  csig=0.5,
  lambda=0.05) {

  nk = 30
  nl = 10

  alpha_sd = 1
  psi_sd   = 1

  # let's draw some FE
  psi   = qnorm(1:nk/(nk+1)) * alpha_sd
  alpha = qnorm(1:nl/(nl+1)) * psi_sd

  csort = 0.5 # sorting effect
  cnetw = 0.2 # network effect

  # lets create type specific transition matrices
  # we are going to use joint normal centered on different values
  G = array(0,c(nl,nk,nk))
  for (l in 1:nl) for (k in 1:nk) {
    G[l,k,] = dnorm(csort*(psi[k] - alpha[l])) * dnorm(cnetw*(psi[k] - psi))
    G[l,k,] = dnorm( psi - cnetw *psi[k] - csort * alpha[l],sd = csig )
    G[l,k,] = G[l,k,]/sum(G[l,k,])
  }

  # we then solve for the stationary distribution over psis for each alpha value
  H = array(1/nk,c(nl,nk))
  for (l in 1:nl) {
    M = G[l,,]
    for (i in 1:100) {
      H[l,] = t(G[l,,]) %*% H[l,]
    }
  }


  # we simulate a panel
  network    = array(0,c(ni,nt))
  spellcount = array(0,c(ni,nt))
  A = rep(0,ni)

  for (i in 1:ni) {
    # we draw the worker type
    l = sample.int(nl,1)
    A[i]=l
    # at time 1, we draw from H
    network[i,1] = sample.int(nk,1,prob = H[l,])
    for (t in 2:nt) {
      if (runif(1)<lambda) {
        network[i,t] = sample.int(nk,1,prob = G[l,network[i,t-1],])
        spellcount[i,t] = spellcount[i,t-1] +1
      } else {
        network[i,t]    = network[i,t-1]
        spellcount[i,t] = spellcount[i,t-1]
      }
    }
  }

  data  = data.table(melt(network,c('i','t')))
  data2 = data.table(melt(spellcount,c('i','t')))
  setnames(data,"value","k")
  data <- data[,spell := data2$value]
  data <- data[,l := A[i],i]
  data <- data[,alpha := alpha[l],l]
  data <- data[,psi := psi[k],k]

  within_firm_count = ni/(firm_size*nk*nt)

  dspell <- data[,list(len=.N),list(i,spell,k)]
  dspell <- dspell[,fid := sample( 1: pmax(1,sum(len)/(firm_size*nt) ) ,.N,replace=TRUE) , k]
  dspell <- dspell[,fid := .GRP, list(k,fid)]

  setkey(data,i,spell)
  setkey(dspell,i,spell)

  data <- data[, fid:= dspell[data,fid]]
  data <- data[, y := alpha + psi + w_sigma * rnorm(.N) * exp( hetero_g * psi ) ]
  data[,wid:=i]

  return(data)
}

#' simulates a panel
#' @export
m2.trace.simulate.fromnt <-function(mc.opts) {

  # monte-carlo comparing calculation in difference versus
  # computation in levels
  # create sdata/jdata
  data = m2.trace.simulate.panel(nt = 2,ni = mc.opts$ni,firm_size = mc.opts$firm_size,w_sigma=0.8,hetero_g = 0,lambda = mc.opts$lambda)
  # data = m2.trace.simulate.panel(nt = 5,ni = 20000,firm_size = 20,w_sigma=0.8)
  # save.dta13(data,file = "tmp-simdata.dta")

  # extract leave-out set
  setkey(data,i,t)
  data[, fid.lag := data[J(i,t-1),fid]]
  jdata = data[fid!=fid.lag,list(f1=fid.lag,f2=fid)]
  # jdata = get.largest.leaveoutset.fid(jdata)
  jdata = get.largest.leaveoutset.fid2(jdata)
  fids = jdata[,unique(c(f1,f2))]
  data = data[fid %in% fids]

  # attach number of movers
  setkey(data,i,t)
  data[, fid.lag := data[J(i,t-1),fid]]
  jdata = data[fid!=fid.lag,list(f1=fid.lag,f2=fid)]
  mcount = rbind(jdata[,list(f1,f2)],jdata[,list(f1=f2,f2=f1)])[,.N,f1]
  setkey(mcount,f1)
  setkey(data,fid)
  data[,mc := mcount[data,N]]

  # constuct sim data
  movers = data[,length(unique(fid)),wid][V1>1,wid]
  setkey(data,wid,t)
  jdata = data[wid %in% movers,     list(f1 = fid[1],f2=fid[2],y1=y[1],y2=y[2],alpha=alpha[1],psi1=psi[1],psi2=psi[2],mc1=mc[1],mc2=mc[2],year=t[1]),wid]
  sdata = data[! (wid %in% movers), list(f1 = fid[1],f2=fid[2],y1=y[1],y2=y[2],alpha=alpha[1],psi1=psi[1],psi2=psi[2],mc1=mc[1],mc2=mc[2],year=t[1]),wid]
  sim = list(sdata=sdata[!is.na(y1*y2)],jdata=jdata)

  sim$sdata[,psi1_bu := psi1]
  sim$sdata[,psi2_bu := psi1]
  sim$jdata[,psi1_bu := psi1]
  sim$jdata[,psi2_bu := psi2]
  sim$sdata[,f1 := paste(f1)]
  sim$sdata[,f2 := paste(f2)]
  sim$jdata[,f1 := paste(f1)]
  sim$jdata[,f2 := paste(f2)]

  return(sim)
}


# -------- ESTIMATORS ----------

# we should have the following estimators:
# 1) AKM (requires computing the connected set), also computes andrews
# 2) AKM with Kline (requires computing leave-one-out connected set), computes trace correction
# 3) BLM (with the within RE)
# the first estimators should also work at the class level if needed

#' estimates interacted model
#'
#' @param method default is "ns" for non-stationary, "fixb" is for
#' stationary, "prof" is for profiling out B in period 2 first and "linear"
#' to run an AKM type estimation
#' @family step2 interacted
#' @export
m2.cre.estimate <- function(sim,opts=list()) {

  # extract inputs
  ng = sim$sdata[,max(j1)]
  jdata = sim$jdata
  sdata = sim$sdata

  # estimating the group effect
  nd = jdata[,.N]
  Yd = jdata[,y2 - y1]
  Jd = sparseMatrix(1:nd, jdata$j2, x = 1,dims = c(nd,ng))  - sparseMatrix(1:nd, jdata$j1, x = 1,dims = c(nd,ng))
  Jd = Jd[,1:(ng-1)] # here we want to drop the last firm as a normalization
  Yd = jdata[,y2 - y1]

  Md    = Matrix::t(Jd) %*% Jd
  A     = as.numeric(Matrix::solve(Md, Matrix::t(Jd) %*% Yd))
  A1    = c(A,0)

  # get mean of alpha
  EEm = jdata[, mean(y1)-A1[j1] + mean(y2)- A1[j2] ,list(j1,j2)]
  EEm = 0.5*mcast(EEm,"V1","j1","j2",c(ng,ng),0) # what do you think of this 0.5 right here?
  Em  = sdata[, mean(y1)- A1[j1],j1][order(j1),V1]

  # ----------- CRE MOMENTS ---------- #
  sdata = sdata[,psi1 := A1[j1],j1][,psi2 := A1[j2],j2][,mx := Em[j1],list(j1)]
  jdata = jdata[,psi1 := A1[j1],j1][,psi2 := A1[j2],j2][,mx := EEm[j1,j2],list(j1,j2)]
  res = m2.trace.allcovs.bis(sdata,jdata)

  # ------- COMPUTING VARIANCE DECOMPOSITION

  # ------ MODEL PARAMETERS -------- #
  pm = jdata[,.N]/(sdata[,.N] + jdata[,.N])
  ps = sdata[,.N]/(sdata[,.N] + jdata[,.N])

  cdata = data.table(rbind(sdata[,list(psi1,mx)],jdata[,list(psi1,mx)]))
  vdec = list()
  vdec$var_psi       = cdata[,var(psi1)]    + res$params$var_psi
  vdec$cov_alpha_psi = cdata[,cov(psi1,mx)] + ps*res$params$cov_AsPsi1 +pm*res$params$cov_Am1Psi1

  vdec0 = list()
  vdec0$var_psi       = cdata[,var(psi1)]
  vdec0$cov_alpha_psi = cdata[,cov(psi1,mx)]

  res$vdec       = vdec
  res$vdec0      = vdec0
  res$params$A   = A1
  res$params$EEm = EEm
  res$params$Em  = Em

  return(res)

  # TODO: add computation of posterior variance here.
  if (posterior_var==TRUE) {
    post = m2.cre_posterior_var(list(jdata=jdata,sdata=sdata),model)
    model$cre$posterior_var              = post$posterior_var
    model$cre$posterior_cov_ypsi_all     = post$posterior_cov_ypsi_all
    model$cre$posterior_cov_ypsi_movers  = post$posterior_cov_ypsi_movers
    model$cre$posterior_cov_ypsi_stayers = post$posterior_cov_ypsi_stayers
    model$cre$posterior_movers_share     = post$posterior_movers_share
  }

  return(model)
}


#' Estimates a two-way fixed effect model using (f1,f2) as heterogeneity
#' identifiers
#'
#' @param sim input data with movers and stayers
#' @param hetero if TRUE use leave-one-out connected set and compute TRACE correction allowing for heterogeneity
#' @param model0 give a model, this is for development
#' @param var_psi_only if TRUE (default) then only the variance of firm effects is corrected
#' @export
m2.trace.estimate2 <- function(sim, model0=NA,hetero=FALSE,approx=0, check_set=TRUE,var_psi_only=TRUE) {

  stats = list()
  stats$hetero = hetero
  stats$total_number_of_firms   = length(unique(c(sim$jdata[,unique(c(f1,f2))],sim$sdata[,unique(f1)])))
  stats$total_number_of_stayers = sim$sdata[,.N]
  stats$total_number_of_movers  = sim$jdata[,.N]
  stats$total_logwage_var       = var(c(sim$sdata$y1,sim$jdata$y1))
  stats$total_btw_firm          = sim$sdata[,list(mean(y1),.N),f1][,wt.var(V1,N)]

  # EXTRACT CONNECTED SET
  jdata = sim$jdata
  flog.info("[twfe-firm] number of firms in movers %i (t1=%i,t2=%i) and stayers %i",sim$jdata[,length(unique(c(f1,f2)))],sim$jdata[,length(unique(f1))],sim$jdata[,length(unique(f2))],sim$sdata[,length(unique(f1))])
  if (hetero==F) {
    if(check_set){
      f1s   = get.largest.conset.fid(jdata)
      flog.info("[twfe-firm] connected set %i/%i",length(f1s),stats$total_number_of_firms)
    } else {
      f1s = jdata[,unique(c(f1,f2))]
    }
    jdata = jdata[f1 %in% f1s][f2 %in% f1s]
  } else {
    if(check_set){
      jdata = get.largest.leaveoutset.fid(jdata)
    }
    f1s = jdata[,unique(c(f1,f2))]
    flog.info("[twfe-firm] leave-out connected set %i/%i",length(f1s),stats$total_number_of_firms)
  }

  # index firms with integers
  fids = data.table(f1=f1s,nfid=1:length(f1s))
  setkey(fids,f1)
  setkey(jdata,f1)
  jdata[,f1i := fids[jdata,nfid]]
  setkey(jdata,f2)
  jdata[,f2i := fids[jdata,nfid]]
  setkey(sim$sdata,f1)
  sim$sdata[,f1i := fids[sim$sdata,nfid]]

  # extract size in stayers, and mean
  fsize = rbind(sim$sdata[,list(y1,f1)],sim$jdata[,list(y1,f1)])[,list(N=.N,mw=mean(y1)),f1]
  setkey(fids,f1)
  setkey(fsize,f1)
  fids[,size := fsize[fids,N]]
  fids[,mw   := fsize[fids,mw]]
  fids[is.na(size),size:=0] # pads missing size with 0
  fids[size==0, mw:=0] # pads missing size with 0

  # CONSTRUCT SPARSE DESIGN MATRIX + NORMALIZATION
  nf = pmax(max(jdata$f1i),max(jdata$f2i))
  dd = jdata[,list(m1 = y2 - y1,y1,f1i,f2i)]
  dd = rbind(dd,data.frame(m1=0,y1=0,f1i=1,f2i=1))
  N = nrow(dd)
  dd[,v1:=-1][,v2:=1]
  dd[,c1:=f1i][,c2:=f2i]
  dd[N,v1:=0]
  # fixme, rewrite this with regular sparse matrices
  JJ = sparseMatrix2(1:N,dd$c1,dd$v1,c(N,nf)) + sparseMatrix2(1:N,dd$c2,dd$v2,c(N,nf))
  Jq = sparseMatrix2(1:N,dd$c1,1,c(N,nf))
  S  = fids[order(nfid),size]

  stats$set_number_of_firms   = nf
  stats$set_number_of_stayers = sum(S)-(N-1)
  stats$set_number_of_movers  = N-1
  stats$set_logwage_var       = var(c(sim$sdata[f1 %in% fids$f1,y1],sim$jdata$y1))
  stats$set_btw_firm        = sim$sdata[f1 %in% fids$f1,list(mean(y1),.N),f1][,wt.var(V1,N)]

  # COMPUTE INVERSE OF DESIGN MATRIX
  M = SparseM::t(JJ) %*% JJ
  Minv = SparseM::solve(M,nnzlmax=1e8,tmpmax=1e8,nsubmax=1e8)

  # compute firms FE
  psi = as.numeric(SparseM::as.matrix(Minv %*% ( SparseM::t(JJ) %*% dd$m1)))
  E   = dd$m1 - JJ%*%psi
  E   = E[1:(N-1)]

  if (!any(is.na(model0))) {
    psi0 = model0$psi[as.integer(fids[order(nfid),f1])]
    psi0 = psi0-psi0[1]
    flog.info("[twfe-firm] corr= %f", cor(psi0,psi))
  }

  # extract homoskedastic error
  var_e = var(E)*nrow(sim$jdata)/(nrow(sim$jdata)-nf)
  stats$error_var_homo = var_e
  flog.info("[twfe-firm] var_e= %f", var_e)

  # COMPUTE THE TRACE FORMULA - USING THE WEIGHTING OF STAYERS
  tr_correction = var_e*( sum( SparseM::diag(Minv)*S )/sum(S) - sum( S* (Minv %*% rep(1/nf,nf)) )/(sum(S))  )
  stats$trace_term_homo_Q = tr_correction
  stats$trace_term_hetero = NA
  stats$error_var_hetero = NA

  # heteroskedastic variance using BLM groups
  if (hetero==TRUE) {

    S2 = S/sum(S)
    Ds = sparseMatrix2(1:length(S2),1:length(S2),S2,c(length(S2),length(S2)))
    V  = Minv %*% SparseM::t(JJ)
    # we construct a slightly different trace using esitmate of individual variance
    # suing KKS.
    P       = sColSums(SparseM::t(JJ) * V)
    S_i     = as.numeric(dd$m1 * (dd$m1 - JJ%*%psi)/(1-P))
    stats$error_var_hetero = mean(S_i)

    # Finally we need to compute the full correction
    #B  =as.numeric(sColSums(V *  diag(S2) %*% V) - sColSums(diag(S2) %*% V ) ^2)
    iota = rep(1,nrow(Ds))
    B = as.numeric(SparseM::as.matrix((iota %*% Ds) %*% V^2)) - as.numeric( SparseM::as.matrix((iota %*% Ds) %*% V))^2
    tr_correction_hetero = sum( S_i * B)

    flog.info("[twfe-firm] tr0=%f tr1=%f var0=%f var1=%f",tr_correction, tr_correction_hetero,var_e,mean(S_i))
    tr_correction = tr_correction_hetero
    stats$trace_term_hetero = tr_correction_hetero

    # we want to compute the leave-out cov(y,psi)
    psi_lo =  as.numeric( SparseM::as.matrix(  Jq %*% (Minv %*%  (SparseM::t(JJ) %*% (dd$m1 - (dd$m1 - JJ%*%psi)/(1-P)  )) )))
    stats$cov_ypsi_movers_lo = cov(dd$y1,psi_lo)

    # new formula
    eps_i = (dd$m1 - JJ%*%psi)/(1-P)
    R = SparseM::t(JJ) %*% sparseMatrix2(1:N,1:N,eps_i,c(N,N))
    psi_lo   = Jq%*%psi - SparseM::diag( Jq %*% (Minv %*% R))
    #sparseDiagCross(Jq, SparseM::as.matrix(Minv) , R, N)
    stats$cov_ypsi_movers_lo = cov(dd$y1,psi_lo)

    flog.info("[twfe-firm] leave-out cov(y,psi) for movers %4.4f",stats$cov_ypsi_movers_lo)
  }

  stats$trace_term_Q = tr_correction

  # COMPUTE THE VARIANCE OF PSI
  fids[, psi := psi[nfid]]
  var_psi_hat = fids[,wt.var(psi,size)]
  tot = sim$sdata[,var(y1)]
  btw = sim$sdata[,list(mean(y1),.N),f1][,wt.var(V1,N)]

  stats$psi_var = fids[,wt.var(psi,size)]

  if (!any(is.na(model0))) {
    fids[, psi0 := psi0[nfid]]
    rm(psi0)
    flog.info("var_true=%f  var_akm=%f var2=%f trace=%f ", fids[,wt.var(psi0,size)], fids[,wt.var(psi,size)], fids[,wt.var(psi,size)]- tr_correction,tr_correction)
  } else {
    flog.info("tot=%f btwf=%f var_akm=%f var2=%f trace=%f ",tot,btw, fids[,wt.var(psi,size)], fids[,wt.var(psi,size)]- tr_correction,tr_correction)
  }

  res = list(jids=data.table(fids),eps_sd = sqrt(var_e), var_psi= var_psi_hat, stats=stats,
             tr_term_Q = tr_correction)
}

#' Estimates a two-way fixed effect model using (f1,f2) as heterogeneity
#' identifiers
#'
#' @param sim input data with movers and stayers
#' @param hetero if TRUE use leave-one-out connected set and compute TRACE correction allowing for heterogeneity
#' @param model0 give a model, this is for development
#' @param var_psi_only if TRUE (default) then only the variance of firm effects is corrected
#' @export
m2.trace.estimate <- function(sim, model0=NA,hetero=FALSE,approx=0, check_set=TRUE,var_psi_only=TRUE) {

  stats = list()
  stats$hetero = hetero
  stats$total_number_of_firms   = length(unique(c(sim$jdata[,unique(c(f1,f2))],sim$sdata[,unique(f1)])))
  stats$total_number_of_stayers = sim$sdata[,.N]
  stats$total_number_of_movers  = sim$jdata[,.N]
  stats$total_logwage_var       = var(c(sim$sdata$y1,sim$jdata$y1))
  stats$total_btw_firm          = sim$sdata[,list(mean(y1),.N),f1][,wt.var(V1,N)]

  # EXTRACT CONNECTED SET
  jdata = sim$jdata
  flog.info("[twfe-firm] number of firms in movers %i (t1=%i,t2=%i) and stayers %i",sim$jdata[,length(unique(c(f1,f2)))],sim$jdata[,length(unique(f1))],sim$jdata[,length(unique(f2))],sim$sdata[,length(unique(f1))])
  if (hetero==F) {
    if(check_set){
      f1s   = get.largest.conset.fid(jdata)
      flog.info("[twfe-firm] connected set %i/%i",length(f1s),stats$total_number_of_firms)
    } else {
      f1s = jdata[,unique(c(f1,f2))]
    }
    jdata = jdata[f1 %in% f1s][f2 %in% f1s]
  } else {
    if(check_set){
      jdata = get.largest.leaveoutset.fid(jdata)
    }
    f1s = jdata[,unique(c(f1,f2))]
    flog.info("[twfe-firm] leave-out connected set %i/%i",length(f1s),stats$total_number_of_firms)
  }

  # index firms with integers
  fids = data.table(f1=f1s,nfid=1:length(f1s))
  setkey(fids,f1)
  setkey(jdata,f1)
  jdata[,f1i := fids[jdata,nfid]]
  setkey(jdata,f2)
  jdata[,f2i := fids[jdata,nfid]]
  setkey(sim$sdata,f1)
  sim$sdata[,f1i := fids[sim$sdata,nfid]]

  # extract size in stayers, and mean
  fsize = rbind(sim$sdata[,list(y1,f1)],sim$jdata[,list(y1,f1)])[,list(N=.N,mw=mean(y1)),f1]
  setkey(fids,f1)
  setkey(fsize,f1)
  fids[,size := fsize[fids,N]]
  fids[,mw   := fsize[fids,mw]]
  fids[is.na(size),size:=0] # pads missing size with 0
  fids[size==0, mw:=0] # pads missing size with 0

  # CONSTRUCT SPARSE DESIGN MATRIX + NORMALIZATION
  nf = pmax(max(jdata$f1i),max(jdata$f2i))
  dd = jdata[,list(m1 = y2 - y1,y1,f1i,f2i)]
  N = nrow(dd)
  # fixme, rewrite this with regular sparse matrices
  JJ = sparseMatrix(1:N, dd$f2i, x = 1,dims = c(N,nf))  - sparseMatrix(1:N, dd$f1i, x = 1,dims = c(N,nf))
  JJ = JJ[,1:(nf-1)] # here we want to drop the last firm as a normalization
  Jq = sparseMatrix(1:N, dd$f1i, x = 1,dims = c(N,nf))
  Jq = Jq[,1:(nf-1)] # here we want to drop the last firm as a normalization
  S  = fids[order(nfid),size]

  stats$set_number_of_firms   = nf
  stats$set_number_of_stayers = sum(S)-(N-1)
  stats$set_number_of_movers  = N-1
  stats$set_logwage_var       = var(c(sim$sdata[f1 %in% fids$f1,y1],sim$jdata$y1))
  stats$set_btw_firm        = sim$sdata[f1 %in% fids$f1,list(mean(y1),.N),f1][,wt.var(V1,N)]

  t = Matrix:::t
  # COMPUTE INVERSE OF DESIGN MATRIX
  M     = t(JJ) %*% JJ
  M1inv = Matrix::solve(M)

  # compute firms FE
  psi = as.numeric( M1inv %*% ( t(JJ) %*% dd$m1))
  E   = as.numeric(dd$m1 - JJ %*% psi)

  if (!any(is.na(model0))) {
    psi0 = model0$psi[as.integer(fids[order(nfid),f1])]
    psi0 = psi0-psi0[1]
    flog.info("[twfe-firm] corr= %f", cor(psi0,psi))
  }

  # extract homoskedastic error
  var_e = var(E)*nrow(sim$jdata)/(nrow(sim$jdata)-nf)
  stats$error_var_homo = var_e
  flog.info("[twfe-firm] var_e= %f", var_e)

  # COMPUTE THE TRACE FORMULA - USING THE WEIGHTING OF STAYERS
  iota  = rep(1,N)
  M1inv_dense =  as.matrix(M1inv)
  tr2   = as.numeric(N^(-2)*(iota %*% Jq) %*% (M1inv %*% (t(Jq) %*% iota)))
  tr1   = as.numeric(N^(-1)* denseTraceProd(M1inv_dense , as.matrix(t(Jq) %*% Jq)))
  tr    = tr1 - tr2

  tr_correction = var_e*tr
  stats$trace_term_homo_Q = tr_correction
  stats$trace_term_hetero = NA
  stats$error_var_hetero = NA

  # heteroskedastic variance using BLM groups
  if (hetero==TRUE) {

    # we construct leverage
    Pi = sparseDiagCross( t(JJ) ,M1inv_dense , t(JJ), N)
    Si = as.numeric(dd$m1 * (dd$m1 - JJ%*%psi)/(1-Pi))
    stats$error_var_hetero = mean(Si)

    Rt = t(JJ) %*% Diagonal(N,Si) %*% JJ
    tr1 = N^-1 * denseTraceProd( as.matrix(M1inv_dense %*% (t(Jq) %*% Jq )),
                                 as.matrix(M1inv_dense %*% Rt) )
    tr2 = N^-2 * sum( as.numeric( ((iota %*% Jq) %*% M1inv) %*% Rt) * as.numeric( ((iota %*% Jq) %*% M1inv)) )
    tr_correction_hetero = tr1 - tr2

    flog.info("[twfe-firm] tr0=%f tr1=%f var0=%f var1=%f",tr_correction, tr_correction_hetero,var_e,mean(Si))
    tr_correction = tr_correction_hetero
    stats$trace_term_hetero = tr_correction_hetero

    # new formula
    Si2 = as.numeric(dd$y1 * (dd$m1 - JJ%*%psi)/(1-Pi))
    Rt = t(JJ) %*% Diagonal(N,Si2) %*% Jq
    tr1 = N^-1 * denseTraceProd( M1inv_dense, as.matrix(Rt) )
    tr2 = N^-2 * sum(as.numeric( dd$y1 * as.numeric( Jq %*% (M1inv_dense %*%  (t(JJ) %*% Si2 )))    ))
    stats$cov_ypsi_movers_lo = cov(dd$y1, as.numeric(Jq %*% psi)) - (tr1 - tr2)
    flog.info("[twfe-firm] leave-out cov(y,psi) for movers %4.4f",stats$cov_ypsi_movers_lo)
  }

  stats$trace_term_Q = tr_correction

  # COMPUTE THE VARIANCE OF PSI
  fids[, psi := psi[nfid]]
  var_psi_hat = fids[,wt.var(psi,size)]
  tot = sim$sdata[,var(y1)]
  btw = sim$sdata[,list(mean(y1),.N),f1][,wt.var(V1,N)]

  stats$psi_var = fids[,wt.var(psi,size)]

  if (!any(is.na(model0))) {
    fids[, psi0 := psi0[nfid]]
    rm(psi0)
    flog.info("var_true=%f  var_akm=%f var2=%f trace=%f ", fids[,wt.var(psi0,size)], fids[,wt.var(psi,size)], fids[,wt.var(psi,size)]- tr_correction,tr_correction)
  } else {
    flog.info("tot=%f btwf=%f var_akm=%f var2=%f trace=%f ",tot,btw, fids[,wt.var(psi,size)], fids[,wt.var(psi,size)]- tr_correction,tr_correction)
  }

  res = list(jids=data.table(fids),eps_sd = sqrt(var_e), var_psi= var_psi_hat, stats=stats,
             tr_term_Q = tr_correction)
}

#' Estimates a two-way fixed effect model using (f1,f2) as heterogeneity
#' identifiers
#'
#' @param sim input event study data
#' @param check_set if TRUE imposes connected set
#' @export
m2.trace.diff <- function(sim, check_set=TRUE) {

  stats = list()
  stats$hetero = hetero
  stats$total_number_of_firms   = length(unique(c(sim$jdata[,unique(c(f1,f2))],sim$sdata[,unique(f1)])))
  stats$total_number_of_stayers = sim$sdata[,.N]
  stats$total_number_of_movers  = sim$jdata[,.N]
  stats$total_logwage_var       = var(c(sim$sdata$y1,sim$jdata$y1))
  stats$total_btw_firm          = sim$sdata[,list(mean(y1),.N),f1][,wt.var(V1,N)]

  # EXTRACT CONNECTED SET
  jdata = sim$jdata
  flog.info("[twfe-firm] number of firms in movers %i (t1=%i,t2=%i) and stayers %i",sim$jdata[,length(unique(c(f1,f2)))],sim$jdata[,length(unique(f1))],sim$jdata[,length(unique(f2))],sim$sdata[,length(unique(f1))])
  if (hetero==F) {
    if(check_set){
      f1s   = get.largest.conset.fid(jdata)
      flog.info("[twfe-firm] connected set %i/%i",length(f1s),stats$total_number_of_firms)
    } else {
      f1s = jdata[,unique(c(f1,f2))]
    }
    jdata = jdata[f1 %in% f1s][f2 %in% f1s]
  } else {
    if(check_set){
      jdata = get.largest.leaveoutset.fid(jdata)
    }
    f1s = jdata[,unique(c(f1,f2))]
    flog.info("[twfe-firm] leave-out connected set %i/%i",length(f1s),stats$total_number_of_firms)
  }

  # index firms with integers
  fids = data.table(f1=f1s,nfid=1:length(f1s))
  setkey(fids,f1)
  setkey(jdata,f1)
  jdata[,f1i := fids[jdata,nfid]]
  setkey(jdata,f2)
  jdata[,f2i := fids[jdata,nfid]]
  setkey(sim$sdata,f1)
  sim$sdata[,f1i := fids[sim$sdata,nfid]]

  # CONSTRUCT SPARSE DESIGN MATRIX + NORMALIZATION
  nf = pmax(max(jdata$f1i),max(jdata$f2i))
  dd = jdata[,list(m1 = y2 - y1,f1i,f2i)]
  dd = rbind(dd,data.frame(m1=0,f1i=1,f2i=1))
  N = nrow(dd)
  dd[,v1:=-1][,v2:=1]
  dd[,c1:=f1i][,c2:=f2i]
  dd[N,v1:=0]
  JJ = sparseMatrix2(1:N,dd$c1,dd$v1,c(N,nf)) + sparseMatrix2(1:N,dd$c2,dd$v2,c(N,nf))
  S  = fids[order(nfid),size]

  # collect information
  stats$set_number_of_firms   = nf
  stats$set_number_of_stayers = sum(S)-(N-1)
  stats$set_number_of_movers  = N-1
  stats$set_logwage_var       = var(c(sim$sdata[f1 %in% fids$f1,y1],sim$jdata$y1))
  stats$set_btw_firm          = sim$sdata[f1 %in% fids$f1,list(mean(y1),.N),f1][,wt.var(V1,N)]

  # COMPUTE INVERSE OF DESIGN MATRIX
  M = SparseM::t(JJ) %*% JJ
  Minv = SparseM::solve(M,nnzlmax=1e8,tmpmax=1e8,nsubmax=1e8)

  # compute firms FE
  psi = as.numeric(SparseM::as.matrix(Minv %*% ( SparseM::t(JJ) %*% dd$m1)))
  E   = dd$m1 - JJ%*%psi
  E   = E[1:(N-1)]

  if (!any(is.na(model0))) {
    psi0 = model0$psi[as.integer(fids[order(nfid),f1])]
    psi0 = psi0-psi0[1]
    flog.info("[twfe-firm] corr= %f", cor(psi0,psi))
  }

  # extract homoskedastic error
  var_e = var(E)*nrow(sim$jdata)/(nrow(sim$jdata)-nf)
  stats$error_var_homo = var_e
  flog.info("[twfe-firm] var_e= %f", var_e)

  # COMPUTE THE TRACE FORMULA - USING THE WEIGHTING OF STAYERS
  tr_correction_not_Q = NULL
  if (approx==1) {
    ndraws = ceiling(log(nf))
    tr_correction = var_e * m2.trace.tr_approx(Minv,S,ndraws)
  } else if (approx==2) {
    ndraws = ceiling(log(nf))
    W = diag(nf) - 1/(sum(S)) * spread(S,1,nf)
    Q = 1/sum(S)* t(W) %*% diag(nf,x=S) %*% W
    tr_correction = var_e * trapprox(M,Q,ndraws = ndraws, new = 0)
  } else if (approx==3) {
    ndraws = ceiling(log(nf))
    tr_correction = var_e * trapprox(M,S,ndraws = ndraws, new = 1)
  } else {
    tr_correction = var_e*( sum( SparseM::diag(Minv)*S )/sum(S) - sum( S* (Minv %*% rep(1/nf,nf)) )/(sum(S))  )
  }

  stats$trace_term_homo_Q = tr_correction
  stats$trace_term_homo_not_Q = tr_correction_not_Q
  stats$trace_term_hetero = NA
  stats$error_var_hetero = NA

  andrews_vars = c("psi_var_fe","psi_var_bc","cov_fe","cov_bc","alpha_var_fe","alpha_var_bc")
  stats[paste0("full_dec_",andrews_vars)] = 0
  if (var_psi_only==FALSE) {
    fids_con = jdata[,unique(c(f1,f2))]
    adata = rbind(
      sim$sdata[f1 %in% fids_con, list(wid,fid=f1,y=y1,t=1)],
      sim$sdata[f2 %in% fids_con, list(wid,fid=f2,y=y2,t=2)],
      jdata[, list(wid,fid=f1,y=y1,t=1)],
      jdata[, list(wid,fid=f2,y=y2,t=2)])
    if (hetero==FALSE) {
      all_res = m2.get_all_bc(adata) # here the variance is devided by two, because note from differences
    } else {
      all_res = m2.get_all_bc_hetero_bis(adata) # here the variance is devided by two, because note from differences
    }
    stats[paste0("full_dec_",andrews_vars)] = all_res[andrews_vars]
  }

  # heteroskedastic variance using BLM groups
  if (hetero==TRUE) {

    S2 = S/sum(S)
    Ds = sparseMatrix2(1:length(S2),1:length(S2),S2,c(length(S2),length(S2)))
    V  = Minv %*% SparseM::t(JJ)
    # we construct a slightly different trace using esitmate of individual variance
    # suing KKS.
    P       = sColSums(SparseM::t(JJ) * V)
    S_i     = as.numeric(dd$m1 * (dd$m1 - JJ%*%psi)/(1-P))
    stats$error_var_hetero = mean(S_i)

    # Finally we need to compute the full correction
    #B  =as.numeric(sColSums(V *  diag(S2) %*% V) - sColSums(diag(S2) %*% V ) ^2)
    iota = rep(1,nrow(Ds))
    B = as.numeric(SparseM::as.matrix((iota %*% Ds) %*% V^2)) - as.numeric( SparseM::as.matrix((iota %*% Ds) %*% V))^2
    tr_correction_hetero = sum( S_i * B)

    flog.info("[twfe-firm] tr0=%f tr1=%f var0=%f var1=%f",tr_correction, tr_correction_hetero,var_e,mean(S_i))
    tr_correction = tr_correction_hetero
    stats$trace_term_hetero = tr_correction_hetero
  }

  stats$trace_term_Q = tr_correction
  stats$trace_term_not_Q = tr_correction_not_Q

  # COMPUTE THE VARIANCE OF PSI
  fids[, psi := psi[nfid]]
  var_psi_hat = fids[,wt.var(psi,size)]
  tot = sim$sdata[,var(y1)]
  btw = sim$sdata[,list(mean(y1),.N),f1][,wt.var(V1,N)]

  stats$psi_var = fids[,wt.var(psi,size)]

  if (!any(is.na(model0))) {
    fids[, psi0 := psi0[nfid]]
    rm(psi0)
    flog.info("var_true=%f  var_akm=%f var2=%f trace=%f ", fids[,wt.var(psi0,size)], fids[,wt.var(psi,size)], fids[,wt.var(psi,size)]- tr_correction,tr_correction)
  } else {
    flog.info("tot=%f btwf=%f var_akm=%f var2=%f trace=%f ",tot,btw, fids[,wt.var(psi,size)], fids[,wt.var(psi,size)]- tr_correction,tr_correction)
  }

  res = list(jids=data.table(fids),eps_sd = sqrt(var_e), var_psi= var_psi_hat, stats=stats,
             tr_term_Q = tr_correction, tr_term_not_Q = tr_correction_not_Q)
}





#' Estimates a two-way fixed effect model using (f1,f2) as heterogeneity
#' identifiers
#'
#' @param sim input data with movers and stayers
#' @param hetero if TRUE use leave-one-out connected set and compute TRACE correction allowing for heterogeneity
#' @param model0 give a model, this is for development
#' @param var_psi_only if TRUE (default) then only the variance of firm effects is corrected
#' @export
m2.trace.estimate.full <- function(sim, hetero=FALSE, check_set=TRUE,
                                   impose_leaveout = FALSE,
                                   include_stayer_period1 = TRUE, make_each_row_one_wid = TRUE) {

  stats = list()
  stats$hetero = hetero
  stats$total_number_of_firms   = length(unique(c(sim$jdata[,unique(c(f1,f2))],sim$sdata[,unique(f1)])))
  stats$total_number_of_stayers = sim$sdata[,.N]
  stats$total_number_of_movers  = sim$jdata[,.N]
  stats$total_logwage_var       = var(c(sim$sdata$y1,sim$jdata$y1))
  stats$total_btw_firm          = sim$sdata[,list(mean(y1),.N),f1][,wt.var(V1,N)]

  # EXTRACT CONNECTED SET
  jdata = sim$jdata
  flog.info("[twfe-firm] number of firms in movers %i (t1=%i,t2=%i) and stayers %i",sim$jdata[,length(unique(c(f1,f2)))],sim$jdata[,length(unique(f1))],sim$jdata[,length(unique(f2))],sim$sdata[,length(unique(f1))])
  if ((hetero==F) & (impose_leaveout==F)) {
    if(check_set){
      f1s   = get.largest.conset.fid(jdata)
      flog.info("[twfe-firm] connected set %i/%i",length(f1s),stats$total_number_of_firms)
    } else {
      f1s = jdata[,unique(c(f1,f2))]
    }
    jdata = jdata[f1 %in% f1s][f2 %in% f1s]
  } else {
    if(check_set){
      jdata = get.largest.leaveoutset.fid(jdata)
    }
    f1s = jdata[,unique(c(f1,f2))]
    flog.info("[twfe-firm] leave-out connected set %i/%i",length(f1s),stats$total_number_of_firms)
  }

  # index firms with integers
  fids = data.table(f1=f1s,nfid=1:length(f1s))
  setkey(fids,f1)
  setkey(jdata,f1)
  jdata[,f1i := fids[jdata,nfid]]
  setkey(jdata,f2)
  jdata[,f2i := fids[jdata,nfid]]

  stats$set_number_of_firms   = jdata[,length(unique(c(f1,f2)))]
  stats$set_number_of_stayers = sim$sdata[f1==f2][f1%in%fids$f1,.N]
  stats$set_number_of_movers  = jdata[,.N]
  stats$set_logwage_var       = var(c(sim$sdata[f1 %in% fids$f1,y1],sim$jdata$y1))
  stats$set_btw_firm          = sim$sdata[f1 %in% fids$f1,list(mean(y1),.N),f1][,wt.var(V1,N)]

  andrews_vars = c("psi_var_fe","psi_var_bc","cov_fe","cov_bc","alpha_var_fe","alpha_var_bc")
  stats[paste0("full_dec_",andrews_vars)] = 0

  fids_con = jdata[,unique(c(f1,f2))]

  if (make_each_row_one_wid) {
    sim$sdata[,wid2 := 1:.N ]
    wid_stayer_max = sim$sdata[,max(wid2)]
    jdata[, wid2 := (1:.N) + wid_stayer_max]
  } else {
    sim$sdatasdata[, wid2 := wid]
    jdata[, wid2 := wid]
  }

  # adata = rbind(
  #   jdata[, list(wid=wid2,fid=f1,y=y1,t=year)],
  #   jdata[, list(wid=wid2,fid=f2,y=y2,t=year+2)])
  adata = rbind(
    jdata[, list(wid=wid2,fid=f1,y=y1,cs=1,m=1)],
    jdata[, list(wid=wid2,fid=f2,y=y2,cs=0,m=1)])

  # append period 2 stayers
  if (include_stayer_period1) {
    # adata = rbind(adata,sim$sdata[f1 %in% fids_con, list(wid,fid=f1,y=y1,t=year)])
    adata = rbind(adata,sim$sdata[f1 %in% fids_con, list(wid=wid2,fid=f1,y=y1,cs=1,m=0)])
  }

  # make sure we only have one year observation per worker
  # adata = adata[, count := 1:.N, list(t, wid)]
  # adata = adata[count==1]
  m2.getstats(adata)

  if (hetero==FALSE) {
    all_res = m2.get_all_bc(adata) # here the variance is devided by two, because note from differences
  } else {
    all_res = m2.get_all_bc_hetero_bis(adata) # here the variance is devided by two, because note from differences
  }
  stats$cov_ypsi_movers_fe = all_res$cov_ypsi_movers_fe
  stats$cov_ypsi_all_fe = all_res$cov_ypsi_all_fe
  stats$cov_ypsi_stayers_fe = all_res$cov_ypsi_stayers_fe
  stats[paste0("full_dec_",andrews_vars)] = all_res[andrews_vars]

  res = list(jids=data.table(fids), stats=stats)
}

#' Estimates a two-way fixed effect model using (f1,f2) as heterogeneity
#' identifiers
#'
#' In this variation of the function, we never invert the mobility matrix.
#'
#' @param sim input data with movers and stayers
#' @param hetero if TRUE use leave-one-out connected set and compute TRACE correction allowing for heterogeneity
#' @param model0 give a model, this is for development
#' @export
m2.trace.estimate.noinv <- function(sim, model0=NA,hetero=FALSE,approx=0) {

  stats = list()
  stats$hetero = hetero
  stats$total_number_of_firms   = length(unique(c(sim$jdata[,unique(c(f1,f2))],sim$sdata[,unique(f1)])))
  stats$total_number_of_stayers = sim$sdata[,.N]
  stats$total_number_of_movers  = sim$jdata[,.N]
  stats$total_logwage_var       = var(c(sim$sdata$y1,sim$jdata$y1))
  stats$total_btw_firm          = sim$sdata[,list(mean(y1),.N),f1][,wt.var(V1,N)]

  # EXTRACT CONNECTED SET
  jdata = sim$jdata
  flog.info("[twfe-firm] number of firms in movers %i (t1=%i,t2=%i) and stayers %i",sim$jdata[,length(unique(c(f1,f2)))],sim$jdata[,length(unique(f1))],sim$jdata[,length(unique(f2))],sim$sdata[,length(unique(f1))])
  if (hetero==F) {
    f1s   = get.largest.conset.fid(jdata)
    flog.info("[twfe-firm] connected set %i/%i",length(f1s),stats$total_number_of_firms)
    jdata = jdata[f1 %in% f1s][f2 %in% f1s]
  } else {
    jdata = get.largest.leaveoutset.fid(jdata)
    f1s = jdata[,unique(c(f1,f2))]
    flog.info("[twfe-firm] leave-out connected set %i/%i",length(f1s),stats$total_number_of_firms)
  }

  # index firms with integers
  fids = data.table(f1=f1s,nfid=1:length(f1s))
  setkey(fids,f1)
  setkey(jdata,f1)
  jdata[,f1i := fids[jdata,nfid]]
  setkey(jdata,f2)
  jdata[,f2i := fids[jdata,nfid]]

  # extract size in stayers, and mean
  fsize = rbind(sim$sdata[,list(y1,f1)],sim$jdata[,list(y1,f1)])[,list(N=.N,mw=mean(y1)),f1]
  setkey(fids,f1)
  setkey(fsize,f1)
  fids[,size := fsize[fids,N]]
  fids[,mw   := fsize[fids,mw]]
  fids[is.na(size),size:=0] # pads missing size with 0
  fids[size==0, mw:=0] # pads missing size with 0

  # CONSTRUCT SPARSE DESIGN MATRIX + NORMALIZATION
  nf = pmax(max(jdata$f1i),max(jdata$f2i))
  dd = jdata[,list(m1 = y2 - y1,f1i,f2i)]
  dd = rbind(dd,data.frame(m1=0,f1i=1,f2i=1)) # normalization
  N = nrow(dd)
  dd[,v1:=-1][,v2:=1]
  dd[,c1:=f1i][,c2:=f2i]
  dd[N,v1:=0]
  dd[,row := 1:.N]

  # Using package sparseMatrix
  # JJ = sparseMatrix2(1:N,dd$c1,dd$v1,c(N,nf)) + sparseMatrix2(1:N,dd$c2,dd$v2,c(N,nf))

  # Using package Matrix
  #J1 <- sparseMatrix(i=dd$row, j=dd$c1, x = dd$v1, dims = c(N,nf)) + sparseMatrix(i=dd$row, j=dd$c2, x = dd$v2, dims = c(N,nf))

  # using package spam
  J1 <- spam(0, nrow = N, ncol = nf)
  J1[cbind(dd$row, dd$c1)] <- dd$v1
  J1[cbind(dd$row, dd$c2)] <- dd$v2

  S  = fids[order(nfid),size]

  stats$set_number_of_firms   = nf
  stats$set_number_of_stayers = sum(S)-(N-1)
  stats$set_number_of_movers  = N-1
  stats$set_logwage_var       = var(c(sim$sdata[f1 %in% fids$f1,y1],sim$jdata$y1))
  stats$set_btw_firm        = sim$sdata[f1 %in% fids$f1,list(mean(y1),.N),f1][,wt.var(V1,N)]

  # COMPUTE INVERSE OF DESIGN MATRIX
  M = spam::t(J1) %*% J1

  # compute firms FE
  flog.info("[twfe-firm] starting solving for the firm effects")
  psi = spam::solve(M, Matrix::t(J1) %*% dd$m1,sparse=TRUE,tol=1e-7)
  flog.info("[twfe-firm] finshed solving for the firm effects")
  E   = dd$m1 - J1 %*% psi
  E   = E[1:(N-1)]

  if (!any(is.na(model0))) {
    psi0 = model0$psi[as.integer(fids[order(nfid),f1])]
    psi0 = psi0-psi0[1]
    flog.info("[twfe-firm] corr= %f", cor(psi0,psi))
  }

  # extract homoskedastic error
  var_e = var(E)*nrow(sim$jdata)/(nrow(sim$jdata)-nf)
  stats$error_var_homo = var_e

  # COMPUTE THE TRACE FORMULA - USING THE WEIGHTING OF STAYERS
  tr_correction_not_Q = NULL
  ndraws = ceiling(log(nf))
  tr_correction = var_e * m2.trace.tr_approx( M ,S,ndraws)

  stats$trace_term_homo_Q = tr_correction
  stats$trace_term_homo_not_Q = tr_correction_not_Q
  stats$trace_term_hetero = NA
  stats$error_var_hetero = NA

  # heteroskedastic variance using BLM groups
  if (hetero==TRUE) {

    S2 = S/sum(S)
    V  = Minv %*% SparseM::t(JJ)
    # we construct a slightly different trace using esitmate of individual variance
    # suing KKS.
    P       = sColSums(SparseM::t(JJ) * V)
    S_i     = as.numeric(dd$m1 * (dd$m1 - JJ%*%psi)/(1-P))
    stats$error_var_hetero = mean(S_i)

    # Finally we need to compute the full correction
    B = as.numeric(sColSums(V *  diag(S2) %*% V) - sColSums(diag(S2) %*% V ) ^2)
    tr_correction_hetero = sum( S_i * B)

    flog.info("[twfe-firm] tr0=%f tr1=%f var0=%f var1=%f",tr_correction, tr_correction_hetero,var_e,mean(S_i))
    tr_correction = tr_correction_hetero
    stats$trace_term_hetero = tr_correction_hetero
  }

  stats$trace_term_Q = tr_correction
  stats$trace_term_not_Q = tr_correction_not_Q

  # COMPUTE THE VARIANCE OF PSI
  fids[, psi := psi[nfid]]
  var_psi_hat = fids[,wt.var(psi,size)]
  tot = sim$sdata[,var(y1)]
  btw = sim$sdata[,list(mean(y1),.N),f1][,wt.var(V1,N)]

  stats$psi_var = fids[,wt.var(psi,size)]

  if (!any(is.na(model0))) {
    fids[, psi0 := psi0[nfid]]
    rm(psi0)
    flog.info("var_true=%f  var_akm=%f var2=%f trace=%f ", fids[,wt.var(psi0,size)], fids[,wt.var(psi,size)], fids[,wt.var(psi,size)]- tr_correction,tr_correction)
  } else {
    flog.info("tot=%f btwf=%f var_akm=%f var2=%f trace=%f ",tot,btw, fids[,wt.var(psi,size)], fids[,wt.var(psi,size)]- tr_correction,tr_correction)
  }

  res = list(jids=data.table(fids),eps_sd = sqrt(var_e), var_psi= var_psi_hat, stats=stats,
             tr_term_Q = tr_correction, tr_term_not_Q = tr_correction_not_Q)
}





#' Estimates a two-way fixed effect model using (j1,j2) as heterogeneity
#' identifiers
#'
#' @param sim input data with movers and stayers
#' @param hetero if TRUE use leave-one-out connected set and compute TRACE correction allowing for heterogeneity
#' @param model0 give a model, this is for development
#' @param subsample (default is 0) whether to use all movers for within_re
#' @export
m2.trace.estimate.clus <- function(sim, model0=NA,hetero=FALSE,within_re=FALSE,subsample=0) {

  stats = list()
  stats$hetero = hetero
  stats$total_number_of_firms      = length(unique(c(sim$jdata[,unique(c(f1,f2))],sim$sdata[,unique(f1)])))
  stats$total_number_of_clusters   = length(unique(c(sim$jdata[,unique(c(j1,j2))],sim$sdata[,unique(j1)])))
  stats$total_number_of_stayers    = sim$sdata[,.N]
  stats$total_number_of_movers     = sim$jdata[,.N]
  stats$total_logwage_var          = var(c(sim$sdata$y1,sim$jdata$y1))
  stats$total_btw_firm             = sim$sdata[,list(mean(y1),.N),f1][,wt.var(V1,N)]

  # EXTRACT CONNECTED SET
  jdata = sim$jdata
  flog.info("[twfe-clus] number of firms in movers %i (t1=%i,t2=%i) and stayers %i",sim$jdata[,length(unique(c(f1,f2)))],sim$jdata[,length(unique(f1))],sim$jdata[,length(unique(f2))],sim$sdata[,length(unique(f1))])
  flog.info("[twfe-clus] number of clusters in movers %i (t1=%i,t2=%i) and stayers %i",sim$jdata[,length(unique(c(j1,j2)))],sim$jdata[,length(unique(j1))],sim$jdata[,length(unique(j2))],sim$sdata[,length(unique(j1))])
  if (hetero==F) {
    j1s   = get.largest.conset.clus(jdata)
    flog.info("[twfe-clus] connected set %i/%i clusters",length(j1s),length(unique(sim$sdata$j1)))
    jdata = jdata[j1 %in% j1s][j2 %in% j1s]
  } else {
    jdata = get.largest.leaveoutset.clus(jdata)
    j1s = jdata[,unique(c(j1,j2))]
    flog.info("[twfe-clus] leave-out connected set %i/%i",length(j1s),length(unique(sim$sdata$j1)))
  }

  # index groups with integers (should be the case already)
  jids = data.table(j1=j1s,nfid=1:length(j1s))
  setkey(jids,j1)
  setkey(jdata,j1)
  jdata[,j1i := jids[jdata,nfid]]
  setkey(jdata,j2)
  jdata[,j2i := jids[jdata,nfid]]

  # extract size in stayers, and mean
  fsize = rbind(sim$sdata[,list(y1,j1)],sim$jdata[,list(y1,j1)])[,list(N=.N,mw=mean(y1)),j1]
  setkey(jids,j1)
  setkey(fsize,j1)
  jids[,size := fsize[jids,N]]
  jids[,mw   := fsize[jids,mw]]
  jids[is.na(size),size:=0] # pads missing size with 0
  jids[size==0, mw:=0] # pads missing size with 0

  # CONSTRUCT SPARSE DESIGN MATRIX + NORMALIZATION
  nf = pmax(max(jdata$j1i),max(jdata$j2i))
  dd = jdata[,list(m1 = y2 - y1,j1i,j2i)]
  dd = rbind(dd,data.frame(m1=0,j1i=1,j2i=1))
  N = nrow(dd)
  dd[,v1:=-1][,v2:=1]
  dd[,c1:=j1i][,c2:=j2i]
  dd[N,v1:=0]
  JJ = sparseMatrix2(1:N,dd$c1,dd$v1,c(N,nf)) + sparseMatrix2(1:N,dd$c2,dd$v2,c(N,nf))
  S  = jids[order(nfid),size]

  stats$set_number_of_firms   = nf
  stats$set_number_of_stayers = sum(S)-(N-1)
  stats$set_number_of_movers  = N-1
  stats$set_logwage_var       = var(c(sim$sdata[j1 %in% jids$j1,y1],sim$jdata$y1))
  stats$total_btw_firm        = sim$sdata[j1 %in% jids$j1,list(mean(y1),.N),j1][,wt.var(V1,N)]

  # COMPUTE INVERSE OF DESIGN MATRIX
  M = SparseM::t(JJ) %*% JJ
  Minv = SparseM::solve(M,nnzlmax=1e8,tmpmax=1e8,nsubmax=1e8)

  # compute firms FE
  psi = as.numeric(SparseM::as.matrix(Minv %*% ( SparseM::t(JJ) %*% dd$m1)))
  E   = dd$m1 - JJ%*%psi
  E   = E[1:(N-1)]

  # extract random effect variance
  if (within_re==TRUE) {
    jdata[,psi1 := psi[j1i]]
    jdata[,psi2 := psi[j2i]]
    omega = m2.trace.reExt(jdata,subsample=subsample)
  }

  if (!any(is.na(model0))) {
    psi0 = model0$psi[as.integer(jids[order(nfid),j1])]
    psi0 = psi0-psi0[1]
    flog.info("[twfe-clus] corr= %f", cor(psi0,psi))
  }

  # extract homoskedastic error
  var_e = var(E)*nrow(sim$jdata)/(nrow(sim$jdata)-nf)
  stats$error_var_homo = var_e

  # COMPUTE THE TRACE FORMULA - USING THE WEIGHTING OF STAYERS
  tr_correction = var_e*( sum( SparseM::diag(Minv)*S )/sum(S) - sum( S* (Minv %*% rep(1/nf,nf)) )/(sum(S))  )
  stats$trace_term_homo = tr_correction
  stats$trace_term_hetero = NA
  stats$error_var_hetero = NA

  # heteroskedastic variance using BLM groups
  if (hetero==TRUE) {

    S2 = S/sum(S)
    V  = Minv %*% SparseM::t(JJ)
    # we construct a slightly different trace using esitmate of individual variance
    # suing KKS.
    P       = sColSums(SparseM::t(JJ) * V)
    S_i     = as.numeric(dd$m1 * (dd$m1 - JJ%*%psi)/(1-P))
    stats$error_var_hetero = mean(S_i)

    # Finally we need to compute the full correction
    B = as.numeric(sColSums(V *  diag(S2) %*% V) - sColSums(diag(S2) %*% V ) ^2)
    tr_correction_hetero = sum( S_i * B)

    flog.info("[twfe-clus] tr0=%f tr1=%f var0=%f var1=%f",tr_correction, tr_correction_hetero,var_e,mean(S_i))
    tr_correction = tr_correction_hetero
    stats$trace_term_hetero = tr_correction_hetero
  }

  stats$trace_term = tr_correction

  # COMPUTE THE VARIANCE OF PSI
  jids[, psi := psi[nfid]]
  if (within_re==TRUE) {
    jids[,omega := omega[nfid]]
  }
  var_psi_hat = jids[,wt.var(psi,size)]
  tot = sim$sdata[,var(y1)]
  btw = sim$sdata[,list(mean(y1),.N),j1][,wt.var(V1,N)]

  stats$psi_var = jids[,wt.var(psi,size)]

  if (!any(is.na(model0))) {
    jids[, psi0 := psi0[nfid]]
    rm(psi0)
    flog.info("[twfe-clus] var_true=%f  var_akm=%f var2=%f trace=%f ", jids[,wt.var(psi0,size)], jids[,wt.var(psi,size)], jids[,wt.var(psi,size)]- tr_correction,tr_correction)
  } else {
    flog.info("[twfe-clus] tot=%f btwf=%f var_akm=%f var2=%f trace=%f ",tot,btw, jids[,wt.var(psi,size)], jids[,wt.var(psi,size)]- tr_correction,tr_correction)
  }

  res = list(jids=data.table(jids),eps_sd = sqrt(var_e), var_psi= var_psi_hat, stats=stats)
}

#' Extract the set of firms, cluster firms based on distributions and compute TWFE at
#' the cluster level
#'
#' @param sim input data with movers and stayers
#' @param set type of set to use on the data (0:full, 1:connected, 2: leave-out connected)
#' @export
m2.trace.blm <- function(sim, set=0, nclus=10,nstart=1000,subsample=0) {
  jdata = sim$jdata
  sdata = sim$sdata

  # apply the set
  if (set==1) {
    flog.info("computing connected set")
    f0s   = get.largest.conset.fid(jdata)
    jdata = sim$jdata[f1%in%f0s][f2%in%f0s]
    sdata = sim$sdata[f1%in%f0s]
  } else if (set==2) {
    flog.info("computing leave-out connected set")
    jdata = get.largest.leaveoutset.fid(jdata)
    f1s = jdata[,unique(c(f1,f2))]
    sdata = sim$sdata[f1%in%f1s]
  } else {
    flog.info("[twfe-blm] using the full data")
    # nothing!
  }

  # cluster
  ms    = grouping.getMeasures(list(sdata = sdata,jdata=jdata),"ecdf",Nw=20,y_var = "y1")
  grps  = grouping.classify.once(ms,k = nclus,nstart = nstart,iter.max = 200,step=100)
  sim = grouping.append(list(sdata = sdata,jdata=jdata),grps$best_cluster, drop=T)

  # estimate
  res = m2.trace.estimate.clus(sim,hetero=F,within_re = T,subsample = subsample)
  res$stats$set=set
  res$stats$omega_var     = res$jids[,wt.mean(omega,size)]
  res$stats$omega_var_pos = res$jids[,wt.mean(pmax(omega,0),size)]

  return(res)
}



#' Ridge AKM
#' @export
m2.firmfe.pen  <- function(sim, model, lambda=1,holdout=0.1) {

  # index firms with integers
  jdata = sim$jdata
  f1s   = unique(c(sim$jdata[,unique(f1,f2)],unique(sim$sdata[,unique(f1)] )))
  fids  = data.table(f1=f1s,nfid=1:length(f1s))
  setkey(fids,f1)
  setkey(jdata,f1)
  jdata[,f1i := fids[jdata,nfid]]
  setkey(jdata,f2)
  jdata[,f2i := fids[jdata,nfid]]

  # extract size in stayers
  fsize = rbind(sim$sdata[,list(y1,f1,j1)],sim$jdata[,list(y1,f1,j1)])[,.N,list(f1,j1)]
  setkey(fids,f1)
  setkey(fsize,f1)
  fids[,size := fsize[fids,N]]
  fids[is.na(size),size:=0] # pads missing size with 0
  fids[,j1 := fsize[fids,j1]]

  # create the matrix
  nf = pmax(max(jdata$f1i),max(jdata$f2i))
  dd = jdata[,list(m1 = y2 - y1,f1i,f2i)]
  N = nrow(dd)
  dd[,v1:=-1][,v2:=1]
  dd[,c1:=f1i][,c2:=f2i]
  JJ = sparseMatrix2(1:N,dd$c1,dd$v1,c(N,nf)) + sparseMatrix2(1:N,dd$c2,dd$v2,c(N,nf))

  # get the vector of sizes
  S  = fids[order(nfid),size]

  # create the penalty, just append the values
  R  = as.numeric(model$A1)[fids[order(nfid),j1]]
  RJ = as(length(R),"matrix.diag.csr")

  # we create a hold out (by setting a random set of obsevartions to 0)
  I = sample.int(N,ceiling(holdout*N),replace = FALSE)

  XX = rbind(JJ,RJ)
  YY = c(dd$m1,R)
  WW1 = rep(1,nrow(dd))
  WW1[I] = 0
  WW = c( WW1, lambda * rep(1,length(R)) )
  fit = SparseM::slm.wfit(XX,YY,WW,nnzlmax=1e8,tmpmax=1e8,nsubmax=1e8)

  # compute the prediction error on holdout
  MSE = mean( ( dd$m1[I]- JJ[I,] %*% fit$coefficients)^2)

  fids[, psi := fit$coefficients[nfid]]
  flog.info(" Ridge AKM lambda=%f var(psi)=%f mse=%f",lambda,fids[,wt.var(psi,size)]/sim$sdata[,var(y1)],MSE)
  list(fids=fids,mse=MSE)
}

#' Computes an estimate of the within variance of firm heterogeneity (see web appendix of BLM)
#' @param subsample when >=0 only uses a fixed number of mover for each firm (this weights by firms rather than weight by movers)
#' @export
m2.trace.reExt <- function(jdata,subsample=0) {

  nf = max(c(jdata$j1,jdata$j2))

  omega = rep(0,nf)
  for (l1 in 1:nf) {
    # extract the firms with 2 movers in this group
    num= 0; den=0;
    firm_in_grp = jdata[l1==j1,.N,f1][N>=2,f1]
    #flog.info("found %i firms for j1=%i",length(firm_in_grp),l1)
    if (length(firm_in_grp)>1) {
      for (firm_cur in firm_in_grp) {
        dt = jdata[f1==firm_cur]
        if (subsample>0) dt = dt[sample.int(.N,pmin(.N,subsample))];

        # create all pairs, make sure they move to different firms
        F2 = spread(dt$f2,2,nrow(dt))
        YY = spread(dt$y2 - dt$psi2 - dt$y1+dt$psi1,2,nrow(dt))

        # add this to the measure of variance!
        num = num + sum((F2 != t(F2)) * YY * t(YY))/2
        den = den + sum((F2 != t(F2)))/2
      }
    }
    firm_in_grp = jdata[l1==j2,.N,f2][N>=2,f2]
    if (length(firm_in_grp)>1) {
      for (firm_cur in firm_in_grp) {
        dt = jdata[f2==firm_cur]
        if (subsample>0) dt = dt[sample.int(.N,pmin(.N,subsample))];

        # create all pairs, make sure they move to different firms
        F1 = spread(dt$f1,2,nrow(dt))
        YY = spread(dt$y2 - dt$psi2 - dt$y1+dt$psi1,2,nrow(dt))

        # add this to the measure of variance!
        num = num + sum((F1 != t(F1)) * YY * t(YY))/2
        den = den + sum((F1 != t(F1)))/2
      }
    }
    if (den>0) omega[l1] = num/den;
    #flog.info("num=%f den=%f var=%f j1=%i",num,den,num/den,l1)
  }

  return(omega)
}


#' Computes an estimate of the within variance of firm heterogeneity, and the covariances to (see web appendix of BLM)
#' @param subsample when >=0 only uses a fixed number of mover for each firm (this weights by firms rather than weight by movers)
#' @export
m2.trace.reExt.all2 <- function(sdata,jdata,subsample=0,subsample.firms=0,include_covY1_terms=FALSE) {

  nf = max(c(jdata$j1,jdata$j2))

  # Variance of psi within
  cov_dYm_dYmc    = rep(0,nf)         # covariance between Y2-Y1 and Y2-Y1 for movers leaving from the same firm (+ movers goign to same firm)
  cov_Y1s_Y1sc   = rep(0,nf)          # covariance between Y1 and Y1coworker for stayers in the same firm
  cov_Y1s_dYmc   = rep(0,nf)          # covariance btween Y1 of stayers and Y2-Y1 of movers leaving same firm
  var_Y1s        = rep(0,nf)          # variance between Y1_i1 and Y1_i2 for stayers in the same firm
  var_dYm        = array(0,c(nf,nf))  # variance between Y1_i1 and Y1_i2 for stayers in the same firm

  flog.info("[CRE] Cov(Y2-Y1,Y2-Y1) for movers leaving from the same firm")
  for (l1 in 1:nf) {
    # extract the firms with 2 movers in this group
    num= 0; den=0;
    firm_in_grp = jdata[l1==j1,.N,f1][N>=2]
    #flog.info("[CRE] found %i firms in j1=%i",nrow(firm_in_grp),l1)
    if ((subsample.firms>0) & (nrow(firm_in_grp)>1)) firm_in_grp = firm_in_grp[sample.int(.N,pmin(.N,subsample.firms),prob=firm_in_grp$N)];
    firm_in_grp = firm_in_grp$f1

    flog.info("[CRE] processing %i firms for moving from j1=%i ",length(firm_in_grp),l1)
    if (length(firm_in_grp)>1) {
      count = 0
      last_time = as.numeric(Sys.time())
      for (firm_cur in firm_in_grp) {
        count = count+1
        dt = jdata[f1==firm_cur,list(f2,f1,y1,y2,psi1,psi2)]
        if (subsample>0) dt = dt[sample.int(.N,pmin(.N,subsample))];

        # create all pairs, make sure they move to different firms
        F2 = spread(dt$f2,2,nrow(dt))
        YY = spread(dt$y2 - dt$psi2 - dt$y1+dt$psi1,2,nrow(dt))

        # add this to the measure of variance!
        num = num + sum((F2 != t(F2)) * YY * t(YY))/2
        den = den + sum((F2 != t(F2)))/2
        if (as.numeric(Sys.time()) >= last_time + 10) {
          flog.info("[CRE] done with %i firms out of %i for j1=%i",count,length(firm_in_grp),l1)
          last_time = as.numeric(Sys.time())
        }
        rm(dt)
      }
    }

    firm_in_grp = jdata[l1==j2,.N,f2][N>=2]
    #flog.info("[CRE] found %i firms in j1=%i",nrow(firm_in_grp),l1)
    if ((subsample.firms>0) & (nrow(firm_in_grp)>1)) firm_in_grp = firm_in_grp[sample.int(.N,pmin(.N,subsample.firms),prob=firm_in_grp$N)];
    firm_in_grp = firm_in_grp$f2
    flog.info("[CRE] processing %i firms for moving to j1=%i",length(firm_in_grp),l1)
    if (length(firm_in_grp)>1) {
      count = 0
      last_time = as.numeric(Sys.time())
      for (firm_cur in firm_in_grp) {
        count = count+1
        dt = jdata[f2==firm_cur]
        if (subsample>0) dt = dt[sample.int(.N,pmin(.N,subsample))];

        # create all pairs, make sure they move to different firms
        F1 = spread(dt$f1,2,nrow(dt))
        YY = spread(dt$y2 - dt$psi2 - dt$y1+dt$psi1,2,nrow(dt))

        # add this to the measure of variance!
        num = num + sum((F1 != t(F1)) * YY * t(YY))/2
        den = den + sum((F1 != t(F1)))/2
        rm(dt)
        if (as.numeric(Sys.time()) >= last_time + 10) {
          flog.info("[CRE] done with %i firms out of %i for j2=%i",count,length(firm_in_grp),l1)
          last_time = as.numeric(Sys.time())
        }
      }
    }

    if (den>0) cov_dYm_dYmc[l1] = num/den;
    flog.info("[CRE] cov_dYm_dYmc num=%f den=%f var=%f j1=%i",num,den,num/den,l1)
  }


  if(include_covY1_terms){

    subsample = 100

    flog.info("CRE - cov(Y1,Y1c) between Y1 and Y1 for co-workers, stayers")
    # next we compute the covariance between pair of stayers at each firm
    for (l1 in 1:nf) {
      # extract the firms with at least 2 stayers
      num= 0; den=0;
      firm_in_grp = sdata[l1==j1,.N,f1][N>=2,f1]
      #flog.info("found %i firms for j1=%i",length(firm_in_grp),l1)
      if (length(firm_in_grp)>1) {
        for (firm_cur in firm_in_grp) {
          dt = sdata[f1==firm_cur]
          if (subsample>0) dt = dt[sample.int(.N,pmin(.N,subsample))];

          # create all pairs, not using when the worker is the same
          F2 = spread(1:nrow(dt),2,nrow(dt))
          YY = spread(dt$y1 - dt$psi1 - dt$mx,2,nrow(dt))

          # add this to the measure of variance!
          num = num + sum((F2 != t(F2)) * YY * t(YY))/2
          den = den + sum((F2 != t(F2)))/2
        }
      }
      if (den>0) cov_Y1s_Y1sc[l1] = num/den;
    }



    flog.info("CRE - cov_Y1s_dYmc, cov(Y1,Y2-Y1) between stayers and movers")
    # next we compute the covariance between pair of stayers with movers
    for (l1 in 1:nf) {
      # extract the firms with at least 2 stayers
      num= 0; den=0;
      firm_in_grp = sdata[l1==j1,.N,f1][N>=1,f1]
      flog.info("found %i firms for j1=%i",length(firm_in_grp),l1)
      if (length(firm_in_grp)>1) {
        for (firm_cur in firm_in_grp) {
          dts = sdata[f1==firm_cur]
          dtm = jdata[f1==firm_cur][j2!=l1]
          if (subsample>0) {
            dtm = dtm[sample.int(.N,pmin(.N,subsample))];
            dts = dts[sample.int(.N,pmin(.N,subsample))];
          }

          # create all pairs, not using when the worker is the same
          YY1 = spread(dts$y1 - dts$psi1 - dts$mx,2,nrow(dtm))
          YY2 = spread(dtm$y2 - dtm$y1 - dtm$psi2 + dtm$psi1,1,nrow(dts))

          # add this to the measure of variance!
          num = num + sum(YY1 * YY2)
          den = den + sum(YY1==YY1)
        }
      }
      if (den>0) cov_Y1s_dYmc[l1] = num/den;
    }


  }


  flog.info("CRE - Var(Y1) for stayers")
  # get the total variance for the stayers
  for (l1 in 1:nf) {
    var_Y1s[l1] = sdata[l1==j1,var(y1-psi1)]
  }

  flog.info("CRE - Var(dY) for movers")
  # get the growth variance of movers
  for (l1 in 1:nf) for (l2 in 1:nf) {
    var_dYm[l1,l2] = jdata[(l1==j1) & ( l2==j2), var(y2-psi2-y1+psi1)]
  }
  # ----- extracting the structural parameters -----
  psi_sd   = sqrt(pmax(cov_dYm_dYmc,0))
  #a_s_sign = sign(cov_Y1s_dYm)
  #a_s      = (sqrt(cov_Y1s_Y1sc)/psi_sd-1)/2
  #a_s      = (sqrt(cov_Y1s_Y1sc)/psi_sd-1)/2
  a_s      = 0.5*(-cov_Y1s_Y1sc/cov_Y1s_dYmc-1)

  # fixme
  eps_sd   = sqrt(pmax(0.5*diag(var_dYm) -cov_dYm_dYmc,0))
  xi_sd    = sqrt(pmax(var_Y1s - cov_Y1s_Y1sc - eps_sd^2,0))

  #EEvar    = mcast(jdata[,var(y1-psi1)-eps_sd[j1]^2-psi_sd[j1]^2,list(j1,j2)],'V1','j1','j2',c(nf,nf),0)
  EEvar    = mcast(jdata[,cov(y1,y2),list(j1,j2)],'V1','j1','j2',c(nf,nf),0)
  EEsd     = sqrt(pmax(EEvar,0,na.rm=TRUE))

  Evar     = sdata[,var(y1)-eps_sd[j1]^2-psi_sd[j1]^2,list(j1)][order(j1)][,V1]
  Esd      = sqrt(pmax(Evar,0,na.rm=TRUE))

  return(list(moments=list(
              cov_dYm_dYmc=cov_dYm_dYmc,
              cov_Y1s_Y1sc=cov_Y1s_Y1sc,
              cov_Y1s_dYmc=cov_Y1s_dYmc,
              var_Y1s=var_Y1s,var_dYm=var_dYm),
              p = list(psi_sd=psi_sd,a_s=a_s,eps_sd=eps_sd,xi_sd=xi_sd,EEsd=EEsd,Esd=Esd)))
}

#' Computes an estimate of the within variance of firm heterogeneity, and the covariances to (see web appendix of BLM)
#' @param subsample when >=0 only uses a fixed number of mover for each firm (this weights by firms rather than weight by movers)
#' @export
m2.trace.reExt.all <- function(sdata,jdata,subsample=0,subsample.firms=0,include_covY1_terms=FALSE) {

  nf = max(c(jdata$j1,jdata$j2))

  # Variance of psi within
  cov_dYm_dYmc    = rep(0,nf)         # covariance between Y2-Y1 and Y2-Y1 for movers leaving from the same firm (+ movers goign to same firm)
  cov_Y1s_Y1sc    = rep(0,nf)          # covariance between Y1 and Y1coworker for stayers in the same firm
  cov_Y1s_dYmc    = rep(0,nf)          # covariance btween Y1 of stayers and Y2-Y1 of movers leaving same firm
  var_Y1s         = rep(0,nf)          # variance between Y1_i1 and Y1_i2 for stayers in the same firm
  var_dYm         = array(0,c(nf,nf))  # variance between Y1_i1 and Y1_i2 for stayers in the same firm

  flog.info("[CRE] Cov(Y2-Y1,Y2-Y1) for movers leaving from the same firm")
  for (l1 in 1:nf) {
    accu=NA

    firm_in_grp = jdata[l1==j1,.N,f1][N>=2] # select firms with at least 2 movers
    # subsample if necessary
    if ((subsample.firms>0) & (nrow(firm_in_grp)>1)) firm_in_grp = firm_in_grp[sample.int(.N,pmin(.N,subsample.firms),prob=firm_in_grp$N)];
    firm_in_grp = firm_in_grp$f1

    flog.info("[CRE] processing %i firms for moving from j1=%i ",length(firm_in_grp),l1)
    if (length(firm_in_grp)>1) {
      last_time = as.numeric(Sys.time())

      for (firm_cur in firm_in_grp) {
        dt = jdata[f1==firm_cur,list(y=y1-psi1-(y2-psi2),c=j2)]
        if (subsample>0) dt = dt[sample.int(.N,pmin(.N,subsample))];
        accu = accu.moms.pairs_exlcude(dt$y, dt$y, dt$c, dt$c, accu)
        rm(dt)
      }
    }

    firm_in_grp = jdata[l1==j2,.N,f2][N>=2] # select firms with at least 2 movers
    if ((subsample.firms>0) & (nrow(firm_in_grp)>1)) firm_in_grp = firm_in_grp[sample.int(.N,pmin(.N,subsample.firms),prob=firm_in_grp$N)];
    firm_in_grp = firm_in_grp$f2

    flog.info("[CRE] processing %i firms for moving to j2=%i",length(firm_in_grp),l1)
    if (length(firm_in_grp)>1) {
      for (firm_cur in firm_in_grp) {
        dt = jdata[f2==firm_cur,list(y=y2-psi2-(y1-psi1),c=j1)]
        if (subsample>0) dt = dt[sample.int(.N,pmin(.N,subsample))];
        accu = accu.moms.pairs_exlcude(dt$y, dt$y, dt$c, dt$c, accu)
        rm(dt)
      }
    }

    # combine the accumulated moments
    cov_dYm_dYmc[l1] = accu.compute(accu)$cov
  }

  flog.info("CRE - cov_Y1s_dYmc, cov(Y1,Y2-Y1) between stayers and movers")
  # next we compute the covariance between pair of stayers with movers
  for (l1 in 1:nf) {
    accu=NA
    # extract the firms with at least 2 stayers
    firm_in_grp = sdata[l1==j1,.N,f1][N>=1,f1]

    #flog.info("found %i firms for j1=%i",length(firm_in_grp),l1)
    if (length(firm_in_grp)>1) {
      for (firm_cur in firm_in_grp) {

        dts = sdata[f1==firm_cur,list(y= y1 - psi1 - mx)]
        dtm = jdata[f1==firm_cur,list(y= y1 - psi1 - (y2-psi2))]
        if (subsample>0) {
          dtm = dtm[sample.int(.N,pmin(.N,subsample))];
          dts = dts[sample.int(.N,pmin(.N,subsample))];
        }
        accu = accu.moms.pairs(dtm$y, dts$y, accu)
      }
    }
    cov_Y1s_dYmc[l1] = accu.compute(accu)$cov;
  }

  flog.info("CRE - Var(Y1) for stayers")
  # get the total variance for the stayers
  for (l1 in 1:nf) {
    var_Y1s[l1] = sdata[l1==j1,var(y1-psi1)]
  }

  flog.info("CRE - Var(dY) for movers")
  # get the growth variance of movers
  for (l1 in 1:nf) for (l2 in 1:nf) {
    var_dYm[l1,l2] = jdata[(l1==j1) & ( l2==j2), var(y2-psi2-y1+psi1)]
  }
  # ----- extracting the structural parameters -----
  psi_sd   = sqrt(pmax(cov_dYm_dYmc,0))
  #a_s_sign = sign(cov_Y1s_dYm)
  #a_s      = (sqrt(cov_Y1s_Y1sc)/psi_sd-1)/2
  #a_s      = (sqrt(cov_Y1s_Y1sc)/psi_sd-1)/2
  a_s      = 0.5*(-cov_Y1s_Y1sc/cov_Y1s_dYmc-1)

  # fixme
  eps_sd   = sqrt(pmax(0.5*diag(var_dYm) -cov_dYm_dYmc,0))
  xi_sd    = sqrt(pmax(var_Y1s - cov_Y1s_Y1sc - eps_sd^2,0))

  #EEvar    = mcast(jdata[,var(y1-psi1)-eps_sd[j1]^2-psi_sd[j1]^2,list(j1,j2)],'V1','j1','j2',c(nf,nf),0)
  EEvar    = mcast(jdata[,cov(y1,y2),list(j1,j2)],'V1','j1','j2',c(nf,nf),0)
  EEsd     = sqrt(pmax(EEvar,0,na.rm=TRUE))

  Evar     = sdata[,var(y1)-eps_sd[j1]^2-psi_sd[j1]^2,list(j1)][order(j1)][,V1]
  Esd      = sqrt(pmax(Evar,0,na.rm=TRUE))

  return(list(moments=list(
    cov_dYm_dYmc=cov_dYm_dYmc,
    cov_Y1s_Y1sc=cov_Y1s_Y1sc,
    cov_Y1s_dYmc=cov_Y1s_dYmc,
    var_Y1s=var_Y1s,var_dYm=var_dYm),
    p = list(psi_sd=psi_sd,a_s=a_s,eps_sd=eps_sd,xi_sd=xi_sd,EEsd=EEsd,Esd=Esd)))
}


#' Computes an estimate of the within variance of firm heterogeneity, and the covariances to (see web appendix of BLM)
#' @param subsample when >=0 only uses a fixed number of mover for each firm (this weights by firms rather than weight by movers)
#' @export
m2.trace.allcovs <- function(sdata,jdata,opts) {

  ng = max(c(jdata$j1,jdata$j2))
  flog.info("[CRE-MOMS] starting")

  # index firms with integers
  fids = jdata[,unique(c(f1,f2))]
  fids = data.table(f1=fids,nfid=1:length(fids))

  setkey(fids,f1)
  setkey(jdata,f1)
  jdata[,f1i := fids[jdata,nfid]]
  setkey(jdata,f2)
  jdata[,f2i := fids[jdata,nfid]]
  setkey(sdata,f1)
  sdata[,f1i := fids[sdata,nfid]]

  # we first iterate on clusters
  for (l1 in 1:ng) {
    accu=NA

    firm_in_grp = jdata[j1==l1,unique(c(f1,f2))]
    flog.info("[CRE-MOMS] processing %i firms in cluster %i",length(firm_in_grp),l1)

    accu_s_m11 = accu.new()
    accu_s_m12 = accu.new()
    accu_s_m21 = accu.new()
    accu_s_m22 = accu.new()
    accu_s_s   = accu.new()
    accu_m11_m11 = accu.new()
    accu_m11_m12 = accu.new()
    accu_m12_m12 = accu.new()
    accu_m21_m21 = accu.new()
    accu_m21_m22 = accu.new()
    accu_m22_m22 = accu.new()
    accu_m11_m21 = accu.new()
    accu_m11_m22 = accu.new()
    accu_m12_m21 = accu.new()
    accu_m12_m22 = accu.new()

    for (firm_cur in firm_in_grp) {
      # extract all relevant workers
      dm1 = jdata[f1==firm_cur,list(y1=y1-psi1-mx,y2=y2-psi2-mx,i=1:.N,j1,j2,f1=f1i,f2=f2i)]
      dm2 = jdata[f2==firm_cur,list(y1=y1-psi1-mx,y2=y2-psi2-mx,i=1:.N,j1,j2,f1=f1i,f2=f2i)]
      ds  = sdata[f1==firm_cur,list(y=y1-psi1-mx,i=1:.N)]

      if ((nrow(dm1)<2) | (nrow(dm2)<2) | (nrow(ds)<2)) {
        next
      }

      accu_s_m11 = accu.moms.pairs(ds$y,dm1$y1,accu_s_m11 )
      accu_s_m12 = accu.moms.pairs(ds$y,dm1$y2,accu_s_m12 )
      accu_s_m21 = accu.moms.pairs(ds$y,dm2$y1,accu_s_m21 )
      accu_s_m22 = accu.moms.pairs(ds$y,dm2$y2,accu_s_m22 )
      accu_s_s   = accu.moms.pairs_exclude(ds$y,ds$y,ds$i,ds$i,accu_s_s)

      # movers out of firm_cur with themsevles
      accu_m11_m11 = accu.moms.pairs_exclude(dm1$y1,dm1$y1,dm1$f2,dm1$f2,accu_m11_m11 )
      accu_m11_m12 = accu.moms.pairs_exclude(dm1$y1,dm1$y2,dm1$f2,dm1$f2,accu_m11_m12 )
      accu_m12_m12 = accu.moms.pairs_exclude(dm1$y2,dm1$y2,dm1$f2,dm1$f2,accu_m12_m12 )

      # movers into firm_cur with themsevles
      accu_m21_m21 = accu.moms.pairs_exclude(dm2$y1,dm2$y1,dm2$f1,dm2$f1,accu_m21_m21 )
      accu_m21_m22 = accu.moms.pairs_exclude(dm2$y1,dm2$y2,dm2$f1,dm2$f1,accu_m21_m22 )
      accu_m22_m22 = accu.moms.pairs_exclude(dm2$y2,dm2$y2,dm2$f1,dm2$f1,accu_m22_m22 )

      # movers out of firm_cur with movers into firm_cur
      accu_m11_m21 = accu.moms.pairs_exclude(dm1$y1,dm2$y1,dm1$f2,dm2$f1,accu_m11_m21 )
      accu_m11_m22 = accu.moms.pairs_exclude(dm1$y1,dm2$y2,dm1$f2,dm2$f1,accu_m11_m22 )
      accu_m12_m21 = accu.moms.pairs_exclude(dm1$y2,dm2$y1,dm1$f2,dm2$f1,accu_m12_m21 )
      accu_m12_m22 = accu.moms.pairs_exclude(dm1$y2,dm2$y2,dm1$f2,dm2$f1,accu_m12_m22 )
    }
  }

  rr = data.frame(); ff = function(name,accu) data.frame(name=name , cov=accu.compute(accu)$cov, N=accu$n )
  rr = rbind(rr, ff("accu_s_m11",accu_s_m11) )
  rr = rbind(rr, ff("accu_s_m12",accu_s_m12) )
  rr = rbind(rr, ff("accu_s_m21",accu_s_m21) )
  rr = rbind(rr, ff("accu_s_m22",accu_s_m22) )
  rr = rbind(rr, ff("accu_s_s"  ,accu_s_s) )
  rr = rbind(rr, ff("accu_m11_m11",accu_m11_m11))
  rr = rbind(rr, ff("accu_m11_m12",accu_m11_m12))
  rr = rbind(rr, ff("accu_m12_m12",accu_m12_m12))
  rr = rbind(rr, ff("accu_m21_m21",accu_m21_m21))
  rr = rbind(rr, ff("accu_m21_m22",accu_m21_m22))
  rr = rbind(rr, ff("accu_m22_m22",accu_m22_m22))
  rr = rbind(rr, ff("accu_m11_m21",accu_m11_m21))
  rr = rbind(rr, ff("accu_m11_m22",accu_m11_m22))
  rr = rbind(rr, ff("accu_m12_m21",accu_m12_m21))
  rr = rbind(rr, ff("accu_m12_m22",accu_m12_m22))

  print(rr)
  return(rr)
}


#' Computes an estimate of the within variance of firm heterogeneity, and the covariances to (see web appendix of BLM)
#' @param subsample when >=0 only uses a fixed number of mover for each firm (this weights by firms rather than weight by movers)
#' @export
m2.trace.allcovs.bis <- function(sdata,jdata,opts) {
  # in this variant we are going to directly rely on
  # formula equivalence suing Cov(y1, y2_bar_j)

  ng = max(c(jdata$j1,jdata$j2))
  flog.info("[CRE-MOMS] starting")

  # we construct movers moving in and movers moving out, and stayers
  dm1 = jdata[,list(y1=y1-psi1-mx,y2=y2-psi2-mx,j1,j2,f1=f1,f2=f2)]
  dm2 = jdata[,list(y1=y1-psi1-mx,y2=y2-psi2-mx,j1,j2,f1=f1,f2=f2)]
  ds  = sdata[,list(y1=y1-psi1-mx,f1,j1)]

  # get averages and sizes
  dm1.f = dm1[,list(ym11j=mean(y1),ym12j = mean(y2), nm1j = .N),list(f=f1)]
  dm2.f = dm2[,list(ym21j=mean(y1),ym22j = mean(y2), nm2j = .N),list(f=f2)]
  ds[, y1sj := mean(y1),f1]
  ds[, nsj := .N,f1]

  # merge averages back into data
  ds = merge(ds,dm1.f,by.x="f1",by.y="f")
  ds = merge(ds,dm2.f,by.x="f1",by.y="f")
  dm1 = merge(dm1,dm1.f,by.x="f1",by.y="f")
  dm2 = merge(dm2,dm2.f,by.x="f2",by.y="f")
  dm1 = merge(dm1,dm2.f,by.x="f1",by.y="f")

  # fixme, exclude mover pairs that go to same cluster

  # finally we compute the moments
  res = data.frame()
  res = rbind(res, ds[nsj>1,   list(name="s_s",.N,value=cov(y1, (nsj*y1sj - y1)/(nsj-1) ))])
  res = rbind(res, ds[nm1j>0,  list(name="s_m11",.N,value=cov(y1,ym11j))])
  res = rbind(res, ds[nm1j>0,  list(name="s_m12",.N,value=cov(y1,ym12j))])
  res = rbind(res, ds[nm2j>0,  list(name="s_m21",.N,value=cov(y1,ym21j))])
  res = rbind(res, ds[nm2j>0,  list(name="s_m22",.N,value=cov(y1,ym22j))])
  res = rbind(res, dm1[nm1j>1, list(name="m11_m11",.N,value=cov(y1, (nm1j*ym11j - y1)/(nm1j-1) ))])
  res = rbind(res, dm1[nm1j>1, list(name="m11_m12",.N,value=cov(y1, (nm1j*ym12j - y2)/(nm1j-1) ))])
  res = rbind(res, dm1[nm1j>1, list(name="m12_m12",.N,value=cov(y2, (nm1j*ym12j - y2)/(nm1j-1) ))])
  res = rbind(res, dm2[nm2j>1, list(name="m21_m21",.N,value=cov(y1, (nm2j*ym21j - y1)/(nm2j-1) ))])
  res = rbind(res, dm2[nm2j>1, list(name="m21_m22",.N,value=cov(y1, (nm2j*ym22j - y2)/(nm2j-1) ))])
  res = rbind(res, dm2[nm2j>1, list(name="m22_m22",.N,value=cov(y2, (nm2j*ym22j - y2)/(nm2j-1) ))])
  res = rbind(res, dm1[nm2j>0, list(name="m11_m21",.N,value=cov(y1,ym21j))])
  res = rbind(res, dm1[nm2j>0, list(name="m11_m22",.N,value=cov(y1,ym22j))])
  res = rbind(res, dm1[nm2j>0, list(name="m12_m21",.N,value=cov(y2,ym21j))])
  res = rbind(res, dm1[nm2j>0, list(name="m12_m22",.N,value=cov(y2,ym22j))])

  # extract parameters
  p = list()
  p$cov_Am1Am2  = res[name=="m12_m21",value]
  p$cov_Am2Psi1 = res[name=="m11_m21",value] - p$cov_Am1Am2
  p$cov_Am1Psi2 = res[name=="m12_m22",value] - p$cov_Am1Am2
  p$var_psi_m12 = res[name=="m11_m22",value] - p$cov_Am1Am2 - p$cov_Am2Psi1 - p$cov_Am1Psi2

  # using movers leaving from 1
  p$cov_Am1Am1  = res[name=="m12_m12",value]
  p$cov_Am1Psi1 = res[name=="m11_m12",value] - p$cov_Am1Am1
  p$var_psi_m1  = res[name=="m11_m11",value] - p$cov_Am1Am1 - 2*p$cov_Am1Psi1

  # using movers leaving from 2
  p$cov_Am2Am2  = res[name=="m21_m21",value]
  p$cov_Am2Psi2 = res[name=="m21_m22",value] - p$cov_Am2Am2
  p$var_psi_m2  = res[name=="m22_m22",value] - p$cov_Am2Am2 - 2*p$cov_Am2Psi2

  # looking at stayers
  p$cov_AsAm1 = res[name=="s_m12",value] - p$cov_Am1Psi1
  p$cov_AsAm2 = res[name=="s_m21",value] - p$cov_Am2Psi2
  p$psi_plus_cov1 = res[name=="s_m11",value] - res[name=="s_m12",value]
  p$psi_plus_cov2 = res[name=="s_m22",value] - res[name=="s_m21",value]

  p$var_psi = (p$var_psi_m2 + p$var_psi_m1)/2
  p$cov_AsPsi1 = (p$psi_plus_cov1 + p$psi_plus_cov2) - p$var_psi
  p$cov_AsAs  = res[name=="s_s",value] - p$var_psi - 2*p$cov_AsPsi1

  return(list(moments=res,params=p))
}


# -------- HYBRID ESTIMATOR ----------


check.data <- function(sim) {
  sf1 = sim$sdata[,unique(f1)]
  jf1 = sim$jdata[,unique(f1)]
  jf2 = sim$jdata[,unique(f2)]

  # compute firms in sdata but not in jdata
  flog.info(" %i firms in sdata but not in jdata",   length(setdiff( sf1, c(jf1,jf2) )  ))
  flog.info(" %i firms in jdata.f1 but not in sdata",length(setdiff( jf1, sf1 )))
  flog.info(" %i firms in jdata.f2 but not in sdata",length(setdiff( jf2, sf1 )))
  flog.info(" %i firms in p2 but not in p1",length(setdiff( jf2, c(sf1,jf1 ))))
}

#' @export
m2.hyb.add_data_stats <- function(rr,sim,...) {
  rr = m2.hyb.add_stat(rr,"nf"      ,length(unique(c(sim$jdata[,unique(c(f1,f2))],sim$sdata$f1))),...)
  rr = m2.hyb.add_stat(rr,"nw"      ,sim$sdata[,.N] + sim$jdata[,.N],...)
  rr = m2.hyb.add_stat(rr,"wage_var",sim$sdata[,var(y1)],...)
  rr = m2.hyb.add_stat(rr,"btw_firm",sim$sdata[,list(mean(y1),.N),f1][,wt.var(V1,N)],...)
  return(rr)
}

#' @export
m2.hyb.add_akmtrace_stats <- function(rr,res,...) {
  rr = m2.hyb.add_stat(rr,"psi_var"      ,res$var_psi,...)
  rr = m2.hyb.add_stat(rr,"cov"          ,res$jids[,wt.cov(mw-psi,psi,size)],...)
  rr = m2.hyb.add_stat(rr,"trace"        ,res$stats$trace_term,...)
  if ("omega" %in% names(res$jids))  rr = m2.hyb.add_stat(rr,"omega_var"    ,res$jids[,wt.mean(omega,size)],...);
  if ("omega" %in% names(res$jids))  rr = m2.hyb.add_stat(rr,"omega_var_pos"    ,res$jids[,wt.mean(pmax(omega,0),size)],...);


  if (nrow(res$jids[own_firm_cluster==TRUE])>2) {
    rr = m2.hyb.add_stat(rr,"psi_var_own",res$jids[own_firm_cluster==TRUE,wt.var(psi,size)],...)
  } else {
    rr = m2.hyb.add_stat(rr,"psi_var_own",NA,...)
  }

  rr
}

#' @export
m2.hyb.add_stat <- function(rr,variable,value,...) {
  r1 = data.frame(list(...))
  r0 =cbind(r1,data.frame(variable=variable,value=value))
  return(data.table(rbind(rr,r0)))
}


#' estimates a hybrid between BLM and AKM
#'
#' the function returns a bunch of stuff. It returns info about different sets:
#'  - set0 is just the data it starts with at each nm
#'  - set1 is AKM connected set
#'  - set2 is the leave-out connected set
#'  - set3 is the connected set only among firms with at least nm movers
#'
#' @export
m2.trace.blmhyb <- function(sim,use_con=FALSE,nclus=10,nm_list=c(seq(1,10),seq(20,50,by=5),100,150,Inf)) {

  # extract connected set
  if (use_con==T) {
    f0s   = get.largest.conset.fid(sim$jdata)
    jdata = sim$jdata[f1%in%f0s][f2%in%f0s]
    sdata = sim$sdata[f1%in%f0s]
  }

  # save a copy of the data
  sim.copy = copy(sim)

  # next we group differently,
  res_all = data.frame()
  for (nm in nm_list) {

    stats = list()
    sim = copy(sim.copy)

    # select firms from AKM
    if (use_con==T) {
      sim$sdata = sim$sdata[f1 %in% f0s]
      sim$jdata = sim$jdata[f1 %in% f0s][f2 %in%f0s]
    }

    # ------- (0) STATS ON CONNECTED SET ------ #
    # we find firms with at least nm movers
    large.firms = sim$jdata[,list(f1=c(f1,f2))][,.N,f1][N>=nm,f1]
    res_all     = res_all %>%
                m2.hyb.add_data_stats(sim,nm=nm,set=0) %>%
                m2.hyb.add_stat("firms_with_own_type",length(large.firms),nm=nm,set=0)

    # we cluster the other firms
    if (nm>1) {
      ms    = grouping.getMeasures(list(sdata = sim$sdata[!(f1 %in% large.firms)],jdata=sim$jdata),"ecdf",Nw=20,y_var = "y1")
      grps  = grouping.classify.once(ms,k = nclus,nstart = 1000,iter.max = 200,step=100)

      # we create the full classification
      clus0 = grps$best_cluster
      cluster_with_many_firms = sort(unique(clus0))
      clus1 = 1:length(large.firms) + nclus
      names(clus1) = large.firms
      clus = c(clus0,clus1)
    } else {
      cluster_with_many_firms = c()
      clus = 1:length(large.firms)
      names(clus) = large.firms
    }

    # append the clusters
    sim  = grouping.append(sim,clus,drop=T,sort=FALSE)

    # ------- (1) AKM + ANDREWS ------ #
    fids_con   = get.largest.conset.clus(sim$jdata)
    sim$jdata  = sim$jdata[j1%in%fids_con][j2%in%fids_con]
    sim$sdata  = sim$sdata[j1%in%fids_con][j2%in%fids_con]
    res_all %<>% m2.hyb.add_data_stats(sim,nm=nm,set=1)

    res_hybrid = m2.trace.estimate.clus(sim,within_re = TRUE)
    res_hybrid$jids[,own_firm_cluster := !(j1 %in% cluster_with_many_firms) ]
    res_all %<>% m2.hyb.add_akmtrace_stats(res_hybrid,nm=nm,set=1)

    # ------- (2) HETERO TRACE ------ #
    sim$jdata = get.largest.leaveoutset.clus(sim$jdata)
    jids_con  = sim$jdata[,unique(c(j1,j2))]
    sim$sdata = sim$sdata[j1%in%jids_con][j2%in%jids_con]
    res_all %<>% m2.hyb.add_data_stats(sim,nm=nm,set=2)

    res_hybrid  = m2.trace.estimate.clus(sim,hetero = T,within_re = TRUE)
    res_hybrid$jids[,own_firm_cluster := !(j1 %in% cluster_with_many_firms) ]
    res_all %<>% m2.hyb.add_akmtrace_stats(res_hybrid,nm=nm,set=2)

    # ------- (3) AKM ON FIRM WITH OWN TYPE ------ #
    sim = copy(sim.copy)
    sim$sdata = sim$sdata[f1 %in% large.firms]
    sim$jdata = sim$jdata[f1 %in% large.firms][f2 %in% large.firms]
    res_all %<>% m2.hyb.add_data_stats(sim,nm=nm,set=3)

    if (nrow(sim$jdata)>0) if (sim$jdata[,length(unique(c(f1,f2)))]>1) {
      fids_con   = get.largest.conset.fid(sim$jdata)
      sim$jdata  = sim$jdata[f1%in%fids_con][f2%in%fids_con]
      sim$sdata  = sim$sdata[f1%in%fids_con][f2%in%fids_con]
      if (nrow(sim$jdata)>0) {
        res_hybrid  = m2.trace.estimate(sim)
        res_hybrid$jids[,own_firm_cluster := 1]
        res_all %<>% m2.hyb.add_akmtrace_stats(res_hybrid,nm=nm,set=3)
      }
    }

    # ------- (4) AKM-HETERO ON FIRM WITH OWN TYPE ------ #
    sim = copy(sim.copy)
    sim$sdata = sim$sdata[f1 %in% large.firms]
    sim$jdata = sim$jdata[f1 %in% large.firms][f2 %in% large.firms]

    if (nrow(sim$jdata)>0) if (sim$jdata[,length(unique(c(f1,f2)))]>1) {
      # leave-one-out
      sim$jdata = get.largest.leaveoutset.fid(sim$jdata)
      if (nrow(sim$jdata)>0) {
        jids_con  = sim$jdata[,unique(c(f1,f2))]
        sim$sdata = sim$sdata[f1%in%jids_con][f2%in%jids_con]
        res_all %<>% m2.hyb.add_data_stats(sim,nm=nm,set=4)

        res_hybrid  = m2.trace.estimate(sim,hetero = T)
        res_hybrid$jids[,own_firm_cluster := 1]
        res_all %<>% m2.hyb.add_akmtrace_stats(res_hybrid,nm=nm,set=4)
      }
    }
  }

  return(res_all)
}




