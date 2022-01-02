# testing some stuff
library(smfe)
library(testthat)

create_obs <- function(data,...) {
  dd=data.frame(list(...))
  dd$id = nrow(data) + 1:nrow(dd)
  return(data.table(rbind(data,dd)))
}

context("testing linear estimators")

test_that("Testing leave-one-out connected set", {

  jdata = data.table(wid=1:4,f1=c(1,1,2,3),f2=c(2,3,3,4))
  jdata = data.table(jdata)
  res = get.largest.leaveoutset.fid(jdata)
  expect_true(all(  sort(res$wid) == c(1,2,3) ))

  jdata = data.table(wid=1:5,f1=c(1,1,2,3,5),f2=c(2,3,3,4,5))
  res = get.largest.leaveoutset.fid(jdata)
  expect_true(all(  sort(res$wid) == c(1,2,3) ))

  jdata = data.table(wid=1:6,f1=c(1,1,2,3,4,5),
                             f2=c(2,3,3,4,5,4))
  res = get.largest.leaveoutset.fid(jdata)
  expect_true(all(  sort(res$wid) == c(1,2,3) ))

  jdata = data.table(wid=1:6,f1=c(1,1,2,3,4,5),
                     f2=c(2,3,3,4,5,4))
  jdata[,f1:=paste(f1)][,f2:=paste(f2)]
  res = get.largest.leaveoutset.fid(jdata)
  expect_true(all(  sort(res$wid) == c(1,2,3) ))

  jdata = data.table(wid=1:6,j1=c(1,1,2,3,4,5),
                     j2=c(2,3,3,4,5,4))
  res = get.largest.leaveoutset.clus(jdata)
  expect_true(all(  sort(res$wid) == c(1,2,3) ))

  # try empyt set
  jdata = data.frame()   %>%
    create_obs(f1="1",f2="2",y1=0,y2=1) %>%
    create_obs(f1="2",f2="3",y1=0,y2=1)
  jdata[,f1 := paste(f1)][,f2:=paste(f2)]
  res = get.largest.leaveoutset.fid(jdata)
  expect_true(nrow(res)==0)

  # testing using string names
  jdata = data.table(wid=paste(1:6),f1=c(1,1,2,3,4,5),
                     f2=c(2,3,3,4,5,4))
  fnames = c("g","h","u","t","b")
  jdata[,f1:=fnames[f1]][,f2:=fnames[f2]]
  res = get.largest.leaveoutset.fid(jdata)
  expect_true(all(  sort(res$wid) == c(1,2,3) ))


})

test_that("Testing connected set", {

  jdata = data.table(wid=1:4,f1=c(1,1,2,3),f2=c(2,3,3,4))
  res = get.largest.conset.fid(jdata)
  expect_true(all(  sort(res) == c(1,2,3,4) ))

  jdata[,f1:=paste(f1)][,f2:=paste(f2)]
  res = get.largest.conset.fid(jdata)
  expect_true(all(  sort(res) == c("1","2","3","4") ))

  jdata = data.frame()   %>%
    create_obs(f1="1",f2="2",y1=0,y2=1) %>%
    create_obs(f1="1",f2="2",y1=0,y2=1) %>%
    create_obs(f1="2",f2="1",y1=1,y2=0) %>%
    create_obs(f1="2",f2="1",y1=1,y2=0)
  f1s   = get.largest.conset.fid(jdata)

})


test_that("Testing simple estimation on connected and leave-one-out connected", {
  model   = m2.trace.new(nf = 5,nm = 200,eps_sd=0)
  model$S[] = 1
  sim     = m2.trace.simulate(model)
  res     = m2.trace.estimate(sim,hetero=T)
  expect_true( sum(  (model$psi - mean(model$psi) - res$jids[order(f1)][,psi-mean(psi)])^2 ) < 1e-20)

  res     = m2.trace.estimate(sim,hetero=F)
  expect_true( sum(  (model$psi - mean(model$psi) - res$jids[order(f1)][,psi-mean(psi)])^2 ) < 1e-20)
})

test_that("Testing simple estimation with few firms", {

  jdata = data.frame()   %>%
    create_obs(f1="1",f2="2",y1=0,y2=1) %>%
    create_obs(f1="1",f2="2",y1=0,y2=1) %>%
    create_obs(f1="2",f2="1",y1=1,y2=0) %>%
    create_obs(f1="2",f2="1",y1=1,y2=0)
  sdata = data.frame()   %>%
    create_obs(f1="1",f2="1",y1=0,y2=0) %>%
    create_obs(f1="1",f2="1",y1=0,y2=0) %>%
    create_obs(f1="2",f2="2",y1=1,y2=1) %>%
    create_obs(f1="2",f2="2",y1=1,y2=1)
  jdata[,f1:=paste(f1)][,f2:=paste(f2)]
  sdata[,f1:=paste(f1)][,f2:=paste(f2)]
  sim= list(jdata=jdata,sdata=sdata)
  res     = m2.trace.estimate(sim,hetero=F)
  expect_true( sum(  (c(-0.5,0.5) - res$jids[order(f1)][,psi-mean(psi)])^2 ) < 1e-20)

  jdata = data.frame()   %>%
    create_obs(f1="1",f2="2",y1=0,y2=1) %>%
    create_obs(f1="1",f2="2",y1=0,y2=1) %>%
    create_obs(f1="1",f2="3",y1=0,y2=2) %>%
    create_obs(f1="3",f2="1",y1=2,y2=0) %>%
    create_obs(f1="3",f2="2",y1=2,y2=1)
  sdata = data.frame()   %>%
    create_obs(f1="1",f2="1",y1=0,y2=0) %>%
    create_obs(f1="1",f2="1",y1=0,y2=0) %>%
    create_obs(f1="3",f2="3",y1=2,y2=2) %>%
    create_obs(f1="3",f2="3",y1=2,y2=2) %>%
    create_obs(f1="2",f2="2",y1=1,y2=1) %>%
    create_obs(f1="2",f2="2",y1=1,y2=1)
  jdata[,f1:=paste(f1)][,f2:=paste(f2)]
  sdata[,f1:=paste(f1)][,f2:=paste(f2)]
  sim= list(jdata=jdata,sdata=sdata)
  res     = m2.trace.estimate(sim,hetero=F)
  expect_true( mean(  (c(-1,0,1) - res$jids[order(f1)][,psi-mean(psi)])^2 ) < 1e-20)

  jdata = data.frame()   %>%
    create_obs(f1="1",f2="2",j1=1,j2=2,y1=0,y2=1) %>%
    create_obs(f1="1",f2="2",j1=1,j2=2,y1=0,y2=1) %>%
    create_obs(f1="1",f2="3",j1=1,j2=3,y1=0,y2=2) %>%
    create_obs(f1="3",f2="1",j1=3,j2=1,y1=2,y2=0) %>%
    create_obs(f1="3",f2="2",j1=3,j2=2,y1=2,y2=1)
  sdata = data.frame()   %>%
    create_obs(f1="1",f2="1",j1=1,j1=1,y1=0,y2=0) %>%
    create_obs(f1="1",f2="1",j1=1,j1=1,y1=0,y2=0) %>%
    create_obs(f1="3",f2="3",j1=3,j1=3,y1=2,y2=2) %>%
    create_obs(f1="3",f2="3",j1=3,j1=3,y1=2,y2=2) %>%
    create_obs(f1="2",f2="2",j1=2,j1=2,y1=1,y2=1) %>%
    create_obs(f1="2",f2="2",j1=2,j1=2,y1=1,y2=1)
  jdata[,f1:=paste(f1)][,f2:=paste(f2)]
  sdata[,f1:=paste(f1)][,f2:=paste(f2)]
  sim= list(jdata=jdata,sdata=sdata)
  res     = m2.trace.estimate.clus(sim,hetero=F)
  expect_true( mean(  (c(-1,0,1) - res$jids[order(j1)][,psi-mean(psi)])^2 ) < 1e-20)

  jdata = data.frame()   %>%
    create_obs(f1="1",f2="2",j1=1,j2=2,y1=0,y2=1) %>%
    create_obs(f1="1",f2="2",j1=1,j2=2,y1=0,y2=1) %>%
    create_obs(f1="1",f2="3",j1=1,j2=3,y1=0,y2=2)
  sdata = data.frame()   %>%
    create_obs(f1="1",f2="1",j1=1,j1=1,y1=0,y2=0) %>%
    create_obs(f1="1",f2="1",j1=1,j1=1,y1=0,y2=0) %>%
    create_obs(f1="3",f2="3",j1=3,j1=3,y1=2,y2=2) %>%
    create_obs(f1="3",f2="3",j1=3,j1=3,y1=2,y2=2) %>%
    create_obs(f1="2",f2="2",j1=2,j1=2,y1=1,y2=1) %>%
    create_obs(f1="2",f2="2",j1=2,j1=2,y1=1,y2=1)
  jdata[,f1:=paste(f1)][,f2:=paste(f2)]
  sdata[,f1:=paste(f1)][,f2:=paste(f2)]
  sim= list(jdata=jdata,sdata=sdata)
  res     = m2.trace.estimate.clus(sim,hetero=F)
  expect_true( mean(  (c(-1,0,1) - res$jids[order(j1)][,psi-mean(psi)])^2 ) < 1e-20)

})


test.m2.trace.homoskedastic <- function() {
  require(rmutil)
  model   = m2.trace.new(nf = 1000,nm = 3000,eps_sd=1.5)
  sim     = m2.trace.simulate(model)

  res     = m2.trace.estimate(sim,model0=model,hetero=F)
  res$stats$trace_term
}


test.m2.trace <- function() {
  require(rmutil)

  model   = m2.trace.new(nf = 1000,nm = 3000,eps_sd=1.5)
  sim     = m2.trace.simulate(model)

  # cluster
  ms    = grouping.getMeasures(sim,"ecdf",Nw=20,y_var = "y1")
  grps  = grouping.classify.once(ms,k = 5,nstart = 1000,iter.max = 200,step=100)
  sim   = grouping.append(sim,grps$best_cluster)

  res     = m2.trace.estimate(sim,model0=model,hetero=T)
  res$stats$trace_term
  res     = m2.trace.estimate(sim,model0=model,hetero=F)
  res$stats$trace_term

  # estimate BLM
  res     = m2.mini.estimate(sim$jdata,sim$sdata,method = "linear.ss")

  # estimate RIDGE AKM
  rr = m2.firmfe.pen(sim,res,lambda=10)

  # check if things match at all
  ff = sim$sdata[,list(psi=psi1[1],N=.N),list(f1,j1)]
  ff[, psi_g := res$A1[j1],j1]
  setkey(ff,f1)
  setkey(rr,f1)
  ff[, psi_hat := rr[ff,psi]]
  ff[,cov(psi,psi_hat)]
  ff[,cov(psi,psi_g)]

  ff[,wt.var(psi,N)]
  ff[,wt.var(psi_g,N)]
  ff[,wt.var(psi_hat,N)]

  II = sim$jdata[,list(f1=c(f1,f2))][,.N,f1][N %in% c(2,3,4),f1]

  dd = data.frame()
  for (lambda in 10^(seq(-6,6,l=10))) {
    akm.ridge = m2.firmfe.pen(sim,res,lambda=lambda,holdout = 0.4)
    rr = akm.ridge$fids
    ff = sim$sdata[,list(psi=psi1[1],N=.N),list(f1,j1)]
    ff[, psi_g := res$A1[j1],j1]
    setkey(ff,f1)
    setkey(rr,f1)
    ff[, psi_hat := rr[ff,psi]]

    flog.info("var0=%f lambda=%f var_psi=%f mse0=%f",ff[,wt.var(psi,N)],lambda,ff[,wt.var(psi_hat,N)],ff[II,wt.mean((psi-psi_hat)^2,N)])
    dd = rbind(dd,data.frame(lambda=log(lambda)/log(10), var0=ff[,wt.var(psi,N)],var_ridge=ff[,wt.var(psi_hat,N)],mse=akm.ridge$mse))
  }

  # KKS
  S = runif(4)
  S = S/sum(S)
  M = spread(S,1,4)
  (diag(4)-t(M) )%*% diag(S) %*% (diag(4) - M) - (diag(4)-t(M) ) %*% diag(S)

  # computing the element of the trace of the within
  nf = 5
  nm = 10
  V = matrix(runif(nf*nm),nf,nm)
  S = runif(nf)
  S = S/sum(S)

  # exact formula
  W = diag(nf) - spread(S,1,nf)
  diag(t( W %*% V) %*% diag(S) %*% (W %*% V))

  # efficient formula 1
  colSums(V *  diag(S) %*% V) - colSums(V *  (spread(S,2,nf) %*% diag(S) %*% V))

  # efficient formula 2
  colSums(V *  diag(S) %*% V) - colSums( (diag(S) %*% V ) *  (spread(rep(1,length(S)),2,nf) %*% diag(S) %*% V))

  # efficient formula 3
  colSums(V *  diag(S) %*% V) - colSums(diag(S) %*% V ) ^2

  # ---- SHORT TESTING ------ #
  model   = m2.trace.new(nf = 1000,nm = 2000,eps_sd=1.5)
  sim     = m2.trace.simulate(model)
  jdata   = get.largest.leaveoutset.fid(sim$jdata)

  res     = m2.trace.estimate(sim,model0=model,hetero=F)
  res     = m2.trace.estimate(sim,model0=model,hetero=T)

  sim$jdata   = get.largest.leaveoutset.fid(sim$jdata)
  res     = m2.trace.estimate(sim,model0=model,hetero=F)
  res     = m2.trace.estimate(sim,model0=model,hetero=T)


  # testing the code with some firms not present in period 1
  model   = m2.trace.new(nf = 1000,nm = 2000,eps_sd=1.5)
  sim     = m2.trace.simulate(model)
  #some_fids = sample(unique(sim$jdata$f1),5)
  #sim$sdata = sim$sdata[!(f1 %in% some_fids)]
  #sim$jdata = sim$jdata[!(f1 %in% some_fids)]
  rr      = m2.trace.blmhyb(sim,nm_list = c(1,10,100,Inf))


}


testing.reExt <- function() {

  model   = m2.trace.new(nf = 1000,nm = 2000,eps_sd=1.5)
  sim     = m2.trace.simulate(model)

  # attach clusters
  ms    = grouping.getMeasures(sim,"ecdf",Nw=20,y_var = "y1")
  grps  = grouping.classify.once(ms,k = 10,nstart = 1000,iter.max = 200,step=100)
  sim = grouping.append(sim,grps$best_cluster)

  # estimate linear BLM
  sim$sdata[,f1t:=f1]
  sim$jdata[,f1t:=f1][,f2t:=f2]
  sim$sdata[,f1:=paste(j1)][,f2:=paste(j1)]
  sim$jdata[,f1:=paste(j1)][,f2:=paste(j2)]
  jdata = sim$jdata

  f1s   = get.largest.conset.fid(jdata)
  flog.info("connected set %i/%i",length(f1s),length(unique(sim$sdata$f1)))
  jdata = jdata[f1 %in% f1s][f2 %in% f1s]

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
  dd = rbind(dd,data.frame(m1=0,f1i=1,f2i=1))
  N = nrow(dd)
  dd[,v1:=-1][,v2:=1]
  dd[,c1:=f1i][,c2:=f2i]
  dd[N,v1:=0]
  JJ = sparseMatrix2(1:N,dd$c1,dd$v1,c(N,nf)) + sparseMatrix2(1:N,dd$c2,dd$v2,c(N,nf))
  S  = fids[order(nfid),size]

  # COMPUTE INVERSE OF DESIGN MATRIX
  M = SparseM::t(JJ) %*% JJ
  Minv = SparseM::solve(M,nnzlmax=1e8,tmpmax=1e8,nsubmax=1e8)

  # compute firms FE
  psi = as.numeric(SparseM::as.matrix(Minv %*% ( SparseM::t(JJ) %*% dd$m1)))
  E   = dd$m1 - JJ%*%psi
  E   = E[1:(N-1)]

  # let's do something simple to start. For each cluster,
  # - find firms with at least 2 movers
  # - for these firms, extract a list of pair of movers who move to different clusters
  omega = m2.trace.reExt(jdata,psi)

  ggplot(data.frame(x=jdata[,var(psi1),j1][order(j1),V1],y=omega),aes(x=x,y=y)) +
    geom_point() + geom_abline() + theme_bw()




}



test.within_re <- function() {

  model   = m2.trace.new(nf = 500,nm = 50000,eps_sd=1.5)
  #model$S[]=50
  sim     = m2.trace.simulate(model)

  # attach clusters
  ms    = grouping.getMeasures(sim,"ecdf",Nw=20,y_var = "y1")
  grps  = grouping.classify.once(ms,k = 3,nstart = 1000,iter.max = 200,step=100)
  sim   = grouping.append(sim,grps$best_cluster)

  # draw psi_dev for each firm
  fids = sim$sdata[,list(j1=j1[1],psi=psi1[1],.N),f1]
  omega = c(1,2,3) #rep(1,3)#exp(rnorm(10))
  fids[, psi_j := mean(psi),j1]
  fids[, psi_d := rnorm(.N)*omega[j1]]

  # attach it to jdata
  jdata = sim$jdata
  setkey(fids,f1)
  setkey(jdata,f1)
  jdata[,psi1 := fids[jdata,psi_j]]
  jdata[,dev1 := fids[jdata,psi_d]]
  setkey(jdata,f2)
  jdata[,psi2 := fids[jdata,psi_j]]
  jdata[,dev2 := fids[jdata,psi_d]]

  # simulate the outcome
  jdata[,y1 := psi1 + dev1 + rnorm(.N)*1.5]
  jdata[,y2 := psi2 + dev2 + rnorm(.N)*1.5]
  # jdata[,y1 := dev1+ rnorm(.N)]
  # jdata[,y2 := dev2+ rnorm(.N)]
  # jdata[,psi1 := 0]
  # jdata[,psi2 := 0]

  rr = m2.trace.reExt(jdata)
  rr = m2.trace.reExt(jdata,2)
  sqrt(rr)

  # extract some pairs that move out of one firm


  plot(omega^2,rr)

}

test_removing_movers <- function() {

  model   = m2.trace.new(nf = 2000,nm = 10000)

  # let's do a simple monte-carlo for starter

  # simulate data
  rr = data.frame()
  for (i in 1:100) {
    sim     = m2.trace.simulate(model)
    res_akm_homo   = m2.trace.estimate(sim,hetero=F)
    res_akm_hetero = m2.trace.estimate(sim,hetero=T)

    res = list()
    res$akm1_psivar = res_akm_homo$stats$psi_var
    res$akm2_psivar = res_akm_homo$stats$psi_var   - res_akm_homo$stats$trace_term
    res$akm3_psivar = res_akm_hetero$stats$psi_var - res_akm_hetero$stats$trace_term
    res$true_cs_psivar  = sim$sdata[f1 %in% res_akm_homo$jids$f1,var(psi1)]
    res$true_all_psivar = sim$sdata[,var(psi1)]

    rr = rbind(rr,res)
  }

  rr[,list(sd(akm1_psivar),sd(akm2_psivar),sd(akm3_psivar),sd(true_psivar))]
  rr[,list(mean(akm1_psivar),mean(akm2_psivar),mean(akm3_psivar),mean(true_psivar))]

  res     = m2.trace.estimate(sim,hetero=T)

}


# in the following we test the code that tries to recover
# within cluster variance of psi as well as the
# covariance between alpha and psi
# we create a BLM model, and layer within variance on top
test.within_re <- function() {

  model   = m2.mini.new(5,fixb = T,linear = T,serial=F)
  model$A2 = model$A1
  model$B2 = model$B1

  # setting the number of movers and stayers
  model$Ns   = array(300000/model$nf,model$nf)
  model$Nm   = 100*toeplitz(ceiling(seq(100,10,l=model$nf)))

  # creating a simulated data set
  ad =  m2.mini.simulate(model)
  #res = m2.mini.estimate(ad$jdata,ad$sdata,method="linear.cre",cre.sub = c(10,100))

  # draw psi_dev for each firm
  fids = ad$sdata[,list(j1=j1[1],.N),f1]
  param.re = list(
    psi_sd=c(0.5,1,1.5,0.8,1.2),
    a_m=rep(0.5,5),
    a_s=seq(0.5,1,l=5),
    xi_sd=seq(0.5,1.5,l=5),
    xim_sd = 0.2)


  fids[, psi_j := model$A1[j1],j1]
  fids[, psi_d := rnorm(.N)*param.re$psi_sd[j1]]

  # attach it to jdata
  jdata = ad$jdata
  setkey(fids,f1)
  setkey(jdata,f1)
  jdata[,psi1 := fids[jdata,psi_j]]
  jdata[,dev1 := fids[jdata,psi_d]]
  setkey(jdata,f2)
  jdata[,psi2 := fids[jdata,psi_j]]
  jdata[,dev2 := fids[jdata,psi_d]]
  sdata = ad$sdata
  sdata[,f2:=f1]
  setkey(sdata,f1)
  sdata[,psi1 := fids[sdata,psi_j]]
  sdata[,dev1 := fids[sdata,psi_d]]
  setkey(sdata,f2)
  sdata[,psi2 := fids[sdata,psi_j]]
  sdata[,dev2 := fids[sdata,psi_d]]

  # draw the alphas deviations
  sdata[, alpha_dev := 2*param.re$a_s[j1]*dev1 + rnorm(.N)*param.re$xi_sd[j1], j1 ]
  jdata[, alpha_dev := (param.re$a_m[j1]*dev1 + param.re$a_m[j2]*dev2)+ rnorm(.N)*param.re$xim_sd, list(j1,j2)]

  # simulate the outcome
  jdata[,y1 := (alpha + alpha_dev) + psi1 + dev1 + rnorm(.N)*1.5]
  jdata[,y2 := (alpha + alpha_dev) + psi2 + dev2 + rnorm(.N)*1.5]
  sdata[,y1 := (model$Em[j1] + alpha_dev) + psi1 + dev1 + rnorm(.N)*1.5,j1]
  sdata[,y2 := (model$Em[j2] + alpha_dev) + psi2 + dev2 + rnorm(.N)*1.5,j2]
  # jdata[,y1 := dev1+ rnorm(.N)]
  # jdata[,y2 := dev2+ rnorm(.N)]
  # jdata[,psi1 := 0]
  # jdata[,psi2 := 0]
  sdata[,mx := model$Em[j1],j1]

  addmom <- function(A1,A2,name,rr=data.frame(),type="mean") {
    rr1 = melt(A1)
    rr2 = melt(A2)
    rr = rbind(rr,data.frame(name=name,val1=rr1$value,val2=rr2$val,type=type))
    return(rr)
  }

  res = m2.trace.reExt.all(sdata,jdata,subsample = 10,100)



  rr = addmom( res$p$psi_sd, param.re$psi_sd, "psi_sd")
  rr = addmom( res$p$a_s, param.re$a_s,"a_s",rr = rr)
  rr = addmom( res$p$eps_sd, rep(1.5,model$nf),"eps_sd",rr = rr)
  rr = addmom( res$p$xi_sd, param.re$xi_sd,"xi_sd",rr = rr)
  rr = addmom( res$moments$cov_Y1s_dYm ,-(2*param.re$a_s+1)*param.re$psi_sd^2 ,"cov(Y1s,dYm)",rr = rr)
  rr = addmom( res$moments$cov_Y1s_Y1sc, (2*param.re$a_s+1)^2*param.re$psi_sd^2 ,"cov(Y1s,Y1sc)",rr = rr)
  print(ggplot(rr,aes(x=val2,y=val1,color=type)) + geom_point() + facet_wrap(~name,scale="free") + theme_bw() + geom_abline(linetype=2))

  res = m2.mini.estimate(jdata,sdata,method="linear.cre",cre.sub = c(10,100))

  cre = readRDS("/Users/thibautlamadon/Dropbox/paper-lms/data-results/tmp/AK_CRE_moments.rds")

  cre_all_0  =  data.table(read.csv("/Users/thibautlamadon/Dropbox/paper-lms/data-results/tmp/AK_full_CRE_mat_sub0.csv"))
  cre_all_10 =  data.table(read.csv("/Users/thibautlamadon/Dropbox/paper-lms/data-results/tmp/AK_full_CRE_mat_sub10.csv"))

  vdec = cre_all_0[j1!=10, list(
    var_psi_btw= wt.var(psi1,N),
    var_psi_wth= wt.mean(psi_sd^2,N),
    var_x_btw  = wt.var(Em,N),
    var_x_wth  = wt.mean(4*a_s^2*psi_sd^2 + xi_sd^2,N),
    cov_x_psi_btw  = 2*wt.cov(psi1,Em,N),
    cov_x_psi_wth  = 2*wt.mean( -cov_Y1s_dYmc - psi_sd^2    ,N),
    var_res = wt.mean(eps_sd^2,N))]

  get.cre.vdec.from.all <- function(all) {
     all[,psi_var := cov_dYm_dYmc]
     all[,eps_var := 0.5*var_dYm - psi_var]
     all[,x_var   := var_Y1s - psi_var - eps_var]

     r = as.list(all[,list(
       var_firm   = wt.var(psi1,N) + wt.mean(psi_var,N),
       var_worker = wt.var(Em,N)   + wt.mean(x_var,N)  ,
       cov        = 2*wt.cov(psi1,Em,N),
       residual   = wt.mean(eps_var,N),
       var_worker_within = wt.mean(x_var,N),
       var_segregation   = wt.var(Em,N))])

     r$corr = with(r, 0.5*cov/(sqrt(var_firm * var_worker)))
     return(r)
  }

  get.blm.vdec.from.res <- function(res) {
    all = res$cre$all
    all[,x_var   := res$Esd[j1]^2,j1]
    all[,eps_var := res$eps1_sd[j1]^2,j1]

    r = all[,list(
      var_worker_within = wt.mean(x_var,N),
      var_segregation   = wt.var(Em,N),
      var_firm          = wt.var(psi1,N),
      cov               = 2*wt.cov(psi1,Em,N),
      residual          = wt.mean(eps_var,N))]

    return(r)
  }

  get_blm_decomp <- function(res) {

    # extract the outcome
    jall =


    all = res$cre$all
    all[,x_var   := res$Esd[j1]^2,j1]
    all[,eps_var := res$eps1_sd[j1]^2,j1]

    r = all[,list(
      method               = "BLM",
      x_within_var         = wt.mean(x_var,N),
      segregation_var      = wt.var(Em,N),
      x_var                = wt.var(Em,N)   + wt.mean(x_var,N)  ,
      psi_var              = wt.var(psi1,N),
      psi_x_cov            = wt.cov(psi1,Em,N),
      residual_var             = wt.mean(eps_var,N))]


    r$psi_x_cor = with(r, psi_x_cov/(sqrt(psi_var * x_var)))
    r$total_between_var = with(r, psi_var + 2*psi_x_cov + segregation_var)
    r$total_within_var = with(r, x_within_var + residual_var)
    r$total_var = with(r, total_between_var + total_within_var)
    return(r)
  }

  vdec[ ,  (cov_x_psi_btw+cov_x_psi_wth)/(sqrt( var_psi_btw + var_psi_wth )*sqrt( var_x_btw + var_x_wth )) ]


  cre_all_0[, cov_x_psi_wth := 2*( -cov_Y1s_dYmc - psi_sd^2)]

  load("/Users/thibautlamadon/git/smfe/inst/local-res/res_all_ATG.RData")

  load("/Users/thibautlamadon/git/smfe/inst/local-res/res_akm_IT_Kline_2.Rdata")

}

test.allbc.terms <- function() {

  mc.opts = list(serial=0,grouping="split",lambda=0.5,eps_sd=1.5,hetero_factor=0)
  mc.opts = list(rhom=0,serials=1,grouping="split",lambda=0.5,eps_sd=1.5,hetero_factor=4)

  mc.opts = list(rhom=0,serials=1,grouping="all",lambda=0.5,eps_sd=0.1,hetero_factor=0)
  sim = make_data(mc.opts)


  require(sparseutils)

  # monte-carlo comparing calculation in difference versus
  # computation in levels
  # create sdata/jdata
  data = m2.trace.simulate.panel(nt = 2,ni = 10000,firm_size = 10,w_sigma=0.8,hetero_g = 0,lambda = mc.opts$lambda)
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

  rr = data.frame()
  rr_cov = data.frame()
  rr_cov2= data.frame()
  rr_cov3= data.frame()
  rr_alpha = data.frame()
  for (i in 1:500) {
    sim$sdata[, y1:= alpha + psi1_bu + mc.opts$eps_sd*rnorm(.N)*exp(  mc.opts$hetero_factor/mc1 )]
    sim$sdata[, y2:= alpha + psi2_bu + mc.opts$eps_sd*rnorm(.N)*exp(  mc.opts$hetero_factor/mc2 )]

    sim$jdata[, e1 := rnorm(.N)]
    sim$jdata[, e2 := mc.opts$rhom *e1 + sqrt(1-mc.opts$rhom^2)*rnorm(.N)]
    sim$jdata[, y1 := alpha + psi1_bu + e1 * mc.opts$eps_sd*exp(  mc.opts$hetero_factor/mc1 )]
    sim$jdata[, y2 := alpha + psi2_bu + e2 * mc.opts$eps_sd*exp(  mc.opts$hetero_factor/mc2 )]

    if (mc.opts$serials==TRUE) {
      sim$sdata[, y2:= y1]
    }

    # FE approaches
    res1 = m2.trace.estimate2(sim,hetero = FALSE, check_set = FALSE)
    res2 = m2.trace.estimate2(sim,hetero = TRUE, check_set = FALSE)
    res3 = m2.trace.estimate.full(sim, check_set = FALSE,make_each_row_one_wid  = TRUE)
    res4 = m2.trace.estimate.full(sim,hetero = TRUE, check_set = FALSE, make_each_row_one_wid = TRUE)

    # grouping
    if (mc.opts$grouping=="all") {
      measures = grouping.getMeasures(list(sdata = rbind(sim$sdata[,list(y1,f1)],sim$jdata[,list(y1,f1)])),Nw = 20,y_var = "y1")
      sim$jdata[,use_classification:=FALSE]
    } else if (mc.opts$grouping=="stayers") {
      measures = grouping.getMeasures(sim,Nw = 20,y_var = "y1")
      sim$jdata[,use_classification:=FALSE]
    } else if (mc.opts$grouping=="split") {
      # we split movers to use some in classification
      ns = sim$sdata[,.N]
      nm = sim$jdata[,.N]

      pr_keep_mover = pmin(0.1 * (1 + nm/ns),1)
      sim$jdata[, use_classification := runif(.N) > pr_keep_mover]
      sim$sdata[, use_classification := TRUE]

      measures = grouping.getMeasures(list(sdata = rbind(sim$sdata[,list(y1,f1)],sim$jdata[use_classification == TRUE,list(y1,f1)])),Nw = 20,y_var = "y1")
    } else {
      flog.error("unrecognized grouping method")
    }
    clus = grouping.classify.once(measures,k=10,nstart=1000,iter.max=200,step=20)
    sim2 = grouping.append(copy(sim),clus$best_cluster,drop = T)

    # blm
    res_cre  = m2.mini.estimate(jdata=sim2$jdata[use_classification==FALSE],
                                sdata=sim2$sdata,method="linear.cre",posterior_var = T,include_covY1_terms = T)
    res_cre2 = Do.CRE.stats(res_cre$cre$all)
    res_cre0 = Do.CRE0.stats(res_cre)

    cov_alpha_psi = res_cre$cre$all[, wt.mean(cov_Y1s_dYmc,Nm) ] - res_cre$cre$all[, wt.mean(cov_dYm_dYmc,Nm) ]
    sim2$sdata[,list(cov(alpha,psi1_bu),N),j1][,wt.mean(V1,N)]

    cdata = rbind(sim$sdata[,list(y1,psi1 = psi1_bu,alpha)],sim$sdata[,list(y1,psi1 = psi1_bu,alpha)])

    res = list(
      model              = cdata[,var(psi1)],
      fe                 = res1$stats$psi_var,
      homo_diff          = res1$stats$psi_var - res1$stats$trace_term_Q,
      hetero_diff        = res2$stats$psi_var - res2$stats$trace_term_Q,
      homo_full          = res3$stats$full_dec_psi_var_bc,
      hetero_full        = res4$stats$full_dec_psi_var_bc,
      blm_cre0           = res_cre0$psi_var,
      blm_cre            = res_cre2$psi_var,
      #blm_cre_post       = res_cre$cre$posterior_var,
      rep = i)
    res_cov = list(
      model              = cdata[,cov(psi1,alpha)],
      fe                 = res3$stats$full_dec_cov_fe,
      homo_full          = res3$stats$full_dec_cov_bc,
      hetero_full        = res4$stats$full_dec_cov_bc,
      blm_cre0           = res_cre0$psi_x_cov,
      blm_cre            = res_cre2$psi_x_cov,
      #blm_cre_post       = res_cre$cre$posterior_cov_ypsi - res_cre$cre$posterior_var,
      rep = i)
    res_alpha = list(
      model              = cdata[,var(alpha)],
      fe                 = res3$stats$full_dec_alpha_var_fe,
      homo_full          = res3$stats$full_dec_alpha_var_bc,
      hetero_full        = res4$stats$full_dec_alpha_var_bc,
      blm_cre0           = res_cre0$x_var,
      blm_cre            = res_cre2$x_var,
      rep = i)
    res_cov2 = list(
      model              = cdata[,cov(psi1,y1)],
      fe                 = res4$stats$cov_ypsi_all_fe,
      hetero_full        = res4$stats$full_dec_psi_var_bc+res4$stats$full_dec_cov_bc,
      homo_full          = res3$stats$full_dec_psi_var_bc+res3$stats$full_dec_cov_bc,
      rep = i)
    res_cov3 = list(
      model              = jdata[,cov(psi1,y1)],
      fe                 = res4$stats$cov_ypsi_movers_fe,
      lo                 = res2$stats$cov_ypsi_movers_lo,
      blm_cre_post       = res_cre$cre$posterior_cov_ypsi_movers,
      rep = i)

    rr     = rbind(rr,data.frame(res))
    rr_cov = rbind(rr_cov,data.frame(res_cov))
    rr_cov2 = rbind(rr_cov2,data.frame(res_cov2))
    rr_cov3 = rbind(rr_cov3,data.frame(res_cov3))
    rr_alpha = rbind(rr_alpha,data.frame(res_alpha))
    flog.info(" >>>>>>>>>> done with %i",i)
  }

  cf_str = paste(paste(str_remove_all(names(mc.opts),"_"),str_remove(mc.opts,"\\."),sep=""),collapse = "_")
  save(rr,rr_cov,rr_alpha,mc.opts,file=sprintf("bootstrap_results_%s.dat",cf_str))

  rrm = data.table(melt( rr, id.vars = "rep"))
  ggplot(rrm[variable != 'model'],aes(x=value,group=variable,fill=variable)) +
    geom_density(alpha=0.6) +theme_bw() +
    geom_vline(xintercept = rrm[variable=="model",value],linetype=2) +
    ggtitle("variance of firm effect")
  data.table(rrm)[,list(m=mean(value),sd=sd(value)),variable]

  rrm = data.table(melt( rr_cov, id.vars = "rep"))
  ggplot(rrm[variable != 'model'],aes(x=value,group=variable,fill=variable)) +
    geom_density(alpha=0.6) +theme_bw() +
    geom_vline(xintercept = rrm[variable=="model",value],linetype=2) +
    ggtitle("covariance")
  data.table(rrm)[,list(m=mean(value),sd=sd(value)),variable]

  rrm = data.table(melt( rr_alpha, id.vars = "rep"))
  ggplot(rrm[variable != 'model'],aes(x=value,group=variable,fill=variable)) +
    geom_density(alpha=0.6) +theme_bw() +
    geom_vline(xintercept = rrm[variable=="model",value],linetype=2)
  data.table(rrm)[,list(m=mean(value),sd=sd(value)),variable]

  rrm = data.table(melt( rr_cov3, id.vars = "rep"))
  data.table(rrm)[,list(m=mean(value),sd=sd(value)),variable]


  load('bootstrap_results_rhom05_serials1_groupingall_lambda05_epssd15_heterofactor4.dat')

  load('bootstrap_results_rhom05_serials1_groupingsplit_lambda05_epssd15_heterofactor4.dat')

}

test.m2.posterior <- function() {

  data = m2.trace.simulate.panel(nt = 2,ni = 100000,firm_size = 200,w_sigma=0.8,hetero_g = 0)
  data[,fid := paste(fid)]

  # constuct sim data
  movers = data[,length(unique(fid)),wid][V1>1,wid]
  setkey(data,wid,t)
  jdata = data[wid %in% movers,     list(f1 = fid[1],f2=fid[2],y1=y[1],y2=y[2],alpha=alpha[1],psi1=psi[1],psi2=psi[2],year=t[1]),wid]
  sdata = data[! (wid %in% movers), list(f1 = fid[1],f2=fid[2],y1=y[1],y2=y[2],alpha=alpha[1],psi1=psi[1],psi2=psi[2],year=t[1]),wid]

  f1s = jdata[,unique(c(f1,f2))]
  fids = data.table(f1=f1s,nfid=1:length(f1s))
  setkey(fids,f1)
  setkey(jdata,f1)
  jdata[,f1i := fids[jdata,nfid]]
  setkey(sdata,f1)
  sdata[,f1i := fids[sdata,nfid]]
  setkey(jdata,f2)
  jdata[,f2i := fids[jdata,nfid]]
  sim = list(sdata=sdata,jdata=jdata)

  jdata = as.data.frame(jdata)
  sdata = as.data.frame(sdata)
  saveRDS(sdata,file="~/Dropbox/paper-small-firm-effects/results/simsdata.rds")
  saveRDS(jdata,file="~/Dropbox/paper-small-firm-effects/results/simjdata.rds")

  res1 = m2.trace.estimate(sim,hetero = FALSE, check_set = FALSE)


  sim = list(sdata=as.data.frame(sdata[!is.na(y1*y2)]),jdata=as.data.frame(jdata))


  # group the firms
  measures = grouping.getMeasures(sim,Nw = 20,y_var = "y1")
  clus = grouping.classify.once(measures,k=10,nstart=1000,iter.max=200,step=20)
  sim = grouping.append(sim,clus$best_cluster)

  res  = m2.mini.estimate(jdata=sim$jdata,sdata=sim$sdata,method="linear.cre")
  pvar = m2.cre_posterior_var(jdata=sim$jdata,sdata=sim$sdata,res)
  res_cre_vdec = Do.CRE.stats(res$cre$all)
}


test.ploting.cre <- fucntion() {

  dd2 = data.table(read.csv('~/Dropbox/paper-small-firm-effects/results/sept2019/raw_moments/final_output_US-states.csv'))
  dd  = data.table(read.csv("~/Dropbox/paper-small-firm-effects/results/sept2019/raw_moments/final_cre_all_US-states.csv"))

  dd2[suffix=="_connected_clusters10"][!is.na(posterior_var),list(psi_var,posterior_var,country)]

  dd[country=='US-WV-late'][suffix=='_connected_clusters10']

  dd[psi_sd=='0'][suffix=='_connected_clusters10']
  dd[eps_sd=='0'][suffix=='_connected_clusters10']
}




test.cov <- function() {

  N=10
  dd = data.table(u1 = rnorm(N),c1 = 1 + 1.0*(runif(N)>0.2))
  dd[,c2 := 1 + 1*(runif(N)>0.2) ]
  dd[,u2 := c2 + 0.5 + u1 + rnorm(.N)]

  p = accu.moms.pairs(dd$u1,dd$c1,dd$u2,dd$c2,p=list(nc=2))


  Y1 = as.numeric(spread(dd$u1,1,N))
  Y2 = as.numeric(spread(dd$u2,2,N))

  mean(Y1)
  p$s1/p$n

  mean(Y2)
  p$s2/p$n

  mean(Y1*Y2)
  p$v12/p$n

  cov(Y1,Y2)
  p$v12/p$n -  (p$s1/p$n) * (p$s2/p$n)

  mean(Y1*Y2) - mean(Y1)*mean(Y2)
  cov(Y1,Y2)

  # trying it out on jdata

  cov_Ys_DYm = rep(0,10)
  for (j1c in 1:10) {
    fids = sim2$sdata[j1==j1c,unique(f1)]
    p=NA
    for (fi in fids) {
      dm = sim2$jdata[f1==fi][j2!=j1c]
      ds = sim2$sdata[f1==fi]
      p = pcov.accu(ds$y1,dm$y1-dm$y2,p)
    }
    cov_Ys_DYm[j1c] = pair.cov.accu.cov(p)
  }

  vtrue = sim2$sdata[,cov(alpha,psi1_bu) + var(psi1_bu),j1][order(j1),V1]

  plot(vtrue, cov_Ys_DYm)

}


testing.promises <- function() {

  mc.opts = list(rhom=0,serials=1,grouping="all",lambda=0.5,eps_sd=0.1,hetero_factor=0,ni=50000,firm_size=20)
  sim = m2.trace.simulate.fromnt(mc.opts)

  measures = grouping.getMeasures(list(sdata = rbind(sim$sdata[,list(y1=psi1_bu,f1)],
                                                     sim$jdata[,list(y1=psi1_bu,f1)])),Nw = 20,y_var = "y1")
  clus = grouping.classify.once(measures,k=10,nstart=1000,iter.max=200,step=20)
  sim = grouping.append(sim,clus$best_cluster,drop = T)

  saveRDS(sim,file="tmp_bothdata.rds")

  jdata = sim$jdata
  sdata = sim$sdata
  f1s = jdata[,unique(c(f1,f2))]
  fids = data.table(f1=f1s,nfid=1:length(f1s))
  setkey(fids,f1)
  setkey(jdata,f1)
  jdata[,f1i := fids[jdata,nfid]]
  setkey(sdata,f1)
  sdata[,f1i := fids[sdata,nfid]]
  setkey(jdata,f2)
  jdata[,f2i := fids[jdata,nfid]]

  jdata = as.data.frame(jdata)
  sdata = as.data.frame(sdata)

  saveRDS(sim$sdata,file="tmp_sdata.rds")
  saveRDS(sim$jdata,file="tmp_jdata.rds")

  # extract pseudo-true values
  # we need to compute cov(alpha,alpha') and var(psi)

  # we start by computing mean alpha, and size in each firm
  cdata = rbind(sim$sdata[,list(y1,psi1_bu,alpha,f1,j1)],sim$jdata[,list(y1,psi1_bu,alpha,f1,j1)])

  cdata[,alpha_bar_j := mean(alpha),f1]
  cdata[,n_j := .N,f1]
  pstrue = cdata[n_j>1, list(.N,cov_ap = cov(alpha,psi1_bu), var_p = var(psi1_bu),
               cov_aa = cov(alpha, (n_j*alpha_bar_j - alpha)/(n_j-1) )),j1]

  pstrue = pstrue[, list(cov_ap = wt.mean(cov_ap,N),
                         var_p  = wt.mean(var_p,N),
                         cov_aa = wt.mean(cov_aa,N)) ]

  pstrue[, cov_aa + 2 * cov_ap + var_p]

  # focus on stayers first
  sdata = sim$sdata
  sdata[,alpha_bar_j := mean(alpha),f1]
  sdata[,alpha_bar_j := alpha_bar_j - mean(alpha_bar_j),j1]
  sdata[,psi1_wth_bu := psi1_bu - mean(psi1_bu),j1]
  sdata[,n_j := .N,f1]
  pstrue = sdata[n_j>1, list(.N,cov_ap = cov(alpha,psi1_wth_bu),
                                 var_p = var(psi1_wth_bu),
                                cov_aa = cov(alpha, (n_j*alpha_bar_j - alpha)/(n_j-1) )),j1]
  pstrue = pstrue[, list(cov_ap = wt.mean(cov_ap,N),
                         var_p  = wt.mean(var_p,N),
                         cov_aa = wt.mean(cov_aa,N)) ]
  pstrue[, cov_aa + 2 * cov_ap + var_p]


  # let's simulate from know stuff here
  sdata[, psi1_bu := mean(y1),f1]
  sdata[, psi1_bu := mean(y1),f1]

  res = m2.cre.estimate(sim)
  res

  res$cov_m12_m12
  res$cov_m21_m21
  pstrue

  res$cov_s_m12
  res$cov_s_m21
  res$cov_m11_m12
  res$cov_m21_m22


  # let's cosntruct simple wages to start with
  mc.opts = list(rhom=0,serials=1,grouping="all",
                 lambda=0.5,eps_sd=0.1,hetero_factor=0,firm_size=20,ni=50000)
  sim = m2.trace.simulate.fromnt(mc.opts)
  measures = grouping.getMeasures(list(sdata = rbind(sim$sdata[,list(y1=psi1_bu,f1)],
                                                     sim$jdata[,list(y1=psi1_bu,f1)])),Nw = 20,y_var = "y1")
  clus = grouping.classify.once(measures,k=10,nstart=1000,iter.max=200,step=20)
  sim = grouping.append(sim,clus$best_cluster,drop = T)

  jdata = sim$jdata
  sdata = sim$sdata
  sdata[, psi1_bu := rnorm(1),f1 ]
  fpsi  = unique(sdata[,list(f=f1,psi=psi1_bu)])
  jdata$psi1_bu = NULL
  jdata$psi2_bu = NULL
  jdata = merge(jdata,fpsi[,list(f1=f,psi1_bu=psi)],by="f1")
  jdata = merge(jdata,fpsi[,list(f2=f,psi2_bu=psi)],by="f2")

  p = list(as=0.5,am1=0.2,am2=0.2,psi_sd = 1.0,eps_sd= 0.5,alpha_sd=0.5)
  sdata[, alpha := p$as * psi1_bu    + p$alpha_sd*rnorm(.N)]
  sdata[, y1    := alpha + psi1_bu   + p$eps_sd*rnorm(.N)]
  jdata[, alpha := p$am1 * psi1_bu   + p$am2*psi2_bu + p$alpha_sd*rnorm(.N)]
  jdata[, y1    := alpha + psi1_bu   + p$eps_sd*rnorm(.N)]
  jdata[, y2    := alpha + psi2_bu   + p$eps_sd*rnorm(.N)]

  res = m2.cre.estimate(list(sdata=sdata,jdata=jdata))
  res$cov_s_s
  with(p,(as^2+2*as+1)*psi_sd^2 )
  res$cov_m11_m11
  with(p,(am1^2+2*am1+1)*psi_sd^2 )
  res$cov_m22_m22
  with(p,(am2^2+2*am2+1)*psi_sd^2 )
  res$cov_s_m11
  with(p,(as+1)*(am1+1)*psi_sd^2 )

  res = data.table(read.csv("~/Downloads/SD_cre_moments.csv"))
  res = data.table(read.csv("inst/local-res/cre-moms-SW.csv"))

  res = readRDS("inst/local-res/cre__connected_clusters10.rds")$moments

  #res = data.table(read.csv("~/Downloads/WV_cre_moments.csv"))
  res$name = as.character(res$name)

  # covariances between psi and alpha
  cov_Am1Am2  = res[name=="m12_m21",value]
  cov_Am2Psi1 = res[name=="m11_m21",value] - cov_Am1Am2
  cov_Am1Psi2 = res[name=="m12_m22",value] - cov_Am1Am2
  var_psi_m12 = res[name=="m11_m22",value] - cov_Am1Am2 - cov_Am2Psi1 - cov_Am1Psi2

  # using movers leaving from 1
  cov_Am1Am1 = res[name=="m12_m12",value]
  cov_Am1Psi1 = res[name=="m11_m12",value] - cov_Am1Am1
  var_psi_m1  = res[name=="m11_m11",value] - cov_Am1Am1 - 2*cov_Am1Psi1

  # using movers leaving from 2
  cov_Am2Am2 = res[name=="m21_m21",value]
  cov_Am2Psi2 = res[name=="m21_m22",value] - cov_Am2Am2
  var_psi_m2  = res[name=="m22_m22",value] - cov_Am2Am2 - 2*cov_Am2Psi2

  # looking at stayers
  cov_AsAm1 = res[name=="s_m12",value] - cov_Am1Psi1
  cov_AsAm2 = res[name=="s_m21",value] - cov_Am2Psi2
  psi_plus_cov1 = res[name=="s_m11",value] - res[name=="s_m12",value]
  psi_plus_cov2 = res[name=="s_m22",value] - res[name=="s_m21",value]

  psi_plus_cov1 - var_psi_m1
  psi_plus_cov2 - var_psi_m2

  # first construct the table with the moments

  library(textables)
  vals = res[,value]
  names(vals) = res[,name]
  tt =  TR(c("","$Y^s$","$Y_1^{m_1}$","$Y_2^{m_1}$","$Y_1^{m_2}$","$Y_2^{m_2}$")) +
    tt_rule_mid() +
    TR("$Y^s$")       %:% TR( vals[c("s_s","s_m11","s_m12","s_m21","s_m22")] ) +
    TR("$Y_1^{m_1}$") %:% TR("") %:% TR( vals[c("m11_m11","m11_m12","m11_m21","m11_m22")] ) +
    TR("$Y_2^{m_1}$") %:% TR("") %:% TR("") %:% TR( vals[c("m12_m12","m12_m21","m12_m22")] ) +
    TR("$Y_1^{m_2}$") %:% TR("") %:% TR("") %:% TR("") %:% TR( vals[c("m21_m21","m21_m22")] ) +
    TR("$Y_2^{m_2}$") %:% TR("") %:% TR("") %:% TR("") %:% TR("") %:%  TR( vals[c("m12_m12")] )

  tab <- tt_tabularize(tt, header=c("l|rrrrr"), pretty_rules=F)
  tt_save(tab,filename='cre_moms_SW2.tex',stand_alone=T)

  #construct simple table of results
  tt = TR("$\\langle \\alpha^{m_1},\\alpha^{m_1} \\rangle $") %:% TR(cov_Am1Am1) +
    TR("$\\langle \\alpha^{m_1},\\alpha^{m_2} \\rangle $") %:% TR(cov_Am1Am2) +
    TR("$\\langle \\alpha^{m_2},\\alpha^{m_2} \\rangle $") %:% TR(cov_Am2Am2) +
    TR("$\\langle \\alpha^{s},\\alpha^{m_1} \\rangle $") %:% TR(cov_AsAm1) +
    TR("$\\langle \\alpha^{s},\\alpha^{m_2} \\rangle $") %:% TR(cov_AsAm2) +
    tt_rule_mid() +
    TR("$\\langle \\alpha^{m_1}, \\psi_1 \\rangle $") %:% TR(cov_Am1Psi1) +
    TR("$\\langle \\alpha^{m_1}, \\psi_2 \\rangle $") %:% TR(cov_Am1Psi2) +
    TR("$\\langle \\alpha^{m_2}, \\psi_1 \\rangle $") %:% TR(cov_Am2Psi1) +
    TR("$\\langle \\alpha^{m_2}, \\psi_2 \\rangle $") %:% TR(cov_Am2Psi2) +
    tt_rule_mid() +
    TR("$\\sigma^2_\\psi $ using $m_1$ and $m_2$") %:% TR(var_psi_m12) +
    TR("$\\sigma^2_\\psi $ using $m_1$") %:% TR(var_psi_m1) +
    TR("$\\sigma^2_\\psi $ using $m_2$") %:% TR(var_psi_m2) +
    tt_rule_mid() +
    TR("$\\sigma^2_\\psi + \\langle \\alpha^s , \\psi \\rangle $ using $m_1$") %:% TR(psi_plus_cov1) +
    TR("$\\sigma^2_\\psi + \\langle \\alpha^s , \\psi \\rangle $ using $m_2$") %:% TR(psi_plus_cov2) +
    tt_rule_mid() +
    TR("$\\langle \\alpha^s , \\psi \\rangle $ using $m_1$") %:% TR(psi_plus_cov1 - var_psi_m1) +
    TR("$\\langle \\alpha^s , \\psi \\rangle $ using $m_2$") %:% TR(psi_plus_cov2 - var_psi_m2)

  tab <- tt_tabularize(tt, header=c("lr"), pretty_rules=F)
  tt_save(tab,filename='cre_params_SW2.tex',stand_alone=T)




}


test_levereage <- function() {

  X = array(rnorm(100*5),c(100,5))
  M = solve(t(X) %*% X)

  Pii = rep(0,100)
  for (i in 1:100) {
    Pii[i] = sum( X[i,] * (M %*% X[i,]) )
  }

  # testing the approximation
  Pii2 = rep(0,100)
  nk = 100
  for (k in 1:nk) {
    # draw a Radamacher matrix
    R    = 2*(runif(100)<0.5)-1
    Z    = M %*% ( t(X) %*% R)
    Pii2 = Pii2 + 1/nk *  rowSums( X %*% Z)^2
  }

  plot(Pii,Pii2)

}

test_python <- function() {

  sdata = data.table(readRDS('tmp_sdata.rds'))
  jdata = data.table(readRDS('tmp_jdata.rds'))

  res = m2.trace.estimate.full(list(sdata=sdata,jdata=jdata),hetero=TRUE)
  res = m2.cre.estimate(list(sdata=sdata,jdata=jdata))

}

vre.testing <- function() {

  dd_vre = data.table(read.csv('/Users/thibautlamadon/git/vre/build/mc_network_vre_N20_T0.csv'))
  dd_fe  = data.table(read.csv('/Users/thibautlamadon/git/vre/build/mc_network_fe_N20_T0.csv'))
  dd = rbind(dd_vre[,list(rho,model='vre')],dd_fe[,list(rho,model='fe')])
  dd[,list(mean(rho),sd(rho),length(unique(rho)),.N),model]

  ggplot(dd,aes(x=rho,fill=model)) +
    geom_density(alpha=0.5) + theme_bw() +
    geom_vline(xintercept = 1,linetype=2) +
    coord_cartesian(xlim=c(0,2.5))
  ggsave('~/Dropbox/Variational/figures/network_N20.pdf',width = 6,height=4)

  ggplot(dd,aes(x=rho,fill=model)) +
    geom_histogram(alpha=0.5,binwidth = 0.1, position="identity") + theme_bw() +
    geom_vline(xintercept = 1,linetype=2) +
    coord_cartesian(xlim=c(0,2.5))
  ggsave('~/Dropbox/Variational/figures/network_N20_hist.pdf',width = 6,height=4)

  # ggplot(dd_vre,aes(x=gamma)) + geom_density(fill='blue',alpha=0.5) +
  #   theme_bw() + geom_vline(xintercept = 0.25, linetype=2)

  dd_vre = data.table(read.csv('/Users/thibautlamadon/git/vre/build/mc_probitwt_vre_N100_T10.csv'))
  dd_fe  = data.table(read.csv('/Users/thibautlamadon/git/vre/build/mc_probitwt_fe_N100_T10.csv'))
  dd = rbind(dd_vre[,list(rho,model='vre')],dd_fe[,list(rho,model='fe')])
  dd[,list(mean(rho),sd(rho),length(unique(rho)),.N),model]

  ggplot(dd,aes(x=rho,fill=model,color=model)) +
    geom_histogram(alpha=0.5,binwidth = 0.05, position="identity") + theme_bw() +
    geom_vline(xintercept = 0.5,linetype=2)

  ggsave('~/Dropbox/Variational/figures/probit_withtime_T10.pdf',width = 6,height=4)

  rr = data.table(read.csv('/Users/thibautlamadon/git/vre/pyvre/network_fe.csv'))

  # interest rate calculation


  dd = data.table(read.csv("/System/Volumes/Data/Users/thibautlamadon/Dropbox/uchicago-admission-2020/round2-names.csv",header = FALSE))
  dd$v1 = NULL
  dd$round1 = paste(dd$V1)
  readers = unique(dd$round1)
  readers = c("alex",'golosov','fernando','tibo','dinerstein')

  dd[, round2 := sample(setdiff(readers,round1),.N,replace=TRUE),round1 ]

  cat( paste(dd$round2,collapse = "\n") )


  dd


}

testing.grouping.obserbables <- function() {
  model   = m2.trace.new(nf = 1000,nm = 3000,eps_sd=1.5)
  sim     = m2.trace.simulate(model)

  # create a random observable
  sim$sdata$x = sample.int(6,length(sim$sdata$x),replace=T)

  # cluster
  ms    = grouping.getMeasures(sim,"frequency",y_var = "x")
  grps  = grouping.classify.once(ms,k = 5,nstart = 1000,iter.max = 200,step=100)
  sim   = grouping.append(sim,grps$best_cluster)

}

