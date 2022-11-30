setwd("")

# Read-in original dataset
data <- read.csv("FinalAE.csv", header=T)
data35 <- data[ ,c(1:24, seq(25,173, by=2)+1)]
data35 <- data35[ ,c(1:24,42)] # Pneumonitis

library(reshape2)
datal35 <- melt(data35, id.vars = c(2,5,6,8,10,15,17:19),
                measure.vars = 25,
                variable.name = "AE",        # 75 AEs
                value.name = "Y")

datal35$cutoff.no.reporting.for.grades.3.4[datal35$Trial.New.==15] <- 1
datal35$cutoff.no.reporting.for.grades.3.4[datal35$Trial.New.==82] <- 0
datal35$cutoff.no.reporting.for.grades.3.4[datal35$Trial.New.==84] <- 0
datal35$cutoff.no.reporting.for.grades.3.4[datal35$Trial.New.==86] <- 2

#--------------------------------------------------------------
# Create a true Y to check true/false NA
# if C2 = 0, then false NA, Y = 0
datal35$Y[is.na(datal35$Y) & 
            datal35$cutoff.no.reporting.for.grades.3.4==0] <- 0
# if C2 >= 1, then true NA, Y = NA
datal35$Y[is.na(datal35$Y) & 
            datal35$cutoff.no.reporting.for.grades.3.4>=1] <- NA

# Create a "R" variable
# if Y is observed, then R=1
# if Y is missing, then R=0 (left-censored)
datal35$R <- ifelse(is.na(datal35$Y), 0, 1)

# L: lower bound (all grade); U: upper bound
# if R=0, then L = C2; if R=1, then L=-1 
datal35$L <- ifelse(datal35$R==0, 
                    datal35$cutoff.no.reporting.for.grades.3.4,
                    -1)
# if R=0 or R=1, then U = N
datal35$U <-  datal35$No..of.treated.patients

data.obs <- datal35[!is.na(datal35$Y), ] # subset: 98
data.mis <- datal35[is.na(datal35$Y), ]  # subset: 55
data.all <- rbind(data.obs, data.mis)    # total: 153

Z <- rep(1, dim(data.mis)[1]) # RC: Z=0, LC: Z=1
# Right-censored: cut = U; Left-censored: cut = L; 
cut <- data.mis$L 

Y <- as.numeric(data.obs$Y) # true number of AEs (observed)
Z <- as.numeric(Z)          # "missing" AEs
cut <- as.numeric(cut)
J1 <- length(Y)
J2 <- length(Z)
N <- as.numeric(data.all$No..of.treated.patients)  # number of pts

study <- as.numeric(as.factor(data.all$Trial.New.))
#study <- as.numeric(as.factor(data.obs$Trial.New.)) # for observed-only model
#study <-as.numeric(factor(as.character(data.all$Trial.New), level = unique(as.character(data.all$Trial.New.))))
dose  <- as.numeric(as.factor(data.all$Dose))
# tb <- table(data.all$Dose) # 15 levels!!!
drug <- as.numeric(factor(data.all$Drug, 
                          level = c("Nivolumab", "Pembrolizumab",
                                    "Atezolizumab", "Avelumab",
                                    "Durvalumab"))) # 5 levels
cancer <- as.numeric(factor(data.all$New.Tumor.Category, 
                            level = c("Lung cancer","GU cancer",
                                      "Melanoma", "Other cancer",
                                      "Mixed cancer types", "GI cancer", 
                                      "Hematologic malignancy"))) # 7

n.study <- length(table(study))   # [1] 125
n.drug <- length(table(drug))     # [1] 5
n.cancer <-length(table(cancer))  # [1] 7
#--------------------------------------------------------------

# dose level (n = 1,2,...,15)
a.dose.pre <- as.numeric(factor(dose*1000+drug)) 
# step1: table(factor(dose*1000+drug)); 
# step2: table(a.dose.pre) 
n.ad <- max(a.dose.pre) # NEW: 23 levels

swap <- function(vec, from, to) {
  tmp <- to[match(vec, from) ]
  tmp[is.na(tmp)] <- vec[is.na(tmp)]
  return(tmp)
}

a.dose <- swap(a.dose.pre, c(19,1,2,4,3,11),c(1,19,4,2,11,3))
a.dose <- swap(a.dose, c(5,4), c(4,5))
a.dose <- swap(a.dose, c(6,5), c(5,6))
# step3: table(a.dose) 

# group a.dose by drug; mapping drug.dose[1:5] = 1:5; a vector of "23"
drug.dose.pre <- drug[!duplicated(a.dose.pre)][order(a.dose.pre[!duplicated(a.dose.pre)])] # NEW
# [1] 1 1 1 "2" "4" "5" 1 2 3 1 "3" 5 1 2 3 2 3 1 "1" 1 1 2 3
temp1 <- replace(drug.dose.pre, c(1, 19), drug.dose.pre[c(19, 1)])
temp2 <- replace(temp1, c(2, 4), temp1[c(4, 2)])
temp3 <- replace(temp2, c(3, 11), temp2[c(11, 3)])
temp4 <- replace(temp3, c(4, 5), temp3[c(5, 4)])
drug.dose <- replace(temp4, c(5, 6), temp4[c(6, 5)])
# [1] 1 2 3 4 5 1 1 2 3 1 1 5 1 2 3 2 3 1 1 1 1 2 3

# For plot, group a.dose by dose; a vector of "23"
dose.ad.pre <- dose[!duplicated(a.dose.pre)][order(a.dose.pre[!duplicated(a.dose.pre)])]
# [1]  1  2  "3"  "3"  "3"  3  4  4  4  5 "6"  7  8  8  9 10 11 12 "13" 14 15 15 15
temp1 <- replace(dose.ad.pre, c(1, 19), dose.ad.pre[c(19, 1)])
temp2 <- replace(temp1, c(2, 4), temp1[c(4, 2)])
temp3 <- replace(temp2, c(3, 11), temp2[c(11, 3)])
temp4 <- replace(temp3, c(4, 5), temp3[c(5, 4)])
dose.ad <- replace(temp4, c(5, 6), temp4[c(6, 5)])
# [1] 13  3  6  3  3  2  4  4  4  5  3  7  8  8  9 10 11 12  1 14 15 15 15

# For study.adj, replace "mu" with "theta.ad[study.ad[i1]]"
library(dplyr)
test <- data[ ,c(2,5,8,10,15)] %>% 
  group_by(Trial.New.) %>% 
  filter(No..of.treated.patients == max(No..of.treated.patients))
# only keep high dose row if study has duplicates
test <- data.frame(test[-c(8,57,81,104),]) # 125 rows
test$drug <- as.numeric(factor(test$Drug, 
                               level = c("Nivolumab", "Pembrolizumab",
                                         "Atezolizumab", "Avelumab",
                                         "Durvalumab"))) # 5 levels
test$dose  <- as.numeric(as.factor(test$Dose))
test$ad.pre <- as.numeric(factor(test$dose*1000+test$drug))
# table(factor(test$dose*1000+test$drug)); table(test$ad.pre); # "19 levels"

# mapping to a.dose.pre (19 levels to 23 levels); note: 8, 11 missing
test$ad.pre <- swap(test$ad.pre,c(19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2),
                    c(23,22,21,19,18,17,16,15,14,12,11,10,8,7,6,5,4,3))
# mapping to a.dose (first five: main dose)
test$ad <- swap(test$ad.pre, c(19,4,11,5,6,1,2,3,4,5), 
                c(1,2,3,4,5,19,4,11,5,6))

# View(test)
# main dose for a study
study.ad <- test$ad # a vector of length 125

# For study.adj, "theta.cancer[study.cancer[i1]]"
# Keep unique one, Trial 55, 122 have different cancer types (include study 1)
test2 <- unique(data[,c(2,5,8)])
test2 = test2[-c(55,123), ]
test2$cancer <- as.numeric(factor(test2$New.Tumor.Category, 
                                  level = c("Lung cancer","GU cancer",
                                            "Melanoma", "Other cancer",
                                            "Mixed cancer types", "GI cancer", 
                                            "Hematologic malignancy")))
# cancer type for each study
# which(test2$New.Tumor.Category=="Mixed cancer types")
study.cancer <- test2$cancer # length 125

library(rjags)
set.seed(1)
a <- 0.0016 # scale.prior: A = 25 (default)
#a <- 0.16   # scale.prior: A = 2.5 # jags paper
#a <- 0.01   # scale.prior: A = 10 (cauchy prior) simulation


data <- list (Y=Y, N=N, Z=Z, cut=cut, J1=J1, J2=J2, study=study, 
              n.study=n.study, a.dose=a.dose, 
              cancer=cancer, n.cancer=n.cancer, study.ad=study.ad,
              n.ad=n.ad, a=a, study.cancer=study.cancer)
inits <- function() {list (mu=rnorm(1),mu.study=rnorm(1),mu.cancer=rnorm(1))}

parameters <- c("sigma.study", "sigma.ad", "sigma.cancer", 
                "p.study.adj","p.cancer.adj", "p.ad.adj")
# test model: pass
# model.1 <- jags.model("jrsm.g35.m1.txt", data, inits, n.chains=3, n.adapt=100)

library(R2jags)
library(loo) # waic

system.time(model.1 <- jags.parallel(data=data, inits=inits, parameters.to.save=parameters,
                         model.file="jrsm.g35.m1.txt", # "jrsm.g35.m1.bug",
                         n.chains = 3, n.iter = 45000, n.burnin = 15000, #n.iter = 30000, n.burnin = 30000,
                         n.thin = 3, n.cluster = 3, DIC = TRUE, jags.seed = 123))

#model.1 <- jags(data = data, inits=inits, parameters.to.save = parameters, n.iter = 45000, 
#           model.file = "jrsm.g35.m1.bug", n.chains = 3, n.burnin = 15000)

#library(coda)
#View(model.1$BUGSoutput$sims.list)
#res <- as.data.frame(round(model.1$BUGSoutput$summary[-1,c(5,3,7)],4))

DIC1 = model.1$BUGSoutput$DIC; DIC1
pD1 = model.1$BUGSoutput$pD; pD1
DIC1 - 2*pD1

WAIC1 <- waic(-model.1$BUGSoutput$sims.list$deviance/2); #WAIC1
WAIC1$estimates["waic",]

#DIC <- m1$BUGSoutput$DIC
#pD <- m1$BUGSoutput$pD


# JAGS in parallel
library(dclone)
n.cores=3
timer <- proc.time()
cl <- makePSOCKcluster(n.cores)
tmp <- clusterEvalQ(cl, library(dclone))
m1 <- jags.parfit(cl = cl, data = data, params = parameters, 
                  model ="jrsm.g35.m1.txt", 
                  inits = inits,
                  n.chains = 3, 
                  n.adapt = 1000, 
                  n.update = 30000,
                  n.iter = 30000, 
                  thin = 3)
stopCluster(cl)
time.taken <- proc.time() - timer
timings <- time.taken[3] #~300s

# deviance
system.time(model.1 <- jags.model("jrsm.g35.m1.txt", data, inits, n.chains=3, n.adapt=1000)) # 10s
system.time(update(model.1,30000)); # 330s
system.time(m1 <- coda.samples(model=model.1, variable.names=parameters, n.iter=30000, thin=3)) # 330s
system.time(dic.pd.m1 <- dic.samples(model=model.1, n.iter=30000, type="pD")); #340s
dic.pd.m1
# dic1 = sum(dic.pd.m1$deviance) +  sum(dic.pd.m1$penalty)
# Mean deviance: 189.2 penalty 21.93 

mcmc.df = rbind(as.data.frame(m1[[1]]), as.data.frame(m1[[2]]), as.data.frame(m1[[3]]))
mean(D1); D1_mean; pD1 # 189.42; 168.87; 20.55


data <- list (Y=Y, N=N, Z=Z, cut=cut, J1=J1, J2=J2, study=study, 
              n.study=n.study, drug=drug, n.drug=n.drug, a=a)
inits <- function() {list (mu=rnorm(1),mu.study=rnorm(1))}
#parameters <- c("sigma.study", "sigma.drug", "theta.study", "theta.drug", "mu.adj","p.study.adj","p.drug.adj")
parameters <- c("sigma.study", "sigma.drug",
                "p.study.adj","p.drug.adj")

system.time(model.2 <- jags.parallel(data=data, inits=inits, parameters.to.save=parameters,
                         model.file="jrsm.g35.m2.txt", # "jrsm.g35.m2.bug",
                         n.chains = 3, n.iter = 45000, n.burnin = 15000, #n.iter = 30000, n.burnin = 30000,
                         n.thin = 3, n.cluster = 3, DIC = TRUE, jags.seed = 123))
as.data.frame(round(model.2$BUGSoutput$summary[,c(5,3,7)],4))
DIC2 = model.2$BUGSoutput$DIC; DIC2
pD2 = model.2$BUGSoutput$pD; pD2
DIC2 - 2*pD2

WAIC2 <- waic(-model.2$BUGSoutput$sims.list$deviance/2)
WAIC2$estimates["waic",]

# deviance
system.time(model.2 <- jags.model("jrsm.g35.m2.txt", data, inits, n.chains=3, n.adapt=1000)) # 7s
system.time(update(model.2,30000)); # 200s
system.time(m2 <- coda.samples(model=model.2, variable.names=parameters, n.iter=30000, thin=3)) # 200s
system.time(dic.pd.m2 <- dic.samples(model=model.2, n.iter=30000, type="pD")); #210s
dic.pd.m2

mcmc.df = rbind(as.data.frame(m2[[1]]), as.data.frame(m2[[2]]), as.data.frame(m2[[3]]))


data <- list (Y=Y, N=N, Z=Z, cut=cut, J1=J1, J2=J2, study=study, 
              n.study=n.study, a.dose=a.dose, study.ad=study.ad,
              n.ad=n.ad, a=a)
inits <- function() {list (mu=rnorm(1),mu.study=rnorm(1))}
# parameters <- c("sigma.study", "sigma.ad", "theta.study","theta.ad",
#                 "mu.adj","p.study.adj","p.ad.adj")
parameters <- c("sigma.study", "sigma.ad", 
                "p.study.adj","p.ad.adj")

system.time(model.3 <- jags.parallel(data=data, inits=inits, parameters.to.save=parameters,
                                     model.file="jrsm.g35.m3.txt", # "jrsm.g35.m3.bug",
                                     n.chains = 3, n.iter = 45000, n.burnin = 15000, #n.iter = 30000, n.burnin = 30000,
                                     n.thin = 3, n.cluster = 3, DIC = TRUE, jags.seed = 123)) #180s
as.data.frame(round(model.3$BUGSoutput$summary[,c(5,3,7)],4))
DIC3 = model.3$BUGSoutput$DIC; DIC3
pD3 = model.3$BUGSoutput$pD; pD3
DIC3 - 2*pD3

WAIC3 <- waic(-model.3$BUGSoutput$sims.list$deviance/2)
WAIC3$estimates["waic",]

system.time(model.3 <- jags.model("jrsm.g35.m3.txt", data, inits, n.chains=3, n.adapt=1000)) # 8s
system.time(update(model.3,30000)); # 220s
system.time(m3 <- coda.samples(model=model.3, variable.names=parameters, n.iter=30000, thin=3)) # 220s
system.time(dic.pd.m3 <- dic.samples(model=model.3, n.iter=30000, type="pD")); #220s
dic.pd.m3

mcmc.df = rbind(as.data.frame(m3[[1]]), as.data.frame(m3[[2]]), as.data.frame(m3[[3]]))

data <- list (Y=Y, N=N, Z=Z, cut=cut, J1=J1, J2=J2, study=study, 
              n.study=n.study, a=a)
inits <- function() {list (mu.study=rnorm(1))}
parameters <- c("sigma.study", #"theta.study","mu.adj",
                "p.study.adj")
parameters <- c("theta")

system.time(model.4 <- jags.parallel(data=data, inits=inits, parameters.to.save=parameters,
                                     model.file="jrsm.g35.m4.txt", # "jrsm.g35.m4.bug",
                                     n.chains = 3, n.iter = 45000, n.burnin = 15000, #n.iter = 30000, n.burnin = 30000,
                                     n.thin = 3, n.cluster = 3, DIC = TRUE, jags.seed = 123)) #180s
# pD = var(deviance)/2
DIC4 = model.4$BUGSoutput$DIC; DIC4
pD4 = model.4$BUGSoutput$pD; pD4
DIC4 - 2*pD4

WAIC4 <- waic(-model.4$BUGSoutput$sims.list$deviance/2)
WAIC4$estimates["waic",]

system.time(model.4 <- jags.model("jrsm.g35.m4.txt", data, inits, n.chains=3, n.adapt=1000)) # 4s
system.time(update(model.4,30000)); # 75s
system.time(m4 <- coda.samples(model=model.4, variable.names=parameters, n.iter=30000, thin=3)) # 75s
finalData <- as.data.frame(round(summary(m4)[[2]][,c(3,1,5)],4)); View(finalData)
system.time(dic.pd.m4 <- dic.samples(model=model.4, n.iter=30000, type="pD")); #220s
dic.pd.m4
# Mean deviance:  188.8 


mcmc.df = rbind(as.data.frame(m4[[1]]), as.data.frame(m4[[2]]), as.data.frame(m4[[3]]))
theta.obs = mcmc.df[,1:J1]; N.obs = N[1:J1]
theta.cens = mcmc.df[,J1+1:J2]; N.cens = N[J1+1:J2]

# manual calculation: deviance 
D4 <- c()
M = nrow(mcmc.df)
system.time(for(i in 1:M){
  D4[i] <- -2*sum(dbinom(x=Y,size=N.obs,prob=as.numeric(theta.obs[i,]),log=T),
                  log(pbinom(q=cut, size=N.cens, prob=as.numeric(theta.cens[i,]))))
})
mean(D4) # 188.86

D4_mean <- -2*sum(dbinom(x=Y,size=N.obs,prob=as.numeric(apply(theta.obs[,],2,mean)),log=T),
                  log(pbinom(q=cut, size=N.cens, prob=as.numeric(apply(theta.cens[,],2,mean)))))

pD4 = mean(D4) - D4_mean # 22.94
# The more complicated model, the lower the likelihood (or, the higher the deviance) at the posterior mean.
# mean(D4) + pD4; D4_mean + 2*pD4


# FINAL MODEL!!!!
data <- list (Y=Y, N=N, Z=Z, cut=cut, J1=J1, J2=J2, study=study, 
              cancer=cancer, n.cancer=n.cancer, study.cancer=study.cancer,
              n.study=n.study, drug=drug, n.drug=n.drug, a=a)
# data <- list (Y=Y, N=N[1:J1], J1=J1, study=study[1:J1], 
#               cancer=cancer[1:J1], n.cancer=n.cancer,
#               n.study=n.study, drug=drug[1:J1], n.drug=n.drug, a=a)
inits <- function() {list (mu=rnorm(1),mu.study=rnorm(1),mu.cancer=rnorm(1))}
#parameters <- c("mu.adj")
parameters <- c("sigma.study", "sigma.drug", "sigma.cancer", "mu.adj",
                "p.study.adj","p.drug.adj","p.cancer.adj")

system.time(model.5 <- jags.model("jrsm.g35.m5.txt", data, inits, n.chains=3, n.adapt=1000)) # 10s
system.time(update(model.5,30000)); # 75s
system.time(m5 <- coda.samples(model=model.5, variable.names=parameters, n.iter=30000, thin=3)) # 75s

library(MCMCvis)
MCMCsummary(m5, Rhat=T)[,6] # Rhat = 1

system.time(dic.pd.m5 <- dic.samples(model=model.5, n.iter=30000, type="pD"));
dic.pd.m5

mcmc.df = rbind(as.data.frame(m5[[1]]), as.data.frame(m5[[2]]), as.data.frame(m5[[3]]))

save(m5, mcmc.df, file = "sim.G35.final.RData")

library(R2jags)
library(loo) 
system.time(model.5 <- jags.parallel(data=data, inits=inits, parameters.to.save=parameters,
                                     model.file="jrsm.g35.m5.txt", 
                                     n.chains = 3, n.iter = 45000, n.burnin = 15000, #n.iter = 30000, n.burnin = 30000,
                                     n.thin = 3, n.cluster = 3, DIC = TRUE, jags.seed = 123)) #160s

DIC5 = model.5$BUGSoutput$DIC; DIC5
pD5 = model.5$BUGSoutput$pD; pD5

finalData <- as.data.frame(round(model.5$BUGSoutput$summary[,c(5,3,7)],4))

WAIC5 <- waic(-model.5$BUGSoutput$sims.list$deviance/2)
WAIC5$estimates["waic",]


data <- list (Y=Y, N=N, Z=Z, cut=cut, J1=J1, J2=J2, study=study, 
              n.study=n.study, cancer=cancer, n.cancer=n.cancer, 
              a=a, study.cancer=study.cancer)
inits <- function() {list (mu.study=rnorm(1),mu.cancer=rnorm(1))}
parameters <- c("sigma.study", "sigma.cancer", 
                "p.study.adj","p.cancer.adj")

system.time(model.6 <- jags.parallel(data=data, inits=inits, parameters.to.save=parameters,
                                     model.file="jrsm.g35.m6.txt", 
                                     n.chains = 3, n.iter = 45000, n.burnin = 15000, #n.iter = 30000, n.burnin = 30000,
                                     n.thin = 3, n.cluster = 3, DIC = TRUE, jags.seed = 123)) #160s
as.data.frame(round(model.6$BUGSoutput$summary[,c(5,3,7)],4))
DIC6 = model.6$BUGSoutput$DIC; DIC6
waic(-model.6$BUGSoutput$sims.list$deviance/2)$estimates["waic",]


system.time(model.6 <- jags.model("jrsm.g35.m6.txt", data, inits, n.chains=3, n.adapt=1000)) # 4s
system.time(update(model.6,30000)); # 250s
system.time(m6 <- coda.samples(model=model.6, variable.names=parameters, n.iter=30000, thin=3)) # 250s
system.time(dic.pd.m6 <- dic.samples(model=model.6, n.iter=30000, type="pD")); #250s
dic.pd.m6

mcmc.df = rbind(as.data.frame(m6[[1]]), as.data.frame(m6[[2]]), as.data.frame(m6[[3]]))
theta.obs = mcmc.df[,1:J1]; N.obs = N[1:J1]
theta.cens = mcmc.df[,J1+1:J2]; N.cens = N[J1+1:J2]

# manual calculation: deviance 
D <- c()
M = nrow(mcmc.df)
system.time(for(i in 1:M){
  D[i] <- -2*sum(dbinom(x=Y,size=N.obs,prob=as.numeric(theta.obs[i,]),log=T),
                  log(pbinom(q=cut, size=N.cens, prob=as.numeric(theta.cens[i,]))))
})
mean(D) # 188.86

D_mean <- -2*sum(dbinom(x=Y,size=N.obs,prob=as.numeric(apply(theta.obs[,],2,mean)),log=T),
                  log(pbinom(q=cut, size=N.cens, prob=as.numeric(apply(theta.cens[,],2,mean)))))

pD = mean(D) - D_mean; pD # 22.94

library(R2jags)
library(loo) 
#set.seed(123)
system.time(model.6 <- jags.parallel(data=data, inits=inits, parameters.to.save=parameters,
                                     model.file="jrsm.g35.m6.txt", 
                                     n.chains = 3, n.iter = 45000, n.burnin = 15000, #n.iter = 30000, n.burnin = 30000,
                                     n.thin = 3, n.cluster = 3, DIC = TRUE, jags.seed = 123))

DIC6 = model.6$BUGSoutput$DIC; DIC6
pD6 = model.6$BUGSoutput$pD; pD6
DIC6 - 2*pD6

WAIC6 <- waic(-model.6$BUGSoutput$sims.list$deviance/2)
WAIC6$estimates["waic",]




#save(dic.pd.m, finalData, file = "resM.RData")
#diffdic(dic.pd.m1, dic.pd.m2)


finalData <- as.data.frame(round(summary(m1)[[2]][,c(3,1,5)],4)); View(finalData)
plot(m1[ ,c("mu.adj","p.study.adj[1]", "p.ad.adj[1]", "p.cancer.adj[1]")])
plot(m1[ ,c("sigma.study", "sigma.ad", "sigma.cancer")])

colnames(finalData) <- c("rate","l95","u95") 
finalData$subgroup <- c("deviance", "mu.adj",
                        rep("cancer",7), rep("drug",5), rep("study", 125),
                        rep("sigma",3)) 


# study plot
sub.pre <- rbind(finalData[which(finalData$subgroup=="study"|finalData$subgroup=="mu.adj"), ],
                  finalData[which(finalData$subgroup=="drug"), ]) 
# 1 + 125 studies + 5 drug = 131 obs.
study.name <- c("Overall", as.character(unique(data35$Source)), rep("",5))
#"Nivolumab","Pembrolizumab","Atezolizumab","Avelumab","Durvalumab"
sub <- cbind(sub.pre, study.name)

sub$level <- sub$study.name
sub$order <- c(137,2:47, 50:98, 101:115, 118:126, 129:134, 1,49,100,117,128)

source("functions_plot.R")

pdf("sim_G35study.pdf", height = 15, width = 8)
forest.pd(sub$rate*100, ci.lb= sub$l95*100, ci.ub=sub$u95*100, 
          xlab="Incidence, %", 
          slab=as.character(sub$level), 
          psize=1, pch=4, cex=.6, digits=2,
          rows=138-sub$order, refline=sub$rate[1]*100,
          at=seq(0,4.5,by=0.5),
          at.lab = c("0",NA,"1",NA,"2",NA,"3",NA,"4",NA),
          alim=c(0,4.5),                  
          xlim=c(-4,8),                
          ylim=c(1, 142))
text(-4,142, "Grade 3 or higher AEs", pos=4, cex=.8, col="blue")
text(5.7,142, "Incidence [95% CI]", pos=4, cex=.8, col="blue")
text(-4,137, "Nivolumab", pos=4, cex=.7, font=2, col="blue")
text(-4,89, "Pembrolizumab", pos=4, cex=.7, font=2, col="blue")
text(-4,38, "Atezolizumab", pos=4, cex=.7, font=2, col="blue")
text(-4,21, "Avelumab", pos=4, cex=.7, font=2, col="blue")
text(-4,10, "Durvalumab", pos=4, cex=.7, font=2, col="blue")
dev.off()
