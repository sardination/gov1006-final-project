################################################################################
#### SUMMARY STATISTICS ########################################################
################################################################################

summ %>%
  ungroup() %>%
  dplyr::select(
    PolicySocial_mean:MassEcon_mean, DemPID2Party_mean, DemControl) %>%
  as.data.frame() %>%
  stargazer(
    type="text",
    digits=2,
    covariate.labels=c(
      "Social Policy Liberalism",
      "Economic Policy Liberalism",
      "Mass Social Liberalism",
      "Mass Economic Liberalism",
      "Democratic PID",
      "Democratic Control"),
    float=FALSE
)

################################################################################
#### DIAGNOSTICS ###############################################################
################################################################################

### LAG/UNIT ROOT
## Levin-Lin-Chu (2002)
## H0: Unit root (delta = 1); H1: stationarity (delta < 1)
vars <- c("PolicySocial", "PolicyEcon", "MassSocial", "MassEcon")
test.args <- list(data=subset(summ.pd, !StPO %in% c("AK", "HI")),
                  test="levinlin", lags="AIC")
loop_purtest <- function (pvars, int, args=test.args) {
  plyr::llply(pvars, function (var) {
    do.call(purtest, args=c(list(formula(paste(var, "~", int))), args))
  })
}
mean_lags <- function (purtest.res) {
  plyr::llply(purtest.res, function (x) {
    plyr::laply(x$idres, function (y) y$lags)
  }) %>% plyr::llply(mean)
}
median_lags <- function (purtest.res) {
  plyr::llply(purtest.res, function (x) {
    plyr::laply(x$idres, function (y) y$lags)
  }) %>% plyr::llply(median)
}
summ_trho <- function (purtest.res) {
  plyr::llply(purtest.res, function (x) {
    plyr::laply(x$idres, function (y) y$trho)
  }) %>%
    plyr::llply(function (z) c(summary(z), "Prop. Neg." = mean(z < 0)))
}
MOCmean <- function (mod.ls) {
  if (all(plyr::laply(mod.ls, function (x) "moc" %in% names(x)))) {
    plyr::llply(mod.ls, function (x) apply(x$moc, 2, mean))
  } else {
    plyr::llply(mod.ls, function (x) apply(x, 2, mean))
  }
}
MOCsd <- function (mod.ls) {
  if (all(plyr::laply(mod.ls, function (x) "moc" %in% names(x)))) {
    plyr::llply(mod.ls, function (x) apply(x$moc, 2, sd))
  } else {
    plyr::llply(mod.ls, function (x) apply(x, 2, sd))
  }
}


## Demeaned within year (per year FEs)
pur0none <- loop_purtest(paste0(vars, "0_mean"), int=0)
names(pur0none) <- vars
pur0none
mean_lags(pur0none)
median_lags(pur0none)
summ_trho(pur0none)

## Demeaned within year (per year FEs) and state-specific intercepts
pur0int <- loop_purtest(paste0(vars, "0_mean"), int=1)
names(pur0int) <- vars
pur0int
mean_lags(pur0int)
median_lags(pur0int)
summ_trho(pur0int)

## Demeaned within year (per year FEs) and state-specific trends
pur0trend <- loop_purtest(paste0(vars, "0_mean"), int="trend")
names(pur0trend) <- vars
pur0trend
mean_lags(pur0trend)
median_lags(pur0trend)
summ_trho(pur0trend)

## Standardized within year (per year FEs)
pur00none <- loop_purtest(paste0(vars, "00_mean"), int=0)
names(pur00none) <- vars
pur00none
mean_lags(pur00none)
summ_trho(pur00none)

## Standardized within year (per year FEs) and state-specific intercepts
pur00int <- loop_purtest(paste0(vars, "00_mean"), int=1)
names(pur00int) <- vars
pur00int
mean_lags(pur00int)
summ_trho(pur00int)

##> According to AIC or SIC, mean optimal lag length for state policy time
##> series is generally between 1 and 2 (1 for unstandardized with state FEs).
##> Unit root can be convincingly rejected for economic variables, but not
##> necessarly for social (esp. policy) unless standardized within year (or
##> trended).
  
################################################################################
#### ANALYSES ##################################################################
################################################################################
  
set.seed(1)
its <- sort(sample(unique(data$It), min(500, length(unique(data$It)))))

model.names <- c(
  ## Table 1
  "xs_south_social", "xs_south_econ", "fe_south_social", "fe_south_econ",
  "dyn_south_social", "dyn_south_econ", "dp_south_social", "dp_south_econ",
  ## Table 2
  "sel_social", "sel_econ", "sel_econ_social", "sel_econ_social_pid1",
  "sel_econ_social_pid2",
  ## Table 3
  "dem_social", "dem_econ", "dp_social", "dp_econ", "adap_social", "adap_econ",
  "ey_social", "ey_econ",
  ## Table 4
  "dp_social_time", "dp_econ_time", "dp_social_time_south",
  "dp_econ_time_south", "dp_social_inst", "dp_econ_inst"
)

model.ls <- vector("list", length=length(model.names))
names(model.ls) <- model.names
  
### CROSS-SECTIONAL
## Social
model.ls[["xs_south_social"]]$model <-
  plm(Policy_mean ~
        MassM1_mean * South11 +
        Year * South11,
      data=summ.pd.social, model="pooling")
(model.ls[["xs_south_social"]]$coeftest <-
   clusterSE(model.ls[["xs_south_social"]]$model))
model.ls[["xs_south_social"]]$moc <-
  MOC(data=data.pd.social, its=its, model.ls[["xs_south_social"]]$model)
MOCsumm(model.ls[["xs_south_social"]]$moc)
model.ls[["xs_south_social"]]$moc %>%
  ggplot(aes(x=MassM1_mean)) +
  geom_histogram()

## Econ
model.ls[["xs_south_econ"]]$model <- model.ls[["xs_south_social"]]$model %>%
  update(data=summ.pd.social)
(model.ls[["xs_south_econ"]]$coeftest <-
   clusterSE(model.ls[["xs_south_econ"]]$model))
model.ls[["xs_south_econ"]]$moc <-
  MOC(data=data.pd.econ, its=its, model.ls[["xs_south_econ"]]$model)
MOCsumm(model.ls[["xs_south_econ"]]$moc)
model.ls[["xs_south_econ"]]$moc %>%
  ggplot(aes(x=MassM1_mean)) +
  geom_histogram()

### TWO-WAY FIXED EFFECTS
## Social
model.ls[["fe_south_social"]]$model <-
  plm(Policy_mean ~
        MassM1_mean * South11 +
        Year * South11 +
        StPO
      , data=summ.pd.social, model="pooling")
(model.ls[["fe_south_social"]]$coeftest <-
   clusterSE(model.ls[["fe_south_social"]]$model))
model.ls[["fe_south_social"]]$moc <-
  MOC(data=data.pd.social, its=its, model.ls[["fe_south_social"]]$model)
MOCsumm(model.ls[["fe_south_social"]]$moc)
model.ls[["fe_south_social"]]$moc %>%
  ggplot(aes(x=MassM1_mean)) +
  geom_histogram()

## Econ
model.ls[["fe_south_econ"]]$model <- model.ls[["fe_south_social"]]$model %>%
  update(data=summ.pd.econ)
(model.ls[["fe_south_econ"]]$coeftest <-
   clusterSE(model.ls[["fe_south_econ"]]$model))
model.ls[["fe_south_econ"]]$moc <-
  MOC(data=data.pd.econ, its=its, model.ls[["fe_south_econ"]]$model)
MOCsumm(model.ls[["fe_south_econ"]]$moc)
model.ls[["fe_south_econ"]]$moc %>%
  ggplot(aes(x=MassM1_mean)) +
  geom_histogram()

### DYNAMIC (LDV)
## Social
model.ls[["dyn_south_social"]]$model <-
  plm(Policy_mean ~
        MassM1_mean * South11 +
        Year * South11 +
        PolicyM1_mean
      , data=summ.pd.social, model="pooling")
(model.ls[["dyn_south_social"]]$coeftest <-
   clusterSE(model.ls[["dyn_south_social"]]$model))
model.ls[["dyn_south_social"]]$moc <-
  MOC(data=data.pd.social, its=its, model.ls[["dyn_south_social"]]$model)
MOCsumm(model.ls[["dyn_south_social"]]$moc)

## Econ
model.ls[["dyn_south_econ"]]$model <- model.ls[["dyn_south_social"]]$model %>%
  update(data=summ.pd.econ)
(model.ls[["dyn_south_econ"]]$coeftest <-
   clusterSE(model.ls[["dyn_south_econ"]]$model))
model.ls[["dyn_south_econ"]]$moc <-
  MOC(data=data.pd.econ, its=its, model.ls[["dyn_south_econ"]]$model)
MOCsumm(model.ls[["dyn_south_econ"]]$moc)
model.ls[["dyn_south_econ"]]$moc %>%
  dplyr::select(contains("MassM1_mean")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram()

## Test for significance of state-specific intercepts
pFtest(terms(model.ls[["dyn_south_social"]]$model), data=summ.pd.social,
       effect="individual")
pFtest(terms(model.ls[["dyn_south_econ"]]$model), data=summ.pd.econ,
       effect="individual")
plmtest(model.ls[["dyn_south_social"]]$model, effect="individual")
plmtest(model.ls[["dyn_south_econ"]]$model, effect="individual")
##> Some evidence for state FEs

### DYNAMIC PANEL (LDV + FE)
## Social
model.ls[["dp_south_social"]]$model <- 
  plm(Policy_mean ~
        MassM1_mean * South11 +
        Year * South11 +
        PolicyM1_mean +
        StPO 
      , data=summ.pd.social, model="pooling")
(model.ls[["dp_south_social"]]$coeftest <-
   clusterSE(model.ls[["dp_south_social"]]$model))
model.ls[["dp_south_social"]]$moc <-
  MOC(data=data.pd.social, its=its, model.ls[["dp_south_social"]]$model)
MOCsumm(model.ls[["dp_south_social"]]$moc)
model.ls[["dp_south_social"]]$moc %>%
  dplyr::select(contains("MassM1_mean")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram()

## Econ
model.ls[["dp_south_econ"]]$model <- model.ls[["dp_south_social"]]$model %>%
  update(data=summ.pd.econ)
(model.ls[["dp_south_econ"]]$coeftest <-
   clusterSE(model.ls[["dp_south_econ"]]$model))
model.ls[["dp_south_econ"]]$moc <-
  MOC(data=data.pd.econ, its=its, model.ls[["dp_south_econ"]]$model)
MOCsumm(model.ls[["dp_south_econ"]]$moc)
model.ls[["dp_south_econ"]]$moc %>%
  dplyr::select(contains("MassM1_mean")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram() +
  geom_vline(xintercept=0, linetype="dotted")

## responsiveness in South
model.ls[["dp_south_social"]]$moc %>%
  mutate(MassSouth = MassM1_mean + `MassM1_mean:South11South`) %>%
  MOCsumm

model.ls[["dp_south_econ"]]$moc %>%
  mutate(MassSouth = MassM1_mean + `MassM1_mean:South11South`) %>%
  MOCsumm


### Long-run Multiplier
## Social
model.ls[["dp_south_social"]]$moc %>%
  mutate(MassM1Nonsouth = `MassM1_mean`,
         MassM1South = `MassM1_mean` + `MassM1_mean:South11South`,
         MassM1Nation = 39/50 * MassM1Nonsouth + 11/50 * MassM1South,
         PolicyM1 = `PolicyM1_mean`,
         MassM1NonsouthLRM = MassM1Nonsouth / (1 - PolicyM1),
         MassM1SouthLRM = MassM1South / (1 - PolicyM1),
         MassM1NationLRM = MassM1Nation / (1 - PolicyM1)
         ) %>%
  MOCsumm

## Econ
model.ls[["dp_south_econ"]]$moc %>%
  mutate(MassM1Nonsouth = `MassM1_mean`,
         MassM1South = `MassM1_mean` + `MassM1_mean:South11South`,
         MassM1Nation = 39/50 * MassM1Nonsouth + 11/50 * MassM1South,
         PolicyM1 = `PolicyM1_mean`,
         MassM1NonsouthLRM = MassM1Nonsouth / (1 - PolicyM1),
         MassM1SouthLRM = MassM1South / (1 - PolicyM1),
         MassM1NationLRM = MassM1Nation / (1 - PolicyM1)
         ) %>%
  MOCsumm

### SELECTION
## Social
model.ls[["sel_social"]]$model <-
  plm(DemControl ~
        MassSocialM1_mean +
        DemControlM1 +
        StPO + Year
    , data=subset(summ.pd, YearAfterHouseElection)
    , model="pooling")
(model.ls[["sel_social"]]$coeftest <-
   clusterSE(model.ls[["sel_social"]]$model))
model.ls[["sel_social"]]$moc <- subset(data.pd, YearAfterHouseElection) %>%
  MOC(its=its, model.ls[["sel_social"]]$model)
MOCsumm(model.ls[["sel_social"]]$moc)
model.ls[["sel_social"]]$moc %>%
  dplyr::select(contains("Mass")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram()

## Econ
model.ls[["sel_econ"]]$model <- model.ls[["sel_social"]]$model %>%
  update(formula=. ~ . + MassEconM1_mean - MassSocialM1_mean)
(model.ls[["sel_econ"]]$coeftest <-
   clusterSE(model.ls[["sel_econ"]]$model))
model.ls[["sel_econ"]]$moc <- subset(data.pd, YearAfterHouseElection) %>%
  MOC(its=its, model.ls[["sel_econ"]]$model)
MOCsumm(model.ls[["sel_econ"]]$moc)
model.ls[["sel_econ"]]$moc %>%
  dplyr::select(contains("Mass")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram()

## Econ + Social
model.ls[["sel_econ_social"]]$model <- model.ls[["sel_social"]]$model %>%
  update(formula=. ~ . + MassEconM1_mean)
(model.ls[["sel_econ_social"]]$coeftest <-
   clusterSE(model.ls[["sel_econ_social"]]$model))
model.ls[["sel_econ_social"]]$moc <- subset(data.pd, YearAfterHouseElection) %>%
  MOC(its=its, model.ls[["sel_econ_social"]]$model)
MOCsumm(model.ls[["sel_econ_social"]]$moc)
model.ls[["sel_econ_social"]]$moc %>%
  dplyr::select(contains("Mass")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram()

## Econ + Social + PIDM1
model.ls[["sel_econ_social_pid1"]]$model <- model.ls[["sel_social"]]$model %>%
  update(formula=. ~ . + MassEconM1_mean + DemPID2PartyM1_mean)
(model.ls[["sel_econ_social_pid1"]]$coeftest <-
   clusterSE(model.ls[["sel_econ_social_pid1"]]$model))
model.ls[["sel_econ_social_pid1"]]$moc <- subset(data.pd, YearAfterHouseElection) %>%
  MOC(its=its, model.ls[["sel_econ_social_pid1"]]$model)
MOCsumm(model.ls[["sel_econ_social_pid1"]]$moc)
model.ls[["sel_econ_social_pid1"]]$moc %>%
  dplyr::select(contains("Mass")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram()

## Econ + Social + PIDM2
model.ls[["sel_econ_social_pid2"]]$model <- model.ls[["sel_social"]]$model %>%
  update(formula=. ~ . + MassEconM1_mean + DemPID2PartyM2_mean)
(model.ls[["sel_econ_social_pid2"]]$coeftest <-
   clusterSE(model.ls[["sel_econ_social_pid2"]]$model))
model.ls[["sel_econ_social_pid2"]]$moc <-
  subset(data.pd, YearAfterHouseElection) %>%
  MOC(its=its, model.ls[["sel_econ_social_pid2"]]$model)
MOCsumm(model.ls[["sel_econ_social_pid2"]]$moc)
model.ls[["sel_econ_social_pid2"]]$moc %>%
  dplyr::select(contains("Mass")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram()

## model.ls[["sel_econ_social_pid2"]]$model <- model.ls[["sel_social"]]$model %>%
##   update(formula=. ~ . + MassEconM1_mean
##          + DemPID2PartyM2_mean)
## (model.ls[["sel_econ_social_pid2"]]$coeftest <-
##    clusterSE(model.ls[["sel_econ_social_pid2"]]$model))
## model.ls[["sel_econ_social_pid2"]]$moc <- subset(data.pd, YearAfterGovElection) %>%
##   MOC(its=its, model.ls[["sel_econ_social_pid2"]]$model)
## MOCsumm(model.ls[["sel_econ_social_pid2"]]$moc)


## Social and economic jointly significant?
model.ls[["sel_econ_social"]]$moc %>%
  mutate(MassSocialPlusEcon = MassSocialM1_mean + MassEconM1_mean) %>%
  MOCsumm

model.ls[["sel_econ_social_pid1"]]$moc %>%
  mutate(MassSocialPlusEcon = MassSocialM1_mean + MassEconM1_mean) %>%
  MOCsumm(digits=4)

model.ls[["sel_econ_social_pid2"]]$moc %>%
  mutate(MassSocialPlusEcon = MassSocialM1_mean + MassEconM1_mean) %>%
  MOCsumm(digits=4)

## model.ls[["sel_econ_social_pid2"]]$moc %>%
##   mutate(SocialNonsouthSumm = MassSocialM1_mean,
##          SocialSouthSumm = MassSocialM1_mean +
##            `MassSocialM1_mean:South11South`,
##          EconNonsouthSumm = MassEconM1_mean,
##          EconSouthSumm = MassEconM1_mean +
##            `South11South:MassEconM1_mean`) %>%
##   MOCsumm("Summ", digits=4)


### PARTY EFFECTS
## Social
model.ls[["dem_social"]]$model <-
  plm(Policy_mean ~
        DemControl + 
        PolicyM1_mean +
        StPO + Year
    , data=summ.pd.social
    , model="pooling")
(model.ls[["dem_social"]]$coeftest <-
   clusterSE(model.ls[["dem_social"]]$model))
model.ls[["dem_social"]]$moc <- data.pd.social %>%
  MOC(its=its, model.ls[["dem_social"]]$model)
MOCsumm(model.ls[["dem_social"]]$moc)
model.ls[["dem_social"]]$moc %>%
  dplyr::select(matches("Mass|Dem")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram()

## Econ
model.ls[["dem_econ"]]$model <- model.ls[["dem_social"]]$model %>%
  update(data=summ.pd.econ)
(model.ls[["dem_econ"]]$coeftest <-
   clusterSE(model.ls[["dem_econ"]]$model))
model.ls[["dem_econ"]]$moc <- data.pd.econ %>%
  MOC(its=its, model.ls[["dem_econ"]]$model)
MOCsumm(model.ls[["dem_econ"]]$moc)
model.ls[["dem_econ"]]$moc %>%
  dplyr::select(matches("Mass|Dem")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram()

### DYNAMIC RESPONSIVENESS
## Social
model.ls[["dp_social"]]$model <-
  plm(Policy_mean ~
        MassM1_mean + 
        PolicyM1_mean +
        StPO + Year
    , data=summ.pd.social
    , model="pooling")
(model.ls[["dp_social"]]$coeftest <-
   clusterSE(model.ls[["dp_social"]]$model))
model.ls[["dp_social"]]$moc <- data.pd.social %>%
  MOC(its=its, model.ls[["dp_social"]]$model)
MOCsumm(model.ls[["dp_social"]]$moc)
model.ls[["dp_social"]]$moc %>%
  dplyr::select(matches("Mass|Dem")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram()

## Econ
model.ls[["dp_econ"]]$model <- model.ls[["dp_social"]]$model %>%
  update(data=summ.pd.econ)
(model.ls[["dp_econ"]]$coeftest <-
   clusterSE(model.ls[["dp_econ"]]$model))
model.ls[["dp_econ"]]$moc <- data.pd.econ %>%
  MOC(its=its, model.ls[["dp_econ"]]$model)
MOCsumm(model.ls[["dp_econ"]]$moc)
model.ls[["dp_econ"]]$moc %>%
  dplyr::select(matches("Mass|Dem")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram()

### ADAPTATION
## Social
model.ls[["adap_social"]]$model <-
  plm(Policy_mean ~
        DemControl + 
        MassM1_mean +
        PolicyM1_mean +
        StPO + Year
    , data=summ.pd.social
    , model="pooling")
(model.ls[["adap_social"]]$coeftest <-
   clusterSE(model.ls[["adap_social"]]$model))
model.ls[["adap_social"]]$moc <- data.pd.social %>%
  MOC(its=its, model.ls[["adap_social"]]$model)
MOCsumm(model.ls[["adap_social"]]$moc)
model.ls[["adap_social"]]$moc %>%
  dplyr::select(matches("Mass|Dem")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram()

data.pd.social %>%
  MOC(its=sample(its, 100),
      update(model.ls[["adap_social"]]$model, . ~ . + DemPropLeg)) %>%
  MOCsumm()

## Econ
model.ls[["adap_econ"]]$model <- model.ls[["adap_social"]]$model %>%
  update(data=summ.pd.econ)
(model.ls[["adap_econ"]]$coeftest <-
   clusterSE(model.ls[["adap_econ"]]$model))
model.ls[["adap_econ"]]$moc <- data.pd.econ %>%
  MOC(its=its, model.ls[["adap_econ"]]$model)
MOCsumm(model.ls[["adap_econ"]]$moc, digits=5)
model.ls[["adap_econ"]]$moc %>%
  dplyr::select(matches("Mass|Dem")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram()

data.pd.econ %>%
  MOC(its=sample(its, 100),
      update(model.ls[["adap_econ"]]$model, . ~ . + DemPropLeg)) %>%
  MOCsumm()


## Social
model.ls[["ey_social"]]$model <-
  plm(Policy_mean ~
        MassM1_mean : YearAfterHouseElection +
        PolicyM1_mean +
        StPO + Year
    , data=summ.pd.social
    , model="pooling")
(model.ls[["ey_social"]]$coeftest <-
   clusterSE(model.ls[["ey_social"]]$model))
model.ls[["ey_social"]]$moc <- data.pd.social %>%
  MOC(its=its, model.ls[["ey_social"]]$model)
MOCsumm(model.ls[["ey_social"]]$moc)

## Econ
model.ls[["ey_econ"]]$model <- model.ls[["ey_social"]]$model %>%
  update(data=summ.pd.econ)
(model.ls[["ey_econ"]]$coeftest <-
   clusterSE(model.ls[["ey_econ"]]$model))
model.ls[["ey_econ"]]$moc <- data.pd.econ %>%
  MOC(its=its, model.ls[["ey_econ"]]$model)
MOCsumm(model.ls[["ey_econ"]]$moc)

## Mediation estimates
## method 1 (multiplication)
## Social
social_on_dem <- subset(model.ls[["sel_social"]]$moc,, MassSocialM1_mean)
dem_on_social <- dplyr::select(model.ls[["dem_social"]]$moc, DemControl)
mediation_social <- social_on_dem * dem_on_social
(mediation_social_est <- colMeans(mediation_social))
(mediation_social_se <- apply(mediation_social, 2, sd))
(mediation_social_z <- mediation_social_est / mediation_social_se)
apply(mediation_social, 2, pNorm)
## Econ
econ_on_dem <- subset(model.ls[["sel_econ"]]$moc,, MassEconM1_mean)
dem_on_econ <- dplyr::select(model.ls[["dem_econ"]]$moc, DemControl)
mediation_econ <- econ_on_dem * dem_on_econ
(mediation_econ_est <- colMeans(mediation_econ))
(mediation_econ_se <- apply(mediation_econ, 2, sd))
(mediation_econ_z <- mediation_econ_est / mediation_econ_se)
apply(mediation_econ, 2, pNorm)

mediation_social_est / model.ls[["dp_social"]]$moc %>%
  MOCsumm() %>%
  subset(variable == "MassM1_mean_est", value)

mediation_econ_est / model.ls[["dp_econ"]]$moc %>%
  MOCsumm() %>%
  subset(variable == "MassM1_mean_est", value)

## method 2 (subtraction)
## social
(model.ls[["dp_social"]]$moc %>%
 MOCsumm(digits=5) %>%
 subset(variable == "MassM1_mean_est", value)) -
  (model.ls[["adap_social"]]$moc %>%
   MOCsumm(digits=5) %>%
   subset(variable == "MassM1_mean_est", value))
## econ
(model.ls[["dp_econ"]]$moc %>%
 MOCsumm(digits=5) %>%
 subset(variable == "MassM1_mean_est", value)) -
  (model.ls[["adap_econ"]]$moc %>%
   MOCsumm(digits=5) %>%
   subset(variable == "MassM1_mean_est", value))

### MODERATORS
## Social by Time
model.ls[["dp_social_time"]]$model <-
  plm(Policy_mean ~
        MassM1_mean * Pre72 +
        PolicyM1_mean +
        StPO + Year
    , data=summ.pd.social, model="pooling")
(model.ls[["dp_social_time"]]$coeftest <-
   clusterSE(model.ls[["dp_social_time"]]$model))
model.ls[["dp_social_time"]]$moc <- data.pd.social %>%
  MOC(its=its, model.ls[["dp_social_time"]]$model)
MOCsumm(model.ls[["dp_social_time"]]$moc)

## Econ
model.ls[["dp_econ_time"]]$model <- model.ls[["dp_social_time"]]$model %>%
  update(data=summ.pd.econ)
(model.ls[["dp_econ_time"]]$coeftest <-
   clusterSE(model.ls[["dp_econ_time"]]$model))
model.ls[["dp_econ_time"]]$moc <- data.pd.econ %>%
  MOC(its=its, model.ls[["dp_econ_time"]]$model)
MOCsumm(model.ls[["dp_econ_time"]]$moc)

## Social by Time by South
model.ls[["dp_social_time_south"]]$model <-
  plm(Policy_mean ~
        MassM1_mean * Pre72 * South11 +
        PolicyM1_mean +
        StPO + Year
    , data=summ.pd.social, model="pooling")
(model.ls[["dp_social_time_south"]]$coeftest <-
   clusterSE(model.ls[["dp_social_time_south"]]$model))
model.ls[["dp_social_time_south"]]$moc <- data.pd.social %>%
  MOC(its=its, model.ls[["dp_social_time_south"]]$model)
MOCsumm(model.ls[["dp_social_time_south"]]$moc)

## Econ by Time by South
model.ls[["dp_econ_time_south"]]$model <-
  model.ls[["dp_social_time_south"]]$model %>%
  update(data=summ.pd.econ)
(model.ls[["dp_econ_time_south"]]$coeftest <-
   clusterSE(model.ls[["dp_econ_time_south"]]$model))
model.ls[["dp_econ_time_south"]]$moc <- data.pd.econ %>%
  MOC(its=its, model.ls[["dp_econ_time_south"]]$model)
MOCsumm(model.ls[["dp_econ_time_south"]]$moc)

## Social (Institutions)
model.ls[["dp_social_inst"]]$model <-
  plm(Policy_mean ~
        MassM1_mean * Pre72 * South11 +
        MassM1_mean * SuffrageRestrictionIndex0 +
        MassM1_mean * CampaignFinanceIndex0 +
        MassM1_mean * CitizenPolicymakingIndex0 +
        MassM1_mean * LogLegDays0 +
        PolicyM1_mean +
        StPO + Year
    , data=summ.pd.social, model="pooling")
(model.ls[["dp_social_inst"]]$coeftest <-
   clusterSE(model.ls[["dp_social_inst"]]$model))
model.ls[["dp_social_inst"]]$moc <- data.pd.social %>%
  MOC(its=its, model.ls[["dp_social_inst"]]$model)
MOCsumm(model.ls[["dp_social_inst"]]$moc)

## Econ (Institutions)
model.ls[["dp_econ_inst"]]$model <- model.ls[["dp_social_inst"]]$model %>%
  update(data=summ.pd.econ)
(model.ls[["dp_econ_inst"]]$coeftest <-
   clusterSE(model.ls[["dp_econ_inst"]]$model))
model.ls[["dp_econ_inst"]]$moc <- data.pd.econ %>%
  MOC(its=its, model.ls[["dp_econ_inst"]]$model)
MOCsumm(model.ls[["dp_econ_inst"]]$moc, digits=5)

################################################################################
#### TABLES ####################################################################
################################################################################

### Table 1
(tab1.ord <- c(grep("(?=.*social)(?=.*south)(?!.*time)",
                    model.names, perl=TRUE),
               grep("(?=.*econ)(?=.*south)(?!.*time)",
                    model.names, perl=TRUE)))
tab1.mods <- plyr::llply(model.ls[tab1.ord], function (x) x$model)
tab1.cl.est <- model.ls[tab1.ord] %>%
  plyr::llply(function (x) x$coeftest[, "Estimate"])
tab1.cl.se <- model.ls[tab1.ord] %>%
  plyr::llply(function (x) x$coeftest[, "Std. Error"])
tab1.moc.est <- MOCmean(model.ls[tab1.ord])
tab1.moc.se <- MOCsd(model.ls[tab1.ord])

tab1.mods %>%
  stargazer(
    type="latex"
  , float=FALSE
  , se=tab1.moc.se
  , coef=tab1.moc.est
  , t.auto=TRUE
  , p.auto=TRUE ## from standard normal (very close to t when DF = 3,500)
  , omit=c("Year", "StPO|Constant|^South11South$")
  , omit.labels=c("Year $\\times$ South FEs", "State FEs")
  , omit.stat=c("F", "rsq")
  , omit.table.layout = "n"
  , align=TRUE
  , initial.zero=FALSE
  , dep.var.caption="DV: Domain-Specific Policy Liberalism ($t$)"
  , dep.var.labels=c(
      "------------ Social ------------ $|$ ----------- Economic -----------")
  , dep.var.labels.include=TRUE
  , column.labels=c("XS", "FE", "LDV", "DP", "XS", "FE", "LDV", "DP")
  , order=c("MassM1\\_mean$", "MassM1\\_mean:South11South",
            "PolicyM1")
  , no.space=TRUE
  , digits=3
  , column.sep.width = "1pt"
  , covariate.labels=c(
      "Mass Liberalism$_{t-1}$",
      "Mass Lib$_{t-1}$ $\\times$ South",
      "Policy Liberalism$_{t-1}$")
  )

### Table 2
(tab2.ord <- c(grep("(?=.*sel)(?!.*pid1)", model.names, perl=TRUE)## ,
               ## grep("(?=.*pid1)", model.names, perl=TRUE)
               ))
tab2.mods <- plyr::llply(model.ls[tab2.ord], function (x) x$model)
tab2.cl.est <- model.ls[tab2.ord] %>%
  plyr::llply(function (x) x$coeftest[, "Estimate"])
tab2.cl.se <- model.ls[tab2.ord] %>%
  plyr::llply(function (x) x$coeftest[, "Std. Error"])
tab2.moc.est <- MOCmean(model.ls[tab2.ord])
tab2.moc.se <- MOCsd(model.ls[tab2.ord])

tab2.mods %>%
  stargazer(
    type="latex"
  , float=FALSE
  , se=tab2.moc.se
  , coef=tab2.moc.est
  , t.auto=TRUE
  , p.auto=TRUE ## from standard normal (very close to t when DF = 3,500)
  , omit=c("Year", "StPO|Constant")
  , omit.labels=c("Year FEs", "State FEs")
  , omit.stat=c("F", "rsq")
  , omit.table.layout = "n"
  , initial.zero=FALSE
  , align=TRUE
  , dep.var.caption="DV: Democratic Control Index  ($t$)"
  , dep.var.labels.include=FALSE
  , no.space=TRUE
  , digits=3
  , column.sep.width = "1pt"
  , order=c("MassSocial", "MassEcon", "PID", "Control")
  , covariate.labels=c(
      "Mass Social Lib$_{t-1}$",
      "Mass Econ Lib$_{t-1}$",
      "Mass Dem PID$_{t-2}$",
      ## "Mass Dem PID$_{t-1}$",
      "Dem Control$_{t-1}$"
      )
  )

### Table 3
(tab3.ord <-
   c(grep(paste0("(?=.*social)((?=.*dem)|(?=.*dp)|(?=.*adap)|(?=.*ey))",
                 "(?!.*south)(?!.*era)(?!.*inst)(?!.*time)"),
          model.names, perl=TRUE),
     grep(paste0("(?=.*econ)((?=.*dem)|(?=.*dp)|(?=.*adap)|(?=.*ey))",
                 "(?!.*south)(?!.*era)(?!.*inst)(?!.*time)"),
          model.names, perl=TRUE)))
tab3.mods <- plyr::llply(model.ls[tab3.ord], function (x) x$model)
tab3.cl.est <- model.ls[tab3.ord] %>%
  plyr::llply(function (x) x$coeftest[, "Estimate"])
tab3.cl.se <- model.ls[tab3.ord] %>%
  plyr::llply(function (x) x$coeftest[, "Std. Error"])
tab3.moc.est <- MOCmean(model.ls[tab3.ord])
tab3.moc.se <- MOCsd(model.ls[tab3.ord])

tab3.moc <- tab3.mods %>%
  stargazer(
    type="latex"
  , float=FALSE
  , se=tab3.moc.se
  , coef=tab3.moc.est
  , t.auto=TRUE
  , p.auto=TRUE ## from standard normal (very close to t when DF = 3,500)
  , omit=c("^Year", "StPO|Constant")
  , omit.labels=c("Year FEs", "State FEs")
  , omit.stat=c("F", "rsq")
  , omit.table.layout = "n"
  , align=TRUE
  , dep.var.caption="DV: Domain-Specific Policy Liberalism ($t$)"
  , dep.var.labels=c(
      "-------------- Social ------------- $|$ ----------- Economic -----------")
  , dep.var.labels.include=TRUE
  , order=c("DemControl", "MassM1\\_mean$", "YearAfterHouseElectionFALSE",
            "YearAfterHouseElectionTRUE", "Policy")
  , single.row=FALSE
  , no.space=TRUE
  , initial.zero=FALSE
  , digits=3
  , column.sep.width = "-10pt"
  , covariate.labels=c(
      "Dem Control$_{t}$",
      "Mass Lib$_{t-1}$",
      "Mass Lib$_{t-1}$ (No Elec$_{t-1}$)",
      "Mass Lib$_{t-1}$ (Elec$_{t-1}$)",
      "Policy Lib$_{t-1}$")    
  )

### Table 4
(tab4.ord <-
   c(grep("(?=.*social)((?=.*inst)|(?=.*time))", model.names, perl=TRUE),
     grep("(?=.*econ)((?=.*inst)|(?=.*time))", model.names, perl=TRUE)))
tab4.mods <- plyr::llply(model.ls[tab4.ord], function (x) x$model)
tab4.cl.est <- model.ls[tab4.ord] %>%
  plyr::llply(function (x) x$coeftest[, "Estimate"])
tab4.cl.se <- model.ls[tab4.ord] %>%
  plyr::llply(function (x) x$coeftest[, "Std. Error"])
tab4.moc.est <- MOCmean(model.ls[tab4.ord])
tab4.moc.se <- MOCsd(model.ls[tab4.ord])

tab4.moc <- tab4.mods %>%
  stargazer(
    type="latex"
  , float=FALSE
  , se=tab4.moc.se
  , coef=tab4.moc.est
  , t.auto=TRUE
  , p.auto=TRUE ## from standard normal (very close to t when DF = 4,500)
  , omit=c("^Year|^Pre72Pre-1972$|^Pre72Pre-1972:South11South$",
           "StPO|Constant|^South11South$")
  , omit.labels=c("Year FEs", "State FEs")
  , omit.stat=c("F", "rsq")
  , omit.table.layout = "n"
  , align=TRUE
  , dep.var.caption="DV: Domain-Specific Policy Liberalism ($t$)"
  , dep.var.labels=c(
      "-------------- Social -------------- $|$ ----------- Economic -----------")
  , dep.var.labels.include=TRUE
  , single.row=FALSE
  , no.space=TRUE
  , initial.zero=FALSE
  , digits=3
  , column.sep.width = "-10pt"
  , order=c("^MassM1\\_mean$",
            "^MassM1\\_mean:Pre72Pre-1972$",
            "^MassM1\\_mean:South11South$",
            "Pre72Pre-1972:South11South$",
            "Suffrage", "Campaign", "Citizen", "LegDays", "PolicyM1")
  , covariate.labels=c(
      "Mass Liberalism$_{t-1}$",
      "Mass Lib$_{t-1} \\times$ Pre-1972",
      "Mass Lib$_{t-1} \\times$ South",
      "Mass Lib$_{t-1} \\times$ Pre-1972 $\\times$ South",
      "Suffrage Restriction",
      "Suff Restrict $\\times$ Mass Lib$_{t-1}$",
      "Contribution Limits",
      "Contrib Limit $\\times$ Mass Lib$_{t-1}$",
      "Citizen Government",
      "Citizen Gov't $\\times$ Mass Lib$_{t-1}$",
      "Legislative Days (Logged)",
      "Leg Days $\\times$ Mass Lib$_{t-1}$",
      "Policy Liberalism$_{t-1}$")
  )
 
################################################################################
#### ROBUSTNESS CHECKS #########################################################
################################################################################

### LDV and LIV --- ADL(5,5,1)
## Social
dp.social.twoway.adl <-
  plm(Policy_mean ~ MassM1_mean + MassM2_mean + MassM3_mean +
        MassM4_mean + MassM5_mean +
        PolicyM1_mean + PolicyM2_mean + PolicyM3_mean +
        PolicyM4_mean + PolicyM5_mean + StPO + Year,
      data=summ.pd.social, model="pooling")
clusterSE(dp.social.twoway.adl)
dp.social.twoway.adl.moc <- MOC(data=data.pd.social, its=its,
                                   dp.social.twoway.adl)
dp.social.twoway.adl.moc2 <- dp.social.twoway.adl.moc %>%
  mutate(Mass14 = c(MassM1_mean + MassM2_mean + MassM3_mean +
                    MassM4_mean),
         Mass15 = c(MassM1_mean + MassM2_mean + MassM3_mean +
                    MassM4_mean + MassM5_mean),
         Policy14 = c(PolicyM1_mean + PolicyM2_mean + PolicyM3_mean +
                    PolicyM4_mean),
         Policy15 = c(PolicyM1_mean + PolicyM2_mean + PolicyM3_mean +
                    PolicyM4_mean + PolicyM5_mean))
MOCsumm(dp.social.twoway.adl.moc2)
  

## Econ
dp.econ.twoway.adl <-
  plm(Policy_mean ~ MassM1_mean + MassM2_mean + MassM3_mean + MassM4_mean
      + MassM5_mean + PolicyM1_mean + PolicyM2_mean + PolicyM3_mean
      + PolicyM4_mean + PolicyM5_mean + StPO + Year,
      data=summ.pd.econ, model="pooling")
clusterSE(dp.econ.twoway.adl)
dp.econ.twoway.adl.moc <- MOC(data=data.pd.econ, its=its, dp.econ.twoway.adl)
dp.econ.twoway.adl.moc2 <- dp.econ.twoway.adl.moc %>%
  mutate(Mass14 = c(MassM1_mean + MassM2_mean + MassM3_mean +
                    MassM4_mean),
         Mass15 = c(MassM1_mean + MassM2_mean + MassM3_mean +
                    MassM4_mean + MassM5_mean),
         Policy14 = c(PolicyM1_mean + PolicyM2_mean + PolicyM3_mean +
                    PolicyM4_mean),
         Policy15 = c(PolicyM1_mean + PolicyM2_mean + PolicyM3_mean +
                    PolicyM4_mean + PolicyM5_mean))
MOCsumm(dp.econ.twoway.adl.moc2)

ADL.mods <- list(
  dp.social.twoway.adl, dp.econ.twoway.adl
)
ADL.moc <- list(
  dp.social.twoway.adl.moc2, dp.econ.twoway.adl.moc2
)

stargazer(ADL.mods
        , type="latex"
        , float=FALSE
        , se=MOCsd(ADL.moc)
        , coef=MOCmean(ADL.moc)
        , t.auto=TRUE
        , p.auto=TRUE
        , omit=c("Year|^South", "StPO|Constant")
        , omit.labels=c("Year FEs", "State FEs")
        , omit.stat=c("F", "rsq")
        , align=TRUE
        , initial.zero=TRUE
        , no.space=TRUE
        , dep.var.caption="DV: Policy Liberalism ($t$)"
        , dep.var.labels.include=FALSE
        , column.labels=c("Social", "Economic")
        , covariate.labels=c(
            paste0("Mass Liberalism ($t-", 1:5, "$)"),
            paste0("Policy Liberalism ($t-", 1:5, "$)") 
          )
        , add.lines=list(
            c("Sum Mass Lib, Lags 1--4"
            , paste0(round(mean(dp.social.twoway.adl.moc2$Mass14), 3), "^{***}")
            , round(mean(dp.econ.twoway.adl.moc2$Mass14), 3)
              ),
            c(""
            , paste0("(", round(sd(dp.social.twoway.adl.moc2$Mass14), 3), ")")
            , paste0("(", round(sd(dp.econ.twoway.adl.moc2$Mass14), 3), ")")
              ),
            c("Sum Mass Lib, Lags 1--5"
            , paste0(round(mean(dp.social.twoway.adl.moc2$Mass15), 3), "^{***}")
            , paste0(round(mean(dp.econ.twoway.adl.moc2$Mass15), 3), "^{**}")
              ),
            c(""
            , paste0("(", round(sd(dp.social.twoway.adl.moc2$Mass15), 3), ")")
            , paste0("(", round(sd(dp.econ.twoway.adl.moc2$Mass15), 3), ")")),
            c("Sum Policy Lib, Lags 1--5"
            , paste0(round(mean(dp.social.twoway.adl.moc2$Policy15), 3),
                     "^{***}")
            , paste0(round(mean(dp.econ.twoway.adl.moc2$Policy15), 3),
                     "^{***}")),
            c(""
            , paste0("(", round(sd(dp.social.twoway.adl.moc2$Policy15), 3), ")")
            , paste0("(", round(sd(dp.econ.twoway.adl.moc2$Policy15), 3), ")"))
          ))

plyr::laply(MOCmean(ADL.moc), function (x) x["adjrsq"])

## ERROR CORRECTION MODEL
## Social
dp.social.D01.twoway.ec <-
  plm(PolicyD01_mean ~ MassM1_mean + PolicyM1_mean + Year + StPO,
      data=summ.pd.social, model="pooling")
clusterSE(dp.social.D01.twoway.ec)
dp.social.D01.twoway.ec.moc <- MOC(data=data.pd.social, its=its,
                                   dp.social.D01.twoway.ec)
MOCsumm(dp.social.D01.twoway.ec.moc)
## Econ
dp.econ.D01.twoway.ec <-
  plm(PolicyD01_mean ~ MassM1_mean  + PolicyM1_mean + Year + StPO,
      data=summ.pd.econ, model="pooling")
clusterSE(dp.econ.D01.twoway.ec)
dp.econ.D01.twoway.ec.moc <- MOC(data=data.pd.econ, its=its,
                                 dp.econ.D01.twoway.ec)
MOCsumm(dp.econ.D01.twoway.ec.moc)
##> Note that the effect of PolicyM1_mean is clearly negative, indicating mean
##> reversion.


### FIRST-DIFFERENCED DV AND NO LAG
## Two-Way FEs (Implies State Trend)
## Social
dp.social.D01.twoway <- plm(PolicyD01_mean ~ MassM1_mean + Year + StPO,
                            data=summ.pd.social, model="pooling")
clusterSE(dp.social.D01.twoway)
dp.social.D01.twoway.moc <- MOC(data=data.pd.social, its=its,
                                dp.social.D01.twoway)
MOCsumm(dp.social.D01.twoway.moc)
## Econ
dp.econ.D01.twoway <- plm(PolicyD01_mean ~ MassM1_mean + Year + StPO,
                          data=summ.pd.econ, model="pooling")
clusterSE(dp.econ.D01.twoway)
dp.econ.D01.twoway.moc <- MOC(data=data.pd.econ, its=its, dp.econ.D01.twoway)
MOCsumm(dp.econ.D01.twoway.moc)
##> Estimate robust except for econ twoway

## Year Intercepts
## Social
dp.social.D01.time <-
  plm(PolicyD01_mean ~ MassM1_mean + Year, data=summ.pd.social, model="pooling")
clusterSE(dp.social.D01.time)
dp.social.D01.time.moc <- MOC(data=data.pd.social, its=its, dp.social.D01.time)
MOCsumm(dp.social.D01.time.moc)
## Econ
dp.econ.D01.time <-
  plm(PolicyD01_mean ~ MassM1_mean + Year, data=summ.pd.econ, model="pooling")
clusterSE(dp.econ.D01.time)
dp.econ.D01.time.moc <- MOC(data=data.pd.econ, its=its, dp.econ.D01.time)
MOCsumm(dp.econ.D01.time.moc)

### DIFFERENCED DV AND IV
## First Difference
## Social
dp.social.D01D12.time <-
  plm(I(Policy_mean - PolicyM1_mean) ~ PolicyM1_mean +
        I(MassM1_mean - MassM2_mean) + MassM2_mean +
        Year
    , data=summ.pd.social, model="pooling")
clusterSE(dp.social.D01D12.time)
dp.social.D01D12.time.moc <-
  MOC(data=data.pd.social, its=its, dp.social.D01D12.time)
MOCsumm(dp.social.D01D12.time.moc)
## Econ
dp.econ.D01D12.time <-
  plm(I(Policy_mean - PolicyM1_mean) ~ PolicyM1_mean +
        I(MassM1_mean - MassM2_mean) + MassM2_mean +
        Year
    , data=summ.pd.econ, model="pooling")
clusterSE(dp.econ.D01D12.time)
dp.econ.D01D12.time.moc <-
  MOC(data=data.pd.econ, its=its, dp.econ.D01D12.time)
MOCsumm(dp.econ.D01D12.time.moc)

D01.mods <- list(
  dp.social.D01.twoway.ec, dp.social.D01.twoway, dp.social.D01.time,
  dp.social.D01D12.time,
  dp.econ.D01.twoway.ec, dp.econ.D01.twoway, dp.econ.D01.time,
  dp.econ.D01D12.time
)
D01.moc <- list(
  dp.social.D01.twoway.ec.moc, dp.social.D01.twoway.moc, dp.social.D01.time.moc,
  dp.social.D01D12.time.moc,
  dp.econ.D01.twoway.ec.moc, dp.econ.D01.twoway.moc, dp.econ.D01.time.moc,
  dp.econ.D01D12.time.moc
)

stargazer(D01.mods
        , type="latex"
        , float=FALSE
        , se=MOCsd(D01.moc)
        , coef=MOCmean(D01.moc)
        , t.auto=TRUE
        , p.auto=TRUE
        , omit=c("Year|^South", "StPO|Constant")
        , omit.labels=c("Year FEs", "State FEs")
        , omit.stat=c("F")
          ## , omit.table.layout = "n"
        , align=TRUE
        , initial.zero=FALSE
        , dep.var.caption="DV: First-Differenced Policy Liberalism ($t$)"
        , dep.var.labels.include=FALSE
        , dep.var.labels=c(
            "-------------- Social ------------------------- $|$ ---------------------- Economic -----------")
          , order=c("Policy", "Mass")
        , covariate.labels=c("Policy Lib ($t-1$)",
                             "Mass Lib ($t-1$)",
                             "$\\Delta$ Mass Lib ($t-1$)",
                             "Mass Lib ($t-2$)")
          )

### INSTITUTIONS
## Social
dp.social.inst <-
  plm(Policy_mean ~
        MassM1_mean * Pre72 * South11 +
        MassM1_mean * scale1sd(union_contribution_ban2) +
        MassM1_mean * scale1sd(individual_limit_mm) +
        MassM1_mean * scale1sd(corporate_limit_mm) +
        MassM1_mean * scale1sd(poll_tax) +
        MassM1_mean * scale1sd(literacy_test) +
        MassM1_mean * scale1sd(direct_dem) +
        MassM1_mean * scale1sd(term_limits_enact) +
        MassM1_mean * scale1sd(LogLegDays0) +
        PolicyM1_mean
      + Year + StPO
    , data=summ.pd.social
    , model="pooling")
clusterSE(dp.social.inst)
dp.social.inst.moc <- data.pd.social %>%
  MOC(its=its, dp.social.inst)
dp.social.inst.moc2 <- dp.social.inst.moc %>%
  mutate(massXinst = c(`MassM1_mean:scale1sd(union_contribution_ban2)` +
                       `MassM1_mean:scale1sd(individual_limit_mm)` +
                       `MassM1_mean:scale1sd(corporate_limit_mm)` +
                       -`MassM1_mean:scale1sd(poll_tax)` +
                       -`MassM1_mean:scale1sd(literacy_test)` +
                       `MassM1_mean:scale1sd(direct_dem)` +
                       `MassM1_mean:scale1sd(term_limits_enact)` +
                       `MassM1_mean:scale1sd(LogLegDays0)`))
MOCsumm(dp.social.inst.moc2)

## Econ
dp.econ.inst <-
  plm(Policy_mean ~
        MassM1_mean * Pre72 * South11 +
        MassM1_mean * scale1sd(union_contribution_ban2) +
        MassM1_mean * scale1sd(individual_limit_mm) +
        MassM1_mean * scale1sd(corporate_limit_mm) +
        MassM1_mean * scale1sd(poll_tax) +
        MassM1_mean * scale1sd(literacy_test) +
        MassM1_mean * scale1sd(direct_dem) +
        MassM1_mean * scale1sd(term_limits_enact) +
        MassM1_mean * scale1sd(LogLegDays0) +
        PolicyM1_mean
      + Year + StPO
    , data=summ.pd.econ
    , model="pooling")
clusterSE(dp.econ.inst)
dp.econ.inst.moc <- MOC(data=data.pd.econ, its=its, dp.econ.inst)
dp.econ.inst.moc2 <- dp.econ.inst.moc %>%
  mutate(massXinst = c(`MassM1_mean:scale1sd(union_contribution_ban2)` +
                       `MassM1_mean:scale1sd(individual_limit_mm)` +
                       `MassM1_mean:scale1sd(corporate_limit_mm)` +
                       -`MassM1_mean:scale1sd(poll_tax)` +
                        -`MassM1_mean:scale1sd(literacy_test)` +
                         `MassM1_mean:scale1sd(direct_dem)` +
                         `MassM1_mean:scale1sd(term_limits_enact)` +
                         `MassM1_mean:scale1sd(LogLegDays0)`))
MOCsumm(dp.econ.inst.moc2)

inst.mods <- list(dp.social.inst, dp.econ.inst)
inst.moc <- list(dp.social.inst.moc, dp.econ.inst.moc)

stargazer(inst.mods
        , type="latex"
        , float=FALSE
        , se=MOCsd(inst.moc)
        , coef=MOCmean(inst.moc)
        , t.auto=TRUE
        , p.auto=TRUE
        , omit=c("Year|^South|^Pre72Pre-1972$|^Pre72Pre-1972:South11South|StPO|Constant|PolicyM1_mean")
        , omit.labels=c("Year FEs \\& State FEs \\& LDV")
        , omit.stat=c("F")
        , align=TRUE
        , initial.zero=TRUE
        , no.space=TRUE
        , dep.var.caption="DV: Domain-Specific Policy Liberalism ($t$)"
        , dep.var.labels.include=FALSE
        , column.labels=c("Social", "Economic")
        , order=c("PolicyM1",
                  "^MassM1\\_mean$",
                  "^MassM1\\_mean:Pre72Pre-1972$",
                  "^MassM1\\_mean:South11South$",
                  "Pre72Pre-1972:South11South$"
                  )
        , covariate.labels=c(
      "Mass Liberalism$_{t-1}$",
      "Mass Lib$_{t-1} \\times$ Pre-1972",
      "Mass Lib$_{t-1} \\times$ South",
      "Mass Lib$_{t-1} \\times$ Pre-1972 $\\times$ South",
      "Union Contrib Ban",
      "Individual Contrib Limit",
      "Corporate Contrib Limit",
      "Poll Tax",
      "Literacy Test",
      "Direct Democracy",
      "Term Limits",
      "Legislative Days (Logged)",
      "Union Contrib Ban $\\times$ Mass Lib$_{t-1}$",
      "Individual Contrib Limit $\\times$ Mass Lib$_{t-1}$",
      "Corporate Contrib Limit $\\times$ Mass Lib$_{t-1}$",
      "Poll Tax $\\times$ Mass Lib$_{t-1}$",
      "Literacy Test $\\times$ Mass Lib$_{t-1}$",
      "Direct Democracy $\\times$ Mass Lib$_{t-1}$",
      "Term Limits $\\times$ Mass Lib$_{t-1}$",
      "Leg Days  $\\times$ Mass Lib$_{t-1}$")
        , add.lines=list(
            c("Sum of Signed Institution Interactions"
            , paste0(round(mean(dp.social.inst.moc2$massXinst), 3))
            , paste0(round(mean(dp.econ.inst.moc2$massXinst), 3))
              ),
            c(""
            , paste0("(", round(sd(dp.social.inst.moc2$massXinst), 3), ")")
            , paste0("(", round(sd(dp.econ.inst.moc2$massXinst), 3), ")")
              )
          ))


car::vif(lm(PolicySocial0_mean ~
              MassSocial0_mean +
              Pre72 + #South11  +
        union_contribution_ban2 +
        individual_limit_mm +
        corporate_limit_mm +
        poll_tax +
        literacy_test +
        direct_dem +
        term_limits_enact +
        LogLegDays0 +
        PolicySocial00M1_mean
       + StPO
    , data=summ))

car::vif(lm(PolicyEcon0_mean ~
              MassEcon0_mean +
              Pre72 + #South11  +
        union_contribution_ban2 +
        individual_limit_mm +
        corporate_limit_mm +
        poll_tax +
        literacy_test +
        direct_dem +
        term_limits_enact +
        LogLegDays0 +
        PolicyEcon00M1_mean
       + StPO
    , data=summ))

### Is there any era in which responsiveness is greater the year after an
### election?
## Social

plm(PolicySocial_mean ~
      (MassSocialM1_mean : YearAfterHouseElection : Era4) + 
      (MassSocialM1_mean : Era4) +
      PolicySocialM1_mean 
  , data=summ.pd, model="within", effect="twoway") %>%
  clusterSE()
## Econ
plm(PolicyEcon_mean ~
      (MassEconM1_mean : YearAfterHouseElection : Era4) + 
      (MassEconM1_mean : Era4) +
      PolicyEconM1_mean 
  , data=summ.pd, model="within", effect="twoway") %>%
  clusterSE()
## No, except maybe 1936-1953.

### How much does MOC change the results?
plyr::llply(names(tab3.cl.est), function (n) {
  tab3.moc.est[[n]][grep("MassM1_mean", names(tab3.moc.est[[n]]))]/
    tab3.cl.est[[n]][grep("MassM1_mean", names(tab3.cl.est[[n]]))]
}) %>% unlist %>% sort
##> MOC most estimates around 2/3 as large
plyr::llply(names(tab3.cl.se), function (n) {
  tab3.moc.se[[n]][grep("MassM1_mean", names(tab3.moc.se[[n]]))]/
    tab3.cl.se[[n]][grep("MassM1_mean", names(tab3.cl.se[[n]]))]
}) %>% unlist %>% sort
##> MOC SEs about the same

setwd(paste0(rep.dir, "/output-data"))
save.image("Analyze.RData")
