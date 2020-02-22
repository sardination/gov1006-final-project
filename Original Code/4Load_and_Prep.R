setwd("../") ## replication directory
(rep.dir <- getwd())

library(foreign) ## for read.dta
library(dplyr) ## for %>%, group_by, etc.
library(plm) ## for panel models
library(stargazer) ## for nice latex tables
library(rms) ## for robust SEs
library(lmtest) ## for coeftest
library(clusterSEs) ## for bootstrapped SEs
library(reshape2) ## for melt
library(car)
library(ggplot2)
library(reshape2)
library(maps)
library(mapproj)

################################################################################
#### FUNCTIONS #################################################################
################################################################################

# Function that returns the passed in information scaled by one standard
#   deviation.
scale1sd <- function (x, center=TRUE) {
  s <- sd(x, na.rm=TRUE)
  m <- if (center) mean(x, na.rm=TRUE) else 0
  (x - m) / (1 * s)
}
# Function that returns the passed in information scaled by two standard
#   deviations.
scale2sd <- function (x, center=TRUE) {
  s <- sd(x, na.rm=TRUE)
  m <- if (center) mean(x, na.rm=TRUE) else 0
  (x - m) / (2 * s)
}
# Clean plot theme
theme_clean <- function (base_size = 12) { 
  theme_grey(base_size) %+replace%
  theme(
    axis.title = element_blank(),
    axis.text = element_blank() ,
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.length = grid::unit(0, "cm"),
    panel.spacing = grid::unit(0, "lines"),
    plot.margin = grid::unit(c(0, 0, 0, 0), "lines"),
    complete = TRUE
  ) 
}
# Basic statistical functions, stripping NA from the included values
meanNA <- function (x) mean(x, na.rm=TRUE)
medianNA <- function (x) median(x, na.rm=TRUE)
sdNA <- function (x) sd(x, na.rm=TRUE)
NC <- function (x) as.numeric(as.character(x))
clusterSE <- function(x, method="arellano", cluster="group") {
  # Preform a z-test on the values of `x` using robust covariance matrix estimators
  if (any(grepl("plm", class(x)))) {
    coeftest(x, plm::vcovHC(x, method=method, cluster=cluster))
  }
}
plmHC <- function (mod) {
  # Get robust covariance matrix estimators of `mod`
  plm::vcovHC(mod, method="arellano", cluster="group")
}
MOC <- function (data, its, model, vcov=plmHC, prog.int=100, rsq=TRUE) {
  ## Method of Composition (as described by Treier and Jackman)
  tildeB <- as.data.frame(matrix(nrow=length(its), ncol=length(coef(model))))
  R2 <- data.frame(rsq=rep(NA, length(its)), adjrsq=rep(NA, length(its)))
  for (s in seq_along(its)) {
    if (!s %% prog.int) print(s)
    it <- its[s]
    ## (1) Sample from p(x)
    data_s <- subset(data, It == it)
    names(data_s) <- gsub("\\_s$", "\\_mean", names(data_s)) 
    ## (2) Sample from p(B|x,y):
    ##     (a) Estimate B_s and Cov(B_s) conditional on x_s.
    mod_s <- update(model, data=data_s)
    hatB_s <- coef(mod_s)
    hatV_s <- vcov(mod_s)
    ##     (b) Sample \tilde{B_s} from MV(\hat{B_s}, \hat{Cov(B_s)}).
    tildeB[s, ] <- MASS::mvrnorm(n = 1, mu = hatB_s, Sigma = hatV_s)
    if (rsq) {
      R2$rsq[s] <- summary(mod_s)$r.squared["rsq"]
      R2$adjrsq[s] <- summary(mod_s)$r.squared["adjrsq"]
    }
  }
  names(tildeB) <- names(coef(model))
  if (rsq) {
    tildeB$rsq <- R2$rsq
    tildeB$adjrsq <- R2$adjrsq
  }
  return(tildeB)
}
pEmp <- function (x) {
  # The probability of being in a range around `x`
  p_pos <- mean(x < 0)
  p_neg <- mean(x > 0)
  p2side <- 2 * min(p_pos, p_neg)
  p2side
}
pNorm <- function (x) {
  # The probability of being in a range around a normally-distributed `x`
  z <- abs(mean(x)/sd(x))
  p2side <- 2*(1 - pnorm(z))
  p2side
}
MOCsumm <- function (moc, regex="Mass|Policy|Dem|rsq", digits=3,
                     ff=funs(est = mean, se = sd, z = mean(.)/sd(.),
                             pnorm = pNorm(.),
                             pemp = pEmp(.))) {
  # Summary of the method of composition as a dataframe
  summarise_at(moc, .vars=vars(matches(regex)), .funs=ff) %>%
    mutate_all(funs(round(., digits=digits))) %>%
    reshape2::melt(measure.vars=names(.))
}

################################################################################
#### DATA ######################################################################
################################################################################

### LOAD
# Read in the merged variable and sample data saved at the end of Part 3
setwd(paste0(rep.dir, "/intermediate-data"))
data <- read.dta("data_for_analysis.dta")

### TRANSFORM
# Add columns:
#   Pre72: marks whether the sample point was before or after 1972
#   name: lowercase state name
#   South11: whether the state is southern
#   GovElection: whether it is the year of a governor election
#   YearAfterHouseElection: whether it is the year after a house election
#   Era4: what era of years this occurred in
#   South11Pre72: whether the state is southern and the point is befor 1972
#   DemGov: what the governor party is
#   DemControl: whether the democrats control the state
#   CampaignFinanceIndex: campaign finance index
#   CampaignFinanceIndex0: mean of the campaign finance index
#   SuffrageRestrictionIndex: suffrage restriction index
#   SuffrageRestrictionIndex0: mean of the suffrage restriction index
#   CitizenPolicymakingIndex: citizen policymaking index
#   CitizenPolicymakingIndex0: mean of the citizen policymaking index
#   LogLegDays: log of the legislative days
#   LogLegDays0: log of the legislative days normalized around the mean
#   DemPropLeg: proportion of the legislature that is democrat
#   DemPID2Party_s: proportion of the population that identifies as democrat
# Create more columns for measure values normalized around the mean or scaled
#   by the standard deviation.
# Split column data into multiple descriptive variables for all samples taken
data <- data %>%
  mutate(
    Pre72 = ifelse(year < 1972, "Pre-1972", "Post-1972"),
    name = tolower(NAME),
    South11 = factor(ifelse(south == 1 & !StPO %in% c("KY", "OK", "WV"),
                            "South", "Non-South")),
    GovElection = !is.na(demprop2),
    YearAfterHouseElection=as.logical(YearAfterHouseElection),
    Era4 = cut(year, breaks=c(1935, 1953, 1971, 1993, 2014), dig.lab=4),
    Era4 = factor(Era4, levels=rev(levels(Era4))),
    South11Pre72 = South11 == "South" & Pre72 == "Pre-1972",
    DemGov = gov_party,
    DemControl = dem_control,
    CampaignFinanceIndex = union_contribution_ban2 + individual_limit_mm +
      corporate_limit_mm,
    CampaignFinanceIndex0 = CampaignFinanceIndex -
      mean(CampaignFinanceIndex, na.rm=TRUE),
    SuffrageRestrictionIndex = poll_tax + literacy_test,
    SuffrageRestrictionIndex0 = SuffrageRestrictionIndex -
      mean(SuffrageRestrictionIndex, na.rm=TRUE),
    CitizenPolicymakingIndex = direct_dem + term_limits_enact,
    CitizenPolicymakingIndex0 = CitizenPolicymakingIndex -
      mean(CitizenPolicymakingIndex, na.rm=TRUE),
    LogLegDays = log(LegDays),
    LogLegDays0 = LogLegDays - mean(LogLegDays, na.rm=TRUE),
    DemPropLeg = (sen_dem_prop_all_centered +
                  hs_dem_prop_all_centered)/200 + .5,
    DemPID2Party_s = DemPID_s / (DemPID_s + RepPID_s)) %>%
  group_by(It) %>%
  mutate_at(.vars=vars(matches("\\_s$")), .funs=funs(scale1sd)) %>% ## _s for "sampled"
  arrange(It, Year, StPO) %>%
  group_by(It, Year) %>%
  mutate(
    PolicySocial0_s = PolicySocial_s - mean(PolicySocial_s, na.rm=TRUE),
    PolicyEcon0_s = PolicyEcon_s - mean(PolicyEcon_s, na.rm=TRUE),
    MassSocial0_s = MassSocial_s - mean(MassSocial_s, na.rm=TRUE),
    MassEcon0_s = MassEcon_s - mean(MassEcon_s, na.rm=TRUE),
    PolicySocial00_s = scale1sd(PolicySocial_s),
    PolicyEcon00_s = scale1sd(PolicyEcon_s),
    MassSocial00_s = scale1sd(MassSocial_s),
    MassEcon00_s = scale1sd(MassEcon_s)) %>%
  group_by(It, StPO) %>%
  mutate(
    PolicySocialP1_s = lead(PolicySocial_s, 1), ## "P" for "plus"
    PolicySocialM1_s = lag(PolicySocial_s, 1),  ## "M" for "minus"
    PolicySocialM2_s = lag(PolicySocial_s, 2),
    PolicySocialM3_s = lag(PolicySocial_s, 3),
    PolicySocialM4_s = lag(PolicySocial_s, 4),
    PolicySocialM5_s = lag(PolicySocial_s, 5),
    PolicySocialD01_s = PolicySocial_s - PolicySocialM1_s,  ## "D" for "diff"
    PolicySocialD12_s = PolicySocialM1_s - PolicySocialM2_s,
    PolicyEconP1_s = lead(PolicyEcon_s, 1), 
    PolicyEconM1_s = lag(PolicyEcon_s, 1),
    PolicyEconM2_s = lag(PolicyEcon_s, 2),
    PolicyEconM3_s = lag(PolicyEcon_s, 3),
    PolicyEconM4_s = lag(PolicyEcon_s, 4),
    PolicyEconM5_s = lag(PolicyEcon_s, 5),
    PolicyEconD01_s = PolicyEcon_s - PolicyEconM1_s,
    PolicyEconD12_s = PolicyEconM1_s - PolicyEconM2_s,
    MassSocialP1_s = lead(MassSocial_s, 1), 
    MassSocialM1_s = lag(MassSocial_s, 1),
    MassSocialM2_s = lag(MassSocial_s, 2),
    MassSocialM3_s = lag(MassSocial_s, 3),
    MassSocialM4_s = lag(MassSocial_s, 4),
    MassSocialM5_s = lag(MassSocial_s, 5),
    MassSocialD01_s = MassSocial_s - MassSocialM1_s,
    MassSocialD12_s = MassSocialM1_s - MassSocialM2_s,
    MassEconP1_s = lead(MassEcon_s, 1), 
    MassEconM1_s = lag(MassEcon_s, 1),
    MassEconM2_s = lag(MassEcon_s, 2),
    MassEconM3_s = lag(MassEcon_s, 3),
    MassEconM4_s = lag(MassEcon_s, 4),
    MassEconM5_s = lag(MassEcon_s, 5),
    MassEconD01_s = MassEcon_s - MassEconM1_s,
    MassEconD12_s = MassEconM1_s - MassEconM2_s,
    PolicySocial00P1_s = lead(PolicySocial00_s, 1),
    PolicySocial00M1_s = lag(PolicySocial00_s, 1),
    PolicySocial00M2_s = lag(PolicySocial00_s, 2),
    PolicySocial00M3_s = lag(PolicySocial00_s, 3),
    PolicySocial00M4_s = lag(PolicySocial00_s, 4),
    PolicySocial00M5_s = lag(PolicySocial00_s, 5),
    PolicyEcon00P1_s = lead(PolicyEcon00_s, 1), 
    PolicyEcon00M1_s = lag(PolicyEcon00_s, 1),
    PolicyEcon00M2_s = lag(PolicyEcon00_s, 2),
    PolicyEcon00M3_s = lag(PolicyEcon00_s, 3),
    PolicyEcon00M4_s = lag(PolicyEcon00_s, 4),
    PolicyEcon00M5_s = lag(PolicyEcon00_s, 5),
    MassSocial00P1_s = lead(MassSocial00_s, 1), 
    MassSocial00M1_s = lag(MassSocial00_s, 1),
    MassSocial00M2_s = lag(MassSocial00_s, 2),
    MassSocial00M3_s = lag(MassSocial00_s, 3),
    MassSocial00M4_s = lag(MassSocial00_s, 4),
    MassSocial00M5_s = lag(MassSocial00_s, 5),
    MassEcon00P1_s = lead(MassEcon00_s, 1), 
    MassEcon00M1_s = lag(MassEcon00_s, 1),
    MassEcon00M2_s = lag(MassEcon00_s, 2),
    MassEcon00M3_s = lag(MassEcon00_s, 3),
    MassEcon00M4_s = lag(MassEcon00_s, 4),
    MassEcon00M5_s = lag(MassEcon00_s, 5),
    DemPID2PartyM1_s = lag(DemPID2Party_s, 1),
    DemPID2PartyM2_s = lag(DemPID2Party_s, 2),
    DemPID2PartyM3_s = lag(DemPID2Party_s, 3),
    DemPID2PartyM4_s = lag(DemPID2Party_s, 4),
    DemPID2PartyM5_s = lag(DemPID2Party_s, 5),
    DemControlM1 = lag(DemControl, 1),
    DemControlM2 = lag(DemControl, 2),
    DemControlM3 = lag(DemControl, 3),
    DemControlM4 = lag(DemControl, 4),
    DemControlM5 = lag(DemControl, 5),
    DemGovM1 = lag(DemGov, 1),
    HouseElection = lag(YearAfterHouseElection, 1),
    YearAfterGovElection = lead(GovElection, 1)) %>%
  ungroup()

### SUMMARIZE
# Summarize all sample data around year, state, and other added measures
#   in the previous data manipulation stage over variables listed in
#   `inst_names`
inst_names <- c("union_contribution_ban2", "individual_limit_mm",
                "corporate_limit_mm", "poll_tax", "literacy_test", "direct_dem",
                "term_limits_enact", "LogLegDays")

summ <- data %>%
  group_by(Year, year, StPO, name, Pre72, South11, HouseElection,
           YearAfterHouseElection, YearAfterGovElection, Era4, South11Pre72) %>%
  summarise_at(.vars=vars(matches(
                 "\\_s$|Dem|Index|LogLegDays|PropLeg", FALSE),
                 one_of(inst_names)),
               .funs=mean.default)
names(summ) <- gsub("\\_s", "\\_mean", names(summ))
glimpse(summ)

### PANEL DATA FRAME
# Store relevant summary data for each type of sample data into individual
#   summary data frames
summ.pd <- pdata.frame(summ, index=c("StPO", "Year"), row.names=FALSE)
summ.pd.econ <- summ.pd
names(summ.pd.econ) <- gsub("Econ", "", names(summ.pd), fixed=TRUE)
summ.pd.social <- summ.pd
names(summ.pd.social) <- gsub("Social", "", names(summ.pd), fixed=TRUE)

# Store relevant sample data for each type of sample data into individual
#   summary data frames
data.pd <- pdata.frame(data, index=c("StPO", "Year"), row.names=FALSE)
data.pd.econ <- data.pd
names(data.pd.econ) <- gsub("Econ", "", names(data.pd), fixed=TRUE)
data.pd.social <- data.pd
names(data.pd.social) <- gsub("Social", "", names(data.pd), fixed=TRUE)
