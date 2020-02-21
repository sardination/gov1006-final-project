setwd("../") ## replication directory
(rep.dir <- getwd())

library(foreign) ## for read.dta
library(dplyr) ## for %>%, group_by, etc.

################################################################################
#### DATA ######################################################################
################################################################################

mass.social.name <-
  "170628_1936-2014_social_varyingitems_blackurban_nocovariates"
mass.econ.name <-
  "170915_1936-2014_economic_varyingitems_blackurban_nocovariates_1000iter_DC"

### LOAD
## Policy
setwd(paste0(rep.dir, "/intermediate-data"))
policy.social.samp <- read.dta(
  "samples_dynamic_irt_continuous_evolving_diff_stan-social-10003.dta")
policy.econ.samp <- read.dta(
  "samples_dynamic_irt_continuous_evolving_diff_stan-economic-1000.dta")

## Opinion
mass.social.samp <- read.dta(paste0("samples", mass.social.name, "-st_est.dta"))
mass.econ.samp <- read.dta(paste0("samples", mass.econ.name, "-st_est.dta"))
mass.pid.samp <- read.dta("samples_PID.dta")

setwd(paste0(rep.dir, "/input-data"))
## Legislative days
days <- read.csv("stateleg_term_length_imp_est.csv")
## Other Variables
other.df <- mget(load("opinion_TSCS.RData"))$data

klarner.df <- read.csv("Partisan_Balance_For_Use2011_06_09b.csv")

## Merge in Klarner data
other.df <- other.df %>%
  merge(select(klarner.df, c(year, state,
                             YearAfterHouseElection=hs_elections_this_year)),
        by.x=c("year", "NAME"), by.y=c("year", "state"))

### TRANSFORM
policy.social.samp <- policy.social.samp %>%
  select(StPO, Year, ItPS = Iteration, PolicySocial = Liberalism) %>%
  filter(Year %in% 1936:2014 & StPO != "DC") %>%
  mutate(PolicySocial = ifelse(!StPO %in% c("AK", "HI") | Year %in% 1959:2014,
                               PolicySocial, NA)) %>%
  arrange(Year, StPO, ItPS) %>%
  droplevels
policy.econ.samp <- policy.econ.samp %>%
  select(StPO, Year, ItPE = Iteration, PolicyEcon = Liberalism) %>%
  filter(Year %in% 1936:2014 & StPO != "DC") %>%
  mutate(PolicyEcon = ifelse(!StPO %in% c("AK", "HI") | Year %in% 1959:2014,
                             PolicyEcon, NA)) %>%
  arrange(Year, StPO, ItPE) %>%
  droplevels
mass.social.samp <- mass.social.samp %>%
  select(StPO, Year, ItMS = Iteration, MassSocial = MassSocialLib) %>%
  filter(Year %in% 1936:2014 & StPO != "DC") %>%
  mutate(MassSocial = ifelse(!StPO %in% c("AK", "HI") | Year %in% 1959:2014,
                             MassSocial, NA),
         Year = factor(Year)) %>%
  arrange(Year, StPO, ItMS) %>%
  droplevels
mass.econ.samp <- mass.econ.samp %>%
  select(StPO, Year, ItME = Iteration, MassEcon = MassEconLib) %>%
  filter(Year %in% 1936:2014 & StPO != "DC") %>%
  mutate(MassEcon = ifelse(!StPO %in% c("AK", "HI") | Year %in% 1959:2014,
                           MassEcon, NA),
         Year = factor(Year)) %>%
  arrange(Year, StPO, ItME) %>%
  droplevels
mass.pid.samp <- mass.pid.samp %>%
  select(StPO, Year, ItPID = Iteration, DemPID, RepPID) %>%
  filter(Year %in% 1936:2014 & StPO != "DC") %>%
  mutate(DemPID = ifelse(!StPO %in% c("AK", "HI") | Year %in% 1959:2014,
                         DemPID, NA),
         RepPID = ifelse(!StPO %in% c("AK", "HI") | Year %in% 1959:2014,
                           RepPID, NA),
         Year = factor(Year, levels=1936:2014)) %>%
  arrange(Year, StPO, ItPID)

## Subsample
n.samps <- 500
set.seed(1)
ps.subsamp <- sort(sample(unique(policy.social.samp$ItPS), n.samps))
pe.subsamp <- sort(sample(unique(policy.econ.samp$ItPE), n.samps)) 
ms.subsamp <- sort(sample(unique(mass.social.samp$ItMS), n.samps)) 
me.subsamp <- sort(sample(unique(mass.econ.samp$ItME), n.samps))
pid.subsamp <- sort(sample(unique(mass.pid.samp$ItPID), n.samps))

pid.pre46 <- data.frame(
  subset(policy.social.samp, ItPS %in% ps.subsamp
         & !Year %in% unique(mass.pid.samp$Year), c(Year, StPO)),
  ItPID = NA, DemPID = NA, RepPID = NA)
pid.pre46$ItPID <- rep(sort(pid.subsamp), length.out=nrow(pid.pre46))
mass.pid.samp <- rbind(pid.pre46, mass.pid.samp) %>%
  droplevels

### MERGE
## Check state
identical(subset(policy.social.samp, ItPS %in% ps.subsamp)$StPO,
          subset(policy.econ.samp, ItPE %in% pe.subsamp)$StPO)
identical(subset(policy.social.samp, ItPS %in% ps.subsamp)$StPO,
          subset(mass.social.samp, ItMS %in% ms.subsamp)$StPO)
identical(subset(policy.social.samp, ItPS %in% ps.subsamp)$StPO,
          subset(mass.econ.samp, ItME %in% me.subsamp)$StPO)
identical(subset(mass.pid.samp, ItPID %in% pid.subsamp)$StPO,
          subset(policy.social.samp, ItPS %in% ps.subsamp)$StPO)
identical(subset(mass.pid.samp, ItPID %in% pid.subsamp)$StPO,
          subset(policy.econ.samp, ItPE %in% pe.subsamp)$StPO)
identical(subset(mass.pid.samp, ItPID %in% pid.subsamp)$StPO,
          subset(mass.social.samp, ItMS %in% ms.subsamp)$StPO)
identical(subset(mass.pid.samp, ItPID %in% pid.subsamp)$StPO,
          subset(mass.econ.samp, ItME %in% me.subsamp)$StPO)
## Check year
identical(subset(policy.social.samp, ItPS %in% ps.subsamp)$Year,
          subset(policy.econ.samp, ItPE %in% pe.subsamp)$Year)
identical(subset(policy.social.samp, ItPS %in% ps.subsamp)$Year,
          subset(mass.social.samp, ItMS %in% ms.subsamp)$Year)
identical(subset(policy.social.samp, ItPS %in% ps.subsamp)$Year,
          subset(mass.econ.samp, ItME %in% me.subsamp)$Year)
identical(subset(mass.pid.samp, ItPID %in% pid.subsamp)$Year,
          subset(policy.social.samp, ItPS %in% ps.subsamp)$Year)
identical(subset(mass.pid.samp, ItPID %in% pid.subsamp)$Year,
          subset(policy.econ.samp, ItPE %in% pe.subsamp)$Year)
identical(subset(mass.pid.samp, ItPID %in% pid.subsamp)$Year,
          subset(mass.social.samp, ItMS %in% ms.subsamp)$Year)
identical(subset(mass.pid.samp, ItPID %in% pid.subsamp)$Year,
          subset(mass.econ.samp, ItME %in% me.subsamp)$Year)

subsamp.df <- cbind(
  subset(policy.social.samp, ItPS %in% ps.subsamp),
  subset(policy.econ.samp, ItPE %in% pe.subsamp, c(ItPE, PolicyEcon)),
  subset(mass.social.samp, ItMS %in% ms.subsamp, c(ItMS, MassSocial)),
  subset(mass.econ.samp, ItME %in% me.subsamp, c(ItME, MassEcon)),
  subset(mass.pid.samp, ItPID %in% pid.subsamp, c(ItPID, DemPID, RepPID)))

subsamp.df <- subsamp.df %>%
  group_by(StPO, Year) %>%
  mutate(It = row_number(Year)) %>%
  select(StPO, Year, It, starts_with("It"), everything())

names(subsamp.df)[!grepl("StPO|Year|It", names(subsamp.df))] <-
  paste0(names(subsamp.df)[!grepl("StPO|Year|It", names(subsamp.df))], "_s")

cov.df <- other.df %>%
  group_by(year) %>%
  filter(!duplicated(abb)) %>% ## drop duplicated 1961 obs
  ungroup %>%
  mutate(StPO = factor(abb, levels=levels(subsamp.df$StPO)),
         Year = factor(year, levels=levels(subsamp.df$Year)),
         South11 = factor(ifelse(
           south == 1 & !StPO %in% c("KY", "OK", "WV"),
           "South", "Non-South")),
         BienniumYr1 = 2*trunc((year + 1)/2) - 1)

cov.df <- merge(x=cov.df, y=days, by.x=c("NAME", "BienniumYr1"),
                by.y=c("State", "BienniumYr1"), all.x=TRUE)


cov.df <- cov.df %>% mutate(LegDays = LegDays_ImpEst)

for (s in 1:nlevels(cov.df$StPO)) {
  st <- levels(cov.df$StPO)[s]
  cov.df[cov.df$StPO == st & cov.df$year < 1941, "LegDays"] <-
    cov.df[cov.df$StPO == st & cov.df$year == 1941, "LegDays"]
}

## Fix year after election
with(cov.df, table(YearAfterHouseElection, year, useNA='ifany'))
cov.df <- cov.df %>%
  mutate(YearAfterHouseElection=
           ifelse(year > 2011 & StPO %in% c("LA", "MS") &
                  as.integer(year) %% 4 == 0, 1, YearAfterHouseElection),
         YearAfterHouseElection=
           ifelse(year > 2011 & StPO %in% c("LA", "MS") &
                  as.integer(year) %% 4 != 0, 0, YearAfterHouseElection),
         YearAfterHouseElection=
           ifelse(year > 2011 & StPO %in% c("NJ", "VA") &
                  as.integer(year) %% 2 == 0, 1, YearAfterHouseElection),
         YearAfterHouseElection=
           ifelse(year > 2011 & StPO %in% c("NJ", "VA") &
                  as.integer(year) %% 2 != 0, 0, YearAfterHouseElection),
         YearAfterHouseElection=
           ifelse(year > 2011 & !StPO %in% c("LA", "MS", "NJ", "VA") &
                  as.integer(year) %% 2 == 1, 1, YearAfterHouseElection), 
         YearAfterHouseElection=
           ifelse(year > 2011 & !StPO %in% c("LA", "MS", "NJ", "VA") &
                  as.integer(year) %% 2 != 1, 0, YearAfterHouseElection), 
         YearAfterHouseElection=
           ifelse(StPO %in% "NE" & as.integer(year) %% 2 == 1,
                  1, YearAfterHouseElection), 
         YearAfterHouseElection=
           ifelse(StPO %in% "NE" & as.integer(year) %% 2 != 1,
                  0, YearAfterHouseElection))

glimpse(cov.df)

merged.df <- merge(subsamp.df, cov.df, by=c("StPO", "Year"), all.x=TRUE) %>%
  select(StPO, Year, It, everything()) %>%
  arrange(It, Year, StPO)

merged.df <- merged.df %>%
  select(-c(policy, policy_lead_t1, policy_leads_delta2, lagged_policy,
            public_opinion_liberalism, public_opinion_liberalism_social,
            public_opinion_liberalism_race, public_opinion_liberalism_2013est,
            state.x, social_policy, race_policy, state.y,
            hs_dem_per_2pty.y, yearly_liberalism,
            public_opinion_liberalism_demeaned, hs.include, X))

glimpse(merged.df)

setwd(paste0(rep.dir, "/intermediate-data"))
write.dta(merged.df, "data_for_analysis.dta")

q()
