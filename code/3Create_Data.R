setwd("../") ## replication directory
(rep.dir <- getwd())

library(foreign) ## for read.dta
library(dplyr) ## for %>%, group_by, etc.

################################################################################
#### DATA ######################################################################
################################################################################

# Selection of social and economic opinion data to use
mass.social.name <-
  "170628_1936-2014_social_varyingitems_blackurban_nocovariates"
mass.econ.name <-
  "170915_1936-2014_economic_varyingitems_blackurban_nocovariates_1000iter_DC"

### LOAD
## Policy
# Read in the intermediate data that was manipulated in Part 1 for the policy
#   measures over time for social and economic policy.
setwd(paste0(rep.dir, "/intermediate-data"))
policy.social.samp <- read.dta(
  "samples_dynamic_irt_continuous_evolving_diff_stan-social-10003.dta")
policy.econ.samp <- read.dta(
  "samples_dynamic_irt_continuous_evolving_diff_stan-economic-1000.dta")

## Opinion
# Read in the social and economic opinion sample data as well as party affiliation
#   sample data.
mass.social.samp <- read.dta(paste0("samples", mass.social.name, "-st_est.dta"))
mass.econ.samp <- read.dta(paste0("samples", mass.econ.name, "-st_est.dta"))
mass.pid.samp <- read.dta("samples_PID.dta")

setwd(paste0(rep.dir, "/input-data"))
## Legislative days
# Read data on how many legislative days for each biennium year in each state
days <- read.csv("stateleg_term_length_imp_est.csv")
## Other Variables
# Read in several other variables for each state and year.
other.df <- mget(load("opinion_TSCS.RData"))$data
# Read in legislative data for each state and year.
klarner.df <- read.csv("Partisan_Balance_For_Use2011_06_09b.csv")

## Merge in Klarner data
# Merge the legislative data into the other variable data by year and state
other.df <- other.df %>%
  merge(select(klarner.df, c(year, state,
                             YearAfterHouseElection=hs_elections_this_year)),
        by.x=c("year", "NAME"), by.y=c("year", "state"))

### TRANSFORM
# Adjust the social policy sample data to only have the columns corresponding
#   to the state abbreviation, year, iteration, and liberalism value.
#   Only include relevant years and exclude DC.
#   Make liberalism values for Alaska and Hawaii in all years before 1959 as `NA`.
#   Sort the rows in order of year, state, and then iteration.
#   Drop all unused factor levels from the dataframe.
policy.social.samp <- policy.social.samp %>%
  select(StPO, Year, ItPS = Iteration, PolicySocial = Liberalism) %>%
  filter(Year %in% 1936:2014 & StPO != "DC") %>%
  mutate(PolicySocial = ifelse(!StPO %in% c("AK", "HI") | Year %in% 1959:2014,
                               PolicySocial, NA)) %>%
  arrange(Year, StPO, ItPS) %>%
  droplevels
# Repeat the transformation process applied to the social policy sample data
#   above for the economic policy sample data.
policy.econ.samp <- policy.econ.samp %>%
  select(StPO, Year, ItPE = Iteration, PolicyEcon = Liberalism) %>%
  filter(Year %in% 1936:2014 & StPO != "DC") %>%
  mutate(PolicyEcon = ifelse(!StPO %in% c("AK", "HI") | Year %in% 1959:2014,
                             PolicyEcon, NA)) %>%
  arrange(Year, StPO, ItPE) %>%
  droplevels
# Repeat the transformation process applied to the social policy sample data
#   above for the social public opinion data.
mass.social.samp <- mass.social.samp %>%
  select(StPO, Year, ItMS = Iteration, MassSocial = MassSocialLib) %>%
  filter(Year %in% 1936:2014 & StPO != "DC") %>%
  mutate(MassSocial = ifelse(!StPO %in% c("AK", "HI") | Year %in% 1959:2014,
                             MassSocial, NA),
         Year = factor(Year)) %>%
  arrange(Year, StPO, ItMS) %>%
  droplevels
# Repeat the transformation process applied to the social policy sample data
#   above for the economic public opinion data.
mass.econ.samp <- mass.econ.samp %>%
  select(StPO, Year, ItME = Iteration, MassEcon = MassEconLib) %>%
  filter(Year %in% 1936:2014 & StPO != "DC") %>%
  mutate(MassEcon = ifelse(!StPO %in% c("AK", "HI") | Year %in% 1959:2014,
                           MassEcon, NA),
         Year = factor(Year)) %>%
  arrange(Year, StPO, ItME) %>%
  droplevels
# Adjust the party affiliation sample data to only have the columns corresponding
#   to the state abbreviation, year, iteration, democratic affiliation, and
#   republican affiliation.
#   Only include relevant years and exclude DC.
#   Make proportion values for Alaska and Hawaii in all years before 1959 as `NA`.
#   Sort the rows in order of year, state, and then iteration.
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
# Take `n.samps` from the social policy measures, economic policy measures,
#   social opinions, economic opinions, and partisan identification by selecting
#   random `n.samps` iterations.
n.samps <- 500
set.seed(1)
ps.subsamp <- sort(sample(unique(policy.social.samp$ItPS), n.samps))
pe.subsamp <- sort(sample(unique(policy.econ.samp$ItPE), n.samps)) 
ms.subsamp <- sort(sample(unique(mass.social.samp$ItMS), n.samps)) 
me.subsamp <- sort(sample(unique(mass.econ.samp$ItME), n.samps))
pid.subsamp <- sort(sample(unique(mass.pid.samp$ItPID), n.samps))

# Take the random subsample of social policy liberalism measures and select
#   year and state abbreviation columns. Add empty columns for iteration,
#   democratic proportion, and republic proportion
pid.pre46 <- data.frame(
  subset(policy.social.samp, ItPS %in% ps.subsamp
         & !Year %in% unique(mass.pid.samp$Year), c(Year, StPO)),
  ItPID = NA, DemPID = NA, RepPID = NA)
# Add iteration number of each selected sample to the `ItPID` column and
#   combine with the relevant party affiliation information as rows.
#   Drop unused factor levels.
pid.pre46$ItPID <- rep(sort(pid.subsamp), length.out=nrow(pid.pre46))
mass.pid.samp <- rbind(pid.pre46, mass.pid.samp) %>%
  droplevels

### MERGE
## Check state
# Check to make sure that the states are the same across all sample datasets
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
# Check to make sure that the yeares are the same across all sample datasets
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

# Take subsamples of each dataset and bind the columns selected in each
#   subsect of each dataset together into one larger dataset
subsamp.df <- cbind(
  subset(policy.social.samp, ItPS %in% ps.subsamp),
  subset(policy.econ.samp, ItPE %in% pe.subsamp, c(ItPE, PolicyEcon)),
  subset(mass.social.samp, ItMS %in% ms.subsamp, c(ItMS, MassSocial)),
  subset(mass.econ.samp, ItME %in% me.subsamp, c(ItME, MassEcon)),
  subset(mass.pid.samp, ItPID %in% pid.subsamp, c(ItPID, DemPID, RepPID)))

# Group the collected subsamples by state and year and then add a column
#   `It` containing the iteration number and then select only the columns
#   with the state abbreviation, year, iteration numbers coming first
#   and then the remaining columns at the end.
subsamp.df <- subsamp.df %>%
  group_by(StPO, Year) %>%
  mutate(It = row_number(Year)) %>%
  select(StPO, Year, It, starts_with("It"), everything())

# Change variable names in the subsample dataframe to have "_s" appended to the
#   end except for the StPO, Year, and It* variable names.
names(subsamp.df)[!grepl("StPO|Year|It", names(subsamp.df))] <-
  paste0(names(subsamp.df)[!grepl("StPO|Year|It", names(subsamp.df))], "_s")

# Group the other variables dataframe by year and then remove all duplicate entries.
#   Add columns StPO (containing factor of state abbreviation), Year (containing
#   factor of year value), South11 (labeled as South or Non-South), and
#   BienniumYr1 (the year of the biennium)
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

# Merge the above dataframe of the other variables with the day dataframe
#   (the dataframe with the legislative days numbers), merging by state
#   and biennium year
cov.df <- merge(x=cov.df, y=days, by.x=c("NAME", "BienniumYr1"),
                by.y=c("State", "BienniumYr1"), all.x=TRUE)

# Rename the legislative days column to `LegDays`
cov.df <- cov.df %>% mutate(LegDays = LegDays_ImpEst)

# For each state, replace the `LegDays` values for which the year is before
#   1941 with the `LegDays` value for the year 1941, since we are only concerned
#   with the latter time period.
for (s in 1:nlevels(cov.df$StPO)) {
  st <- levels(cov.df$StPO)[s]
  cov.df[cov.df$StPO == st & cov.df$year < 1941, "LegDays"] <-
    cov.df[cov.df$StPO == st & cov.df$year == 1941, "LegDays"]
}

## Fix year after election
# Build a table of counts for each combination of years after house election and
#   actual year and then convert the years after house election column to the
#   appropriate value based on state.
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

# Merge the combine sample dataframe with the merged "other variables" dataframe
#   by state abbreviation and year and keep columns for state, year, and iteration
#   number first, followed by the rest of the columns. Finally, sort all rows
#   by iteration number, then year, then state.
merged.df <- merge(subsamp.df, cov.df, by=c("StPO", "Year"), all.x=TRUE) %>%
  select(StPO, Year, It, everything()) %>%
  arrange(It, Year, StPO)

# Keep all columns except for the ones listed in the select function below.
merged.df <- merged.df %>%
  select(-c(policy, policy_lead_t1, policy_leads_delta2, lagged_policy,
            public_opinion_liberalism, public_opinion_liberalism_social,
            public_opinion_liberalism_race, public_opinion_liberalism_2013est,
            state.x, social_policy, race_policy, state.y,
            hs_dem_per_2pty.y, yearly_liberalism,
            public_opinion_liberalism_demeaned, hs.include))

glimpse(merged.df)

# Save the final merged dataframe into an intermediate data store.
setwd(paste0(rep.dir, "/intermediate-data"))
write.dta(merged.df, "data_for_analysis.dta")

# q()