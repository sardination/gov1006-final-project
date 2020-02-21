# -------- 1Save_Samples.R --------
setwd("../") ## replication directory
(rep.dir <- getwd())

# Selection of social and economic opinion data to use
mass.social.name <-
  "170628_1936-2014_social_varyingitems_blackurban_nocovariates"
mass.econ.name <-
  "170915_1936-2014_economic_varyingitems_blackurban_nocovariates_1000iter_DC"

### Read
# Read in input data, including social opinion, economic opinion, social policy liberalism measures,
#   economic policy liberalism measures, and public party identification information for states by year
setwd(paste0(rep.dir, "/input-data"))
# Read in the opinion data specified above
social.opinion.samples <-
  mget(load(paste0("dgirt_output_", mass.social.name, ".RData")))
economic.opinion.samples <- 
  mget(load(paste0("dgirt_output_", mass.econ.name, ".RData")))
# Read in policy measures over time. Each variable in the dataframe of these measures
#   is named with two "indices": the year (1-indexed) and the state (1-indexed by abbreviation).
#   Each variable contains several rows, each representing another iteration of the policy
#   liberalism measure algorithm (since the measure calculation involves executing a
#   random walk).
social.policy.samples <-
  mget(load("dynamic.irt.continuous.evolving.diff.stan-social-10003.Rdata"))
economic.policy.samples <- 
  mget(load("dynamic.irt.continuous.evolving.diff.stan-economic-1000.Rdata"))
# Read in state public party identification information from a Stata data file into an R dataframe.
#   This dataframe contains party identification information for each year for each state.
pid.samples <- foreign::read.dta("StYrPID-poststratified-posterior-170918.dta")

### Transform
# Convert the sample sets into a dataframe of relevant variables, instead of being
#   dataframes with a single column containing all the relevant information.
social.opinion.samples.df <- as.data.frame(social.opinion.samples$dgirt_out)
economic.opinion.samples.df <- as.data.frame(economic.opinion.samples$dgirt_out)
social.policy.samples.df <- as.data.frame(social.policy.samples$stan.cmb)
economic.policy.samples.df <- as.data.frame(economic.policy.samples$stan.cmb)

library(plyr)
library(dplyr)
library(reshape2)

# Verify that the list of state abbreviations is in ascending alphabetical order
stpos <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "HI",
           "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI",
           "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV",
           "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT",
           "VA", "VT", "WA", "WI", "WV", "WY")
length(stpos)
identical(stpos, sort(stpos))

# From the social policy liberalism metrics, select all theta measures, then, using
#   melt, assign each numerical theta measure to its own row with its relevant value. 
#   Group the rows by theta measure name and then add another column `iteration` that
#   records the iteration number for each value of each theta measure.
#   Finally, add in two more columns: Year - take the year "index" provided in each theta
#   variable name in each row and convert it to the actual year; StPO - take the state
#   "index" provided in each theta variable name in each row and convert it to the actual
#   state abbreviation.
social.policy.samples.df <- social.policy.samples.df %>%
  select(contains("theta")) %>%
  melt %>%
  group_by(variable) %>%
  mutate(iteration = row_number(value)) %>%
  arrange(iteration) %>%
  ungroup %>%
  mutate(Year = as.integer(gsub("theta\\[(.*),.*\\]", "\\1", variable)),
         Year = factor(Year, labels=as.character(1935:2014)),
         StPO = as.integer(gsub("theta\\[.*,(.*)\\]", "\\1", variable)),
         StPO = factor(StPO, labels=stpos))

# Repeat the process executed above for the social policy liberalism metrics applied
#   to the economic policy liberalism metrics.
economic.policy.samples.df <-
  economic.policy.samples.df %>%
  select(contains("theta")) %>%
  melt %>%
  group_by(variable) %>%
  mutate(iteration = row_number(value)) %>%
  arrange(iteration) %>%
  ungroup %>%
  mutate(Year = as.integer(gsub("theta\\[(.*),.*\\]", "\\1", variable)),
         Year = factor(Year, labels=as.character(1935:2014)),
         StPO = as.integer(gsub("theta\\[.*,(.*)\\]", "\\1", variable)),
         StPO = factor(StPO, labels=stpos))

# Update the social policy liberalism measure dataframe to only contain the columns
#   that indicate the actual year (Year), actual state abbreviation (StPO), 
#   liberalism measure (Liberalism), and iteration (Iteration). All extraneous
#   column names are omitted. This dataframe is then sorted first by iteration,
#   then year, and then state abbreviation.
social.policy.samples.df <- social.policy.samples.df %>%
  select(c(StPO=StPO, Year=Year, Liberalism=value, Iteration=iteration))
social.policy.samples.df <- social.policy.samples.df %>%
  arrange(Iteration, Year, StPO)

# Repeat the process executed above for the social policy liberalism dataframe applied
#   to the economic policy liberalism dataframe.
economic.policy.samples.df <- economic.policy.samples.df %>%
  select(c(StPO=StPO, Year=Year, Liberalism=value, Iteration=iteration))
economic.policy.samples.df <- economic.policy.samples.df %>%
  arrange(Iteration, Year, StPO)

# From the party identification data, remove all entries that have missing state
#   abbreviations and then rename `iterations` column to `Iteration`,
#   add a column `StPO` that contains the state abbreviation, and convert the
#   `Year` strings to integers. Then convert the dataframe to rows of 
#   Year, StPo, and Iteration with the respective proportions of the sampled
#   population that identify as democrat, independent, and republican under
#   the column headers `DemPID`, `IndPID`, and `RepPID`, respectively.
pid.samples.df <- pid.samples %>%
  filter(!is.na(StPOAbrv)) %>%
  mutate(Iteration = iterations,
         StPO = factor(StPOAbrv, levels=stpos),
         Year = as.integer(Year)) %>%
  group_by(Year, StPO, Iteration) %>%
  summarise(DemPID = pid_total[PID=="D"],
            IndPID = pid_total[PID=="I"],
            RepPID = pid_total[PID=="R"])

# Get the public "liberalism" measure by dividing the proportion of 
#   the population that is democrat-identifying by the total proportion of 
#   the population that identifiies as either democrat or republicam, and then
#   store this value in the `Dem2PID` column and sort the dataframe by 
#   Iteration, Year, and finally StPO (state abbreviation).
pid.samples.df <- pid.samples.df %>%
  mutate(Dem2PID = DemPID / (DemPID + RepPID)) %>%
  arrange(Iteration, Year, StPO)

## Write
# Write the modified social opinion, economic opinion, social policy, and economic
#   policy liberalism value dataframes as well as the modified public party
#   identification dataframe into intermediate data store files.
setwd(paste0(rep.dir, "/intermediate-data"))
foreign::write.dta(
  social.opinion.samples.df,
  paste0("samples", mass.social.name, ".dta"))
foreign::write.dta(
  economic.opinion.samples.df,
  paste0("samples", mass.econ.name, ".dta"))
foreign::write.dta(
  social.policy.samples.df,
  "samples_dynamic_irt_continuous_evolving_diff_stan-social-10003.dta")
foreign::write.dta(
  economic.policy.samples.df,
  "samples_dynamic_irt_continuous_evolving_diff_stan-economic-1000.dta")
foreign::write.dta(
  pid.samples.df,
  "samples_PID.dta")

q()

# -------- 2Poststratify_Samples.R --------
setwd("../") ## replication directory
(rep.dir <- getwd())

library(dgo)
library(dplyr)
library(foreign)

# Selection of social and economic opinion data to use
mass.social.name <-
  "170628_1936-2014_social_varyingitems_blackurban_nocovariates"
mass.econ.name <-
  "170915_1936-2014_economic_varyingitems_blackurban_nocovariates_1000iter_DC"

### Read
# Read in the intermediate data that was manipulated in Part 1 for the social
#   opinion sample data and the economic opinion sample data.
#   Also read in the target input data with categorical proportion data.
setwd(paste0(rep.dir, "/intermediate-data"))
social.opinion.samples.df <-
  read.dta(paste0("samples", mass.social.name, ".dta"))
economic.opinion.samples.df <-
  read.dta(paste0("samples", mass.econ.name, ".dta"))
setwd(paste0(rep.dir, "/input-data"))
targets <- readRDS(file = "targets.rds")

## Mutate
# Convert the year column (D_year) from string to integer.
#   Add a column `D_black_x_urban4` that records the type of target
#   that the row corresponds to in terms of black/non-black and rural/urban
#   with a value "D_black_x_urban#".
group.targets <- targets %>%
  mutate(D_year = as.integer(D_year),
         D_black_x_urban4 = ifelse(     #1: non-black, rural
           D_black == "D_blacknon-black" & D_urban == "D_urban0",
           "D_black_x_urban1", NA),
         D_black_x_urban4 = ifelse(     #2: non-black, urban
           D_black == "D_blacknon-black" & D_urban == "D_urban1",
           "D_black_x_urban2", D_black_x_urban4),
         D_black_x_urban4 = ifelse(     #3: black, rural
           D_black == "D_blackblack" & D_urban == "D_urban0",
           "D_black_x_urban3", D_black_x_urban4),
         D_black_x_urban4 = ifelse(     #4: black, urban
           D_black == "D_blackblack" & D_urban == "D_urban1",
           "D_black_x_urban4", D_black_x_urban4),
         D_black_x_urban4 = factor(as.character(D_black_x_urban4)))

# Create a dataframe with columns: state abbreviation, year, `D_black_x_urban4`
#   (which contains region type), and column that sums the proportions found
#   for all rows in the `targets` dataframe that match the state, year, and 
#   population type.
st.targets <-
  aggregate(Prop ~ D_abb + D_year + D_black_x_urban4, group.targets, sum)
st.targets <- as.data.frame(st.targets)
glimpse(st.targets)

### Poststratify
yrs.to.est <- 1936:2014

# Save the subset of samples that have an iteration number in
#   `social.opinion.samples.subset`
social.iters <- 1:max(social.opinion.samples.df$iteration)
social.opinion.samples.subset <- social.opinion.samples.df %>%
  filter(iteration %in% social.iters)
# For each iteration, stratify the sample data by state and year, aggregating
#   over the population type and recording the strata proportions in `Prop`,
#   and then bind the dataframe generated for each iteration into one large
#   dataframe containing all the strata information.
social.opinion.samples.st <- social.iters %>%
  plyr::llply(function (i) {
    social.opinion.samples.df %>%
      filter(iteration == i) %>%
      poststratify(target_data=st.targets,
                   strata_names=c("D_abb", "D_year"),
                   aggregated_names="D_black_x_urban4",
                   proportion_name="Prop") %>%
      mutate(Iteration = i)
  }) %>%
  bind_rows
summary(social.opinion.samples.st)

# Take the social opinion sample data and only select the state, year,
#   liberalism value, and iteration columns and then convert the state
#   abbreviation column `StPO` to only contain the abbreviation itself,
#   without the "D_abb" leading string.
social.opinion.samples.st <- social.opinion.samples.st %>%
  select(StPO = D_abb,
         Year = D_year,
         MassSocialLib = value,
         Iteration = Iteration) %>%
  mutate(StPO = factor(gsub("D\\_abb(.*)", "\\1", StPO)))

# Repeat the process executed above for the social policy liberalism metrics applied
#   to the economic policy liberalism metrics.
economic.iters <- 1:max(economic.opinion.samples.df$iteration)
economic.opinion.samples.subset <- economic.opinion.samples.df %>%
  filter(iteration %in% economic.iters)
economic.opinion.samples.st <- economic.iters %>%
  plyr::llply(function (i) {
    economic.opinion.samples.df %>%
      filter(iteration == i) %>%
      poststratify(target_data=st.targets,
                   strata_names=c("D_abb", "D_year"),
                   aggregated_names="D_black_x_urban4",
                   proportion_name="Prop") %>%
      mutate(Iteration = i)
  }) %>%
  bind_rows
summary(economic.opinion.samples.st)

economic.opinion.samples.st <- economic.opinion.samples.st %>%
  select(StPO = D_abb,
         Year = D_year,
         MassEconLib = value,
         Iteration = Iteration) %>%
  mutate(StPO = factor(gsub("D\\_abb(.*)", "\\1", StPO)))

### Write
# Write the modified social opinion and economic opinion value dataframes
#   into intermediate data store files.
setwd(paste0(rep.dir, "/intermediate-data"))
write.dta(
  social.opinion.samples.st,
  paste0("samples", mass.social.name, "-st_est.dta"))
write.dta(
  economic.opinion.samples.st,
  paste0("samples", mass.econ.name, "-st_est.dta"))

q()

# -------- 3Create_Data.R --------
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
