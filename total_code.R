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
# Build a column indicating whether it is the year after the house election
#   for each state in question.
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
            public_opinion_liberalism_demeaned, hs.include, X))

glimpse(merged.df)

# Save the final merged dataframe into an intermediate data store.
setwd(paste0(rep.dir, "/intermediate-data"))
write.dta(merged.df, "data_for_analysis.dta")

q()

# -------- 4Load_and_Prep.R --------
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
