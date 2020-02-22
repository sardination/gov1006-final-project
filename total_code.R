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
  # This accounts for error in regression analyses
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
  # Summary of the method of composition error-accounting results as a dataframe
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

# -------- 5Analyze.R --------
################################################################################
#### SUMMARY STATISTICS ########################################################
################################################################################

# Take summary statistics collected from Part 4 and select only the columns
#   of all sample liberalism measure means as well as the means of 
#   democratic party proportions and democratic control information
# Output this summary information as a LaTeX formatted table
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
  # Get mean of lags in summary dataframe
  plyr::llply(purtest.res, function (x) {
    plyr::laply(x$idres, function (y) y$lags)
  }) %>% plyr::llply(mean)
}
median_lags <- function (purtest.res) {
  # Get median of lags in summary dataframe
  plyr::llply(purtest.res, function (x) {
    plyr::laply(x$idres, function (y) y$lags)
  }) %>% plyr::llply(median)
}
summ_trho <- function (purtest.res) {
  # Summarize trho value from summary dataframe
  plyr::llply(purtest.res, function (x) {
    plyr::laply(x$idres, function (y) y$trho)
  }) %>%
    plyr::llply(function (z) c(summary(z), "Prop. Neg." = mean(z < 0)))
}
MOCmean <- function (mod.ls) {
  # Mean of Method of Composition error-accounted variables
  if (all(plyr::laply(mod.ls, function (x) "moc" %in% names(x)))) {
    plyr::llply(mod.ls, function (x) apply(x$moc, 2, mean))
  } else {
    plyr::llply(mod.ls, function (x) apply(x, 2, mean))
  }
}
MOCsd <- function (mod.ls) {
  # Standard deviation of Method of Composition error-accounted variables
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

# Get a random sample of 500 iterations to use, then sort the chosen iterations
#   in ascending order
set.seed(1)
its <- sort(sample(unique(data$It), min(500, length(unique(data$It)))))

# All model estimates will be stored in the same large dataframe under each
#    of these column names
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
# Apply a linear model to the social policy liberalism using social opinion
#   liberalism, southernness, and year as factors. Store this model,
#   the z-test results, and method of composition error-accounting results.
#   Plot the regression results with error accounted for by method of composition
#   against social opinion policy liberalism.
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
# Update the above social liberalism model to use the same variables, but 
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well and plot the same way as above.
model.ls[["xs_south_econ"]]$model <- model.ls[["xs_south_social"]]$model %>%
  update(data=summ.pd.social) # NOTE SK: THIS SHOULD SAY `summ.pd.econ`
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
# Apply a linear model to the social policy liberalism using social opinion
#   liberalism, southernness, year, and state as factors. Store this model,
#   the z-test results, and method of composition error-accounting results.
#   Plot the regression results with error accounted for by method of composition
#   against social opinion policy liberalism.
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
# Update the above social liberalism model to use the same variables, but 
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well and plot the same way as above.
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
# Apply a linear model to the social policy liberalism using social opinion
#   liberalism, southernness, year, and first-group social policy liberalism means as factors. 
#   Store this model, the z-test results, and method of composition error-accounting results.
#   Plot the regression results with error accounted for by method of composition
#   against social opinion policy liberalism.
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
# Update the above social liberalism model to use the same variables, but 
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well and plot the same way as above.
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
# Apply a linear model to the social policy liberalism using social opinion
#   liberalism, southernness, year, first-group social policy liberalism means,
#   and state as factors. Store this model, the z-test results, and
#   method of composition error-accounting results.
#   Plot the regression results with error accounted for by method of composition
#   against social opinion policy liberalism.
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
# Update the above social liberalism model to use the same variables, but 
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well and plot the same way as above.
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
# Summarize the responsiveness values found through linear modeling of
#   policy liberalism against variables + opinion liberalism.
model.ls[["dp_south_social"]]$moc %>%
  mutate(MassSouth = MassM1_mean + `MassM1_mean:South11South`) %>%
  MOCsumm

model.ls[["dp_south_econ"]]$moc %>%
  mutate(MassSouth = MassM1_mean + `MassM1_mean:South11South`) %>%
  MOCsumm


### Long-run Multiplier
## Social
# Adjust regressed variables to apply for time-lagged applications
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
# Adjust regressed variables to apply for time-lagged applications
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
# Apply a linear model to the democratic control variable using social opinion
#   liberalism, first-group democratic control, state, and year
#   as factors. Store this model, the z-test results, and method of composition error-accounting results.
#   Plot the regression results with error accounted for by method of composition
#   against social opinion policy liberalism.
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
# Update the above social liberalism model to use the same variables, but 
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well and plot the same way as above.
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
# Combine the social and economic selection models and plot against
#   opinion data.
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
# Combine the social and economic social models with first-group party identification
#   proportions and plot against opinion data.
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
# Combine the social and economic social models with second-group party identification
#   proportions and plot against opinion data.
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
# Record joint model significance
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
# Apply a linear model to the social policy liberalism using democratic control
#   proportions, social opinion liberalism, state, and year
#   as factors. Store this model, the z-test results, and
#   method of composition error-accounting results.
#   Plot the regression results with error accounted for by method of composition
#   against social opinion policy liberalism.
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
# Update the above social liberalism model to use the same variables, but 
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well and plot the same way as above.
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
# Apply a linear model to the social policy liberalism using social opinion 
#   liberalism, state, and year as factors. Store this model, the z-test results, and
#   method of composition error-accounting results.
#   Plot the regression results with error accounted for by method of composition
#   against social opinion policy liberalism.
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
# Update the above social liberalism model to use the same variables, but 
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well and plot the same way as above.
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
# Apply a linear model to the social policy liberalism using party identification
#   proportions, social opinion liberalism, first-group social policy liberalism,
#   state, and year as factors. Store this model, the z-test results, and
#   method of composition error-accounting results.
#   Plot the regression results with error accounted for by method of composition
#   against social opinion policy liberalism.
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

# For a random sample of 100 iterations, update the model with democrat
#   legislative proportion data.
data.pd.social %>%
  MOC(its=sample(its, 100),
      update(model.ls[["adap_social"]]$model, . ~ . + DemPropLeg)) %>%
  MOCsumm()

## Econ
# Update the above social liberalism model to use the same variables, but 
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well and plot the same way as above.
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

# For a random sample of 100 iterations, update the model with democrat
#   legislative proportion data.
data.pd.econ %>%
  MOC(its=sample(its, 100),
      update(model.ls[["adap_econ"]]$model, . ~ . + DemPropLeg)) %>%
  MOCsumm()


## Social
# Apply a linear model to the social policy liberalism using social opinion liberalism, 
#   years after the house election, first-group social policy liberalism, state,
#   and year as factors. Store this model, the z-test results, and
#   method of composition error-accounting results.
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
# Update the above social liberalism model to use the same variables, but 
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well.
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
# Select an (unknown) subset of columns for social selection estimates
#   as well as the party effect model results to calculate multiplicative
#   mediation estimates for effect and reverse effect.
social_on_dem <- subset(model.ls[["sel_social"]]$moc,, MassSocialM1_mean)
dem_on_social <- dplyr::select(model.ls[["dem_social"]]$moc, DemControl)
mediation_social <- social_on_dem * dem_on_social
(mediation_social_est <- colMeans(mediation_social))
(mediation_social_se <- apply(mediation_social, 2, sd))
(mediation_social_z <- mediation_social_est / mediation_social_se)
apply(mediation_social, 2, pNorm)
## Econ
# Repeat the above process done for social estimates for economic estimates.
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
# Calculate subtractive mediation estimates for adaptation and dynamic
#   responsiveness.
(model.ls[["dp_social"]]$moc %>%
    MOCsumm(digits=5) %>%
    subset(variable == "MassM1_mean_est", value)) -
  (model.ls[["adap_social"]]$moc %>%
     MOCsumm(digits=5) %>%
     subset(variable == "MassM1_mean_est", value))
## econ
# Repeat the above procedure done for social estimates for economi estimates.
(model.ls[["dp_econ"]]$moc %>%
    MOCsumm(digits=5) %>%
    subset(variable == "MassM1_mean_est", value)) -
  (model.ls[["adap_econ"]]$moc %>%
     MOCsumm(digits=5) %>%
     subset(variable == "MassM1_mean_est", value))

### MODERATORS
## Social by Time
# Apply a linear model to the social policy liberalism using social opinion liberalism, 
#   the year being before 1972, first-group social policy liberalism, state, 
#   and year as factors. Store this model, the z-test results, and
#   method of composition error-accounting results.
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
# Update the above social liberalism model to use the same variables, but 
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well.
model.ls[["dp_econ_time"]]$model <- model.ls[["dp_social_time"]]$model %>%
  update(data=summ.pd.econ)
(model.ls[["dp_econ_time"]]$coeftest <-
    clusterSE(model.ls[["dp_econ_time"]]$model))
model.ls[["dp_econ_time"]]$moc <- data.pd.econ %>%
  MOC(its=its, model.ls[["dp_econ_time"]]$model)
MOCsumm(model.ls[["dp_econ_time"]]$moc)

## Social by Time by South
# Apply a linear model to the social policy liberalism using social opinion liberalism, 
#   the year being before 1972, southernness, first-group social policy liberalism, state, 
#   and year as factors. Store this model, the z-test results, and
#   method of composition error-accounting results.
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
# Update the above social liberalism model to use the same variables, but 
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well.
model.ls[["dp_econ_time_south"]]$model <-
  model.ls[["dp_social_time_south"]]$model %>%
  update(data=summ.pd.econ)
(model.ls[["dp_econ_time_south"]]$coeftest <-
    clusterSE(model.ls[["dp_econ_time_south"]]$model))
model.ls[["dp_econ_time_south"]]$moc <- data.pd.econ %>%
  MOC(its=its, model.ls[["dp_econ_time_south"]]$model)
MOCsumm(model.ls[["dp_econ_time_south"]]$moc)

## Social (Institutions)
# Apply a linear model to the social policy liberalism using first-group
#   social opinion liberalism, the year being before 1972, southernness,
#   suffrage restriction index, campaign finance index, citizen policymaking
#   index, the log of legislative days, first-group social policy liberalism,
#   state, and year as factors. Store this model, the z-test results, and
#   method of composition error-accounting results.
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
# Update the above social liberalism model to use the same variables, but 
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well.
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
# The following code generates formatted LaTeX tables that summarize the findings
#   of much of the analysis performed in the previous section of code.

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
# Check robustness of social opinion liberalism and social policy liberalism
#   impacts by testing across all group means and then running `clusterSE`
#   to check robustness.
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
# Repeat the above procedure executed on social liberalism data with
#   economic liberalism data.
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

# Generate a formatted LaTeX table that summarizes these robustness findings
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
# Create an error correction model that models social policy liberalism against
#   social opinion liberalism, year, and state as factors.
dp.social.D01.twoway.ec <-
  plm(PolicyD01_mean ~ MassM1_mean + PolicyM1_mean + Year + StPO,
      data=summ.pd.social, model="pooling")
clusterSE(dp.social.D01.twoway.ec)
dp.social.D01.twoway.ec.moc <- MOC(data=data.pd.social, its=its,
                                   dp.social.D01.twoway.ec)
MOCsumm(dp.social.D01.twoway.ec.moc)
## Econ
# Create an error correction model that models economic policy liberalism against
#   economic opinion liberalism, year, and state as factors.
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
# Model social policy liberalism differenced with first-group social policy
#   liberalism against first-group social policy liberalism, the difference
#   in first-group and second-group social opinion liberalism, second-group
#   social opinion liberalism, and year as factors.
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
# Repeat the above procedure executed on social liberalism data for 
#   economic liberalism data.
dp.econ.D01D12.time <-
  plm(I(Policy_mean - PolicyM1_mean) ~ PolicyM1_mean +
        I(MassM1_mean - MassM2_mean) + MassM2_mean +
        Year
      , data=summ.pd.econ, model="pooling")
clusterSE(dp.econ.D01D12.time)
dp.econ.D01D12.time.moc <-
  MOC(data=data.pd.econ, its=its, dp.econ.D01D12.time)
MOCsumm(dp.econ.D01D12.time.moc)

# Generate a formatted LaTeX table that summarizes the above differenced
#   model findings.
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
# Model social policy liberalism against a combination of several
#   standard-deviation-scaled measures along with the first-group policy
#   liberalism, year, and state.
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
# Repeat the above procedure done on social liberalism data for economic
#   liberalism data.
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

# Generate a formatted LaTeX table that summarizes the robustness findings
#   from the models generated above.
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

# Calculate variance-inflation factors for the social policy liberalism
#   modeled against social opinion liberalism and the year being before
#   1972 used as factors along with a number of institutional effect
#   factors and state.
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

# Repeat the above calculation executed on social liberalism data, but for
#   economic liberalism data.
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
# Model the impact of the election year and era along with social opinion
#   liberalism on social policy liberalism.
plm(PolicySocial_mean ~
      (MassSocialM1_mean : YearAfterHouseElection : Era4) + 
      (MassSocialM1_mean : Era4) +
      PolicySocialM1_mean 
    , data=summ.pd, model="within", effect="twoway") %>%
  clusterSE()
## Econ
# Model the impact of the election year and era along with economic opinion
#   liberalism on economic policy liberalism.
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


# -------- 6Figures.R --------
################################################################################
#### FIGURES ###################################################################
################################################################################

### FIGURE 2
# Generate a plot of summary models foudn in part 5 with states labeled
summ.map <- merge(summ, map_data("state"), by.x="name", by.y="region")
summ.map <- arrange(summ.map, group, order)

map.yrs <- c(1940, 1975, 2010)

# Create economic and social liberalism US maps colored by opinion data.
## Economic
setwd(paste0(rep.dir, "/pdf"))
pdf("maps_mass_econ.pdf", height=1.5, width=6.5)
summ.map %>%
  filter(Year %in% map.yrs) %>%
  ggplot(aes(x=long, y=lat, group=group, fill=MassEcon00_mean)) +
  facet_wrap(~Year, ncol=3) +
  geom_polygon(colour="black", size=rel(.1)) +
  coord_map("polyconic") +
  viridis::scale_fill_viridis(direction=1) +
  theme_clean() +
  guides(fill=FALSE)
dev.off()

## Social
setwd(paste0(rep.dir, "/pdf"))
pdf("maps_mass_social.pdf", height=1.5, width=6.5)
summ.map %>%
  filter(Year %in% map.yrs) %>%
  ggplot(aes(x=long, y=lat, group=group, fill=MassSocial00_mean)) +
  facet_wrap(~Year, ncol=3) +
  geom_polygon(colour="black", size=rel(.1)) +
  coord_map("polyconic") +
  viridis::scale_fill_viridis(direction=1) +
  theme_clean() +
  guides(fill=FALSE)
dev.off()

### FIGURE 3
# Plot the linear regression of mass liberalism against government liberalism
#   generated in the southern impact model for both economic liberalism
#   and social liberalism, including the grouping by pre vs post 1972 and state.
## Economic
setwd(paste0(rep.dir, "/pdf"))
pdf("xs_respons_econ_by_era_south.pdf", width=7.5, height=4)
summ %>%
  group_by(Pre72, StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            PolicyEcon00_mean = mean(PolicyEcon00_mean, na.rm=TRUE),
            MassEcon00_mean = mean(MassEcon00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassEcon00_mean, PolicyEcon00_mean, label=StPO,
             color=South11, lty=South11)) +
  facet_wrap(~Years, scales="free") +
  geom_smooth(method=lm) +
  geom_text(show.legend=FALSE, alpha=2/3) +
  scale_colour_manual(values = c("black", "grey40")) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Liberalism", y="Government Liberalism", color=NULL, lty=NULL) +
  ggtitle("Economic Policies")
dev.off()

## Social
setwd(paste0(rep.dir, "/pdf"))
pdf("xs_respons_social_by_era_south.pdf", width=7.5, height=4)
summ %>%
  group_by(Pre72, StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            PolicySocial00_mean = mean(PolicySocial00_mean, na.rm=TRUE),
            MassSocial00_mean = mean(MassSocial00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassSocial00_mean, PolicySocial00_mean, label=StPO,
             color=South11, lty=South11)) +
  facet_wrap(~Years, scales="free") +
  geom_smooth(method=lm) +
  geom_text(show.legend=FALSE, alpha=2/3) +
  scale_colour_manual(values = c("black", "grey40")) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Liberalism", y="Government Liberalism", color=NULL, lty=NULL) +
  ggtitle("Social Policies") 
dev.off()


### MORE PLOTS
# Plot 
# Plot the linear regression of mass liberalism against policy liberalism
#   generated for social liberalism including grouping by state and southernness.
summ %>%
  group_by(## Pre72, 
    StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            PolicySocial00_mean = mean(PolicySocial00_mean, na.rm=TRUE),
            MassSocial00_mean = mean(MassSocial00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassSocial00_mean, PolicySocial00_mean, label=StPO## ,
             ## color=South11, lty=South11
  )) +
  facet_wrap(~Years, scales="free", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Liberalism",
       y="Policy Liberalism",
       color=NULL, lty=NULL) +
  ggtitle("Social Issues") 

# Plot the linear regression of mass liberalism against policy liberalism
#   generated for economic liberalism including grouping by state and southernness.
summ %>%
  group_by(## Pre72, 
    StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            PolicyEcon00_mean = mean(PolicyEcon00_mean, na.rm=TRUE),
            MassEcon00_mean = mean(MassEcon00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassEcon00_mean, PolicyEcon00_mean, label=StPO## ,
             ## color=South11, lty=South11
  )) +
  facet_wrap(~Years, scales="free", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Liberalism",
       y="Policy Liberalism",
       color=NULL, lty=NULL) +
  ggtitle("Economic Issues")

# Plot the linear regression of democratic control against social policy liberalism
#   generated for social liberalism including grouping by state and southernness
#   and including summary effects of partisan control of the legislature.
summ %>%
  group_by(## Pre72, 
    StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            PolicySocial00_mean = mean(PolicySocial00_mean, na.rm=TRUE),
            DemControl = mean(DemControl, na.rm=TRUE)) %>%
  ggplot(aes(x=DemControl, PolicySocial00_mean, label=StPO## ,
             ## color=South11, lty=South11
  )) +
  facet_wrap(~Years, scales="free", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Democratic Control",
       y="Social Policy Liberalism",
       color=NULL, lty=NULL) +
  ggtitle("Social Issues") 

# Plot the linear regression of democratic control against economic policy liberalism
#   generated for economic liberalism including grouping by state and southernness
#   and including summary effects of partisan control of the legislature.
summ %>%
  group_by(## Pre72, 
    StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            PolicyEcon00_mean = mean(PolicyEcon00_mean, na.rm=TRUE),
            DemControl = mean(DemControl, na.rm=TRUE)) %>%
  ggplot(aes(x=DemControl, PolicyEcon00_mean, label=StPO## ,
             ## color=South11, lty=South11
  )) +
  facet_wrap(~Years, scales="free", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Democratic Control",
       y="Economic Policy Liberalism",
       color=NULL, lty=NULL) +
  ggtitle("Economic Issues")

# Plot the linear regression of mass liberalism against party identification proportions
#   generated for social liberalism including grouping by state and southernness
#   and including summary effects of public partisan identification.
summ %>%
  group_by(## Pre72, 
    StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            DemPID2Party_mean = mean(DemPID2Party_mean, na.rm=TRUE),
            MassSocial00_mean = mean(MassSocial00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassSocial00_mean, DemPID2Party_mean, label=StPO## ,
             ## color=South11, lty=South11
  )) +
  facet_wrap(~Years, scales="free", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Social Liberalism",
       y="Democratic % of Major-Party Identifiers",
       color=NULL, lty=NULL) +
  ggtitle("Social Liberalism and Democratic Partisanship") 

# Plot the linear regression of mass liberalism against party identification proportions
#   generated for economic liberalism including grouping by state and southernness
#   and including summary effects of public partisan identification.
summ %>%
  group_by(## Pre72, 
    StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            DemPID2Party_mean = mean(DemPID2Party_mean, na.rm=TRUE),
            MassEcon00_mean = mean(MassEcon00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassEcon00_mean, DemPID2Party_mean, label=StPO## ,
             ## color=South11, lty=South11
  )) +
  facet_wrap(~Years, scales="free", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Economic Liberalism",
       y="Democratic % of Major-Party Identifiers",
       color=NULL, lty=NULL) +
  ggtitle("Economic Liberalism and Democratic Partisanship") 

# Plot the linear regression of mass liberalism against partisan legislature control
#   generated for social liberalism including grouping by state and southernness.
summ %>%
  group_by(## Pre72, 
    StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            DemControl = mean(DemControl, na.rm=TRUE),
            MassSocial00_mean = mean(MassSocial00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassSocial00_mean, DemControl, label=StPO## ,
             ## color=South11, lty=South11
  )) +
  facet_wrap(~Years, scales="free_x", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Social Liberalism",
       y="Democratic Control of State Government",
       color=NULL, lty=NULL) +
  ggtitle("Social Liberalism and Democratic Control") 

# Plot the linear regression of mass liberalism against partisan legislature control
#   generated for economic liberalism including grouping by state and southernness.
summ %>%
  group_by(## Pre72, 
    StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            DemControl = mean(DemControl, na.rm=TRUE),
            MassEcon00_mean = mean(MassEcon00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassEcon00_mean, DemControl, label=StPO## ,
             ## color=South11, lty=South11
  )) +
  facet_wrap(~Years, scales="free_x", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Economic Liberalism",
       y="Democratic Control of State Government",
       color=NULL, lty=NULL) +
  ggtitle("Economic Liberalism and Democratic Control") 

# Plot the linear regression of mass liberalism against government liberalism
#   generated for social liberalism labeled by party identification.
summ %>%
  ungroup %>%
  filter(!is.na(DemControl)) %>%
  mutate(Control = cut(DemControl, breaks=2,
                       labels=c("Republican", "Democratic"))) %>% 
  ggplot(aes(x=NC(Year), y=PolicyEcon_mean, group=StPO, color=Control)) +
  theme_bw() +
  geom_line(alpha=1/2) +
  geom_smooth(aes(group=Control)) +
  labs(x="Year", y="Government Liberalism") +
  ggtitle("Economic Policies") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Plot the linear regression of mass liberalism against government liberalism
#   generated for economic liberalism labeled by party identification.
summ %>%
  ungroup %>%
  filter(!is.na(DemControl)) %>%
  mutate(Control = cut(DemControl, breaks=2,
                       labels=c("Republican", "Democratic"))) %>% 
  ggplot(aes(x=NC(Year), y=PolicySocial_mean, group=StPO, color=Control)) +
  theme_bw() +
  geom_line(alpha=1/2) +
  geom_smooth(aes(group=Control)) +
  labs(x="Year", y="Government Liberalism") +
  ggtitle("Social Policies") +
  theme(plot.title = element_text(hjust = 0.5)) 
