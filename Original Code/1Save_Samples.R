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