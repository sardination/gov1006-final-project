setwd("../") ## replication directory
(rep.dir <- getwd())

mass.social.name <-
  "170628_1936-2014_social_varyingitems_blackurban_nocovariates"
mass.econ.name <-
  "170915_1936-2014_economic_varyingitems_blackurban_nocovariates_1000iter_DC"

### Read
setwd(paste0(rep.dir, "/input-data"))
social.opinion.samples <-
  mget(load(paste0("dgirt_output_", mass.social.name, ".RData")))
economic.opinion.samples <- 
  mget(load(paste0("dgirt_output_", mass.econ.name, ".RData")))
social.policy.samples <-
  mget(load("dynamic.irt.continuous.evolving.diff.stan-social-10003.Rdata"))
economic.policy.samples <- 
  mget(load("dynamic.irt.continuous.evolving.diff.stan-economic-1000.Rdata"))
pid.samples <- foreign::read.dta("StYrPID-poststratified-posterior-170918.dta")

### Transform
social.opinion.samples.df <- as.data.frame(social.opinion.samples$dgirt_out)
economic.opinion.samples.df <- as.data.frame(economic.opinion.samples$dgirt_out)
social.policy.samples.df <- as.data.frame(social.policy.samples$stan.cmb)
economic.policy.samples.df <- as.data.frame(economic.policy.samples$stan.cmb)

library(plyr)
library(dplyr)
library(reshape2)

stpos <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "HI",
           "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI",
           "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV",
           "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT",
           "VA", "VT", "WA", "WI", "WV", "WY")
length(stpos)
identical(stpos, sort(stpos))

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

social.policy.samples.df <- social.policy.samples.df %>%
  select(c(StPO=StPO, Year=Year, Liberalism=value, Iteration=iteration))
social.policy.samples.df <- social.policy.samples.df %>%
  arrange(Iteration, Year, StPO)

economic.policy.samples.df <- economic.policy.samples.df %>%
  select(c(StPO=StPO, Year=Year, Liberalism=value, Iteration=iteration))
economic.policy.samples.df <- economic.policy.samples.df %>%
  arrange(Iteration, Year, StPO)

pid.samples.df <- pid.samples %>%
  filter(!is.na(StPOAbrv)) %>%
  mutate(Iteration = iterations,
         StPO = factor(StPOAbrv, levels=stpos),
         Year = as.integer(Year)) %>%
  group_by(Year, StPO, Iteration) %>%
  summarise(DemPID = pid_total[PID=="D"],
            IndPID = pid_total[PID=="I"],
            RepPID = pid_total[PID=="R"])
         
pid.samples.df <- pid.samples.df %>%
  mutate(Dem2PID = DemPID / (DemPID + RepPID)) %>%
  arrange(Iteration, Year, StPO)

## Write
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
