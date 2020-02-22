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