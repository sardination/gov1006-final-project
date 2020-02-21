setwd("../") ## replication directory
(rep.dir <- getwd())

library(dgo)
library(dplyr)
library(foreign)

mass.social.name <-
  "170628_1936-2014_social_varyingitems_blackurban_nocovariates"
mass.econ.name <-
  "170915_1936-2014_economic_varyingitems_blackurban_nocovariates_1000iter_DC"

### Read
setwd(paste0(rep.dir, "/intermediate-data"))
social.opinion.samples.df <-
  read.dta(paste0("samples", mass.social.name, ".dta"))
economic.opinion.samples.df <-
  read.dta(paste0("samples", mass.econ.name, ".dta"))
setwd(paste0(rep.dir, "/input-data"))
targets <- readRDS(file = "targets.rds")

## Mutate
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

st.targets <-
  aggregate(Prop ~ D_abb + D_year + D_black_x_urban4, group.targets, sum)
st.targets <- as.data.frame(st.targets)
glimpse(st.targets)

### Poststratify
yrs.to.est <- 1936:2014

social.iters <- 1:max(social.opinion.samples.df$iteration)
social.opinion.samples.subset <- social.opinion.samples.df %>%
  filter(iteration %in% social.iters)
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

social.opinion.samples.st <- social.opinion.samples.st %>%
  select(StPO = D_abb,
         Year = D_year,
         MassSocialLib = value,
         Iteration = Iteration) %>%
  mutate(StPO = factor(gsub("D\\_abb(.*)", "\\1", StPO)))

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
setwd(paste0(rep.dir, "/intermediate-data"))
write.dta(
  social.opinion.samples.st,
  paste0("samples", mass.social.name, "-st_est.dta"))
write.dta(
  economic.opinion.samples.st,
  paste0("samples", mass.econ.name, "-st_est.dta"))

q()
