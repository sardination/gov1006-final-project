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

model.names <- c(
  ## Table 1
  "xs_contr_social", "xs_contr_econ", "fe_contr_social", "fe_contr_econ",
  "dyn_contr_social", "dyn_contr_econ", "dp_contr_social", "dp_contr_econ"
)

model.ls <- vector("list", length=length(model.names))
names(model.ls) <- model.names

# -------------------------------- MODELS ----------------------------------------------------------------

### CROSS-SECTIONAL
## Social
# Apply a linear model to the social policy liberalism using social opinion
#   liberalism, southernness, and year as factors. Store this model,
#   the z-test results, and method of composition error-accounting results.
#   Plot the regression results with error accounted for by method of composition
#   against social opinion policy liberalism.
model.ls[["xs_contr_social"]]$model <-
  plm(Policy_mean ~
        MassM1_mean * DemControl +
        Year * DemControl,
      data=summ.pd.social, model="pooling")
(model.ls[["xs_contr_social"]]$coeftest <-
    clusterSE(model.ls[["xs_contr_social"]]$model))
model.ls[["xs_contr_social"]]$moc <-
  MOC(data=data.pd.social, its=its, model.ls[["xs_contr_social"]]$model)
MOCsumm(model.ls[["xs_contr_social"]]$moc)
model.ls[["xs_contr_social"]]$moc %>%
  ggplot(aes(x=MassM1_mean)) +
  geom_histogram()

## Econ
# Update the above social liberalism model to use the same variables, but
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well and plot the same way as above.
model.ls[["xs_contr_econ"]]$model <- model.ls[["xs_contr_social"]]$model %>%
  update(data=summ.pd.econ) # NOTE SK: THIS SHOULD SAY `summ.pd.econ`
(model.ls[["xs_contr_econ"]]$coeftest <-
    clusterSE(model.ls[["xs_contr_econ"]]$model))
model.ls[["xs_contr_econ"]]$moc <-
  MOC(data=data.pd.econ, its=its, model.ls[["xs_contr_econ"]]$model)
MOCsumm(model.ls[["xs_contr_econ"]]$moc)
model.ls[["xs_contr_econ"]]$moc %>%
  ggplot(aes(x=MassM1_mean)) +
  geom_histogram()

### TWO-WAY FIXED EFFECTS
## Social
# Apply a linear model to the social policy liberalism using social opinion
#   liberalism, southernness, year, and state as factors. Store this model,
#   the z-test results, and method of composition error-accounting results.
#   Plot the regression results with error accounted for by method of composition
#   against social opinion policy liberalism.
model.ls[["fe_contr_social"]]$model <-
  plm(Policy_mean ~
        MassM1_mean * DemControl +
        Year * DemControl +
        StPO
      , data=summ.pd.social, model="pooling")
(model.ls[["fe_contr_social"]]$coeftest <-
    clusterSE(model.ls[["fe_contr_social"]]$model))
model.ls[["fe_contr_social"]]$moc <-
  MOC(data=data.pd.social, its=its, model.ls[["fe_contr_social"]]$model)
MOCsumm(model.ls[["fe_contr_social"]]$moc)
model.ls[["fe_contr_social"]]$moc %>%
  ggplot(aes(x=MassM1_mean)) +
  geom_histogram()

## Econ
# Update the above social liberalism model to use the same variables, but
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well and plot the same way as above.
model.ls[["fe_contr_econ"]]$model <- model.ls[["fe_contr_social"]]$model %>%
  update(data=summ.pd.econ)
(model.ls[["fe_contr_econ"]]$coeftest <-
    clusterSE(model.ls[["fe_contr_econ"]]$model))
model.ls[["fe_contr_econ"]]$moc <-
  MOC(data=data.pd.econ, its=its, model.ls[["fe_contr_econ"]]$model)
MOCsumm(model.ls[["fe_contr_econ"]]$moc)
model.ls[["fe_contr_econ"]]$moc %>%
  ggplot(aes(x=MassM1_mean)) +
  geom_histogram()

### DYNAMIC (LDV)
## Social
# Apply a linear model to the social policy liberalism using social opinion
#   liberalism, southernness, year, and first-group social policy liberalism means as factors.
#   Store this model, the z-test results, and method of composition error-accounting results.
#   Plot the regression results with error accounted for by method of composition
#   against social opinion policy liberalism.
model.ls[["dyn_contr_social"]]$model <-
  plm(Policy_mean ~
        MassM1_mean * DemControl +
        Year * DemControl +
        PolicyM1_mean
      , data=summ.pd.social, model="pooling")
(model.ls[["dyn_contr_social"]]$coeftest <-
    clusterSE(model.ls[["dyn_contr_social"]]$model))
model.ls[["dyn_contr_social"]]$moc <-
  MOC(data=data.pd.social, its=its, model.ls[["dyn_contr_social"]]$model)
MOCsumm(model.ls[["dyn_contr_social"]]$moc)

## Econ
# Update the above social liberalism model to use the same variables, but
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well and plot the same way as above.
model.ls[["dyn_contr_econ"]]$model <- model.ls[["dyn_contr_social"]]$model %>%
  update(data=summ.pd.econ)
(model.ls[["dyn_contr_econ"]]$coeftest <-
    clusterSE(model.ls[["dyn_contr_econ"]]$model))
model.ls[["dyn_contr_econ"]]$moc <-
  MOC(data=data.pd.econ, its=its, model.ls[["dyn_contr_econ"]]$model)
MOCsumm(model.ls[["dyn_contr_econ"]]$moc)
model.ls[["dyn_contr_econ"]]$moc %>%
  dplyr::select(contains("MassM1_mean")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram()

## Test for significance of state-specific intercepts
pFtest(terms(model.ls[["dyn_contr_social"]]$model), data=summ.pd.social,
       effect="individual")
pFtest(terms(model.ls[["dyn_contr_econ"]]$model), data=summ.pd.econ,
       effect="individual")
plmtest(model.ls[["dyn_contr_social"]]$model, effect="individual")
plmtest(model.ls[["dyn_contr_econ"]]$model, effect="individual")
##> Some evidence for state FEs

### DYNAMIC PANEL (LDV + FE)
## Social
# Apply a linear model to the social policy liberalism using social opinion
#   liberalism, southernness, year, first-group social policy liberalism means,
#   and state as factors. Store this model, the z-test results, and
#   method of composition error-accounting results.
#   Plot the regression results with error accounted for by method of composition
#   against social opinion policy liberalism.
model.ls[["dp_contr_social"]]$model <-
  plm(Policy_mean ~
        MassM1_mean * DemControl +
        Year * DemControl +
        PolicyM1_mean +
        StPO
      , data=summ.pd.social, model="pooling")
(model.ls[["dp_contr_social"]]$coeftest <-
    clusterSE(model.ls[["dp_contr_social"]]$model))
model.ls[["dp_contr_social"]]$moc <-
  MOC(data=data.pd.social, its=its, model.ls[["dp_contr_social"]]$model)
MOCsumm(model.ls[["dp_contr_social"]]$moc)
model.ls[["dp_contr_social"]]$moc %>%
  dplyr::select(contains("MassM1_mean")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram()

## Econ
# Update the above social liberalism model to use the same variables, but
#   with economic policy liberalism and economic opinion liberalism data.
#   Store the resulting updated model as well and plot the same way as above.
model.ls[["dp_contr_econ"]]$model <- model.ls[["dp_contr_social"]]$model %>%
  update(data=summ.pd.econ)
(model.ls[["dp_contr_econ"]]$coeftest <-
    clusterSE(model.ls[["dp_contr_econ"]]$model))
model.ls[["dp_contr_econ"]]$moc <-
  MOC(data=data.pd.econ, its=its, model.ls[["dp_contr_econ"]]$model)
MOCsumm(model.ls[["dp_contr_econ"]]$moc)
model.ls[["dp_contr_econ"]]$moc %>%
  dplyr::select(contains("MassM1_mean")) %>%
  melt() %>%
  ggplot(aes(x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_histogram() +
  geom_vline(xintercept=0, linetype="dotted")

## responsiveness in South
# Summarize the responsiveness values found through linear modeling of
#   policy liberalism against variables + opinion liberalism.
model.ls[["dp_contr_social"]]$moc %>%
  mutate(MassFullDem = MassM1_mean + `MassM1_mean:DemControl`) %>%
  MOCsumm

model.ls[["dp_contr_econ"]]$moc %>%
  mutate(MassFullDem = MassM1_mean + `MassM1_mean:DemControl`) %>%
  MOCsumm

### Long-run Multiplier
## Social
# Adjust regressed variables to apply for time-lagged applications
model.ls[["dp_contr_social"]]$moc %>%
  mutate(MassM1FullNonDem = `MassM1_mean`,
         MassM1FullDem = `MassM1_mean` + `MassM1_mean:DemControl`,
         MassM1Nation = 1/2 * MassM1FullNonDem + 1/2 * MassM1FullDem,
         PolicyM1 = `PolicyM1_mean`,
         MassM1FullNonDemLRM = MassM1FullNonDem / (1 - PolicyM1),
         MassM1FullDemLRM = MassM1FullDem / (1 - PolicyM1),
         MassM1NationLRM = MassM1Nation / (1 - PolicyM1)
  ) %>%
  MOCsumm

## Econ
# Adjust regressed variables to apply for time-lagged applications
model.ls[["dp_contr_econ"]]$moc %>%
  mutate(MassM1FullNonDem = `MassM1_mean`,
         MassM1FullDem = `MassM1_mean` + `MassM1_mean:DemControl`,
         MassM1Nation = 1/2 * MassM1FullNonDem + 1/2 * MassM1FullDem,
         PolicyM1 = `PolicyM1_mean`,
         MassM1FullNonDemLRM = MassM1FullNonDem / (1 - PolicyM1),
         MassM1FullDemLRM = MassM1FullDem / (1 - PolicyM1),
         MassM1NationLRM = MassM1Nation / (1 - PolicyM1)
  ) %>%
  MOCsumm

# ------------------------------------------------------------------------------------------------