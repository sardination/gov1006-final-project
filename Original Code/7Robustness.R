################################################################################
#### REPLICATION DROPPING STATE FES ############################################
################################################################################

model_ldv.ls <-  model.ls
for (i in seq_along(model.names)) {
  nm <- model.names[i]
  print(nm)
  x <- model.ls[[nm]]
  if (!any(grepl("StPO", formula(x$model)))) next
  newmod <- update(x$model, formula=. ~ . - StPO)
  print(formula(newmod))
  x$model <- newmod
  x$coeftest <- clusterSE(x$model)
  if (grepl("^sel", nm)) {
    x$moc <- MOC(data.pd, its, x$model)
  } else {
    if (grepl("social", nm)) {
      x$moc <- MOC(data.pd.social, its, x$model)
    }
    if (grepl("econ", nm)) {
      x$moc <- MOC(data.pd.econ, its, x$model)
    }
  }
  print(MOCsumm(x$moc))
  model_ldv.ls[[i]] <- x
}


################################################################################
#### REPLICATION WITH STANDARDIZED VARIABLES ###################################
################################################################################

model00.ls <-  model.ls
for (i in seq_along(model.names)) {
  nm <- model.names[i]
  print(nm)
  x <- model.ls[[nm]]
  if (!any(grepl("PolicyM1", formula(x$model)))) next
  sub.ls <- list(MassSocialM1_mean=quote(MassSocial00M1_mean),
                 MassEconM1_mean=quote(MassEcon00M1_mean),
                 MassM1_mean=quote(Mass00M1_mean),
                 Policy_mean=quote(Policy00_mean),
                 PolicyM1_mean=quote(Policy00M1_mean))
  newf <- as.formula(do.call("substitute", list(formula(x$model), sub.ls)))
  newmod <- update(x$model, formula=newf)
  x$model <- newmod
  x$coeftest <- clusterSE(x$model)
  if (grepl("^sel", nm)) {
    x$moc <- MOC(data.pd, its, x$model)
  } else {
    if (grepl("social", nm)) {
      x$moc <- MOC(data.pd.social, its, x$model)
    }
    if (grepl("econ", nm)) {
      x$moc <- MOC(data.pd.econ, its, x$model)
    }
  }
  print(MOCsumm(x$moc))
  model00.ls[[i]] <- x
}

setwd(paste0(rep.dir, "/output-data"))
save(model_ldv.ls, model00.ls, file="Robustness.RData")
