setwd("../") ## replication directory
(rep.dir <- getwd())

load("output-data/Analyze.RData")

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

# This chunk of code generates a number of formatted LaTeX tables
#   summarizing the findings from previous sections.

StarsToBold <- function (text) {
  out <- gsub("([[:digit:]]+)\\^\\{\\*+\\}", "\\\\textbf{\\1}", text)
  cat(out, sep="\n")
  invisible(out)
}

### TABLE 1
# Table 1 lists policy liberalism effects with covariates of opinion
#   liberalism, southernness, and policy liberalism.
tab1.mods %>%
  stargazer(
    type="latex"
  , float=FALSE
  , se=tab1.moc.se
  , coef=tab1.moc.est
  , t.auto=TRUE
  , p.auto=TRUE ## from standard normal (very close to t when DF = 3,500)
  , omit=c("Year|Constant|^South11South$", "StPO")
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
  ) %>%
  StarsToBold()

# Table 2 lists democratic legislative control effects with covariates of opinion
#   liberalism, public partisanness, and democratic legislative control.
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
      "Dem Control$_{t-1}$"
      )
  ) %>%
  StarsToBold()

# Table 3 lists policy liberalism effects with covariates of democrat
#   legislature control, opinion liberalism, whether it is the year
#   after a house election, and policy liberalism.
tab3.mods %>%
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
      "-------------- Social --------------- $|$ ----------- Economic -----------")
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
  ) %>%
  StarsToBold()

# Table 4 lists policy liberalism effects with covariates of opinion
#   liberalism, southernness, pre vs post 1972, southernness, and 
#   institutional effect variables.
tab4.mods %>%
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
  ) %>%
  StarsToBold()

################################################################################
#### APPENDIX ##################################################################
################################################################################

### Autoregressive Distributed Lag Models
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

### First Difference Models
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
        , omit.table.layout = "n"
        , dep.var.caption="DV: First-Differenced Policy Liberalism ($t$)"
        , dep.var.labels.include=FALSE
        , dep.var.labels=c(
            "-------------- Social ------------------------- $|$ ---------------------- Economic -----------")
          , order=c("Policy", "Mass")
        , covariate.labels=c("Policy Lib ($t-1$)",
                             "Mass Lib ($t-1$)",
                             "$\\Delta$ Mass Lib ($t-1$)",
                             "Mass Lib ($t-2$)")
          ) %>% StarsToBold

### Interactions with Individual Institutions

stargazer(inst.mods
        , type="latex"
        , float=FALSE
        , se=MOCsd(inst.moc)
        , coef=MOCmean(inst.moc)
        , t.auto=TRUE
        , p.auto=TRUE
        , omit=c("Year|^South|^Pre72Pre-1972$|^Pre72Pre-1972:South11South|StPO|Constant|PolicyM1_mean")
        , omit.labels=c("Year FEs \\& State FEs \\& LDV")
        , omit.stat=c("F", "rsq")
        ## , omit.table.layout = "n"
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

################################################################################
#### RESULTS WITHOUT CORRECTING FOR MEASUREMENT ERROR ##########################
################################################################################
# This section recreates the four tables at the top, except while excluding
#   corrections for measurement error.
tab1.mods %>%
  stargazer(
    type="latex"
  , float=FALSE
  , se=tab1.cl.se
  , coef=tab1.cl.est
  , t.auto=TRUE
  , p.auto=TRUE ## from standard normal (very close to t when DF = 3,500)
  , omit=c("Year|Constant|^South11South$", "StPO")
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
  ) %>%
  StarsToBold()

tab2.mods %>%
  stargazer(
    type="latex"
  , float=FALSE
  , se=tab2.cl.se
  , coef=tab2.cl.est
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
      "Dem Control$_{t-1}$"
      )
  ) %>%
  StarsToBold()

tab3.mods %>%
  stargazer(
    type="latex"
  , float=FALSE
  , se=tab3.cl.se
  , coef=tab3.cl.est
  , t.auto=TRUE
  , p.auto=TRUE ## from standard normal (very close to t when DF = 3,500)
  , omit=c("^Year", "StPO|Constant")
  , omit.labels=c("Year FEs", "State FEs")
  , omit.stat=c("F", "rsq")
  , omit.table.layout = "n"
  , align=TRUE
  , dep.var.caption="DV: Domain-Specific Policy Liberalism ($t$)"
  , dep.var.labels=c(
      "-------------- Social --------------- $|$ ----------- Economic -----------")
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
  ) %>%
  StarsToBold()

tab4.mods %>%
  stargazer(
    type="latex"
  , float=FALSE
  , se=tab4.cl.se
  , coef=tab4.cl.est
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
  ) %>%
  StarsToBold()
