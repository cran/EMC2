## ----echo = FALSE-------------------------------------------------------------
rm(list = ls())
library(EMC2)
set.seed(21)

## -----------------------------------------------------------------------------
choice_summary <- function(data) {
  out <- as.data.frame(prop.table(table(data$S, data$R), 1))
  names(out) <- c("S", "R", "prob")
  out
}

## -----------------------------------------------------------------------------
matchfun <- function(d) d$S == d$lR

design_ord <- design(
  Rlevels = c("low", "mid", "high"),
  factors = list(subjects = 1, S = c("low", "mid", "high")),
  formula = list(location ~ 0 + S, scale ~ 1, cut ~ 1),
  matchfun = matchfun,
  constants = c(scale = log(1)),
  model = ordered_probit
)

## -----------------------------------------------------------------------------
sampled_pars(design_ord)

## -----------------------------------------------------------------------------
p_vector_ord <- sampled_pars(design_ord)
p_vector_ord[] <- c(-1, 0, 1.2, -0.4, log(1.3))

mapped_pars(design_ord, p_vector_ord)

## ----results = "hide"---------------------------------------------------------
dat_ord <- make_data(parameters = p_vector_ord, design = design_ord, n_trials = 80)

## -----------------------------------------------------------------------------
choice_summary(dat_ord)

## ----results = "hide"---------------------------------------------------------
prior_ord <- prior(
  design = design_ord,
  type = "single",
  pmean = c(
    location_Slow = -0.5,
    location_Smid = 0,
    location_Shigh = 0.5,
    cut_lRlow = -0.2,
    cut_lRmid = log(1.1)
  ),
  psd = c(
    location_Slow = 0.8,
    location_Smid = 0.8,
    location_Shigh = 0.8,
    cut_lRlow = 0.5,
    cut_lRmid = 0.3
  )
)

emc_ord <- make_emc(dat_ord, design_ord, prior_list = prior_ord, type = "single", n_chains = 2)

## ----eval = FALSE-------------------------------------------------------------
# emc_ord <- fit(emc_ord, fileName = "data/response-models-ordered.RData")

## ----include = FALSE----------------------------------------------------------
load("data/response-models-ordered.RData")
emc_ord <- emc
rm(emc)

## -----------------------------------------------------------------------------
summary(emc_ord)

## ----fig.alt = "Posterior parameter densities against true values for the ordered probit example"----
plot_pars(emc_ord, true_pars = p_vector_ord, use_prior_lim = FALSE)

## -----------------------------------------------------------------------------
plot_fit_choice(emc_ord, factors = "S", style = "prob", n_post = 20)

## -----------------------------------------------------------------------------
plot_fit_choice(emc_ord, factors = "S", style = "cumulative", n_post = 20)

## ----eval = FALSE-------------------------------------------------------------
# design_ord_logit <- design(
#   Rlevels = c("low", "mid", "high"),
#   factors = list(subjects = 1, S = c("low", "mid", "high")),
#   formula = list(location ~ 0 + S, scale ~ 1, cut ~ 1),
#   matchfun = matchfun,
#   constants = c(scale = log(1)),
#   model = ordered_logit
# )

