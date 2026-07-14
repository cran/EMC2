## ----echo = FALSE-------------------------------------------------------------
rm(list = ls())
library(EMC2)
set.seed(11)

## -----------------------------------------------------------------------------
matchfun <- function(d) d$S == d$lR

# "Average/difference" coding for the TRUE/FALSE lM factor
ADmat <- matrix(c(-1/2, 1/2), ncol = 1, dimnames = list(NULL, "d"))

design_lba <- design(
  factors = list(subjects = 1, S = c("red", "green", "blue")),
  Rlevels = c("red", "green", "blue"),
  matchfun = matchfun,
  formula = list(v ~ lM + S, B ~ lR, A ~ 1, t0 ~ 1, sv ~ 1),
  contrasts = list(v = list(lM = ADmat)),
  constants = c(sv = log(1)),
  model = LBA
)

## -----------------------------------------------------------------------------
sampled_pars(design_lba)

## -----------------------------------------------------------------------------
mapped_pars(design_lba)

## -----------------------------------------------------------------------------
p_vector <- sampled_pars(design_lba)
p_vector[] <- c(
  v = 1.4,
  v_lMd = 1.8,
  v_Sgreen = 0.15,
  v_Sblue = -0.1,
  B = log(0.7),
  B_lRgreen = 0.1,
  B_lRblue = -0.1,
  A = log(0.3),
  t0 = log(0.25)
)

mapped_pars(design_lba, p_vector)

## ----message=FALSE, fig.alt = "Design-level LBA trajectories for three stimulus identities"----
plot_design(design_lba, p_vector = p_vector, factors = list(v = "S", B = "lR"), plot_factor = "lR", layout = c(1,3))

## ----results = "hide"---------------------------------------------------------
dat <- make_data(parameters = p_vector, design = design_lba, n_trials = 80)

## ----fig.alt = "Defective density plots for three-choice race model simulated data"----
plot_density(dat, factors = "S", layout = c(1,3))

## ----results = "hide"---------------------------------------------------------
prior_lba <- prior(
  design = design_lba,
  type = "single",
  pmean = c(
    v = 1.2,
    v_lMd = 1.5,
    v_Sgreen = 0,
    v_Sblue = 0,
    B = log(0.8),
    B_lRgreen = 0,
    B_lRblue = 0,
    A = log(0.25),
    t0 = log(0.2)
  ),
  psd = c(
    v = .5,
    v_lMd = .6,
    v_Sgreen = .4,
    v_Sblue = .4,
    B = .25,
    B_lRgreen = .25,
    B_lRblue = .25,
    A = .25,
    t0 = .15
  )
)

## ----fig.alt = "Prior densities for three-choice LBA example"-----------------
plot(prior_lba, N = 1e3)

## ----results = "hide"---------------------------------------------------------
emc <- make_emc(dat, design_lba, prior_list = prior_lba, type = "single")

## ----eval = FALSE-------------------------------------------------------------
# emc <- fit(emc, fileName = "data/race-models.RData")

## ----include = FALSE----------------------------------------------------------
load("data/race-models.RData")

## -----------------------------------------------------------------------------
summary(emc)

## ----fig.alt = "Posterior parameter densities against true values for three-choice LBA", fig.height = 9----
plot_pars(emc, true_pars = p_vector, use_prior_lim = FALSE)

## ----results = "hide"---------------------------------------------------------
pp <- predict(emc)

## ----fig.alt = "Posterior predictive defective CDF by stimulus identity in three-choice LBA"----
plot_cdf(dat, pp, factors = "S", layout = c(1,3))

