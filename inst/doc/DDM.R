## ----echo = FALSE-------------------------------------------------------------
rm(list = ls())
library(EMC2)
set.seed(1)

## -----------------------------------------------------------------------------
LexMat <- cbind(d = c(-1, 1))

design_lex <- design(
  factors = list(Lex = c("Non-Word", "Word"), subjects = 1),
  covariates = "Freq",
  Rlevels = c("Non-Word", "Word"),
  formula = list(v ~ Lex + Freq, a ~ 1, t0 ~ 1, Z ~ 1, sv ~ 1, s ~ Lex),
  contrasts = list(Lex = LexMat),
  constants = c(s = log(1)),
  model = DDM
)

## -----------------------------------------------------------------------------
sampled_pars(design_lex)

## -----------------------------------------------------------------------------
mapped_pars(design_lex)

## -----------------------------------------------------------------------------
p_vector <- sampled_pars(design_lex)
p_vector[] <- c(.1, 1.5, .2, log(1.1), log(.3), qnorm(.55), log(.3), .2)

mapped_pars(design_lex, p_vector)

## ----message=FALSE, fig.alt = "Design-level DDM trajectories for lexical decision"----
plot_design(design_lex, p_vector = p_vector, factors = list(v = "Lex"))

## ----results = "hide"---------------------------------------------------------
dat <- make_data(parameters = p_vector, design = design_lex, n_trials = 100)

## -----------------------------------------------------------------------------
word_frequency <- rgamma(sum(dat$Lex == "Word"), shape = 5, rate = .1)
# To make it more normally distributed we log-transform
word_frequency <- log(word_frequency)
# And scale it so that meaning of the intercept remains the mean drift
word_frequency <- as.numeric(scale(word_frequency))

frequency <- numeric(nrow(dat))
frequency[dat$Lex == "Word"] <- word_frequency

# Now we feed it to `make_data()`
dat <- make_data(
  parameters = p_vector,
  design = design_lex,
  n_trials = 100,
  covariates = list(Freq = frequency)
)

## ----fig.alt = "Defective density plots for lexical decision simulated data"----
plot_density(dat, factors = "Lex")

## ----results = "hide"---------------------------------------------------------
prior_lex <- prior(
  design = design_lex,
  type = "single",
  pmean = c(
    v = 0,
    v_Lexd = 2,
    v_Freq = 0,
    a = log(1),
    t0 = log(.25),
    Z = qnorm(.5),
    sv = log(.3),
    s_Lexd = 0
  ),
  psd = c(
    v = 1,
    v_Lexd = 1,
    v_Freq = .5,
    a = .2,
    t0 = .15,
    Z = .25,
    sv = .5,
    s_Lexd = .2
  )
)

## ----fig.alt = "Prior densities for DDM lexical example"----------------------
plot(prior_lex, N = 1e3)

## ----results = "hide"---------------------------------------------------------
emc <- make_emc(dat, design_lex, prior_list = prior_lex, type = "single")

## ----eval = FALSE-------------------------------------------------------------
# emc <- fit(emc, fileName = "data/DDM.RData")

## ----include = FALSE----------------------------------------------------------
load("data/DDM.RData")

## -----------------------------------------------------------------------------
summary(emc)

## ----fig.alt = "Posterior parameter densities against true values"------------
plot_pars(emc, true_pars = p_vector, use_prior_lim = FALSE)

## ----results = "hide"---------------------------------------------------------
pp <- predict(emc)

## ----fig.alt = "Posterior predictive defective CDF by lexicality"-------------
plot_cdf(dat, pp, factors = "Lex")

## ----fig.alt = "Posterior predictive defective CDF by frequency", fig.height = 6----
plot_cdf(dat, pp, factors = "Freq")

