## ----echo = FALSE-------------------------------------------------------------
rm(list = ls())
library(EMC2)

## -----------------------------------------------------------------------------
trend_help()

## -----------------------------------------------------------------------------
# Example trend: log-mean increases linearly with trial
trend_quick <- make_trend(
  par_names = "m",
  cov_names = "trial_nr",
  kernels   = "lin_incr",
  bases     = "lin",
  phase     = "pretransform"
)

data <- get_data(samples_LNR)
# This does not take subject id into account
data$trial_nr <- 1:nrow(data)
data$covariate1 <- rnorm(nrow(data))
data$covariate2 <- rnorm(nrow(data))


# Build a design with the trend
design_trend <- design(
  data     = data,
  trend    = trend_quick,
  matchfun = function(d) d$S == d$lR,
  formula  = list(m ~ lM, s ~ 1, t0 ~ 1),
  contrasts = list(lM = matrix(c(-1/2, 1/2), ncol = 1, dimnames = list(NULL, "d"))),
  model    = LNR
)

# How you would run (not executed here)
# emc <- make_emc(data, design_trend, type = "single")
# fit <- fit(emc)

## -----------------------------------------------------------------------------
trend_lin_decr <- make_trend(
  par_names = "v",
  cov_names = "trial_nr",
  kernels = "lin_incr",
  bases = "lin"
)

## -----------------------------------------------------------------------------
trend_help(kernel = "lin_incr")

## -----------------------------------------------------------------------------
trend_help(base = "lin")

## -----------------------------------------------------------------------------
get_trend_pnames(trend_lin_decr)

## -----------------------------------------------------------------------------
# Linear decreasing trend
trend_lin_decr <- make_trend(
  par_names = "v",
  cov_names = "trial_nr",
  kernels = "lin_decr"
)

# Linear increasing trend
trend_lin_incr <- make_trend(
  par_names = "v",
  cov_names = "trial",
  kernels = "lin_incr"
)

## -----------------------------------------------------------------------------
# Exponential decreasing trend
trend_exp_decr <- make_trend(
  par_names = "v",
  cov_names = "trial_nr",
  kernels = "exp_decr"
)

# Exponential increasing trend
trend_exp_incr <- make_trend(
  par_names = "v",
  cov_names = "trial_nr",
  kernels = "exp_incr"
)

## -----------------------------------------------------------------------------
# Power decreasing trend
trend_pow_decr <- make_trend(
  par_names = "v",
  cov_names = "trial_nr",
  kernels = "pow_decr"
)

# Power increasing trend
trend_pow_incr <- make_trend(
  par_names = "v",
  cov_names = "trial_nr",
  kernels = "pow_incr"
)

## -----------------------------------------------------------------------------
# Quadratic trend
trend_poly2 <- make_trend(
  par_names = "v",
  cov_names = "trial_nr",
  kernels = "poly2"
)

# Cubic trend
trend_poly3 <- make_trend(
  par_names = "v",
  cov_names = "trial_nr",
  kernels = "poly3"
)

# Quartic trend
trend_poly4 <- make_trend(
  par_names = "v",
  cov_names = "trial_nr",
  kernels = "poly4"
)

## -----------------------------------------------------------------------------
# Standard delta learning rule
trend_delta <- make_trend(
  par_names = "v",
  cov_names = "trial_nr",
  kernels = "delta"
)

# Dual learning rate delta rule
trend_delta2 <- make_trend(
  par_names = "v",
  cov_names = "trial_nr",
  kernels = "delta2kernel"
)

## -----------------------------------------------------------------------------
trend_exp_incr <- make_trend(
  par_names = "v",
  cov_names = "trial_nr",
  kernels = "exp_incr",
  bases =  "exp_lin"
)

## -----------------------------------------------------------------------------
# Applying different trends to multiple parameters
trend_multi <- make_trend(
  par_names = c("v", "t0"),
  cov_names = c("trial_nr"),
  kernels = c("exp_incr", "poly2")
)

## -----------------------------------------------------------------------------
# Specifying different covariates for each trend
trend_multi <- make_trend(
  par_names = c("v", "t0"),
  cov_names = c("trial_nr", "covariate1"),
  kernels = c("exp_incr", "poly2")
)

## -----------------------------------------------------------------------------
# Specifying multiple covariates for a trend
trend_multi <- make_trend(
  par_names = c("v", "t0"),
  cov_names = list(c("trial", "covariate1"), "covariate1"),
  kernels = c("exp_incr", "poly2")
)

## -----------------------------------------------------------------------------
# Sharing parameters between trends
trend_shared <- make_trend(
  par_names = c("v", "a"),
  cov_names = "trial_nr",
  kernels = c("exp_incr", "exp_incr"),
  shared = list(intercept = list("v.B0", "a.B0"))
)

## -----------------------------------------------------------------------------
get_trend_pnames(trend_shared)

## -----------------------------------------------------------------------------
# Pre-mapping trend
trend_premap <- make_trend(
  par_names = "v",
  cov_names = "trial_nr",
  kernels   = "exp_incr",
  phase     = "premap"
)

# Pre-transform trend
trend_pretrans <- make_trend(
  par_names = "v",
  cov_names = "trial_nr",
  kernels   = "exp_incr",
  phase     = "pretransform"
)

# Post-transform trend
trend_posttrans <- make_trend(
  par_names = "v",
  cov_names = "trial_nr",
  kernels   = "exp_incr",
  phase     = "posttransform"
)

## -----------------------------------------------------------------------------
trend_par_input <- make_trend(
  par_names = "m",
  cov_names = NULL,
  kernels   = "lin_incr",
  par_input = list("t0"),
  phase     = "pretransform"
)

## -----------------------------------------------------------------------------
trend_at <- make_trend(
  par_names = c("m"),
  cov_names = list("covariate1"),
  kernels   = c("exp_incr"),
  phase     = "pretransform",
  at        = "lR"   # apply only at first level of lR, then expand within subject
)

## -----------------------------------------------------------------------------
trend_multi_same_par <- make_trend(
  par_names = c("m", "m"),
  cov_names = list("covariate1", c("covariate2")),  # second entry could also use par_input
  kernels   = c("exp_incr", "delta"),
  phase     = "pretransform",
  at        = "lR"
)

## -----------------------------------------------------------------------------
trend_phases <- make_trend(
  par_names = c("m", "s", "t0"),
  cov_names = list("covariate1", "covariate1", "covariate2"),
  kernels   = c("lin_incr", "exp_decr", "pow_incr"),
  phase     = c("premap", "pretransform", "posttransform")
)

## -----------------------------------------------------------------------------
trend_shared_kernel <- make_trend(
  par_names = c("m", "s"),
  cov_names = list("covariate1", "covariate2"),
  kernels   = c("poly3", "poly4"),
  shared    = list(shrd = list("m.d1", "s.d1"))
)

## -----------------------------------------------------------------------------
# Example delta trend, capturing trial-wise dynamics
trend_delta <- make_trend(
  par_names = "m",
  cov_names = "trial_nr",
  kernels   = "delta",
  phase     = "pretransform"
)

design_delta <- design(
  factors    = list(subjects = 1, S = 1:2),
  Rlevels    = 1:2,
  covariates = "trial_nr",
  matchfun   = function(d) d$S == d$lR,
  trend      = trend_delta,
  formula    = list(m ~ lM, s ~ 1, t0 ~ 1),
  contrasts  = list(lM = matrix(c(-1/2, 1/2), ncol = 1, dimnames = list(NULL, "d"))),
  model      = LNR
)

# Retrieve trial-wise parameters alongside generated data
# (not executed here)
# res <- make_data(p_vector, design_delta, n_trials = 10,
#                  conditional_on_data = FALSE,
#                  return_trialwise_parameters = TRUE)
# str(attr(res, "trialwise_parameters"))

## -----------------------------------------------------------------------------
trend_premap <- make_trend(
  par_names = c("m", "lMd"),
  cov_names = list("covariate1", "covariate2"),
  kernels   = c("exp_incr", "poly2"),
  phase     = "premap"
)

design_premap <- design(
  data     = data,                   
  trend    = trend_premap,
  formula  = list(m ~ 1, s ~ 1, t0 ~ 1, lMd.d1 ~ lR),
  model    = LNR
)
# mapped_pars(design_premap)  # inspect mapped parameter structure

## -----------------------------------------------------------------------------
trend_pretrans <- make_trend(
  par_names = c("m", "s"),
  cov_names = list("covariate1", "covariate2"),
  kernels   = c("delta", "exp_decr"),
  phase     = "pretransform"
)

design_pretrans <- design(
  data     = data,
  trend    = trend_pretrans,
  formula  = list(m ~ 1, s ~ 1, t0 ~ 1, s.w ~ lR),
  model    = LNR
)
# mapped_pars(design_pretrans)

## -----------------------------------------------------------------------------
trend_posttrans <- make_trend(
  par_names = c("m", "s"),
  cov_names = list("covariate1", "covariate2"),
  kernels   = c("pow_decr", "pow_incr"),
  phase     = "posttransform"
)

design_posttrans <- design(
  data     = data,
  trend    = trend_posttrans,
  formula  = list(m ~ 1, s ~ 1, t0 ~ 1, s.w ~ lR),
  model    = LNR
)
# mapped_pars(design_posttrans)

## ----eval=FALSE---------------------------------------------------------------
# library(EMC2)
# 
# # Write a custom kernel to a separate file
# tf <- tempfile(fileext = ".cpp")
# writeLines(c(
#   "// [[Rcpp::depends(EMC2)]]",
#   "#include <Rcpp.h>",
#   "#include \"EMC2/userfun.hpp\"",
#   "",
#   "// Example: two params (a, b) and two inputs (covariate1, t0)",
#   "Rcpp::NumericVector custom_kernel(Rcpp::NumericMatrix trend_pars, Rcpp::NumericMatrix input) {",
#   "  int n = input.nrow();",
#   "  Rcpp::NumericVector out(n, 0.0);",
#   "  for (int i = 0; i < n; ++i) {",
#   "    double a = (trend_pars.ncol() > 0) ? trend_pars(i, 0) : 0.0;",
#   "    double b = (trend_pars.ncol() > 1) ? trend_pars(i, 1) : 0.0;",
#   "    double in1 = input(i, 0);  // covariate1",
#   "    double in2 = input(i, 1);  // t0",
#   "    if ((i % 2) == 0) out[i] = (Rcpp::NumericVector::is_na(in1) ? 0.0 : in1) + a;",
#   "    else             out[i] = (Rcpp::NumericVector::is_na(in2) ? 0.0 : in2) * b;",
#   "  }",
#   "  return out;",
#   "}",
#   "",
#   "// Export pointer maker for registration",
#   "// [[Rcpp::export]]",
#   "SEXP EMC2_make_custom_kernel_ptr();",
#   "EMC2_MAKE_PTR(custom_kernel)"
# ), tf)
# 
# # Register with parameter names, transforms, and a default base
# ct <- register_trend(
#   trend_parameters = c("a", "b"),
#   file = tf,
#   transforms = c(a = "identity", b = "pnorm"),
#   base = "add"
# )
# 
# # Use in a trend (note par_input to add t0 as an input column)
# trend_custom <- make_trend(
#   par_names  = "m",
#   cov_names  = "covariate1",
#   kernels    = "custom",
#   par_input  = list("t0"),
#   phase      = "pretransform",
#   bases      = NULL,         # uses ct$base (here: add)
#   custom_trend = ct
# )
# 
# design_custom_trend <- design(
#   data = data,
#   trend = trend_custom,
#   formula = list(m ~ 1, s ~ 1, t0 ~ 1, m.a ~ lR),
#   model = LNR
# )

