# Simple joint test
# Take the first entry of the design list
# emc stores a design list (with length 1 for non-joint models)
# or for joint models length x where x is the number of joint components
design_in <- get_design(samples_LNR)[[1]]
prior_in <- get_prior(samples_LNR)
data_in <- get_data(samples_LNR)

prior_joint <- prior(list(a = design_in, b = design_in))

joint <- make_emc(list(data_in, data_in), list(a = design_in, b = design_in), prior_list = prior_joint)
RNGkind("L'Ecuyer-CMRG")
set.seed(123)
test_that("joint", {
  expect_snapshot(init_chains(joint, particles = 10, cores_for_chains = 1)[[1]]$samples)
})
