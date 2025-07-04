#'
#' Specify Priors for the Chosen Model
#'
#' These values are entered manually by default but can be recycled from another prior (given in the `update` argument).
#'
#' Where a value is not supplied, the user is prompted to enter numeric values (or functions that evaluate to numbers).
#'
#' To get the prior help use `prior_help(type)`. With `type` e.g. 'diagonal'.
#'
#' @param design Design list for which a prior is constructed, typically the output of `design()`
#' @param update Prior list from which to copy values
#' @param type Character. What type of group-level model you plan on using i.e. `diagonal`
#' @param group_design An `emc.group_design` object created with `group_design()`
#' @param do_ask Character. For which parameter types or hyperparameters to ask for prior specification,
#' i.e. `Sigma`, `mu` or `loadings` for factor models, but `theta_mu_mean` or `A` also works.
#' @param fill_default Boolean, If `TRUE` will fill all non-specified parameters, and parameters outside of `do_ask`, to default values
#' @param ... Either values to prefill, i.e. `theta_mu_mean = c(1:6)`, or additional arguments such as `n_factors = 2`
#' @return A prior list object
#' @examples
#' # First define a design for the model
#' design_DDMaE <- design(data = forstmann,model=DDM,
#'                            formula =list(v~0+S,a~E, t0~1, s~1, Z~1, sv~1, SZ~1),
#'                            constants=c(s=log(1)))
#' # Then set up a prior using prior
#' p_vector=c(v_Sleft=-2,v_Sright=2,a=log(1),a_Eneutral=log(1.5),a_Eaccuracy=log(2),
#'                      t0=log(.2),Z=qnorm(.5),sv=log(.5),SZ=qnorm(.5))
#' psd <- c(v_Sleft=1,v_Sright=1,a=.3,a_Eneutral=.3,a_Eaccuracy=.3,
#'                      t0=.4,Z=1,sv=.4,SZ=1)
#' # Here we left the variance prior at default
#' prior_DDMaE <- prior(design_DDMaE,mu_mean=p_vector,mu_sd=psd)
#' # Also add a group-level variance prior:
#' pscale <- c(v_Sleft=.6,v_Sright=.6,a=.3,a_Eneutral=.3,a_Eaccuracy=.3,
#'                              t0=.2,Z=.5,sv=.4,SZ=.3)
#' df <- .4
#' prior_DDMaE <- prior(design_DDMaE,mu_mean=p_vector,mu_sd=psd, A = pscale, df = df)
#' # If we specify a new design
#' design_DDMat0E <- design(data = forstmann,model=DDM,
#'                            formula =list(v~0+S,a~E, t0~E, s~1, Z~1, sv~1, SZ~1),
#'                            constants=c(s=log(1)))
#' # We can easily update the prior
#' prior_DDMat0E <- prior(design_DDMat0E, update = prior_DDMaE)
#' @export
prior <- function(design, type = NULL, group_design = NULL, update = NULL,
                      do_ask = NULL, fill_default = TRUE, ...){
  if(!is.null(update) && is.null(type)){
    type <- attr(update, "type")
  }
  if(is.null(type)) type <- 'standard'
  if(type %in% c("diagonal", "blocked")) type <- "standard"
  args <- list(...)
  args$group_design <- group_design
  input <- do.call(get_objects, c(list(design = design, type = type), update, args))
  descriptions <- input$descriptions
  groups <- input$types
  group_descriptions <- input$type_descriptions
  orig <- input <- input$prior
  input <- input[names(input) %in% names(descriptions)]
  input <- check_var_prior(input)
  args <- check_var_prior(args)
  update <- check_var_prior(update)
  # First check if mu_mean, mu_sd, pmean or psd are filled in
  if(!is.null(args$psd)) args$mu_sd <- args$psd
  if(!is.null(args$pmean)) args$mu_mean <- args$pmean
  if(!is.null(args$mu_mean)) args$theta_mu_mean <- args$mu_mean
  if(!is.null(args$mu_sd)) args$theta_mu_var <- args$mu_sd^2
  # Initialize updated_flags
  updated_flags <- lapply(input, make_prior_idx)

  # First, update 'input' with the 'update' list
  result <- update_prior_input(input, update, updated_flags)
  input <- result$input
  updated_flags <- result$flags

  # Then, update 'input' with the ellipsis arguments (taking precedence)
  result <- update_prior_input(input, args, updated_flags)
  input <- result$input
  updated_flags <- result$flags

  # Now, prompt user for un-updated elements
  if(!fill_default) do_ask <- names(input)
  do_ask <- unique(c(do_ask, unlist(groups[names(groups) %in% do_ask])))
  input[names(input) %in% do_ask] <- prompt_for_prior_updates(input[names(input) %in% do_ask],
                                                              updated_flags[names(updated_flags) %in% do_ask],
                                                              descriptions[names(descriptions) %in% do_ask])
  if(!is.null(input$theta_mu_var)){
    if(!is.matrix(input$theta_mu_var) & is.matrix(orig$theta_mu_var)) input$theta_mu_var <- diag(input$theta_mu_var)
  }
  prior <- get_objects(design = design, type = type, prior = input, ...)$prior
  if("Ffactors" %in% names(design)){
    design <- list(design)
  }
  # For fMRI models, design matrices are stored in design until dadm creation
  design <- lapply(design, function(x){
    attr(x, "design_matrix") <- NULL
    return(x)
  })
  attr(prior, "design") <- design
  attr(prior, "group_design") <- group_design
  class(prior) <- "emc.prior"
  return(prior)
}

check_var_prior <- function(inp){
  if(!is.null(inp$theta_mu_var)){
    if(is.matrix(inp$theta_mu_var)){
      tmp <- diag(inp$theta_mu_var)
      names(tmp) <- colnames(inp$theta_mu_var)
      inp$theta_mu_var <- tmp
    }
  }
  return(inp)
}

# Function to initialize updated flags for tracking
make_prior_idx <- function(input_element) {
  if (is.atomic(input_element) && length(input_element) > 1) {
    setNames(rep(FALSE, length(input_element)), names(input_element))
  } else {
    FALSE
  }
}

# Updated helper function to update individual elements and flags
update_prior_element <- function(input_element, update_value, updated_flags_element) {
  # Check if 'input_element' is a vector with length greater than 1
  if (is.vector(input_element) && length(input_element) > 1) {
    if (length(update_value) == 1 && is.null(names(update_value))) {
      # If 'update_value' is a single unnamed value, update all elements
      input_element[] <- update_value
      updated_flags_element[] <- TRUE
    } else if (!is.null(names(update_value))) {
      # If 'update_value' is a named vector, update matching names
      common_names <- intersect(names(input_element), names(update_value))
      input_element[common_names] <- update_value[common_names]
      updated_flags_element[common_names] <- TRUE
    } else if (is.null(names(update_value)) && length(update_value) == length(input_element)) {
      # If 'update_value' is an unnamed vector of same length, update in order
      input_element[] <- update_value
      updated_flags_element[] <- TRUE
    }
    # If there are names, but they do not match and the length is the same
    # still update
    if(!any(updated_flags_element) && length(update_value) == length(input_element)){
      input_element[] <- update_value
      updated_flags_element[] <- TRUE
    }

    # If 'update_value' is an unnamed vector of different length, no action taken
  } else {
    # If 'input_element' is a single value, update it directly
    input_element <- update_value
    updated_flags_element <- TRUE
  }
  return(list(element = input_element, flags = updated_flags_element))
}

# Helper function to update the 'input' list based on 'updates' and flags
update_prior_input <- function(input, updates, updated_flags) {
  for (name in names(updates)) {
    if (name %in% names(input)) {
      # Update the corresponding element in 'input'
      result <- update_prior_element(input[[name]], updates[[name]], updated_flags[[name]])
      input[[name]] <- result$element
      updated_flags[[name]] <- result$flags
    }
    # If the name is not in 'input', it is ignored
  }
  return(list(input = input, flags = updated_flags))
}

# Function to convert user input to appropriate type
convert_prior_input <- function(user_input, current_value) {
  if (is.numeric(current_value)) {
    new_value <- as.numeric(user_input)
    if (is.na(new_value)) {
      new_value <- current_value
    }
  } else if (is.character(current_value)) {
    new_value <- user_input
  } else if (is.logical(current_value)) {
    if (tolower(user_input) %in% c("true", "t", "1")) {
      new_value <- TRUE
    } else if (tolower(user_input) %in% c("false", "f", "0")) {
      new_value <- FALSE
    } else {
      new_value <- current_value
    }
  } else {
    new_value <- user_input
  }
  return(new_value)
}

# Function to prompt user for unfilled values
prompt_for_prior_updates <- function(input, updated_flags, descriptions) {
  for (name in names(input)) {
    flags <- updated_flags[[name]]
    if(all(flags)) next
    element <- input[[name]]
    cat("Specify", descriptions[[name]], "\n")
    cat("Press enter to use default value (", element[1], ")")
    if (is.atomic(element) && length(element) > 1) {
      # It's a vector
      for (i in seq_along(element)) {
        if (!flags[i]) {
          # This element was not updated, prompt the user
          current_value <- element[i]
          elem_name <- names(element)[i]
          prompt_text <- paste0("Enter value for '", elem_name, "': ")
          user_input <- readline(prompt_text)
          if (user_input == "") {
            # Use default value
            new_value <- current_value
          } else {
            new_value <- convert_prior_input(user_input, current_value)
          }
          element[i] <- new_value
        }
      }
      input[[name]] <- element
    } else {
      # Single value
      if (!flags) {
        current_value <- element
        prompt_text <- paste0("Enter value: ")
        user_input <- readline(prompt_text)
        if (user_input == "") {
          # Use default value
          new_value <- current_value
        } else {
          new_value <- convert_prior_input(user_input, current_value)
        }
        input[[name]] <- new_value
      }
    }
  }
  return(input)
}


check_prior <- function(prior, sampled_p_names, group_design = NULL){
  sampled_p_names <- add_group_par_names(sampled_p_names, group_design)
  if (is.null(names(prior$theta_mu_mean)))
    names(prior$theta_mu_mean) <- sampled_p_names
  if(length(prior$theta_mu_mean) != length(sampled_p_names))
    stop("prior mu should be same length as estimated parameters (p_vector)")
  if (!all(sort(names(prior$theta_mu_mean)) == sort(sampled_p_names)))
    stop("theta_mu_mean names not the same as sampled paramter names")
  # Make sure theta_mu_mean has same order as sampled parameters
  prior$theta_mu_mean <- prior$theta_mu_mean[sampled_p_names]
  pnams <- names(prior$theta_mu_mean)
  if(!is.matrix(prior$theta_mu_var)) {
    if(length(prior$theta_mu_var) != length(sampled_p_names))
      stop("prior theta should be same length as estimated parameters (p_vector)")
    # Make sure theta_mu_var has same order as sampled parameters
    names(prior$theta_mu_var) <- sampled_p_names
    prior$theta_mu_var <- prior$theta_mu_var[sampled_p_names]
  } else {
    if(nrow(prior$theta_mu_var) != length(sampled_p_names))
      stop("prior theta should have same number of rows as estimated parameters (p_vector)")
    if(ncol(prior$theta_mu_var) != length(sampled_p_names))
      stop("prior theta should have same number of columns as estimated parameters (p_vector)")
    # Make sure theta_mu_var has same order as sampled parameters
    dimnames(prior$theta_mu_var) <- list(pnams,pnams)
    prior$theta_mu_var <- prior$theta_mu_var[sampled_p_names,sampled_p_names]
  }
  return(prior)
}

merge_priors <- function(prior_list){
  out <- prior_list[[1]]
  if(length(prior_list) > 1){
    out_names <- names(out)
    for(j in 2:length(prior_list)){
      for(hyper in out_names){
        if(length(out[[hyper]]) > 1){
          if(length(dim(out[[hyper]])) == 2){
            out[[hyper]] <- adiag(out[[hyper]], prior_list[[j]][[hyper]])
          } else{
            out[[hyper]] <- c(out[[hyper]], prior_list[[j]][[hyper]])
          }
        }
      }
    }
  }
  return(out)
}

#' Prior Specification Information
#'
#' Prints information associated with the prior for certain 'type'
#'
#' @param type A character string indicating which 'type' of model to run (e.g. 'standard' or 'single')
#'
#' @return Invisible return with a list of all the information that is also printed
#' @export
#'
#' @examples
#' prior_help('diagonal')
prior_help <- function(type){
  prior <- get_objects(type, return_prior = TRUE, return_info = TRUE)
  # Loop through each type
  for (type in names(prior$types)) {
    # Get the type description
    type_desc <- prior$type_descriptions[[type]]

    # Display type and description
    cat(paste("type:", type, "\n"))
    cat(paste(type_desc, "\n\n"))
    cat("  Hyperparameters: \n")
    # Loop through hyperparameters within the type
    for (param in prior$types[[type]]) {
      # Get the hyperparameter description
      param_desc <- prior$descriptions[[param]]

      # Display hyperparameter and description
      cat(paste("  -", param, ":", param_desc, "\n"))
    }
    # New line for separation between types
    cat("\n")
  }
  return(invisible(prior))
}



# Some S3 classes ---------------------------------------------------------


#' Summary method for emc.prior objects
#'
#' Prints a summary of the prior specification, including descriptions of the prior types
#' and their associated hyperparameters.
#'
#' @param object An object of class 'emc.prior' containing prior specifications
#' @param ... Additional arguments passed to other methods (not currently used)
#'
#' @return Invisibly returns NULL. Called for its side effect of printing the summary.
#'
#' @examples
#' # Take a prior object
#' prior <- get_prior(samples_LNR)
#' summary(prior)
#'
#' @seealso \code{\link{prior}} for creating prior objects
#' @export
summary.emc.prior <- function(object, ...){
  type <- attr(object, "type")
  prior_info <- get_objects(type, return_prior = TRUE, return_info = TRUE)
  for(type in names(prior_info$types)){
    cat(paste(type, " - ", prior_info$type_descriptions[[type]], "\n\n"))
    for (param in prior_info$types[[type]]) {
      # Get the hyperparameter description
      param_desc <- prior_info$descriptions[[param]]
      # Display hyperparameter and description
      cat(paste(param_desc, ": \n"))
      tmp <- object[[param]]
      if(is.matrix(tmp)) tmp <- diag(tmp)
      print(object[[param]])
    }
    cat("\n")
  }
}


#' @export
print.emc.prior <- function(x, ...){
  # Check that the required entries exist
  if (!"theta_mu_mean" %in% names(x) || !"theta_mu_var" %in% names(x)) {
    stop("The object must contain 'theta_mu_mean' and 'theta_mu_var' entries.")
  }
  cat("Mean and variance of the prior on the transformed parameters: \n")
  means <- x$theta_mu_mean
  vars  <- x$theta_mu_var
  if(is.matrix(vars)) vars <- diag(vars)
  if (length(means) != length(vars)) {
    stop("Lengths of 'theta_mu_mean' and 'theta_mu_var' must match.")
  }

  # Helper function to determine how many decimals to keep (max 2)
  get_decimal_places <- function(num) {
    # Convert to character, remove trailing zeros and decimal point if needed
    s <- sub("0+$", "", sub("\\.$", "", as.character(num)))
    # Find the decimal point
    point_pos <- regexpr("\\.", s)
    if (point_pos == -1) {
      # No decimal point => 0 decimals
      return(0)
    } else {
      # Number of decimals = total chars after the decimal
      n <- nchar(s) - point_pos
      # Limit to 2
      return(min(n, 2))
    }
  }

  # Helper to format the number using the above rule
  format_number <- function(num) {
    d <- get_decimal_places(num)
    fmt <- paste0("%.", d, "f")
    sprintf(fmt, num)
  }

  # Use names of means as the left-hand side labels
  lhs_names <- names(means)
  if (is.null(lhs_names)) {
    # Fallback: use the index if no names
    lhs_names <- as.character(seq_along(means))
  }
  # Loop through and print each entry in the desired format
  for (i in seq_along(means)) {
    cat(sprintf("%s ~ \U0001D441(%s, %s)\n", lhs_names[i], format_number(means[i]), format_number(vars[i])))
  }

  cat("\nFor detailed info use summary(<prior>)\n")
}


#' Plot a prior
#'
#' Takes a prior object and plots the selected implied prior
#'
#' @param x An `emc_prior` element
#' @param do_plot Boolean. If `FALSE` will only return prior samples and omit plotting.
#' @param covariates dataframe/functions as specified by the design
#' @inheritParams plot_pars
#' @param ... Optional arguments that can be passed to get_pars, histogram, plot.default (see par()),
#' or arguments required for the types of models e.g. n_factors for type = "factor"
#' @return An invisible mcmc.list object with prior samples of the selected type
#' @export
#'
#' @examples \donttest{
#' # First define a design for the model
#' design_DDMaE <- design(data = forstmann,model=DDM,
#'                            formula =list(v~0+S,a~E, t0~1, s~1, Z~1, sv~1, SZ~1),
#'                            constants=c(s=log(1)))
#' # Then set up a prior using make_prior
#' p_vector=c(v_Sleft=-2,v_Sright=2,a=log(1),a_Eneutral=log(1.5),a_Eaccuracy=log(2),
#'           t0=log(.2),Z=qnorm(.5),sv=log(.5),SZ=qnorm(.5))
#' psd <- c(v_Sleft=1,v_Sright=1,a=.3,a_Eneutral=.3,a_Eaccuracy=.3,
#'           t0=.4,Z=1,sv=.4,SZ=1)
#' # Here we left the variance prior at default
#' prior_DDMaE <- prior(design_DDMaE,mu_mean=p_vector,mu_sd=psd)
#' # Now we can plot all sorts of (implied) priors
#' plot(prior_DDMaE, selection = "mu", N = 1e3)
#' plot(prior_DDMaE, selection = "mu", mapped = FALSE, N=1e3)
#' # We can also plot the implied prior on the participant level effects.
#' plot(prior_DDMaE, selection = "alpha", col = "green", N = 1e3)
#' }

plot.emc.prior <- function(x, selection = "mu", do_plot = TRUE, covariates = NULL,
                       layout = NA, N = 5e4, ...){
  prior <- x
  design <- get_design(prior)
  dots <- add_defaults(list(...), breaks = 30, cut_off = 0.0015, prob = TRUE, by_subject = TRUE, map = TRUE)
  oldpar <- par(no.readonly = TRUE) # code line i
  on.exit(par(oldpar)) # code line i + 1
  if(is.null(design[[1]]$Ffactors)){
    if(selection %in% c('alpha', 'mu') & dots$map){
      warning("For this type of design, map = TRUE is not yet implemented")
    }
    dots$map <- FALSE
  }
  type <- attr(prior, "type")
  if(type == "single") selection <- "alpha"
  samples <-  get_objects(design = design, prior = prior, type = type, sample_prior = T,
                          selection = selection, N = N, ...)
  MCMC_samples <- do.call(get_pars, c(list(samples, selection = selection, type = type, covariates = covariates, stage = "sample"), fix_dots(dots, get_pars)))
  if(do_plot){
    for(i in 1:length(MCMC_samples)){
      xlab <- ifelse(is.null(names(MCMC_samples)[i]), selection, names(MCMC_samples)[i])
      if(any(is.na(layout))){
        par(mfrow = coda_setmfrow(Nchains = length(MCMC_samples[[1]]),
                                  Nparms = ncol(MCMC_samples[[1]][[1]]), nplots = 1))
      } else{
        par(mfrow=layout)
      }
      for(j in 1:ncol(MCMC_samples[[i]][[1]])){
        do.call(robust_hist, c(list(MCMC_samples[[i]][[1]][,j], dots$breaks, dots$cut_off, dots$prob),
                               fix_dots_plot(add_defaults(dots, ylab = "Density", xlab = xlab,
                                                          main = colnames(MCMC_samples[[i]][[1]])[j],
                                                          cex.lab = 1.25, cex.main = 1.5))))
      }
    }
  }
  return(invisible(MCMC_samples))
}

#' @rdname parameters
#' @export
#' @examples
#' # For prior inference:
#' # First set up a prior
#' design_DDMaE <- design(data = forstmann,model=DDM,
#'                        formula =list(v~0+S,a~E, t0~1, s~1, Z~1, sv~1, SZ~1),
#'                        constants=c(s=log(1)))
#' # Then set up a prior using make_prior
#' p_vector=c(v_Sleft=-2,v_Sright=2,a=log(1),a_Eneutral=log(1.5),a_Eaccuracy=log(2),
#'            t0=log(.2),Z=qnorm(.5),sv=log(.5),SZ=qnorm(.5))
#' psd <- c(v_Sleft=1,v_Sright=1,a=.3,a_Eneutral=.3,a_Eaccuracy=.3,
#'          t0=.4,Z=1,sv=.4,SZ=1)
#' # Here we left the variance prior at default
#' prior_DDMaE <- prior(design_DDMaE,mu_mean=p_vector,mu_sd=psd)
#' # Get our prior samples
#' parameters(prior_DDMaE, N = 100)
parameters.emc.prior <- function(x,selection = "mu", N = 1000, covariates = NULL, ...){
  prior <- x
  dots <- add_defaults(list(...), by_subject = TRUE, map = FALSE, return_mcmc = FALSE, merge_chains = TRUE)
  type <- attr(prior, "type")
  if(type == "single") selection <- "alpha"
  samples <-  get_objects(get_design(prior), prior = prior, type = type, sample_prior = T,
                          selection = selection, N = N, ...)
  samples <-  do.call(get_pars, c(list(samples, selection = selection,
                                       type = type, covariates = covariates), fix_dots(dots, get_pars)))
  if(selection == "alpha") samples <- samples[,1,]
  return(as.data.frame(t(samples)))
}

#' @rdname credint
#' @param N An integer. Number of samples to use for the quantile calculation (only for prior.emc objects)
#' @param covariates A list of covariates to use for the quantile calculation (only for prior.emc objects)
#' @export
credint.emc.prior <- function(x, selection="mu", probs = c(0.025, .5, .975),
                        digits = 3, N = 1000, covariates = NULL, ...){
  prior <- x
  dots <- add_defaults(list(...), by_subject = TRUE, map = TRUE)
  type <- attr(prior, "type")
  if(type == "single") selection <- "alpha"
  samples <-  do.call(get_objects, c(list(design = get_design(prior), type = type, sample_prior = T,
                          selection = selection, prior = prior, N = N), fix_dots(dots, get_objects)))
  out <- do.call(get_summary_stat, c(list(samples, selection, get_posterior_quantiles,
                          probs = probs, digits = digits, type = type), fix_dots(dots, get_summary_stat)))
  return(out)
}

#' @param data A data frame needed to exactly match the original design
#' @param n_trials An integer. If `data` isn't provided (although preferred),
#' can generate data based on `n_trials` per cell of `design`
#' @rdname predict.emc
#' @export
predict.emc.prior <- function(object,data = NULL,n_post=50,n_cores=1,
                               n_trials = NULL, ...)
{
  if(is.data.frame(data)) data <- list(data)
  prior <- object
  design <- get_design(prior)
  dots <- add_defaults(list(...), selection = "alpha")
  post_out <- vector('list', length(design))
  for(k in 1:length(design)){
    subjects <- design[[k]]$Ffactors$subjects
    n_subjects <- length(subjects)
    dots$merge_chains <- TRUE; dots$by_subject <- TRUE
    samps <- do.call(parameters, c(list(object, N = n_subjects*n_post), dots))
    simDat <- suppressWarnings(mclapply(1:n_post,function(i){
      do.call(make_data, c(list(samps[(1+((i-1)*n_subjects)):(i*n_subjects),],design = design[[k]], data = data[[k]], n_trials = n_trials, check_bounds = TRUE), fix_dots(dots, make_data)))
    },mc.cores=n_cores))
    # These were returned as FALSE since their parameter values fell outside of model bounds
    in_bounds <- !sapply(simDat, is.logical)
    if(all(!in_bounds)) stop("All prior samples fall outside of model bounds, choose a more informative prior")
    if(sum(!in_bounds) > (length(in_bounds)/2)) warning("Most prior samples fall outside of model bounds, choose a more informative prior")
    if(any(!in_bounds)){
      good_post <- sample(1:n_post, sum(!in_bounds))
      simDat[!in_bounds] <- suppressWarnings(mclapply(good_post,function(i){
        do.call(make_data, c(list(samps[(1+((i-1)*n_subjects)):(i*n_subjects),],design = design[[k]], data = data[[k]], n_trials = n_trials), fix_dots(dots, make_data)))
      },mc.cores=n_cores))
    }
    out <- cbind(postn=rep(1:n_post,times=unlist(lapply(simDat,function(x)dim(x)[1]))),do.call(rbind,simDat))
    attr(out,"pars") <- samps
    post_out[[k]] <- out
  }
  if(length(post_out) == 1) post_out <- post_out[[1]]
  return(post_out)
}

#' @rdname get_design
#' @export
get_design.emc.prior <- function(x)
{
  design <- attr(x, "design")
  class(design) <- "emc.design"
  if(is.null(design)){
    stop("You are likely using samples ran with an old version of EMC2,
         unfortunately this function only works with samples from a newer version of EMC2")
  }
  return(design)
}

#' @rdname plot_design
#' @export
plot_design.emc.prior <- function(x, data = NULL, factors = NULL, plot_factor = NULL, n_data_sim = 10,
                                  p_vector = NULL, functions = NULL, ...){
  if(is.null(p_vector)) p_vector <- x$theta_mu_mean
  design <- get_design(x)
  plot(design, p_vector, data = data, factors = factors, plot_factor = plot_factor, n_data_sim = n_data_sim,
       functions = functions, ...)
}

#' @rdname mapped_pars
#' @export
mapped_pars.emc.prior <- function(x, p_vector = NULL, model = NULL, digits=3,remove_subjects=TRUE,
                                   covariates=NULL,...){
  if(is.null(p_vector)) p_vector <- x$theta_mu_mean
  design <- get_design(x)
  mapped_pars(design, p_vector, digits = digits, remove_subjects=remove_subjects,
              covariates=covariates,...)
}

#' @rdname sampled_pars
#' @export
sampled_pars.emc.prior <- function(x,group_design=NULL,doMap=FALSE, add_da = FALSE, all_cells_dm = FALSE, data=NULL){
  return(sampled_pars(get_design(x), group_design = group_design, doMap = doMap,
                          add_da = add_da, all_cells_dm = all_cells_dm, data = data))
}


