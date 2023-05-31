# Plot global re-scale code
# Author: https://www.zachburchill.ml/constant_scales/

library(purrr)

simple_range_extracter <- function(p, scale) {
  d <- ggplot_build(p)
  return (d$plot$scales$get_scales(scale$aesthetics)$range$range)
}

get_shared_scale <- function(..., scale) {
  plots        <- list(...)
  ranges       <- purrr::map(plots, ~simple_range_extracter(., scale))
  single_range <- range(unlist(ranges))
  scale$limits <- single_range
  return (scale)
}

# Main function
set_scale_union <- function(..., scale) {
  exprs <- rlang::enexprs(...)
  scale <- get_shared_scale(..., scale = scale)
  var_nms <- purrr::map_chr(exprs, rlang::as_name)
  edit_plots_in_place(var_nms, env = parent.frame(),
                      scale = scale)
  # Invisibly return the scale, in case you need it later
  invisible(scale)
}

# Sub-function
edit_plots_in_place <- function(names, env, scale) {
  vars <- rlang::env_has(env = env, nms = names)
  if (!all(vars))
    stop("Environment does not have variables for ",
         paste(names(vars[!vars]), collapse=", "))
  
  purrr:::walk(names, function(nm) {
    og_plot <- rlang::env_get(env, nm = nm)
    message("Changing plot `", nm, "`")
    # Muffles messages about already having scales
    withCallingHandlers(
      assign(x = nm, envir = env,
             value = og_plot + scale),
      message = function(err) {
        if (grepl("already present", err$message))
          invokeRestart("muffleMessage")
      })
  })
}

