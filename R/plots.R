require(latex2exp)
require(ggplot2)

#' Plot SZR curves
#'
#' @param data A data frame in the same format as the `results` dataframe returned by `generateSZRdata()`
#'
#' @return A plot showing the changes in the S, Z and R populations over time
#' @export
plot_SZR <- function(data){
 legend.colors <- c("Susceptible" = "cadetblue1", "Removed" = "steelblue2", "Zombies" = "yellow")
 p <- ggplot2::ggplot(data, ggplot2::aes(x = t)) +
   ggplot2::geom_line(ggplot2::aes(y  = S.t, color = "Susceptible")) +
   ggplot2::geom_line(ggplot2::aes(y  = Z.t, color = "Zombies")) +
   ggplot2::geom_line(ggplot2::aes(y  = R.t, color = "Removed")) +
   ggplot2::labs(color = "Legend") +
   ggplot2::scale_color_manual(values = legend.colors) +
   ggplot2::xlab("Time") +
   ggplot2::ylab("SZR Populations")
 
 return(p)
}


#' Plots the density for each of the model parameters' ABC accepted samples with a black line indicating the true values
#'
#' @param sample A data frame with the accepted parameter values as returned by `abcRej`
#' @param true_vals The true values of the parameters used to generate the data 
#'
#' @return A list with 3 plots for each of the parameters in the SZR model 
#' @export
plot_posterior_samples <- function(sample, true_vals){
  beta_plot <- ggplot2::ggplot(data = sample, ggplot2::aes(x = beta)) + 
    ggplot2::geom_density(colour = "steelblue2", fill = "steelblue2", alpha = 0.5) +
    ggplot2::geom_vline(aes(xintercept = true_vals[1]))+
    ggplot2::labs(x = latex2exp::TeX("$\\hat{\\beta}$"), y = "Accepted Samples") +
    ggplot2::theme_classic()
  kappa_plot <- ggplot2::ggplot(data = sample, ggplot2::aes(x = kappa)) + 
    ggplot2::geom_density(fill = "steelblue2", alpha = 0.5,colour = "steelblue2") +
    ggplot2::geom_vline(aes(xintercept = true_vals[2])) +
    ggplot2::labs(x = latex2exp::TeX("$\\hat{\\kappa}$"), y = "Accepted Samples") +
    ggplot2::theme_classic()
  rho_plot <- ggplot2::ggplot(data = sample, ggplot2::aes(x = rho)) + 
    ggplot2::geom_density(colour = "steelblue2", fill = "steelblue2", alpha = 0.5) +
    geom_vline(aes(xintercept = true_vals[3]))+
    ggplot2::labs(x = latex2exp::TeX("$\\hat{\\rho}$"), y = "Accepted Samples") +
    ggplot2::theme_classic()
  return(list(beta_plot, kappa_plot, rho_plot))
}
