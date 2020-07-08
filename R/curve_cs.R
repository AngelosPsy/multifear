#' curve_cs
#'
#' @description Plot the specification curve
#'
#' \lifecycle{experimental}
#'
#' @param data a data frame with the results of a multiverse analyses.
#'
#' @return A plot
#' @details This plot is a slight modification of the \code{specr::plot_decisiontree}. Please check the references.
#'
#' @references
#' Masur, Philipp K. & Scharkow, M. (2019). specr: Statistical functions for conducting specification curve analyses. Available from https://github.com/masurp/specr.
#' @export

curve_cs <- function(data){

  null = 0
  # Create basic plot
  plot <- data %>%
    format_results(desc = desc, null = null) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$exclusion,
               y = .data$estimate,
               ymin = .data$conf.low,
               ymax = .data$conf.high,
               color = .data$color)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$color),
               size = 1) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_identity() +
    ggplot2::theme(strip.text = ggplot2::element_blank(),
          axis.line = ggplot2::element_line("black", size = .5),
          legend.position = "none",
          panel.spacing = ggplot2::unit(.75, "lines"),
          axis.text = ggplot2::element_text(colour = "black")) +
    ggplot2::labs(x = "")

  # add legends if necessary
  #if (isFALSE(legend)) {
    plot <- plot +
      ggplot2:: theme(legend.position = "none")
  #}

  # add CIs if necessary
  #if (isTRUE(ci)) {
    plot <- plot +
      ggplot2::geom_pointrange(alpha = 0.5,
                      size = .6,
                      fatten = 1)
 # }

  # add ribbon if necessary
  #if (isTRUE(ribbon)) {
    plot <- plot +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$conf.low,
                      ymax = .data$conf.high,
                      color = "lightgrey"),
                  alpha = 0.25)
  #}
  return(plot)
}
