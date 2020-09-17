#' curve_cs
#'
#' @description Plot the specification curve
#'
#' \lifecycle{experimental}
#'
#' @param data a data frame with the results of a multiverse analyses
#' @param choices a vector specifying which analytical choices should be plotted
#' @param labels labels for the two parts of the plot
#' @param rel_heights vector indicating the relative heights of the plot
#' @param ci logical value indicating whether confidence intervals should be
#'   plotted.
#' @param ribbon logical value indicating whether a ribbon instead should be
#'   plotted.
#' @param null Indicate what value represents the 'null' hypothesis (defaults to
#'   zero).
#' @param sample_perc numeric value denoting what percentage of the
#'   specifications should be plotted. Needs to be strictly greater than 0 and smaller than 1.
#'   Defaults to 1 (= all specifications). Drawing a sample from all
#'   specification usually makes only sense of the number of specifications is
#'   very large and one wants to simplify the visualization.
#' @return A plot
#' @details This plot is a slight modification of the \code{specr::plot_specs} function. Please check the references and credit the original authors.
#'
#' @references
#' Masur, Philipp K. & Scharkow, M. (2019). specr: Statistical functions for conducting specification curve analyses. Available from https://github.com/masurp/specr.
#' @export

curve_cs <- function(data,
                     choices = c("exclusion", "cut_off"),
                     labels = c("A", "B"),
                     rel_heights = c(2, 3),
                     null = 0,
                     ci = TRUE,
                     ribbon = FALSE,
                     sample_perc = 1){



    if (!is.null(data)) {

      # Plot only t-tests from the NHST framework
      data %>% dplyr::filter(framework == "NHST", model == "t-test") -> data

      if (sample_perc > 1 | sample_perc < 0) {
        stop("`sample_n` must be greater than 0 and less than 1!")
      }

      # Draw sample
      df <- dplyr::sample_n(data, size = sample_perc*nrow(data))

      # Create both plots
      plot_a <- specr::plot_curve(df, ci = ci, ribbon = ribbon, desc = FALSE, null = null)
      plot_b <- specr::plot_choices(df, choices = choices, desc = FALSE, null = null)
    }

    # Combine plots
    cowplot::plot_grid(plot_a,
                       plot_b,
                       labels = labels,
                       align = "v",
                       axis = "rbl",
                       rel_heights = rel_heights,
                       ncol = 1)

  }
