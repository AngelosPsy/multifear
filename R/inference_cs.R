#' inference_cs
#'
#' @description Function for summarizing the multiverse results
#' @param data a data frame with the results of a multiverse analyses
#' @param alpha_level What should be the alpha level used (default to 0.05)
#' @param add_line Whether to add a line with the alpha level in the produced histogram (default to TRUE)
#' @param na.rm Should NAs be removed? Default to FALSE
#' @return A data frame with results
#' @details For now the function returns only mean p values and proportion of p values below a criterion defined by the \code{alpha_level} parameter (default to 0.05).
#' @export

inference_cs <-
  function(data,
           alpha_level = 0.05,
           add_line = TRUE,
           na.rm = FALSE) {
    # Check data
    inference_warning(data = data)
    mean_p_value <-
      data %>% dplyr::select(p.value) %>% unlist() %>% mean()
    prop_p_value <-
      length(which(data$p.value < alpha_level)) / nrow(data) * 100

    res <-
      data.frame(mean_p_value = mean_p_value, prop_p_value = prop_p_value)

    # Histogram
    p1 <- data %>%
      ggplot2::ggplot(ggplot2::aes(x = p.value)) +
      ggplot2::geom_histogram(
        ggplot2::aes(y = ..density..),
        colour = "black",
        fill = "white",
        bins = 50
      ) +
      ggplot2::geom_density(alpha = .2, fill = "red") +
      ggplot2::xlab("p value") +
      ggplot2::theme_minimal()

    if (add_line) {
      p1 <- p1 + ggplot2::geom_vline(
        ggplot2::aes(xintercept = alpha_level),
        color = "red",
        linetype = "dashed",
        size = 1
      )
    }

    p1 %>% methods::show()

    return(res)
  }
