#' inference_cs
#'
#' \lifecycle{experimental}
#'
#' @description Function for summarizing the multiverse results.
#' @param data a data frame with the results of a multiverse analyses.
#' @param alpha_level What should be the alpha level used (default to 0.05).
#' @param add_line Whether to add a line with the alpha level in the produced histogram (default to \code{TRUE})
#' @param na.rm Should NA's be removed (default to \code{FALSE}). See details.
#' @param framework Inference framework. Values could be "NHST", "Bayesian", or "Both" (no case sensitivity).
#' @return A data frame with results together with a histogram summarizing the results.
  #' @details For now the function returns mean p values and proportion of p values below a criterion defined by the \code{alpha_level} parameter (default to 0.05) as well as mean Bayes factors (please see the `framework` arguument). The user may choose to drop the NAs for the summary statistic. However, for the plot the NAs in the \code{p.value} column are removed automatically -- so what \code{ggplot2} does automatically but here no message is returned.
#'
#' @export

inference_cs <-
  function(data,
           alpha_level = 0.05,
           add_line = TRUE,
           na.rm = FALSE,
           framework = "Both") {

    # Check data and arguments
    inference_warning(data = data)

    framework <- tolower(framework)
    match.arg(framework, c("nhst", "bayesian", "both"))


    if (framework %in% c("nhst", "both")){
    dataNHST <-
      data %>% dplyr::filter(framework == "NHST")

    mean_p_value <-
      dataNHST %>% dplyr::select(p.value) %>% unlist() %>% mean(na.rm = na.rm)

    tmp_p.value <- dataNHST$p.value

    if(na.rm){tmp_p.value <- na.omit(tmp_p.value)}

    prop_p_value <-
      length(which(tmp_p.value < alpha_level)) / length(tmp_p.value) * 100
    }

    if (framework %in% c("bayesian", "both")){
      dataBayes <-
        data %>% dplyr::filter(framework == "Bayesian") %>% dplyr::select(estimate) %>%
        unlist() %>%
        mean(na.rm = na.rm)
    }

    # Generate output
    if (framework %in% c("nhst")) {
      res_tmp <-
        data.frame(mean_p_value = mean_p_value, prop_p_value = prop_p_value)
    } else if (framework %in% c("bayesian")) {
      res_tmp <- data.frame(mean_bf = dataBayes)
    } else if (framework %in% c("both")) {
      res_tmp <-
        data.frame(mean_p_value = mean_p_value,
                   prop_p_value = prop_p_value,
                   mean_bf = dataBayes)
    }

    # Histogram
    p1 <- data %>%
      tidyr::drop_na(p.value) %>%
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

    res <- res_tmp

    return(res)
  }
