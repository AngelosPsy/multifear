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

    median_p_value <-
      dataNHST %>% dplyr::select(p.value) %>% unlist() %>% stats::median(na.rm = na.rm)

    tmp_p.value <- dataNHST$p.value

    if(na.rm){tmp_p.value <- na.omit(tmp_p.value)}

    prop_p_value <-
      length(which(tmp_p.value < alpha_level)) / length(tmp_p.value) * 100
    }

    if (framework %in% c("bayesian", "both")){
      mean_bf_value <-
        data %>% dplyr::filter(framework == "Bayesian") %>%
        dplyr::select(estimate) %>%
        unlist() %>%
        mean(na.rm = na.rm)

      median_bf_value <-
        data %>% dplyr::filter(framework == "Bayesian") %>%
        dplyr::select(estimate) %>%
        unlist() %>%
        stats::median(na.rm = na.rm)

      dataBayes <-
          data %>% dplyr::filter(framework == "Bayesian")

      tmp_bf.value <- dataBayes$estimate

      if(na.rm){tmp_bf.value <- na.omit(tmp_bf.value)}

      prop_bf_value <-
        length(which(tmp_bf.value > 1)) / length(tmp_bf.value) * 100
    }

    # Generate output
    if (framework %in% c("nhst")) {
      res_tmp <-
        data.frame(mean_p_value = mean_p_value, prop_p_value = prop_p_value)
    } else if (framework %in% c("bayesian")) {
      #dataBayes <-
      #  data %>% dplyr::filter(framework == "Bayesian")

      res_tmp <-
        data.frame(mean_bf_value = mean_bf_value,
                   median_bf_value = median_bf_value,
                   prop_bf_value = prop_bf_value)

    } else if (framework %in% c("both")) {

      res_tmp <-
        data.frame(mean_p_value    = mean_p_value,
                   median_p_value  = median_p_value,
                   prop_p_value    = prop_p_value,
                   mean_bf_value   = mean_bf_value,
                   median_bf_value = median_bf_value,
                   prop_bf_value   = prop_bf_value)
    }

    # Histogram
    p1 <- data %>%
      tidyr::drop_na(p.value) %>%
      ggplot2::ggplot(ggplot2::aes(x = p.value)) +
      ggplot2::geom_histogram(bins = 50) +
      ggplot2::xlab("p value") +
      ggplot2::theme_minimal()


    if (framework %in% c("bayesian", "both")){
      p2 <- data %>%
        data.frame() %>%
        dplyr::filter(framework == "Bayesian") %>%
        ggplot2::ggplot(ggplot2::aes(x = estimate)) +
        ggplot2::geom_histogram(bins = 50) +
        ggplot2::xlab("Bayes factor") +
        ggplot2::theme_minimal()

      if (add_line) {
        p2 <- p2 + ggplot2::geom_vline(
          ggplot2::aes(xintercept = 1),
          color = "red",
          linetype = "dashed",
          size = 1
        )
      }
    }

    if (add_line) {
      p1 <- p1 + ggplot2::geom_vline(
        ggplot2::aes(xintercept = alpha_level),
        color = "red",
        linetype = "dashed",
        size = 1
      )
    }

    if (framework %in% c("bayesian", "both")){
       res_tmp_plot <- invisible(gridExtra::grid.arrange(p1, p2, nrow = 1, ncol = 2))
    } else{
      res_tmp_plot <- p1
    }

    res_tmp_plot %>% methods::show()

    return(res_tmp)
  }
