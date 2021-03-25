#' inference_cs
#'
#'
#' @description Function for summarizing the multiverse results.
#' @param data a data frame with the results of a multiverse analyses.
#' @param alpha_level What should be the alpha level used (default to 0.05).
#' @param na.rm Should NA's be removed (default to \code{FALSE}). See details.
#' @param framework Inference framework. Values could be "NHST", "Bayesian", or "Both" (no case sensitivity)
#' @return A data frame with summaries of the results.
  #' @details For now the function returns mean, median, standard deviations of p values and proportion of p values below a criterion defined by the \code{alpha_level} parameter (default to 0.05) as well as mean Bayes factors (please see the `framework` argument). The user may choose to drop the NAs for the summary statistic.
#'
#' @export

inference_cs <-
  function(data,
           alpha_level = 0.05,
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

    sd_p_value <-
      dataNHST %>% dplyr::select(p.value) %>% unlist() %>% stats::sd(na.rm = na.rm)

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

      sd_bf_value <-
        data %>% dplyr::filter(framework == "Bayesian") %>%
        dplyr::select(estimate) %>%
        unlist() %>%
        stats::sd(na.rm = na.rm)

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
        data.frame(mean_p_value = mean_p_value, median_p_value  = median_p_value,
                   sd_p_value = sd_p_value, prop_p_value = prop_p_value)
    } else if (framework %in% c("bayesian")) {
      res_tmp <-
        data.frame(mean_bf_value   = mean_bf_value,
                   median_bf_value = median_bf_value,
                   sd_bf_value     = sd_bf_value,
                   prop_bf_value   = prop_bf_value)

    } else if (framework %in% c("both")) {

      res_tmp <-
        data.frame(mean_p_value    = mean_p_value,
                   median_p_value  = median_p_value,
                   sd_p_value      = sd_p_value,
                   prop_p_value    = prop_p_value,
                   mean_bf_value   = mean_bf_value,
                   median_bf_value = median_bf_value,
                   sd_bf_value = sd_bf_value,
                   prop_bf_value   = prop_bf_value)
    }

    res <- res_tmp
    res
  }
