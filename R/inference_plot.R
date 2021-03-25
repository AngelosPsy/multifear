#' inference_plot
#'
#'
#' @description Function for plotting the multiverse results.
#' @param data a data frame with the results of a multiverse analysis
#' @param alpha_level What should be the alpha level used (default to 0.05)
#' @param add_line Whether to add a line with the alpha level in the produced histogram (default to \code{TRUE})
#' @param na.rm Should NA's be removed (default to \code{FALSE}). See details for more information
#' @param framework Inference framework. Values could be "NHST", "Bayesian", or "Both" (no case sensitivity)
#' @param col A length three vector with the colors to be used for ANOVAS, t-tests, and mixed models (in this order)
#' @param return_plot Whether to return a plot or not (default too TRUE)
#' @return A histogram summarizing the results.
#' @details For the plot the NAs in the \code{p.value} column are removed automatically -- so what \code{ggplot2} does automatically but here no message is returned.
#'
#' The \code{return_plot} argument is there in case you want to combine multiple panels and you do
#' not want to have a plot returned every time you run the code.
#'
#' @export

inference_plot <-
  function(data,
           alpha_level = 0.05,
           add_line = TRUE,
           na.rm = FALSE,
           framework = "Both",
           col = c("gray45", "maroon4", "brown1"),
           return_plot = TRUE) {

    # Check data and arguments
    inference_warning(data = data)

    framework <- tolower(framework)
    match.arg(framework, c("nhst", "bayesian", "both"))

    # Rename factors
    data$model[which(data$model == "rep ANOVA")] <- " ANOVA\n(rep measures)"
    data$model[which(data$model == "t-test")] <- " t-test"
    data$model[which(data$model == "mixed_model")] <- "mixed model"
    data$model[which(data$model == "rep BANOVA")] <- "Bayesian ANOVA\n(rep measures)"
    data$model[which(data$model == "Bayesian t-test")] <- "Bayesian t-test"

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

    # Histogram
    p1 <- data %>%
      tidyr::drop_na(p.value) %>%
      ggplot2::ggplot(ggplot2::aes(x = p.value, fill = model)) +
      ggplot2::geom_histogram(bins = 30) +
      ggplot2::xlab("p value") +
      ggplot2::theme_minimal() +
      ggplot2::scale_color_manual(values = col) +
      ggplot2::scale_fill_manual(values = col) +
      ggplot2::annotate(
        "text",
        x = Inf,
        y = Inf,
        label = paste0("Mean ", r_number(mean_p_value), #round(mean_p_value, 3),
                       "\nMedian ", r_number(median_p_value),
                       "\nSD ", r_number(sd_p_value),
                       "\nProp = ", round(prop_p_value, 2), "%"),
        vjust = 1,
        hjust = 1
      )

    if (framework %in% c("bayesian", "both")){

     p2 <- data %>%
        data.frame() %>%
        dplyr::filter(framework == "Bayesian") %>%
        ggplot2::ggplot(ggplot2::aes(x = estimate, fill = model)) +
        ggplot2::geom_histogram(bins = 50) +
        ggplot2::xlab("Bayes factor") +
        ggplot2::theme_minimal() +
        ggplot2::scale_color_manual(values = col) +
        ggplot2::scale_fill_manual(values = col) +
        ggplot2::annotate(
          "text",
          x = Inf,
          y = Inf,
          label = paste0("Mean ", r_number(mean_bf_value),
                         "\nMedian ", r_number(median_bf_value),
                         "\nSD ", r_number(sd_bf_value),
                         "\nProp ", round(prop_bf_value, 3), "%"),
          vjust = 1,
          hjust = 1
        )

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

    if(!return_plot){
      res_tmp_plot <- invisible(res_tmp_plot)
    }

    res_tmp_plot
  }
