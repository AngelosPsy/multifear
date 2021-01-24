#' forestplot_mf
#'
#' \lifecycle{experimental}
#'
#' @description Basic function for forest plot
#' @param data a universe_mf or multiverse_mf object
#' @param ci should confidence intervals be included -- default to TRUE
#' @param ... any additional argument
#' @details This is a wrapper around the \code{forestplot::forestplot} function.
#' The function only uses the ANOVAs and the t-tests. For the t-tests though
#' it includes only the two-sided, as they are the same with the one-sided ones
#' and having both would probably give a false picture of the effect.
#' @return A plot
#'#'
#' @importFrom dplyr %>%
#' @export

forestplot_mf <-
  function(data, ci = TRUE, ...) {
    data %>%
      dplyr::filter(framework == "NHST") -> data

    data %>% dplyr::mutate(
      method2 =
        dplyr::case_when(
          exclusion == "full_data" ~ "full data",
          exclusion == "full data" ~ "full data",
          exclusion == "ten_per" ~ "trials divited by 10%",
          exclusion == "min_first" ~ "remove 1st trial",
          exclusion == "th3_per" ~ "trials divided by 33%",
          exclusion == "halves" ~ "half of the trials",
          exclusion == "fltrials" ~ "first vs. last trial",
          exclusion == "twenty_per" ~ "trials divided by 20%",
          exclusion == "fl2trials" ~ "first 2 vs last 2 trials",
          exclusion == "per2trials" ~ "per 2 trials",
        )
    ) %>%
      dplyr::arrange(model, method) %>%
      dplyr::filter(!method %in% c("greater", "less")) -> data

      lci <- data$effect.size.ma.lci
      hci <- data$effect.size.ma.hci

      if(ci == FALSE){
        lci <- data$effect.size.ma - 0.0001 # Zero did not work
        hci <- data$effect.size.ma + 0.0001
      }

      data$method[which(data$method == "two.sided")] <- "t-test"

      forestplot::forestplot(
        labeltext = paste(data$method, rep("| data used:", nrow(data), sep = ""),  data$method2),
        mean = data$effect.size.ma,
        lower = lci,
        upper = hci,
        ...
      )

}
