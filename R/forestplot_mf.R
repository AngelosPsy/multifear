#' forestplot_mf
#'
#' \lifecycle{experimental}
#'
#' @description Basic function for forest plot
#' @param data a universe_mf or multiverse_mf object
#' @param ... any additional argument
#' @details This is a wrapper around the \code{forestplot::forestplot} function.
#' @return A plot
#'#'
#' @importFrom dplyr %>%
#' @export

forestplot_mf <-
  function(data, ...) {
    data %>%
      dplyr::filter(framework == "NHST") -> data

    data %>% dplyr::mutate(method2 =
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
                    )) -> data

      forestplot::forestplot(
        labeltext = paste(data$method, rep("| data used:", nrow(data), sep = ""),  data$method2),
        mean = data$effect.size.ma,
        lower = data$effect.size.ma.lci,
        upper = data$effect.size.ma.hci,
        grid = TRUE,
        ...
      )

}
