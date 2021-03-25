#' forestplot_mf
#'
#'
#' @description Basic function for forest plot
#' @param data a universe_mf or multiverse_mf object
#' @param ci should confidence intervals be included -- default to TRUE
#' @param include_label_text Whether the labels for each effect should be include. Default to \code{TRUE}
#' @param ... any additional argument
#' @details This is a wrapper around the \code{forestplot::forestplot} function.
#' The function only uses the ANOVAs and the t-tests. For the t-tests though
#' it includes only the two-sided, as they are the same with the one-sided ones
#' and having both would probably give a false picture of the effect.
#'
#' The include_label_text can be used when multiple plots need to be combined
#' side by side, so one of them does not include the labels.
#' @return A plot
#'#'
#' @importFrom dplyr %>%
#' @export

forestplot_mf <-
  function(data, ci = TRUE, include_label_text = TRUE, ...) {
    data %>%
      dplyr::filter(framework == "NHST") -> data

    data %>%
      dplyr::mutate(
      method2 =
        dplyr::case_when(
          exclusion == "full_data" & model == "rep ANOVA" ~ "single trial data",
          exclusion == "full data" & model == "rep ANOVA" ~ "single trial data",
          exclusion == "full_data" & model == "t-test" ~ "average across block",
          exclusion == "full data" & model == "t-test" ~ "average across block",
          exclusion == "ten_per" ~ "averages of 10% blocks",
          exclusion == "twenty_per" ~ "averages of 20% blocks",
          exclusion == "th3_per" ~ "averages of 33% blocks",
          exclusion == "halves" ~ "averages of 50% blocks",
          exclusion == "fltrials" ~ "first vs. last trial",
          exclusion == "min_first" ~ "average across trials after removing 1st trial",
          exclusion == "fl2trials" ~ "averages of first 2 vs last 2 trials",
          exclusion == "per2trials" ~ "averages per 2 trials",
        )) %>%
        dplyr::mutate(
          method_order =
            dplyr::case_when(
              method2 == "single trial data" ~ 1,
              method2 == "average across block" ~ 1,
              method2 == "first vs. last trial" ~ 2,
              method2 == "averages of first 2 vs last 2 trials" ~ 3,
              method2 == "averages per 2 trials" ~ 4,
              method2 == "averages of 10% blocks" ~ 5,
              method2 == "averages of 20% blocks" ~ 6,
              method2 == "averages of 33% blocks" ~ 7,
              method2 == "averages of 50% blocks" ~ 8,
              method2 == "average across trials after removing 1st trial" ~ 9)) %>%
      dplyr::arrange(method, method_order) %>%
      dplyr::filter(!method %in% c("greater", "less")) -> data

      lci <- data$effect.size.ma.lci
      hci <- data$effect.size.ma.hci

      if(ci == FALSE){
        lci <- data$effect.size.ma - 0.0001 # Zero did not work
        hci <- data$effect.size.ma + 0.0001
      }

      data$method[which(data$method == "two.sided")] <- "t-test"

      if (include_label_text) {
        labeltext <-
          paste(data$method, rep("| data used:", nrow(data), sep = ""),  data$method2)
      } else{
        labeltext <- rep(" ", length(data$effect.size.ma))
      }

      forestplot::forestplot(
        labeltext =  labeltext,
        boxsize = .25,
        mean = data$effect.size.ma,
        lower = lci,
        upper = hci,
        ...
      )

}
