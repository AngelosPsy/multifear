#' exclusion_criteria
#'
#'
#' @description Exclusion criteria
#' @param data a data object generated by the \code{multics::chop_css} function
#' @param cutoff cut off score
#' @return A tibble with a brace of smaller tibbles, with each sub-tibble including a data frame after each one of the exclusion criteria -- mentioned in the \code{multifear::chop_css} function -- is applied.
#' @details Here the different exclusion criteria are applied to the provided data.
#' @export

exclusion_criteria <- function(data, cutoff = 0) {
  # Check data
  chop_css_warning(data)

  if (is.null(data$group)) {
    group_new <-
      data %>%
      dplyr::mutate(group = rep("NULL", nrow(data))) %>%
      dplyr::select(group)
  } else{
    group_new <- data %>%
      dplyr::select(group) %>%
      tibble::as_tibble()
  }

  # Exclude participants with no last 2 trials
  data <-
    data %>% dplyr::mutate(
      diff_2_trials = cs1_l2trial - cs2_l2trial,
      diff_4_trials = cs1_l2trial - cs2_l2trial,
      diff_5_trials = cs1_l5trial - cs2_l5trial,
      group = group
    )

  two_thre  <- -99999999
  four_thre <- -99999999
  five_thre <- -99999999

  data_details <- "full_data"

  if (cutoff == 1){two_thre  <- 0; data_details <- "last_2_trials"}
  if (cutoff == 2){four_thre <- 0; data_details <- "last_4_trials"}
  if (cutoff == 3){five_thre <- 0; data_details <- "last_5_trials"}

  # Summarize full data
  full_data  <- data %>%
    dplyr::filter(diff_2_trials >= two_thre, diff_4_trials >= four_thre, diff_5_trials >= five_thre) %>%
    dplyr::select(dplyr::matches("cs1_t_|cs2_t_"), id, group)
  ten_per    <- data  %>%
    dplyr::filter(diff_2_trials >= two_thre, diff_4_trials >= four_thre, diff_5_trials >= five_thre) %>%
    dplyr::select(cs1_t10per, cs1_b10per, cs2_t10per, cs2_b10per, id, group)
  min_first  <- data %>%
    dplyr::filter(diff_2_trials >= two_thre, diff_4_trials >= four_thre, diff_5_trials >= five_thre) %>%
    #dplyr::select(cs1_minfirst, cs2_minfirst, id, group)
    dplyr::select(dplyr::contains("minfirst"), id, group)
  th3_per    <- data %>%
    dplyr::filter(diff_2_trials >= two_thre, diff_4_trials >= four_thre, diff_5_trials >= five_thre) %>%
    dplyr::select(dplyr::contains("33per"), id, group)
  halves     <- data %>%
    dplyr::filter(diff_2_trials >= two_thre, diff_4_trials >= four_thre, diff_5_trials >= five_thre) %>%
    dplyr::select(cs1_fhalf, cs1_lhalf, cs2_fhalf, cs2_lhalf, id, group)
  fltrials   <- data %>%
    dplyr::filter(diff_2_trials >= two_thre, diff_4_trials >= four_thre, diff_5_trials >= five_thre) %>%
    dplyr::select(cs1_ftrial, cs1_ltrial, cs2_ftrial, cs2_ltrial, id, group)
  twenty_per <- data %>%
    dplyr::filter(diff_2_trials >= two_thre, diff_4_trials >= four_thre, diff_5_trials >= five_thre) %>%
    dplyr::select(dplyr::contains("20per"), id, group)
  fl2trials  <- data %>%
    dplyr::filter(diff_2_trials >= two_thre, diff_4_trials >= four_thre, diff_5_trials >= five_thre) %>%
    dplyr::select(cs1_f2trial, cs1_l2trial, cs2_f2trial, cs2_l2trial, id, group)
  per2trials <- data %>%
    dplyr::filter(diff_2_trials >= two_thre, diff_4_trials >= four_thre, diff_5_trials >= five_thre) %>%
    dplyr::select(dplyr::matches("cs1_per2|cs2_per2"), id, group)

 res <- tidyr::tibble(
    used_data = list(
      full_data   = full_data,
      ten_per     = ten_per,
      min_first   = min_first,
      th3_per     = th3_per,
      halves      = halves,
      fltrials    = fltrials,
      twenty_per  = twenty_per,
      fl2trials   = fl2trials,
      per2trials  = per2trials#,
      )
      )  %>%
   dplyr::mutate(names = names(.$used_data), cutoff = data_details)

 return(res)
}
