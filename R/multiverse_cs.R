#' multiverse_cs
#'
#' \lifecycle{experimental}
#'
#' @description Basic function for conducting multiverse analyses of conditioning
#' data
#' @inheritParams universe_cs
#' @param cs_paired A character vector with the trials that were paired. Default is set to \code{NULL}, suggesting that there was full reinforcement
#' @param cutoff A numeric vector of the cutoff criteria applied. Default to \code{0, 0.05, .1}
#' @details In case of higher order interaction, only the highest order
#' effect is returned.
#' @return A tibble with the following column names:
#' x: the name of the independent variable (e.g., cs)
#' y: the name of the dependent variable as this defined in the \code{dv} argument
#' exclusion: see \code{exclusion} argument
#' model: the model that was run (e.g., t-test)
#' controls: ignore this column for this test
#' method: the model that was run
#' p.value: irrelevant here
#' effect.size: irrelevant here
#' estimate: the estimate of the test run
#' statistic: the t-value
#' conf.low: the lower confidence interval for the estimate
#' conf.high: the higher confidence interval for the estimate
#' data_used: a list with the data used for the specific test.
#'
#' @export

multiverse_cs <-
  function(cs1,
           cs2,
           data,
           subj,
           group = NULL,
           cs_paired = NULL,
           include_bayes = TRUE,
           cutoff = c(0, 0.1, 0.05),
           phase = "acquisition",
           print_output = TRUE) {

    # Check data
    collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj, cs_paired = cs_paired)

    # Excluded participants
    chop <- multifear::chop_css(cs1 = cs1, cs2 = cs2, data = data, subj = subj)
    excl_data_sets <-
      purrr::map_df(cutoff, ~ multifear::exclusion_criteria(chop, cutoff = .))

    # Remove empty data -- this can happen
    # if you have removed a lot of participants
    excl_data_sets_final <-
      excl_data_sets %>%
      dplyr::mutate(excl = excl_data_sets$used_data %>%
                      lapply(plyr::empty) %>%
                      data.frame() %>% t())

    # Return warning if you have excluded any of the data sets
    if (any(excl_data_sets_final$excl == TRUE)) {
      warning(
        paste0(
          "Part of the analyses were excluded due to many excluded cases. Check
          the following cases: ",
          paste(
            excl_data_sets_final$names[excl_data_sets_final$excl == TRUE],
            "with cutoff =",
            excl_data_sets_final$cutoff[excl_data_sets_final$excl == TRUE]
          ),
          ".\n",
          collapse = ""
        )
        )
    }

    # You exclude the empty cases here
    excl_data_sets_final <- excl_data_sets_final %>%
      dplyr::filter(excl == FALSE)

    res <- purrr::map2_dfr(
      excl_data_sets_final$used_data,
      excl_data_sets_final$names,
      ~multifear::universe_cs(
        cs1 = cs1,
        cs2 = cs2,
        data = .x,
        subj = subj,
        group = group,
        include_bayes = include_bayes,
        exclusion = .y
      )
    ) %>% dplyr::mutate(
      cutoff = rep(
        excl_data_sets_final$cutoff,
        each = nrow(.) / length(excl_data_sets_final$cutoff)
      ),
      name_cutoff = rep(
        excl_data_sets_final$nam_cut,
        each = nrow(.) / length(excl_data_sets_final$nam_cut)
      )
    )

    # Should output be printed
    if (print_output) {
      return(res)
    } else{
      invisible(res)
    }
  }
