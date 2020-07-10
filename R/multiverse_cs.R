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
           phase = "acquisition",
           cutoff = c(0, 1, 2, 3),
           print_output = TRUE) {

    # Check data
    if(is.null(cs_paired)){
      collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj)
    } else{
        collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj, cs_paired = cs_paired)
    }

    # Excluded participants
    chop <- multifear::chop_css(cs1 = cs1, cs2 = cs2, data = data, subj = subj)
    excl_data_sets  <- purrr::map_df(cutoff, ~ multifear::exclusion_criteria(chop, cutoff = .)) %>%
      exclude_cases()

    if (!is.null(cs_paired)){
      chop_p <- multifear::chop_css(cs1 = cs1, cs2 = cs2, data = data, subj = subj, cs_paired = cs_paired)
      excl_data_sets_p  <- multifear::exclusion_criteria(chop_p) %>%
        exclude_cases()
      # Change names
      excl_data_sets_p$names <- paste0(excl_data_sets_p$names, "_p")
      excl_data_sets <- dplyr::bind_rows(excl_data_sets, excl_data_sets_p)
    }


    res <- purrr::pmap_dfr(
      list(x = excl_data_sets$used_data,
           y = excl_data_sets$names,
           z = excl_data_sets$cutoff
      ),
      ~with(list(...), multifear::universe_cs(
        cs1 = dplyr::select(data.frame(x), dplyr::contains("cs1")) %>% colnames(),
        cs2 = dplyr::select(data.frame(x), dplyr::contains("cs2")) %>% colnames(),
        data = data.frame(x),
        subj = dplyr::select(data.frame(x), dplyr::contains("id")) %>% colnames(),
        group = group,
        include_bayes = include_bayes,
        exclusion = y,
        cut_off = z
      )
      )
    )


    #>% dplyr::mutate(
    #  cutoff = rep(
    #    excl_data_sets$cutoff,
    #    each = nrow(.) / length(excl_data_sets$cutoff)
    #  )#,
      #name_cutoff = rep(
      #  excl_data_sets$nam_cut,
      #  each = nrow(.) / length(excl_data_sets$nam_cut)
      #)
    #)

    # Should output be printed
    if (print_output) {
      return(res)
    } else{
      invisible(res)
    }
  }
