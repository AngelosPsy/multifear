#' multiverse_cs
#'
#'
#' @description Basic function for conducting multiverse analyses of conditioning
#' data
#' @inheritParams universe_cs
#' @param cs_paired A character vector with the trials that were paired. Default is set to \code{NULL}, suggesting that there was full reinforcement
#' @param cutoff A numeric vector of the cutoff criteria applied. Default to \code{0, 0.05, .1}
#' @param correction whether the Greenhouse-Geisser correction should be applied or not. Default to \code{FALSE}
#' @param meta.effect How the meta-analytic effect should be computed.
#' @details In case of higher order interaction, only the highest order
#' effect is returned.
#'
#' In case the CSs include only 1 observation per participant, or of unequal
#' numbers of CS trials, the
#' function will return the warning ""Skipping ANOVA due to the number of trials for the cs1 and/or
#' cs2."".
#'
#' In principle the multiverse_cs function runs the universe_cs function multiple times, so whatever holds for the universe_cs -- e.g., in terms of warnings, holds for here as well.
#' @return A tibble with the following column names:
#' x: the name of the independent variable (e.g., cs)
#' y: the name of the dependent variable as this defined in the \code{dv} argument
#' exclusion: see \code{exclusion} argument
#' model: the model that was run (e.g., t-test)
#' controls: ignore this column for this test
#' method: the method used
#' p.value: the reported p-value
#' effect.size: the reported effect size
#' estimate: the estimate of the test run
#' statistic: the value of the test
#' conf.low: the lower confidence interval for the estimate
#' conf.high: the higher confidence interval for the estimate
#' framework: were the data analysed within a NHST or Bayesian framework?
#' data_used: a list with the data used for the specific test
#' @export
multiverse_cs <-
  function(cs1,
           cs2,
           data,
           subj,
           group = NULL,
           cs_paired = NULL,
           include_bayes = TRUE,
           include_mixed = FALSE,
           phase = "acquisition",
           cutoff = c(0, 1, 2, 3),
           print_output = TRUE,
           correction = FALSE,
           meta.effect = "d_to_eta2") {

    # Check data
    if(is.null(cs_paired)){
      collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj)
    } else{
      collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj, cs_paired = cs_paired)
    }

    # Excluded participants
    chop <- multifear::chop_css(cs1 = cs1, cs2 = cs2, data = data, subj = subj, group = group)
    excl_data_sets  <- purrr::map_df(cutoff, ~ multifear::exclusion_criteria(chop, cutoff = .)) %>%
      exclude_cases()

    excl_data_sets <- excl_data_sets %>% dplyr::filter(cutoff == "full_data")

    if (!is.null(cs_paired)){
      chop_p <- multifear::chop_css(cs1 = cs1, cs2 = cs2, data = data, subj = subj, cs_paired = cs_paired)
      excl_data_sets_p  <- multifear::exclusion_criteria(chop_p) %>%
        exclude_cases()
      # Change names
      excl_data_sets_p$names <- paste0(excl_data_sets_p$names, "_p")
      excl_data_sets <- dplyr::bind_rows(excl_data_sets, excl_data_sets_p)
      excl_data_sets <- excl_data_sets %>% dplyr::filter(cutoff == "full_data")
    }

    if(is.null(group)){
      res <- purrr::map2_dfr(
        .x = excl_data_sets$used_data,
        .y = excl_data_sets$names,
        .f = function(x, y){ multifear::universe_cs(
          cs1 = dplyr::select(data.frame(x), dplyr::contains("cs1")) %>% colnames(),
          cs2 = dplyr::select(data.frame(x), dplyr::contains("cs2")) %>% colnames(),
          data = data.frame(x),
          subj = dplyr::select(data.frame(x), dplyr::contains("id")) %>% colnames() %>%
            data.frame() %>% dplyr::slice(1) %>% unlist() %>% as.character(),
          group = NULL,
          include_bayes = include_bayes,
          include_mixed = include_mixed,
          exclusion = y,
          phase = phase,
          meta.effect = meta.effect
        )
        }
      )

      #res <- purrr::pmap_dfr(
      #  list(x = excl_data_sets$used_data,
      #       y = excl_data_sets$names,
      #       z = excl_data_sets$cutoff
      #  ),
      #  ~with(list(...), multifear::universe_cs(
      #    cs1 = dplyr::select(data.frame(x), dplyr::contains("cs1")) %>% colnames(),
      #    cs2 = dplyr::select(data.frame(x), dplyr::contains("cs2")) %>% colnames(),
      #    data = data.frame(x),
      #    #subj = dplyr::select(data.frame(x), dplyr::contains("id")) %>% colnames(),
      #    subj = dplyr::select(data.frame(x), dplyr::contains("id")) %>% colnames() %>%
      #      data.frame() %>% slice(1) %>% unlist() %>% as.character(),
      #    group = NULL,
      #    include_bayes = include_bayes,
      #    exclusion = y,
      #    #cut_off = z
      #  )
      #  )
    #  )

    } else {

      res <- purrr::map2_dfr(
        .x = excl_data_sets$used_data,
        .y = excl_data_sets$names,
        .f = function(x, y){ multifear::universe_cs(
          cs1 = dplyr::select(data.frame(x), dplyr::contains("cs1")) %>% colnames(),
          cs2 = dplyr::select(data.frame(x), dplyr::contains("cs2")) %>% colnames(),
          data = data.frame(x),
          subj = dplyr::select(data.frame(x), dplyr::contains("id")) %>% colnames() %>%
            data.frame() %>% dplyr::slice(1) %>% unlist() %>% as.character(),
          group = dplyr::select(data.frame(x), dplyr::contains("group")) %>% colnames(),
          include_bayes = include_bayes,
          include_mixed = include_mixed,
          exclusion = y,
          phase = phase
        )
        }
      )

      #res <- purrr::pmap_dfr(
      #  list(x = excl_data_sets$used_data,
      #       y = excl_data_sets$names,
      #       z = excl_data_sets$cutoff
      #  ),
      #  ~with(list(...), multifear::universe_cs(
      #    cs1 = dplyr::select(data.frame(x), dplyr::contains("cs1")) %>% colnames(),
      #    cs2 = dplyr::select(data.frame(x), dplyr::contains("cs2")) %>% colnames(),
      #    data = data.frame(x),
      #    subj = dplyr::select(data.frame(x), dplyr::contains("id")) %>% colnames() %>%
      #      data.frame() %>% slice(1) %>% unlist() %>% as.character(),
      #    group = dplyr::select(data.frame(x), dplyr::contains("group")) %>% colnames(),
      #    include_bayes = include_bayes,
      #    exclusion = y,
      #    cut_off = z
      #  )
      #  )
      #)
    }

    # Should output be printed
    if (print_output) {
      return(res)
    } else{
      invisible(res)
    }
  }
