#' multiverse_cs
#'
#' @description Basic function for conducting multi-verse analyses of conditioning
#' data
#' @inheritParams universe_cs
#' @param cutoff A numeric vector of the cutoff criteria applied. Default to \code{0, 0.05, .1}
#' @param na.rm Should NAs be removed? Default set the \code{FALSE}
#' @param print_output Whether to print the output or not. Default set to \code{TRUE}
#' @details In case of higher order interaction, only the highest order
#' effect is shown
#' @export

multiverse_cs <-
  function(cs1,
           cs2,
           data,
           subj,
           group = NULL,
           cutoff = c(0, 0.1, 0.05),
           phase = "acquisition",
           na.rm = FALSE,
           print_output = TRUE) {
    # Check data
    collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj)

    # Excluded participants

    chop <- multifear::chop_css(cs1, cs2, data, subj)
    excl_data_sets <-
      purrr::map_df(cutoff, ~ multifear::exclusion_criteria(chop, cutoff = .))

    # Remove empty data -- this can happen
    # if you have removed a lot of participants
    excl_data_sets_final <-
      excl_data_sets %>%
      dplyr::mutate(excl = excl_data_sets$used_data %>%
                      lapply(plyr::empty) %>%
                      data.frame() %>% t()) #%>%
      #dplyr::filter(excl == FALSE)

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




    excl_data_sets_final <-
      excl_data_sets_final %>%
      dplyr::filter(excl == FALSE)

    res <- purrr::map2_dfr(
      excl_data_sets_final$used_data,
      excl_data_sets_final$names,
      ~ multifear::universe_cs(
        cs1 = cs1,
        cs2 = cs2,
        data = .x,
        subj = subj,
        group = group,
        exclusion = .y
      )
    )


    # Should output been printed
    if (print_output) {
      return(res)
    } else{
      invisible(res)
    }
  }

