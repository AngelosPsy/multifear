#' multiverse_cs
#'
#' @description Basic function for conducting multi-verse analyses of conditioning
#' data
#' @inheritParams universe_cs
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
           phase = "acquisition",
           na.rm = FALSE,
           print_output = TRUE) {
    # Check data
    collection_warning(cs1 = cs, cs2 = cs2, data = data, subj = subj)

    # Excluded participants
    excl_data_sets <- multifear::chop_css(cs1, cs2, data, subj) %>%
      multifear::exclusion_criteria()

    # Remove empty data -- this can happen
    # if you have removed a lot of participants
    excl_data_sets_final <-
      excl_data_sets %>%
      dplyr::mutate(excl = excl_data_sets$used_data %>%
                                  lapply(plyr::empty) %>%
                                  purrr::map_df(rbind) %>% t()) %>%
      dplyr::filter(excl == FALSE)

    # Return warning if you have excluded any of the data sets
    if (length(excl_data_sets$used_data) !=
        length(excl_data_sets_final$used_data)) {
      warning(paste("Part of the data were excluded due to many excluded cases. Check
              the following cases: ",
                    excl_data_sets_final$names[which(excl_data_sets_final$excl == TRUE)], collapse = ","))
    }

    res <- purrr::map_df(
      excl_data_sets_final$used_data,
      ~ multifear::universe_cs(
        cs1 = cs1,
        cs2 = cs2,
        data = .,
        subj = subj,
        group = group,
        exclusion = exclusion
      )
    )


    # Should output been printed
    if (print_output) {
      return(res)
    } else{
      invisible(res)
    }
  }

