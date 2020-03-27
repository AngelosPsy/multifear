#' universe_cs
#'
#' @description Basic function for conducting universe analyses of conditioning
#' data
#' @inheritParams rm_anova_mf
#' @param na.rm Should NAs be removed? Default set the \code{FALSE}
#' @param print_output Whether to print the output or not. Default set to \code{TRUE}
#' @param exclusion If any exclusion was done, default to \code{full data}
#' @details In case of higher order interaction, only the highest order
#' effect is shown
#' @export

universe_cs <-
  function(cs1,
           cs2,
           data,
           subj,
           group = NULL,
           phase = "acquisition",
           na.rm = FALSE,
           print_output = TRUE,
           exclusion = "full data") {
    # Check data
    collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj)

    # Prepare data for multiple analyses
    cs1  <-
      data %>% dplyr::select(all_of(!!dplyr::enquo(cs1))) %>% tibble::as_tibble()
    cs2  <-
      data %>% dplyr::select(all_of(!!dplyr::enquo(cs2))) %>% tibble::as_tibble()
    subj <-
      data %>% dplyr::select(all_of(!!dplyr::enquo(subj))) %>% tibble::as_tibble()

    # Renaming objects to make life a bit easier
    cs1  <- cs1 %>% dplyr::select(cs1_ = dplyr::everything())
    cs2  <- cs2 %>% dplyr::select(cs2_ = dplyr::everything())
    subj <- subj %>% dplyr::select(subj = dplyr::everything())

    # What happens in case of groups
    if (is.null(group)) {
      group_new <-
        data %>%
        dplyr::mutate(group = rep("NULL", nrow(data))) %>%
        dplyr::select(group)
      group <- NULL
    } else {
      group_new <- data %>%
        dplyr::select(tidyselect::all_of(!!dplyr::enquo(group)))
    }

    data <- dplyr::bind_cols(subj, cs1, cs2, group_new)

    # In case of 1 trial CS or enequal number of CSs, skip ANOVA
    do_anova = TRUE
    if ((ncol(cs1) == 1 |
         ncol(cs2) == 1) |
        ncol(cs1) != ncol(cs2)) {
      do_anova = FALSE
      message("Skipping ANOVA")
    }

    # Perform ANOVA
    if (is.null(group) & do_anova) {
      anovaNOTIME <-
        multifear::rm_anova_mf(
          cs1 = colnames(cs1),
          cs2 = colnames(cs2),
          time = FALSE,
          subj = colnames(subj),
          data = data,
          group = NULL,
          phase = phase,
          exclusion = exclusion
        )
      anovaTIME <-
        multifear::rm_anova_mf(
          cs1 = colnames(cs1),
          cs2 = colnames(cs2),
          time = TRUE,
          subj = colnames(subj),
          data = data,
          group = NULL,
          phase = phase,
          exclusion = exclusion
        )
    }
    else if (!is.null(group) & do_anova) {
      anovaNOTIME <-
        multifear::rm_anova_mf(
          cs1 = colnames(cs1),
          cs2 = colnames(cs2),
          time = FALSE,
          subj = colnames(subj),
          data = data,
          group = group,
          phase = phase,
          exclusion = exclusion
        )
      anovaTIME <-
        multifear::rm_anova_mf(
          cs1 = colnames(cs1),
          cs2 = colnames(cs2),
          time = TRUE,
          subj = colnames(subj),
          data = data,
          group = group,
          phase = phase,
          exclusion = exclusion
        )
    }

    # Perform t-test
    # First combine css
    csc <-
      multifear::combine_cs(cs1 = colnames(cs1),
                            cs2 = colnames(cs2),
                            data = data)
    ttestFULL <-
      multifear::t_test_mf(cs1 = colnames(cs1),
                           cs2 = colnames(cs2),
                           data = data, subj = colnames(subj),
                           paired = TRUE, phase = phase,
                           exclusion = exclusion)

    combRes <- list(#`Collapsed data` = csc,
                    `t-test full` = ttestFULL)

    if (do_anova) {
      combRes$`repeated measures ANOVA with time` <- anovaTIME
      combRes$`repeated measures ANOVA without time` <- anovaNOTIME
    }

    # Collapse results into one data frame
    res <- purrr::map_df(combRes, rbind)

    class(res) <- c("multi_fear", class(res))
    attr(res, "collapsed data") <- csc
    attr(res, "t-test full") <- ttestFULL
    attr(res, "repeated measures ANOVA with time")  <- anovaTIME
    attr(res, "repeated measures ANOVA without time") <- anovaNOTIME
    attr(res, "main results") <- "multi_cs"

    if (print_output) {
      return(res)
    } else{
      invisible(res)
    }

  }
