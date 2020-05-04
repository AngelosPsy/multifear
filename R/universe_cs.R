#' universe_cs
#'
#' \lifecycle{experimental}
#'
#' @description Basic function for conducting universe analyses for conditioning data
#' data
#' @inheritParams rm_anova_mf
#' @param print_output Whether to print the output or not. Default set to \code{TRUE}
#' @details In case of higher order interaction, only the highest order
#' effect is shown.
#' @return A data frame with the results.
#' @export

universe_cs <-
  function(cs1,
           cs2,
           data,
           subj,
           group = NULL,
           phase = "acquisition",
           dv = "scr",
           print_output = TRUE,
           exclusion = "full data") {
    # Check data
    collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj)

    # Prepare data for multiple analyses
    data <-
      data_preparation_verse(
        cs1 = cs1,
        cs2 = cs2,
        data = data,
        subj = subj,
        group = group
      )

    cs1 = data %>% dplyr::select(dplyr::contains("cs1")) %>% colnames()
    cs2 = data %>% dplyr::select(dplyr::contains("cs2")) %>% colnames()
    subj = data %>% dplyr::select("subj") %>% colnames()
    paired = ifelse(is.null(group), TRUE, FALSE)

    # In case of 1 trial CS or enequal number of CSs, skip ANOVA
    do_anova = TRUE
    if ((length(cs1) == 1 |
         length(cs2) == 1) |
        length(cs1) != length(cs2)) {
      do_anova = FALSE
      message("Skipping ANOVA")
    }

    # Perform ANOVA
    if (is.null(group) & do_anova) {
      anovaNOTIME <-
        multifear::rm_anova_mf(
          cs1 = cs1,
          cs2 = cs2,
          time = FALSE,
          subj = subj,
          data = data,
          group = NULL,
          phase = phase,
          exclusion = exclusion
        )
      anovaTIME <-
        multifear::rm_anova_mf(
          cs1 = cs1,
          cs2 = cs2,
          time = TRUE,
          subj = subj,
          data = data,
          group = NULL,
          phase = phase,
          exclusion = exclusion
        )

      banovaNOTIME <-
        multifear::rm_banova_mf(
          cs1 = cs1,
          cs2 = cs2,
          time = FALSE,
          subj = subj,
          data = data,
          group = NULL,
          exclusion = exclusion,
          phase = phase,
          dv = dv,
          multicore = TRUE
        )

      banovaTIME <-
        multifear::rm_banova_mf(
          cs1 = cs1,
          cs2 = cs2,
          time = TRUE,
          subj = subj,
          data = data,
          group = NULL,
          phase = phase,
          exclusion = exclusion,
          dv = dv,
          multicore = TRUE
        )

      } else if (!is.null(group) & do_anova) {
      anovaNOTIME <-
        multifear::rm_anova_mf(
          cs1 = cs1,
          cs2 = cs2,
          time = FALSE,
          subj = subj,
          data = data,
          group = group,
          phase = phase,
          exclusion = exclusion
        )
      anovaTIME <-
        multifear::rm_anova_mf(
          cs1 = cs1,
          cs2 = cs2,
          time = TRUE,
          subj = subj,
          data = data,
          group = group,
          phase = phase,
          exclusion = exclusion
        )

      banovaNOTIME <-
        multifear::rm_banova_mf(
          cs1 = cs1,
          cs2 = cs2,
          time = FALSE,
          subj = subj,
          data = data,
          group = group,
          phase = phase,
          exclusion = exclusion,
          dv = dv,
          multicore = TRUE
        )

      banovaTIME <-
        multifear::rm_banova_mf(
          cs1 = cs1,
          cs2 = cs2,
          time = TRUE,
          subj = subjs,
          data = data,
          group = group,
          phase = phase,
          exclusion = exclusion,
          dv = dv,
          multicore = TRUE
        )
    }

    # Perform t-test
    # First combine css
    csc <-
      multifear::combine_cs(cs1 = cs1,
                            cs2 = cs2,
                            data = data)
    ttestFULL <-
      multifear::t_test_mf(
        cs1 = cs1,
        cs2 = cs2,
        data = data,
        subj = subj,
        paired = paired,
        phase = phase,
        exclusion = exclusion
      )

    bttestFULL <-
      multifear::bt_test_mf(
        cs1 = cs1,
        cs2 = cs2,
        data = data,
        subj = subj,
        paired = paired,
        phase = phase,
        exclusion = exclusion
      )

    combRes <- list(`t-test full` = ttestFULL,
                    `bayes t-test full` = bttestFULL)

    if (do_anova) {
      combRes$`repeated measures ANOVA with time` <- anovaTIME
      combRes$`repeated measures ANOVA without time` <- anovaNOTIME
      combRes$`repeated measures Bayesian ANOVA with time` <- banovaTIME
      combRes$`repeated measures Bayesian ANOVA without time` <- banovaNOTIME
    }

    # Collapse results into one data frame
    res <- purrr::map_df(combRes, rbind)

    class(res) <- c("multi_fear", class(res))
    attr(res, "collapsed data") <- csc
    attr(res, "t-test full") <- ttestFULL
    attr(res, "repeated measures ANOVA with time")  <- anovaTIME
    attr(res, "repeated measures ANOVA without time") <- anovaNOTIME
    attr(res, "repeated measures Bayesian ANOVA with time")  <- banovaTIME
    attr(res, "repeated measures Bayesian ANOVA without time") <- banovaNOTIME

    attr(res, "main results") <- "multi_cs"

    if (print_output) {
      return(res)
    } else{
      invisible(res)
    }

  }
