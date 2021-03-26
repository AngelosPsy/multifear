#' universe_cs
#'
#' @description Basic function for running a multiverse analysis for a single data set
#' @inheritParams rm_anova_mf
#' @param include_bayes Whether the Bayesian analyses should be run. Default to \code{TRUE}
#' @param include_mixed Whether the mixed model results should be run. Default to \code{FALSE}
#' @param print_output Whether to print the output or not. Default set to \code{TRUE}
#' @param cut_off cut off score
#' @param correction whether the Greenhouse-Geisser correction should be applied or not. Default to \code{FALSE}
#' @param meta.effect How the meta-analytic effect should be computed.
#' @details In case of higher order interaction, only the highest order
#' effect is shown.
#'
#' In case the CSs include only 1 observation per participant, or of unequal numbers of CS trials, the
#' function will return the warning ""Skipping ANOVA due to the number of trials for the cs1 and/or
#' cs2."".
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
#' @examples
#' # Load data and define argument to be fed in universe_cs
#' data("example_data", package = "multifear")
#' example_data <- example_data[1:10, ]
#' cs1 <- paste0("CSP", 1:10)
#' cs2 <- paste0("CSM", 1:10)
#' subj = "id"
#' multifear::universe_cs(cs1, cs2, subj = subj, data = example_data, include_bayes = FALSE)
#' @export

universe_cs <-
  function(cs1,
           cs2,
           data,
           subj,
           group = NULL,
           include_bayes = TRUE,
           include_mixed = FALSE,
           phase = "acquisition",
           dv = "scr",
           print_output = TRUE,
           exclusion = "full data",
           cut_off = "full data",
           correction = FALSE,
           meta.effect = "d_to_eta2") {

    # Check data
    collection_warning(
      cs1 = cs1,
      cs2 = cs2,
      data = data,
      subj = subj
    )

    # Prepare data for multiple analyses
    data <-
      data_preparation_verse(
        cs1 = cs1,
        cs2 = cs2,
        data = data,
        subj = subj,
        group = group
      )

    cs1    <-
      data %>% dplyr::select(dplyr::contains("cs1")) %>% colnames()
    cs2    <-
      data %>% dplyr::select(dplyr::contains("cs2")) %>% colnames()
    subj   <- data %>% dplyr::select("subj") %>% colnames()
    paired <- ifelse(is.null(group), TRUE, FALSE)

    if(!is.null(group)){
      group   <- data %>% dplyr::select("group") %>% colnames()
    }

    # In case of 1 trial CS or enequal number of CSs, skip ANOVA
    do_anova <- TRUE
    if ((length(cs1) == 1 |
         length(cs2) == 1) |
        length(cs1) != length(cs2)) {
      do_anova = FALSE
      message("Skipping ANOVA due to the number of trials for the cs1 and/or cs2.")
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
          exclusion = exclusion,
          cut_off = cut_off,
          correction = correction
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
          exclusion = exclusion,
          cut_off = cut_off,
          correction = correction
        )

      if (include_mixed) {
        # mixed results
        mixedMod <-
          multifear::mixed_mf(
            cs1 = cs1,
            cs2 = cs2,
            data = data,
            subj = subj,
            group = NULL,
            phase = phase,
            dv = dv,
            exclusion = exclusion,
            cut_off = cut_off
          )
      }

      if (include_bayes) {
        banovaNOTIME <-
          multifear::rm_banova_mf(
            cs1 = cs1,
            cs2 = cs2,
            time = FALSE,
            subj = subj,
            data = data,
            group = NULL,
            exclusion = exclusion,
            cut_off = cut_off,
            phase = phase,
            dv = dv)

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
            cut_off = cut_off,
            dv = dv)
      }
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
          exclusion = exclusion,
          cut_off = cut_off
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
          exclusion = exclusion,
          cut_off = cut_off
        )

      # mixed results
      if (include_mixed) {
        mixedMod <-
          multifear::mixed_mf(
            cs1 = cs1,
            cs2 = cs2,
            data = data,
            subj = subj,
            group = group,
            phase = phase,
            dv = dv,
            exclusion = exclusion,
            cut_off = cut_off
          )
      }

      if (include_bayes) {
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
            cut_off = cut_off,
            dv = dv)

        banovaTIME <-
          multifear::rm_banova_mf(
            cs1 = cs1,
            cs2 = cs2,
            time = TRUE,
            subj = subj,
            data = data,
            group = group,
            phase = phase,
            exclusion = exclusion,
            cut_off = cut_off,
            dv = dv)
      }
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
        exclusion = exclusion,
        cut_off = cut_off,
        meta.effect = meta.effect
      )

    combRes <- list(`t-test full` = ttestFULL)

    if (include_bayes) {
      bttestFULL <-
        multifear::bt_test_mf(
          cs1 = cs1,
          cs2 = cs2,
          data = data,
          subj = subj,
          paired = paired,
          phase = phase,
          exclusion = exclusion,
          cut_off = cut_off
        )
      combRes$`bayes t-test full` <- bttestFULL
    }

    if (do_anova) {
      combRes$`repeated measures ANOVA with time` <- anovaTIME
      combRes$`repeated measures ANOVA without time` <- anovaNOTIME
       if (include_mixed){
         combRes$`mixed model` <- mixedMod
       }
       if (include_bayes) {
        combRes$`repeated measures Bayesian ANOVA with time` <- banovaTIME
        combRes$`repeated measures Bayesian ANOVA without time` <-
          banovaNOTIME
      }
    }

    # Collapse results into one data frame
    res <- purrr::map_df(combRes, rbind)

    class(res) <- c("multi_fear", class(res))
    attr(res, "collapsed data") <- csc

    if (print_output) {
      return(res)
    } else{
      invisible(res)
    }

  }
