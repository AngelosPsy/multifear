#' t_test_mf
#'
#' @description Basic function for running a t-test
#' @param cs1 cs 1
#' @param cs2 cs 2
#' @param subj column name with the participant number.
#' It should be a unique number.
#' @param data a data frame containing the dv and iv
#' @param paired whether the t-test refers to a paired or independent t-test.
#' @param phase Different tests will be run for different phases. That is why
#' the phase needs to be specified here. Possible values are \code{acquisition},
#' or \code{acq}, \code{extnction}, or \code{extinction}. See Details for more
#' information.
#' @param dv name of the dependent variable, default to "SCR"
#' @param group the name of the group, if included, default to \code{NULL}
#' @param na.rm Whether NAs should be removed, default to \code{FALSE}
#' @param exclusion If any exclusion was done, default to \code{full data}
#' @details At the moment the function returns only paired samples t-test. The effect size is Hedge's g.
#' @return a basic function for running a t-test within the \code{multifear}
#' package
#' @importFrom dplyr %>%
#' @export

t_test_mf <-
  function(cs1,
           cs2,
           data,
           subj,
           group = NULL,
           na.rm = FALSE,
           paired = TRUE,
           phase = "acquisition",
           dv = "scr",
           exclusion = "full data") {
    # Check data
    collection_warning(
      cs1 = cs1,
      cs2 = cs2,
      data = data,
      subj = subj
    )

    data <-
      data_preparation_ttest(
        cs1 = cs1,
        cs2 = cs2,
        data = data,
        subj = subj,
        group = group,
        na.rm = na.rm
      )

    # Here we run all t.tests and we select later on which one we wants. It is
    # a bit too much to run all tests but we save all the if else
    #
    #####################################
    # No groups
    #####################################
    if (!is.null(group)) {
      ttest_prep_tmp <- purrr::map_dfr(.x = seq_len(3), ~ data) %>%
        dplyr::mutate(
          cs = cs.1 - cs.2,
          alternative = rep(c("two.sided", "less", "greater"),
                            each = nrow(data)),
          group2 = alternative
        )

      ttest_prep <- ttest_prep_tmp %>%
        dplyr::group_by(group2) %>%
        dplyr::group_map(
          ~ stats::t.test(
            formula = .$cs ~ .$group,
            data = .,
            paired = FALSE,
            alternative = .$alternative[1]
          ) %>%
            broom::tidy()
        )

      # Compute effect size
      ttest_es <-
        effsize::cohen.d(
          ttest_prep_tmp$cs ~ ttest_prep_tmp$group,
          pooled = TRUE,
          paired = FALSE,
          na.rm = na.rm,
          hedges.correction = TRUE
        )

    } else {
      #####################################
      # With groups
      #####################################
      #
      ttest_prep <- purrr::map_dfr(.x = seq_len(3), ~ data) %>%
        dplyr::mutate(alternative = rep(c("two.sided", "less", "greater"),
                                        each = nrow(data)),
                      group2 = alternative) %>%
        tidyr::gather(CS, value, -subj, -alternative, -group, -group2) %>%
        tidyr::separate(CS, c("CS", "N")) %>%
        dplyr::group_by(group2) %>%
        dplyr::group_map(
          ~ stats::t.test(
            formula = .$value ~ .$N,
            data = .,
            paired = paired,
            alternative = .$alternative[1]
          ) %>%
            broom::tidy()
        )

      # Compute effect size
      ttest_es <-
        effsize::cohen.d(
          unlist(data %>% dplyr::select(all_of("cs.1"))),
          unlist(data %>% dplyr::select(all_of("cs.2"))),
          pooled = TRUE,
          paired = TRUE,
          na.rm = na.rm,
          hedges.correction = TRUE
        )

    }

    ttest_res <-
      purrr::invoke("rbind", ttest_prep) %>%
      dplyr::mutate(effect.size = rep(ttest_es$estimate, 3))

    # List to be pasted to broom functions
    if (!!phase %in% c("acquisition", "acq")) {
      ttl <-
        ttest_res %>% dplyr::filter(alternative %in% c("two.sided", "greater"))
    }

    if (!!phase %in% c("extinction", "ext")) {
      ttl <-
        ttest_res %>% dplyr::filter(alternative %in% c("two.sided", "less"))
    }

    res <- ttl %>%
      dplyr::mutate(
        method = paste("t-test"),
        x = "cs",
        y = dv,
        exclusion = exclusion,
        model = "t-test",
        controls = NA
      ) %>%
      dplyr::select(
        x,
        y,
        exclusion,
        model,
        controls,
        method,
        p.value,
        effect.size,
        estimate,
        statistic,
        conf.low,
        conf.high
      ) %>%
      dplyr::mutate(data_used = list(data))

    return(res)
  }
