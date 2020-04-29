#' bt_test_mf
#'
#' @description Basic function for running a Bayesian t-test
#' @param cs1 cs 1
#' @param cs2 cs 2
#' @param subj column name with the participant number.
#' It should be a unique number.
#' @param group name of the group variable, if it is present, default to \code{NULL}
#' @param data a data frame containing the dv and iv
#' @param paired whether the t-test refers to a paired or independent t-test.
#' @param phase Different tests will be run for different phases. That is why
#' the phase needs to be specified here. Possible values are \code{acquisition},
#' or \code{acq}, \code{extnction}, or \code{extinction}. See Details for more
#' information.
#' @param dv name of the dependent variable, default to "SCR"
#' @param na.rm Whether NAs should be removed, default to \code{FALSE}
#' @param exclusion If any exclusion was done, default to \code{full data}
#' @param rscale r scale to be used in the prior of the alternative hypothesis
#' @details At the moment the function returns only paired samples t-test. The effect size is Hedge's g.
#' @return a basic function for running a t-test within the \code{multifear}
#' package
#' @importFrom dplyr %>%
#' @export

bt_test_mf <-
  function(cs1,
           cs2,
           data,
           subj,
           group = NULL,
           na.rm = FALSE,
           paired = TRUE,
           rscale = "medium",
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
    # a bit too much to run all tests but we save all the if else statements


    if (is.null(group)) {
      ttest_prep <- purrr::map_dfr(.x = seq_len(3), ~ data) %>%
        dplyr::mutate(alternative = rep(c("c(-Inf, Inf)", "c(-Inf, 0)", "c(0, Inf)"),
                                        each = nrow(data)), group2 = alternative) %>%
        tidyr::gather(CS, value, -subj, -alternative, -group2, -group) %>%
        tidyr::separate(CS, c("CS", "N"),
                        convert = FALSE, remove = TRUE, fill = "right") %>%
        dplyr::group_by(group2)

        ttest_run <- ttest_prep %>%
        dplyr::group_map(
          ~ BayesFactor::ttestBF(
            x = .[.$N == 1,]$value,
            y = .[.$N == 2,]$value,
            rscale = rscale,
            nullInterval = eval(parse(text = .$alternative[1]))
          ) %>%
            BayesFactor::extractBF(onlybf = TRUE)
        ) %>%
        purrr::invoke(.f = "rbind") %>%
        data.frame() %>%
        select(X1) %>%
        unlist()
    } else{
      ttest_prep <- purrr::map_dfr(.x = seq_len(3), ~ data) %>%
        dplyr::mutate(alternative = rep(c("c(-Inf, Inf)", "c(-Inf, 0)", "c(0, Inf)"),
                                        each = nrow(data)), group2 = alternative) %>%
        dplyr::mutate(cs = cs.1 - cs.2) %>%
        tidyr::gather(CS, value, -subj, -alternative, -group2, -group) %>%
        tidyr::separate(CS, c("CS", "N"),
                        convert = FALSE, remove = TRUE, fill = "right") %>%
        dplyr::group_by(group2)

      ttest_run <- ttest_prep %>%
        dplyr::group_map(
          ~ BayesFactor::ttestBF(
            x = .$value,
            y = .$group,
            rscale = rscale,
            nullInterval = eval(parse(text = .$alternative[1]))
          ) %>%
            BayesFactor::extractBF(onlybf = TRUE)
        ) %>%
        purrr::invoke(.f = "rbind") %>%
        data.frame() %>%
        select(X1) %>%
        unlist()

    }

    # List to be pasted to broom functions
    if (!!phase %in% c("acquisition", "acq")) {
      ttl <-
        ttest_run[c(3, 1)]
      type_test = c("higher", "equal")
    }

    if (!!phase %in% c("extinction", "ext")) {
      ttl <-
        ttest_run[c(3, 1)]
      type_test = c("lower", "equal")
    }

    res <- data.frame(
      x = "cs",
      y = dv,
      exclusion = exclusion,
      model = paste("Bayesian t-test", type_test),
      controls = NA,
      method = "t-test",
      p.value = NA,
      effect.size = NA,
      estimate = ttl,
      statistic = NA,
      conf.low = NA,
      conf.high = NA
    ) %>%
      dplyr::mutate(data_used = list(data))

    return(res)
  }
