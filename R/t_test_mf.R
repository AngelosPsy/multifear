#' t_test_mf
#'
#' @description Basic function for running a t-test
#' @param cs1 cs 1
#' @param cs2 cs 2
#' @param subj column nmae with the participant number.
#' It should be a unique number.
#' @param data a data frame containing the dv and iv
#' @param paired whether the t-test refers to a paired or independent t-test.
#' @param phase Different tests will be run for different phases. That is why
#' the phase needs to be specified here. Possible values are \code{acquisition},
#' or \code{acq}, \code{extnction}, or \code{extinction}. See Details for more
#' information.
#' @param dv name of the dependent variable, default to "SCR"
#' @param na.rm Whether NAs should be removed, default to \code{FALSE}
#' @param exclusion If any exclusion was done, default to \code{full data}
#' @return a basic function for running a t-test within the \code{multifear}
#' package
#' @importFrom dplyr %>%
#' @export

t_test_mf <-
  function(cs1,
           cs2,
           data,
           subj,
           na.rm = FALSE,
           paired = TRUE,
           phase = "acquisition",
           dv = "scr",
           exclusion = "full data") {
    # Check data
    collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj)

    # Restructure data. rowMeans is used in case multiple trails have been fed
    cs1 <-
      data %>% dplyr::select(all_of(!!dplyr::enquo(cs1))) %>% rowMeans(na.rm = na.rm) %>% tibble::enframe(name = NULL)  %>% dplyr::rename(cs.1 = value)#dplyr::select(cs.1 = dplyr::everything())
    cs2 <-
      data %>% dplyr::select(all_of(!!dplyr::enquo(cs2))) %>% rowMeans(na.rm = na.rm) %>% tibble::enframe(name = NULL) %>% dplyr::rename(cs.2 = value)#dplyr::select(cs.2 = dplyr::everything())
    subj <-
      data %>% dplyr::select(all_of(!!dplyr::enquo(subj))) %>% tibble::as_tibble() %>% dplyr::select(subj = dplyr::everything())

    data <- dplyr::bind_cols(subj, cs1, cs2)

    # Here we run all t.tests and we select later on which one we wants. It is
    # a bit too much to run all tests but we save all the if else statements
    ttest_res <- purrr::map_dfr(.x = seq_len(3), ~ data) %>%
      dplyr::mutate(alternative = rep(c("two.sided", "less", "greater"),
                                      each = nrow(data)), group2 = alternative) %>%
      tidyr::gather(CS, value, -subj, -alternative, -group2) %>%
      tidyr::separate(CS, c("CS", "N")) %>%
      dplyr::group_by(group2) %>%
      dplyr::group_map(
        ~ stats::t.test(
          formula = .$value ~ .$N,
          data = .,
          paired = TRUE,
          alternative = .$alternative[1]
        ) %>%
          broom::tidy()
      )
    ttest_res <- purrr::invoke("rbind", ttest_res)

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
      dplyr::select(x,
                    y,
                    exclusion,
                    model,
                    controls,
                    method,
                    estimate,
                    statistic,
                    conf.low,
                    conf.high) %>%
      dplyr::mutate(data_used = list(data))

    return(res)
  }
