#' chop_cs
#'
#' \lifecycle{experimental}
#'
#' @description Function for separating the conditioned responses into multiple pieces.
#' @inheritParams t_test_mf
#' @param cs index for the CS responses.
#'
#' @example
#'
#' data(example_data)
#' chop_cs(cs = paste0("CSP", 1:10), data = example_data, subj = "id")
#'
#' @export

chop_cs <- function(cs, data, subj, na.rm = FALSE) {
  # Check data
  collection_warning(cs1 = cs, data = data, subj = subj)

  cs <- data %>% dplyr::select(all_of(!!dplyr::enquo(cs)))
  subj <- data %>% dplyr::select(all_of(!!dplyr::enquo(subj))) %>%
    dplyr::rename(id = 1)

  # Determine how many trials you have. Based on that, you will determine
  # how many parts you will have to make.
  nc_cs <- ncol(cs)
  fh <- nc_cs / 2
  fhp1 <- (nc_cs / 2) + 1
  fhp1Odd <-  ((nc_cs + 1) / 2)
  fhp2Odd <-  fhp1Odd + 1

  # Split data in 2 halfs
  if ((nc_cs %% 2) == 0) {
    # Half of the trials
    cs_fhalf <-
      cs %>% dplyr::select(1:fh) %>%
      rowMeans(na.rm = na.rm) %>%
      tibble::enframe(name = NULL, value = "cs_fhalf")
    cs_lhalf <-
      cs %>% dplyr::select(fhp1:nc_cs) %>%
      rowMeans(na.rm = na.rm) %>%
      tibble::enframe(name = NULL, value = "cs_lhalf")
  } else {
    # You do the nc_cs + 1 trick because it is an odd number. Have to think
    # more of that
    cs_fhalf <-
      data %>% dplyr::select(1:fhp1Odd) %>%
      rowMeans(na.rm = na.rm) %>%
      tibble::enframe(name = NULL, value = "cs_fhalf")
    cs_lhalf <-
      data %>% dplyr::select(fhp2Odd:nc_cs) %>%
      rowMeans(na.rm = na.rm) %>%
      tibble::enframe(name = NULL, value = "cs_lhalf")
  }

  # Just the last trial and the last two trials
  cs_ltrial <-
    cs %>% dplyr::select(utils::tail(names(.), 1)) %>%
    unlist() %>% tibble::enframe(name = NULL, value = "cs_ltrial")
  cs_l2trial <-
    cs %>% dplyr::select(utils::tail(names(.), 2)) %>%
    dplyr::rename(l1trial = 1, l2trial = 2)

  # Difference between last and first trial
  cs_lftdiff <-
    cs %>% dplyr::select(1, utils::tail(names(.), 1)) %>%
    apply(1, diff) %>%
    tibble::enframe(name = NULL, value = "cs_lftdiff")

  cs_half <- dplyr::bind_cols(cs_fhalf, cs_lhalf)

  res <-
    dplyr::bind_cols(subj, cs_ltrial, cs_l2trial, cs_lftdiff, cs_half) %>%
    tidyr::as_tibble()

  return(res)
}
