#' chop_cs
#'
#' @description Function for separating the conditioned responses into multiple pieces
#' @param cs index for the CS responses
#' @param data Data frame containing the CS columns
#' @param subj column nmae with the participant number.
#' It should be a unique number.
#' @param na.rm Should NAs be removed? Default to FALSE.
#' @export

chop_cs <- function(cs, data, subj, na.rm = FALSE) {
  cs <- data %>% dplyr::select(all_of(!!dplyr::enquo(cs)))
  subj <- data %>% dplyr::select(all_of(!!dplyr::enquo(subj)))

  # Determine how many trials you have. Based on that, you will determine
  # how many parts you will have to make.
  nc_cs <- ncol(cs)

  # Split data in 2 halfs
  if ((nc_cs %% 2) == 0) {
    # Half of the trials
    cs_fhalf <-
      data %>% dplyr::select(1, all_of(nc_cs) / 2) %>% rowMeans(na.rm = na.rm) %>%
      tibble::enframe(name = NULL, value = "cs_fhalf")
    cs_lhalf <-
      data %>% dplyr::select(all_of(nc_cs) / 2 + 1, all_of(nc_cs)) %>% rowMeans(na.rm = na.rm) %>%
      tibble::enframe(name = NULL, value = "cs_lhalf")
  } else{
    # You do the nc_cs + 1 trick because it is an odd number. Have to think
    # more of that
    cs_fhalf <-
      data %>% dplyr::select(1, (all_of(nc_cs) + 1) / 2) %>% rowMeans(na.rm = na.rm) %>%
      tibble::enframe(name = NULL, value = "cs_fhalf")
    cs_lhalf <-
      data %>% dplyr::select((all_of(nc_cs) + 1) / 2 + 1, all_of(nc_cs)) %>% rowMeans(na.rm = na.rm) %>%
      tibble::enframe(name = NULL, value = "cs_lhalf")
  }

  # Just the last trial and the last two trials
  cs_ltrial <- cs %>% dplyr::select(utils::tail(names(.), 1)) %>% tibble::enframe(name = NULL, value = "cs_ltrial")
  cs_l2trial <- cs %>% dplyr::select(utils::tail(names(.), 2))

  # Difference between last and first trial
  cs_lftdiff <-
    cs %>% dplyr::select(1, utils::tail(names(.), 1)) %>% apply(1, diff) %>%
    tibble::enframe(name = NULL, value = "cs_lftdiff")

  cs_half  <- dplyr::bind_cols(cs_fhalf, cs_lhalf)

  res <-
    dplyr::bind_cols(subj, cs_ltrial, cs_l2trial, cs_lftdiff, cs_half)

  return(res)
}
