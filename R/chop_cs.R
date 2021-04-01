#' chop_cs
#'
#'
#' @description Function for separating the conditioned responses into multiple pieces
#' @inheritParams t_test_mf
#' @param cs The column name(s) of the conditioned responses
#' @param prefix prefix to be included in the end data frame
#' @param group default to \code{NULL}
#' @return A tibble with the following column names:
#' "id" = id column;
#' "top10per" = mean of top 10% of trials;
#' "bottom10per" = mean of bottom 10% of the trials;
#' "minfirst = all trials minus the first one;
#' "all" = mean of all trials;
#' "t33per = mean of top 33% of the trials;
#' "m33per" = mean of medium 33% of the trials;
#' "b33per" = mean of bottom 33% of the trials;
#' "fhalf" = mean of first half of the trials;
#' "lhalf" = mean of last half of the trials;
#' "ftrial" = first trial;
#' "ltrial" = last trial;
#' "t20per" = mean of top 20% of the trials;
#' "b20per" = mean of bottom 20% of the trials;
#' "f2trial" = mean of first two trials;
#' "l2trial" = mean of last two trials;
#' "per2_X" = mean per two trials (i.e., block), with X referring to the number
#' of block.
#'
#' @examples
#' data(example_data)
#' chop_cs(cs = paste0("CSP", 1:10), data = example_data, subj = "id")
#' @importFrom tibble enframe
#' @export

chop_cs <- function(cs, data, subj, group = NULL, prefix = "cs", na.rm = FALSE) {
  # Check data
  collection_warning(cs1 = cs, data = data, subj = subj)

  cs   <- data %>% dplyr::select(all_of(!!dplyr::enquo(cs)))
  subj <- data %>% dplyr::select(all_of(!!dplyr::enquo(subj))) %>%
    dplyr::rename(id = 1)

  cs_initial <- cs
  colnames(cs_initial) <- paste0(prefix,"_t_", 1:length(cs))

  # Determine how many trials you have. Based on that, you will determine
  # how many parts you will have to make.
  nc_cs   <- ncol(cs)
  fh      <- nc_cs / 2
  fhp1    <- (nc_cs / 2) + 1
  fhp1Odd <-  ((nc_cs + 1) / 2)
  fhp2Odd <-  fhp1Odd + 1

  # Split data in 2 halfs
  if ((nc_cs %% 2) == 0) {
    # Half of the trials
    cs_fhalf <- sumz_trial(cs, 1:fh, prefix, "fhalf")
    cs_lhalf <- sumz_trial(cs, fhp1:nc_cs, prefix, "lhalf")
  } else {
    # You do the nc_cs + 1 trick because it is an odd number. Have to think
    # more of that
    cs_fhalf <- sumz_trial(cs, 1:fhp1Odd, prefix, "fhalf")
    cs_lhalf <- sumz_trial(cs, fhp2Odd:nc_cs, prefix, "lhalf")
  }

  # Difference between last and first trial
  cs_lftdiff <-
    cs %>% dplyr::select(1, utils::tail(names(.), 1)) %>%
    apply(1, diff) %>%
    tibble::enframe(name = NULL, value = paste(prefix, "lftdiff", sep = "_"))

  cs_half <- dplyr::bind_cols(cs_fhalf, cs_lhalf)

  # Select two and bottom 10% of trials
  pertrial <- round((10 / 100 * nc_cs))
  t10p <- sumz_trial(cs,1:pertrial, prefix, "t10per")
  b10p <- sumz_trial(cs, nc_cs:(nc_cs - pertrial), prefix, "b10per")

  # All trials but the first trials excluded
  csmf <- #sumz_trial(cs, -1, prefix, "minfirst")
    cs %>% dplyr::select(-1) %>%
    rowMeans(na.rm = na.rm) %>%
    tibble::enframe(name = NULL, value = paste(prefix, "minfirst", sep = "_"))

  # All trials
  csall <- cs %>%
    rowMeans(na.rm = na.rm) %>%
    tibble::enframe(name = NULL, value = paste(prefix, "all", sep = "_"))

  # 33% of the trials
  pertrial <- round((33 / 100 * nc_cs))
  t33p <- sumz_trial(cs, 1:pertrial, prefix, "t33per")
  m33p <- sumz_trial(cs, pertrial + 1:(nc_cs - pertrial) - 1, prefix, "m33per")
  b33p <- sumz_trial(cs, nc_cs:(nc_cs - pertrial), prefix, "b33per")

  # first and last trial
  ftrial <- sumz_trial(cs, 1, prefix, "ftrial")
  ltrial <- sumz_trial(cs, nc_cs, prefix, "ltrial")

  # 20% of the trials
  pertrial <- round((20 / 100 * nc_cs))
  t20p <- sumz_trial(cs, 1:pertrial, prefix, "t20per")
  b20p <- sumz_trial(cs, nc_cs:(nc_cs - pertrial), prefix, "b20per")

  # first 2 and last 2 trial
  f2t <- sumz_trial(cs, 1:2, prefix, "f2trial")
  l2t <- sumz_trial(cs, nc_cs:(nc_cs - 1), prefix, "l2trial")

  # Split trials per 2
  splitlist <- split(1:nc_cs, ceiling(seq_along(1:nc_cs) / 2))
  sp2 <-
    purrr::map_dfc(splitlist, function(x)
      dplyr::select(cs, x) %>% rowMeans())
  colnames(sp2) <- paste0(prefix, "_per2_", 1:ncol(sp2))

  # Last five trials
  l5t <- sumz_trial(cs, nc_cs:(nc_cs - 5), prefix, "l5trial")

  res <-
    dplyr::bind_cols(
      subj,
      cs_initial,
      t10p,
      b10p,
      csmf,
      csall,
      t33p,
      m33p,
      b33p,
      cs_fhalf,
      cs_lhalf,
      ftrial,
      ltrial,
      t20p,
      b20p,
      f2t,
      l2t,
      sp2,
      l5t
    ) %>%
    tidyr::as_tibble()

  return(res)
}
