#' chop_css
#'
#' @description Function for separating the conditioned responses into multiple pieces for two CSs
#' @inheritParams t_test_mf
#' @export

chop_css <- function(cs1, cs2, data, subj, na.rm = FALSE) {
  # Check data
  collection_warning(
    cs1 = cs1,
    cs2 = cs2,
    data = data,
    subj = subj
  )

  # Stop in case of no equal lengths of cs1 and cs2
  if (length(cs1) != length(cs2)) {
    stop("No equal length of the two CSs.")
  }

  cs1 <- multifear::chop_cs(cs = cs1, data = data, subj = subj)
  cs2 <- multifear::chop_cs(cs = cs2, data = data, subj = subj)

  # Compute differential scores
  csbind <- dplyr::bind_rows(cs2 %>% tibble::rownames_to_column(),
                             cs1 %>% tibble::rownames_to_column()) %>%
    dplyr::select(-id) %>%
    dplyr::group_by(rowname) %>%
    dplyr::summarise_all(diff) %>%
    dplyr::mutate(id = data$id) %>%
    dplyr::select(-rowname)

  res <- dplyr::bind_cols(data, csbind)

  class(res) <- c("chop_css", class(res))

  return(res)
}
