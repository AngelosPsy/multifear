#' internal
#'
#' @description Internal functions, not available to the user
#' @noRd
#' @export subj_warning
#' @export cs_warning
#' @export data_warning
#' @export collection_warning
#' @keywords internal


subj_warning = function(subj_name) {
  if (!is.character(subj_name)) {
    stop("The subject variable is not a character object. Function terminated.")
  }
}

cs_warning = function(cs) {
  if (!is.character(cs)) {
    stop("The cs variable is not a character object. Function terminated.")
  }
}

data_warning = function(data) {
  if (!is.data.frame(data)) {
    stop("The data object is not a data frame. Function terminated.")
  }
}

collection_warning = function(cs1, cs2 = NULL, data, subj) {
  multifear::cs_warning(cs1)
  if (!is.null) {
    multifear::cs_warning(cs2)
  }
  multifear::data_warning(data)
  multifear::subj_warning(subj)
}
