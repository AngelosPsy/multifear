#' internal
#'
#' @description Internal functions, not available to the user
#' @noRd
#' @noMd
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

collection_warning = function(cs1,
                              cs2 = NULL,
                              data,
                              subj = NULL) {
  cs_warning(cs1)
  if (!is.null(cs2)) {
    cs_warning(cs2)
  }
  data_warning(data)
  if (!is.null(subj)) {
    subj_warning(subj)
  }
}

chop_css_warning = function(data) {
  if (!any(class(data) == "chop_css")) {
    stop("The data object is not of class chop_css. Function terminated.")
  }
}

inference_warning = function(data){
  if (!any(class(data) == "data.frame")) {
    stop("The data object is not of class data frame. Function terminated.")
  }
}

select_term = function(obj, term){
  res <- summary(obj)$tTable %>%
    data.frame() %>%
    dplyr::mutate(model = rownames(.)) %>%
    dplyr::filter(model %in% !!dplyr::enquo(term)) %>%
    dplyr::select(Value, t.value, p.value)

  return(res)
}
