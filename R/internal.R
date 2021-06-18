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

group_warning = function(group) {
  if (!is.character(group)) {
    stop("The group variable is not a character object. Function terminated.")
  }
}

between_warning = function(between) {
  if (!is.character(between)) {
    stop("The between variable is not a character object. Function terminated.")
  }
}

data_warning = function(data) {
  if (!is.data.frame(data)) {
    stop("The data object is not a data frame. Function terminated.")
  }
}

collection_warning = function(cs1,
                              cs2 = NULL,
                              cs3 = NULL,
                              data,
                              subj = NULL,
                              group = NULL,
                              between = NULL,
                              cs_paired = NULL) {
  cs_warning(cs1)
  data_warning(data)

  if (!is.null(cs2)) { cs_warning(cs2) }
  if (!is.null(cs3)) { cs_warning(cs3) }
  if (!is.null(subj)) { subj_warning(subj) }
  if (!is.null(group)) { group_warning(group) }
  if (!is.null(between)) { between_warning(between) }
  if (!is.null(cs_paired)) { cs_warning(cs_paired) }
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

select_term = function(obj, term, dv = "scr", exclusion = "full data"){
  if(exists("obj")){
    summ <- summary(obj)

    # meta.analytic effect size
    st <- data.frame(summ$tTable)
    st_es <-
      effectsize::t_to_d(t = st$t.value, df = st$DF) %>%
      dplyr::mutate(param = rownames(st)) %>%
      dplyr::filter(param == term) %>% data.frame()

    valz <- summary(obj)$tTable %>%
      data.frame() %>%
      dplyr::mutate(model = rownames(.)) %>%
      dplyr::filter(model %in% !!dplyr::enquo(term)) %>%
      dplyr::select(Value, t.value, p.value)

    res <- tibble::tibble(
      x = term,
      y = dv,
      exclusion = exclusion,
      model = "mixed_model",
      controls = NA,
      method = paste("mixed_model", x),
      p.value = valz$p.value,
      effect.size = NA,
      efffect.size.ma = st_es$d,
      effect.size.ma.lci = st_es$CI_low,
      effect.size.ma.hci = st_es$CI_high,
      estimate = valz$Value,
      statistic = valz$t.value,
      conf.low = NA,
      conf.high = NA
    )
  } else{
    tibble::tibble(
      x = term,
      y = dv,
      exclusion = exclusion,
      model = "mixed_model",
      controls = NA,
      method = paste("mixed_model", x),
      p.value = NA,
      effect.size = NA,
      efffect.size.ma = st_es$d,
      effect.size.ma.lci = st_es$CI_low,
      effect.size.ma.hci = st_es$CI_high,
      estimate = NA,
      efffect.size.ma = NA,
      statistic = NA,
      conf.low = NA,
      conf.high = NA
    )
  }
  res <- res %>%
    dplyr::mutate(data_used = list(summ$data))

  return(res)
}

data_preparation_anova = function(cs1,
                            cs2,
                            cs3 = NULL,
                            data,
                            subj,
                            time = TRUE,
                            group = NULL,
                            between = NULL){
  # Check data
  collection_warning(
    cs1 = cs1,
    cs2 = cs2,
    cs3 = cs3,
    data = data,
    subj = subj,
    group = group,
    between = between
  )

  cs1 <-
    data %>% dplyr::select(all_of(!!dplyr::enquo(cs1))) %>% tibble::as_tibble()
  cs2  <-
    data %>% dplyr::select(all_of(!!dplyr::enquo(cs2))) %>% tibble::as_tibble()
  subj <-
    data %>% dplyr::select(all_of(!!dplyr::enquo(subj))) %>% tibble::as_tibble()

  # Renaming objects to make life a bit easier
  cs1  <- cs1 %>% dplyr::select(cs1_ = dplyr::everything())
  cs2  <- cs2 %>% dplyr::select(cs2_ = dplyr::everything())
  subj <- subj %>% dplyr::select(subj = dplyr::everything())

  if(!is.null(cs3)){
    cs3  <-
      data %>% dplyr::select(all_of(!!dplyr::enquo(cs3))) %>% tibble::as_tibble()
    cs3  <- cs3 %>% dplyr::select(cs3_ = dplyr::everything())
    }

  if (is.null(group)) {
    group_new <-
      data %>%
      dplyr::mutate(group = rep("NULL", nrow(data))) %>%
      dplyr::select(group)
    group <- NULL
  } else{
    group_new <- data %>%
      dplyr::select(all_of(!!dplyr::enquo(group))) %>%
      tibble::as_tibble() %>%
      dplyr::rename(group = eval(group))
  }

  # Add this in case of a cs3 stimulus
  if(!is.null(cs3)){
    data <- dplyr::bind_cols(data, cs3)
  }

  # Add this in case of a between stimulus
  if(is.null(between)){
    between_new <-
      data %>%
      dplyr::mutate(between = rep("NULL", nrow(data))) %>%
      dplyr::select(between)
    between <- NULL
  } else{
    between_new <- data %>% dplyr::select(all_of(!!dplyr::enquo(between))) %>% tibble::as_tibble()
    colnames(between_new) <- "between"
    #between  <- between %>% dplyr::select(between = dplyr::everything())
  }

  data <- dplyr::bind_cols(subj, cs1, cs2, group_new, between_new)

  #data <- dplyr::bind_cols(data, between_new)

  if (time) {
    # Check if length of cs1 and cs2 is the same. Otherwise stop
    if (ncol(cs1) != ncol(cs2)) {
      stop(
        "You have selected that you want to analyse time effects but the
        cs1 and cs2 have different number of time points. Stopping function
        now."
      )
    }}

 # if (is.null(between)){
  # data %>%
  #  reshape2::melt(
  #    id.var = c("subj", "group"),
  #    variable.name = "var_old",
  #    value.name = "resp",
  #    factorsAsStrings = TRUE
  #  ) %>% # Until pivot_longer gets better
  #  dplyr::mutate(
  #    cs = as.factor(stringr::str_sub(var_old, 1, 3)),
  #    time = as.factor(sub(".*_", "", .$var_old)), # Better than stringr
  #    subj = as.factor(subj),
  #    group = as.factor(group)
  #  ) -> data
  #} else {
    data %>%
      reshape2::melt(
        id.var = c("subj", "group", "between"),
        variable.name = "var_old",
        value.name = "resp",
        factorsAsStrings = TRUE
      ) %>% # Until pivot_longer gets better
      dplyr::mutate(
        cs = as.factor(stringr::str_sub(var_old, 1, 3)),
        time = as.factor(sub(".*_", "", .$var_old)), # Better than stringr
        subj = as.factor(subj),
        group = as.factor(group),
        between = as.factor(between)
      ) -> data
    # }

  colnames(data) <- make.names(colnames(data))

  res <- data

  return(res)
}

data_preparation_ttest = function(cs1,
                                  cs2,
                                  data,
                                  subj,
                                  group = NULL,
                                  na.rm = FALSE){
  collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj)

  # Restructure data. rowMeans is used in case multiple trails have been fed
  cs1 <-
    data %>% dplyr::select(all_of(!!dplyr::enquo(cs1))) %>%
    rowMeans(na.rm = na.rm) %>% tibble::enframe(name = NULL)  %>%
    dplyr::rename(cs.1 = value)
  cs2 <-
    data %>% dplyr::select(all_of(!!dplyr::enquo(cs2))) %>%
    rowMeans(na.rm = na.rm) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(cs.2 = value)
  subj <-
    data %>% dplyr::select(all_of(!!dplyr::enquo(subj))) %>%
    tibble::as_tibble() %>%
    dplyr::select(subj = dplyr::everything())

  tmp_data <- dplyr::bind_cols(subj, cs1, cs2)

  if(is.null(group)){
    tmp_data <- tmp_data %>% dplyr::mutate(group = "NULL")
  } else{
    groupz <- data %>% dplyr::select(all_of(!!dplyr::enquo(group))) %>% unlist()
    if(length(unique(groupz)) != 2){
      stop("Number of levels for group is different than 2. t-test did not run.")
    }
    tmp_data <- tmp_data %>% dplyr::mutate(group = groupz)
    # Create dummy variable for regression
    tmp_data <- tmp_data %>%
      fastDummies::dummy_cols(select_columns = group,
                              remove_first_dummy = TRUE)
  }

  res <- tmp_data

  return(res)
}

data_preparation_verse = function(cs1,
                                  cs2,
                                  cs3 = NULL,
                                  data,
                                  subj,
                                  group = NULL,
                                  between = NULL){

      collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj)

      # Prepare data for multiple analyses
      cs1  <-
        data %>% dplyr::select(all_of(!!dplyr::enquo(cs1))) %>% tibble::as_tibble()
      cs2  <-
        data %>% dplyr::select(all_of(!!dplyr::enquo(cs2))) %>% tibble::as_tibble()
      subj <-
        data %>% dplyr::select(all_of(!!dplyr::enquo(subj))) %>% tibble::as_tibble()

      # Renaming objects to make life a bit easier
      cs1  <- cs1 %>% dplyr::select(cs1_ = dplyr::everything())
      cs2  <- cs2 %>% dplyr::select(cs2_ = dplyr::everything())
      subj <- subj %>% dplyr::select(subj = dplyr::everything())

      # What happens in case of groups
      if (is.null(group)) {
        group_new <-
          data %>%
          dplyr::mutate(group = rep("NULL", nrow(data))) %>%
          dplyr::select(group)
        group = NULL
      } else {
        group_new <- data %>%
          dplyr::select(tidyselect::all_of(!!dplyr::enquo(group))) %>%
          dplyr::rename(group = eval(group))
      }

      res_tmp <- dplyr::bind_cols(subj, cs1, cs2, group_new)

      if(!is.null(cs3)){
        cs3  <-
          data %>% dplyr::select(all_of(!!dplyr::enquo(cs3))) %>% tibble::as_tibble()
        cs3  <- cs3 %>% dplyr::select(cs3_ = dplyr::everything())
        res_tmp <- dplyr::bind_cols(res_tmp, cs3)
      }

      if(!is.null(between)){
        between_new <- data %>%
          dplyr::select(tidyselect::all_of(!!dplyr::enquo(between))) %>%
          dplyr::rename(between = eval(between))
        res_tmp <- dplyr::bind_cols(res_tmp, between_new)
      }

    res <- res_tmp

      return(res)
}

#
sumz_trial <- function(data,
           index,
           prefix = "pre",
           postfix = "post",
           na.rm = TRUE) {
    res <- data %>% dplyr::select(index) %>%
      rowMeans(na.rm = na.rm) %>%
      tibble::enframe(name = NULL, value = paste(prefix, postfix, sep = "_"))
    return(res)
  }

exclude_cases <- function(excl_data_sets){
  # exclude_data_sets should be generated by the multifear::exclusion_criteria
  # function
  # Exclude cases
  # Remove empty data -- this can happen
  # if you have removed too many participants
  excl_data_sets <-
    excl_data_sets %>%
    dplyr::mutate(excl = excl_data_sets$used_data %>%
                    lapply(plyr::empty) %>%
                    data.frame() %>% t())

  # Return warning if you have excluded any of the data sets
  if (any(excl_data_sets$excl == TRUE)) {
    warning(
      paste0(
        "Part of the analyses were excluded due to many excluded cases. Check
        the following cases: ",
        paste(
          excl_data_sets$names[excl_data_sets$excl == TRUE],
          "with cutoff =",
          excl_data_sets$cutoff[excl_data_sets$excl == TRUE]
        ),
        ".\n",
        collapse = ""
      )
      )
  }

  # You exclude the empty cases here
  res <- excl_data_sets %>%
    dplyr::filter(excl == FALSE)

  return(res)
}

# formats results. This function is taken from the specr package
format_results <- function(df, null = 0, desc = FALSE) {
  if (isFALSE(desc)) {
    df <- df %>%
      dplyr::arrange(.data$estimate)
  } else {
    df <- df %>%
      dplyr::arrange(desc(.data$estimate))
  }

  df <- df %>%
    dplyr::mutate(specifications = 1:dplyr::n(),
                  color = dplyr::case_when(conf.low > null ~ "red", #"#377eb8",
                                    conf.high < null ~ "blue", #"#e41a1c",
                                    TRUE ~ "darkgrey"))
  return(df)
}
