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
                              subj = NULL,
                              cs_paired = NULL) {
  cs_warning(cs1)
  data_warning(data)

  if (!is.null(cs2)) { cs_warning(cs2) }
  if (!is.null(subj)) { subj_warning(subj)}
  if (!is.null(cs_paired)) { cs_warning(cs_paired)}
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

    # meta.analytic effect size. I do not think this is correct so consider
    # deleting
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
      efffect.size.ma = NA,#st_es$d,
      effect.size.ma.lci = NA,#st_es$CI_low,
      effect.size.ma.hci = NA,#st_es$CI_high,
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
      efffect.size.ma = NA,#st_es$d,
      effect.size.ma.lci = NA,#st_es$CI_low,
      effect.size.ma.hci = NA,#st_es$CI_high,
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
                            data,
                            subj,
                            time = TRUE,
                            group = NULL){
  # Check data
  collection_warning(cs1 = cs1, cs2 = cs2, data = data, subj = subj)

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

  data <- dplyr::bind_cols(subj, cs1, cs2, group_new)

  # In case time is selected, create a time object
  if (time) {
    # Check if length of cs1 and cs2 is the same. Otherwise stop
    if (ncol(cs1) != ncol(cs2)) {
      stop(
        "You have selected that you want to analyse time effects but the
        cs1 and cs2 have different number of time points. Stopping function
        now."
      )
    }
    }
  data %>%
    reshape2::melt(
      id.var = c("subj", "group"),
      variable.name = "var_old",
      value.name = "resp",
      factorsAsStrings = TRUE
    ) %>% # Until pivot_longer gets better
    dplyr::mutate(
      cs = as.factor(stringr::str_sub(var_old, 1, 3)),
      time = as.factor(sub(".*_", "", .$var_old)), # Better than stringr
      subj = as.factor(subj),
      group = as.factor(group)
    ) -> data

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
                                  data,
                                  subj,
                                  group = NULL){

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
          #dplyr::mutate(group2 = as.factor(group)) %>%
          #dplyr::select(group2) #%>%
          #dplyr::rename(group = group2)
      }

      res <- dplyr::bind_cols(subj, cs1, cs2, group_new)

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
  excl_data_sets <- excl_data_sets %>%
    dplyr::mutate(excl = purrr::map_lgl(excl_data_sets$used_data, plyr::empty))

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

# Convert t value to eta^2
# See here: https://sites.google.com/site/fundamentalstatistics/chapter-13
t_to_eta2 <- function(ttestRes){
  t <- ttestRes$statistic
  df <- ttestRes$parameter
  res <- as.numeric((t^2)/(t^2 + df))
  return(res)
}

# paired t test for bootstrap first for d to eta2 and then from t to eta2

t_test_ind_boot_d_to_eta2 <- function(x, datz) {
  datz <- dplyr::sample_n(datz, nrow(datz), replace = TRUE)
ttest_es_ma <- effsize::cohen.d(
  datz$cs ~ as.factor(datz$group),
  pooled = TRUE,
  paired = FALSE,
  hedges.correction = FALSE
)
res <- esc::eta_squared(d = ttest_es_ma$estimate)
return(res)
}

t_test_paired_boot_d_to_eta2 <- function(x, datz) {
  datz <- dplyr::sample_n(datz, nrow(datz), replace = TRUE)
  ttest_es_ma <-  effsize::cohen.d(
    unlist(datz %>% dplyr::select(all_of("cs.1"))),
    unlist(datz %>% dplyr::select(all_of("cs.2"))),
    pooled = TRUE,
    paired = TRUE,
    hedges.correction = FALSE,
    conf.level = .90
  )
  res <- esc::eta_squared(d = ttest_es_ma$estimate)
  return(res)
}

# Paired t-test for bootstrap
t_test_paired_boot_t_to_eta2 <- function(x, datz) {
  datz <- dplyr::sample_n(datz, nrow(datz), replace = TRUE)
  t_test <- stats::t.test(
    x = datz$cs.1, y = datz$cs.2,
    paired = TRUE,
    alternative = "two.sided",
    var.equal = TRUE
  )
  res <- t_to_eta2(t_test)
  return(res)
}

# independent t test for bootstrap
t_test_ind_boot_t_to_eta2 <- function(x, datz){
  datz <- dplyr::sample_n(datz, nrow(datz), replace = TRUE)
  t_test <- stats::t.test(
    formula = datz$cs ~ datz$group,
    data = datz,
    paired = FALSE,
    alternative = "two.sided",
    var.equal = TRUE
  )
  res <- t_to_eta2(t_test)
  return(res)
}

t_boot <-
  function(data_t_test,
           paired = TRUE,
           meta.effect = "d_to_eta2",
           quanz = c(.05, .95)) {
    # Add column
    data_t_test <- data_t_test %>%
      dplyr::mutate(cs = cs.1 - cs.2)
    #if(is.null(group_prep)){
    if (paired) {
      if (meta.effect == "d_to_eta2") {
        res_tmp <-
          bootstrap::bootstrap(
            x = 1:nrow(data_t_test),
            nboot = 1000,
            theta = t_test_paired_boot_d_to_eta2,
            datz =  data_t_test
          )

      } else{
        res_tmp <-
          bootstrap::bootstrap(
            x = 1:nrow(data_t_test),
            nboot = 1000,
            theta = t_test_paired_boot_t_to_eta2,
            datz =  data_t_test
          )
      }
    } else{
      if (meta.effect == "d_to_eta2") {
        res_tmp <-
          bootstrap::bootstrap(
            x = 1:nrow(data_t_test),
            nboot = 1000,
            theta = t_test_ind_boot_d_to_eta2,
            datz =  data_t_test
          )
      } else{
        res_tmp <-
          bootstrap::bootstrap(
            x = 1:nrow(data_t_test),
            nboot = 1000,
            theta = t_test_ind_boot_t_to_eta2,
            datz =  data_t_test
          )
      }
    }
    #res <- stats::quantile(esc::eta_squared(res_tmp$thetastar), quanz)
    res <- stats::quantile(res_tmp$thetastar, quanz)
    return(res)
  }

#bootstrap(x = data_t_test, nboot = 1000, theta = t_test_paired_boot)

r_number <- function(num, threeshold_min = 0.001, threeshold_max = 1000) {

  stopifnot("some p-values or Bayes factors are not numbers. Check you data. Stopping operation."=  is.numeric(num))

  if(num < threeshold_min){
   res <- paste("<", threeshold_min)
  } else if(num > threeshold_max){
    res <- paste(">", threeshold_max)
  } else {
    res <- paste(" = ", round(num, 3))
  }

  return(res)
}
