.onLoad <- function (libname, pkgname)
{
  # set global variables in order to avoid CHECK notes
  utils::globalVariables (".")
  utils::globalVariables ("cs")
  utils::globalVariables ("cs1_mean")
  utils::globalVariables ("cs2_mean")
  utils::globalVariables ("res")
  utils::globalVariables ("resp")
  utils::globalVariables ("statistic")
  utils::globalVariables ("term")
  utils::globalVariables ("var_old")
  invisible ()
}
