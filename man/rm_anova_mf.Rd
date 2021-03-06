% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rm_anova_mf.R
\name{rm_anova_mf}
\alias{rm_anova_mf}
\title{rm_anova_mf}
\usage{
rm_anova_mf(
  cs1,
  cs2,
  data,
  subj,
  time = TRUE,
  group = NULL,
  phase = "acquisition",
  dv = "scr",
  exclusion = "full data",
  cut_off = "full data",
  correction = FALSE
)
}
\arguments{
\item{cs1}{The column name(s) of the conditioned responses for the first conditioned stimulus}

\item{cs2}{The column name(s) of the conditioned responses for the second conditioned stimulus}

\item{data}{A data frame containing all the relevant columns for the analyses}

\item{subj}{The name of the column including the participant numbers. Unique numbers are expected}

\item{time}{should time be included? Default to \code{TRUE}}

\item{group}{the name of the group, if included, default to \code{NULL}}

\item{phase}{The conditioned phase that the analyses refer to. Accepted values are  \code{acquisition}, \code{acq}, \code{extinction}, or \code{ext}}

\item{dv}{name of the measured conditioned response. Default to \code{"SCR"}}

\item{exclusion}{Name of the data reduction procedure used. Default to \code{full data}}

\item{cut_off}{cut off Name of the cut_off applied. Default to \code{full data}}

\item{correction}{whether the Greenhouse-Geisser correction should be applied or not. Default to \code{FALSE}}
}
\value{
A basic function for running repeated measures ANOVAs.

A tibble with the following column names:

x: the name of the independent variable (e.g., cs)

y: the name of the dependent variable as this defined in the \code{dv} argument

exclusion: see \code{exclusion} argument

model: the model that was run (e.g., t-test)

controls: ignore this column for this test

method: the model that was run

p.value: the p-value of the test

effect.size: the estimated effect size

effect.size.ma: the estimated effect size for the meta-analytic plots

effect.size.ma.lci: low confidence intervals for the meta-analytic effect size

effect.size.ma.hci: high confidence intervals for the meta-analytic effect size

estimate: the estimate of the test run

statistic: the F-value

conf.low: the lower confidence interval for the estimate

conf.high: the higher confidence interval for the estimate

framework: were the data analysed within a NHST or Bayesian framework?

data_used: a list with the data used for the specific test
}
\description{
Basic function for running the frequentist's repeated measures analysis of variance (ANOVA)
}
\details{
In case the \code{time} argument is set to \cite{TRUE} (default value), the function will include this as a within subjects factor, assuming that the columns in
\code{cs1} and \code{cs2} correspond to ascending time points (e.g., cs1
trial 1, cs1 trial 2 ... cs1 trial \code{n}). If this is not the case, the
results are not to be trusted.

The function uses the \code{ez::ezANOVA} function. The function gives by default a warning regarding the collapsing of factors. This function here suppresses this warning but the user should be aware of it. Please note that at the moment no sphericity correction is performed. The reported effect size is omega squared as this is computed by \code{sjstats::omega_sq}. The meta-analytic effect size is eta squared.
}
\examples{
# Load example data
data(example_data)

# Briefly define argument values that will be plugged in later on in the functions
cs1 <- paste0("CSP", 1:10)
cs2 <- paste0("CSM", 1:10)
subj <- "id"
group <- "group"

# Repeated measures ANOVA without groups
rm_anova_mf(cs1 = cs1, cs2 = cs2, subj = subj, data = example_data, time = TRUE)

# Repeated measures ANOVA with groups
rm_anova_mf(cs1 = cs1, cs2 = cs2, subj = subj, group = "group",
data = example_data, time = TRUE)
}
