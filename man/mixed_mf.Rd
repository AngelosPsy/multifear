% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixed_mf.R
\name{mixed_mf}
\alias{mixed_mf}
\title{mixed_mf}
\usage{
mixed_mf(
  cs1,
  cs2,
  data,
  subj,
  group = NULL,
  phase = "acquisition",
  dv = "scr",
  exclusion = "full data",
  cut_off = "full data"
)
}
\arguments{
\item{cs1}{The column name(s) of the conditioned responses for the first conditioned stimulus}

\item{cs2}{The column name(s) of the conditioned responses for the second conditioned stimulus}

\item{data}{A data frame containing all the relevant columns for the analyses}

\item{subj}{The name of the column including the participant numbers. Unique numbers are expected}

\item{group}{the name of the group, if included, default to \code{NULL}}

\item{phase}{The conditioned phase that the analyses refer to. Accepted values are  \code{acquisition}, \code{acq}, \code{extinction}, or \code{ext}}

\item{dv}{name of the measured conditioned response. Default to \code{"SCR"}}

\item{exclusion}{Name of the data reduction procedure used. Default to \code{full data}}

\item{cut_off}{cut off Name of the cut_off applied. Default to \code{full data}}
}
\value{
A data frame with the results.

The data frame returned is the standard one returned in all function in the package. Specifically we have:

A tibble with the following column names:

x: the name of the independent variable (e.g., cs). There, you can see the term of the model that is returned. So, not the full model is returned but only this particular term.

y: the name of the dependent variable as this defined in the \code{dv} argument

exclusion: see \code{exclusion} argument

model: the model that was run (e.g., mixed_model)

controls: ignore this column for this test

method: the model that was run

p.value: the p-value for each factor

effect.size: irrelevant here

effect.size.ma: irrelevant here

effect.size.ma.lci: irrelevant here

effect.size.ma.hci: irrelevant here

statistic: the t-value for each factor

conf.low: the lower confidence interval for the estimate

conf.high: the higher confidence interval for the estimate

data_used: a list with the data used for the specific test
}
\description{
Basic function for running mixed models for the multiverse analysis
}
\details{
The function assumes that you include more than 1 trial per CS. The function returns an error if that is not the function.

The function performs by default two dependent variable standardizations, the one per subject
and the other one without taking subject into account.

In case time is included, the function computes the intercept -- i.e., the 0 point -- on the middle of the time sequence.

The following models are run and compared: a) Intercept only model, b) Intercept plus CS model, and c) Intercept plus CS \code{x} Time interaction.

Separate models are run with `Subject` as random factor, as well as `Subject and Time` as random factors.

The model is fit by maximizing the log-likelihood (i.e., "ML" term in nlme::lme).

The model comparison is done using `BIC`.
}
\examples{
# Load example data
data(example_data)

cs1 <- paste0("CSP", 1:2)
cs2 <- paste0("CSM", 1:2)
subj <- "id"

# mixed models without groups
mixed_mf(cs1 = cs1, cs2 = cs2, subj = subj, data = example_data)

# mixed models with groups
mixed_mf(cs1 = cs1, cs2 = cs2, subj = subj, group = "group", data = example_data)


}
\seealso{
\code{\link[nlme]{lme}}
}
