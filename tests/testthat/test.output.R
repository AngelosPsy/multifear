context("Functions for statistical tests")

library(dplyr)
data("example_data", package = "multifear")
cs1 <- paste0("CSP", 1:10)
cs2 <- paste0("CSM", 1:10)
subj = "id"
time = TRUE
group = "group"
data = example_data
bf_data <- example_data %>% dplyr::filter(id %in% c(1, 2, 3, 4, 5, 7, 8, 9, 10))

tmp <- tempfile()

test_that("bt_test_mf works", {
  expect_known_output(bt_test_mf(cs1, cs2, subj = subj, data = example_data), tmp)
})

test_that("bt_test_mf for groups works", {
  expect_known_output(bt_test_mf(cs1, cs2, subj = subj, group = group, data = example_data), tmp)
})

test_that("t_test_mf works", {
  expect_known_output(t_test_mf(cs1, cs2, subj = subj, data = example_data), tmp)
})

test_that("t_test_mf for groups works", {
  expect_known_output(t_test_mf(
    cs1,
    cs2,
    subj = subj,
    group = group,
    data = example_data
  ),
  tmp)
})

test_that("rm_anova_mf works", {
  expect_known_output(rm_anova_mf(cs1, cs2, subj = subj, data = example_data), tmp)
})

test_that("rm_anova_mf for groups works", {
  expect_known_output(rm_anova_mf(cs1, cs2, subj = subj, group = group, data = example_data), tmp)
})

test_that("rm_banova_mf works", {
  expect_known_output(rm_banova_mf(cs1, cs2, subj = subj, data = example_data), tmp)
})

test_that("rm_banova_mf for groups works", {
  expect_known_output(rm_banova_mf(cs1, cs2, subj = subj, group = group, data = example_data), tmp)
})

#test_that("lm works", {
#  expect_known_output(lm_mf(cs1, cs2, subj = subj, data = bf_data), tmp)
#})

#test_that("lm for groups works", {
#  expect_known_output(lm_mf(cs1, cs2, subj = subj, group = group, data = bf_data), tmp)
#})

test_that("chop cs works", {
  expect_known_output(chop_cs(cs = cs1, data = bf_data, subj = subj), tmp)
})

test_that("chop cs for groups works", {
  expect_known_output(chop_cs(cs = cs1, data = bf_data, subj = subj, group = group), tmp)
})

test_that("chop css works", {
  expect_known_output(chop_css(cs1 = cs1, cs2 = cs2, data = bf_data, subj = subj, group = group), tmp)
})

test_that("combine cs works", {
  expect_known_output(combine_cs(cs1 = cs1, cs2 = cs2, data = bf_data), tmp)
})

context("Functions for universe")

test_that("universe works", {
  expect_known_output(universe_cs(cs1, cs2, subj = subj, data = example_data, include_bayes = FALSE), tmp)
})

test_that("universe works mixed", {
  expect_known_output(universe_cs(cs1, cs2, subj = subj, data = example_data, include_bayes = FALSE, include_mixed = TRUE), tmp)
})

test_that("universe works with groups", {
  expect_known_output(universe_cs(cs1, cs2, subj = subj, data = example_data, group = group, include_bayes = FALSE), tmp)
})

context("Functions for multiverse")

test_that("multiverse works", {
  expect_known_output(multiverse_cs(cs1, cs2, subj = subj, data = example_data, include_bayes = FALSE), tmp)
})

test_that("multiverse works with groups", {
  expect_known_output(multiverse_cs(cs1, cs2, subj = subj, data = example_data, group = group, include_bayes = FALSE), tmp)
})

#context("Functions plots")

#test_that("plots", {
#  tmp <- universe_cs(cs1, cs2, subj = subj, data = example_data, include_bayes = FALSE)
#  vdiffr::expect_doppelganger("forestplot", forestplot_mf(tmp, new_page = FALSE))
#})
