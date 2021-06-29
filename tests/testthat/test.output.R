library(dplyr)
#withr::deferred_run()
local_edition(3)

data("example_data")
cs1 <- paste0("CSP", 1:10)
cs2 <- paste0("CSM", 1:10)
cs3 <- paste0("CSU", 1:10)
between <- "counterbalancing"
subj = "id"
time = TRUE
group = "group"
between = "counterbalancing"
data = example_data
bf_data <- example_data %>% dplyr::filter(id %in% c(1, 2, 3, 4, 5, 7, 8, 9, 10))

tmp <- tempfile()

test_that("bt_test_mf works", {
  expect_snapshot(bt_test_mf(cs1, cs2, subj = subj, data = example_data))
})

test_that("bt_test_mf for groups works", {
  expect_snapshot(bt_test_mf(cs1, cs2, subj = subj, group = group, data = example_data))
})

test_that("t_test_mf works", {
  expect_snapshot(t_test_mf(cs1, cs2, subj = subj, data = example_data))
})

test_that("t_test_mf for groups works", {
  expect_snapshot(t_test_mf(
    cs1 = cs1,
    cs2 = cs2,
    subj = subj,
    group = group,
    data = example_data
  ))
})

test_that("rm_anova_mf works", {
  expect_snapshot(rm_anova_mf(cs1, cs2, subj = subj, data = example_data, time = FALSE, group = NULL, between = NULL))
})

test_that("rm_anova_mf 3 stimuli works", {
  expect_snapshot(rm_anova_mf(cs1, cs2, cs3, subj = subj, data = example_data, time = FALSE, group = NULL, between = NULL))
})


test_that("rm_anova_mf for groups works", {
  expect_snapshot(rm_anova_mf(cs1, cs2, subj = subj, group = group, data = example_data))
})

test_that("rm_anova_mf 3 stimuli works and group", {
  expect_snapshot(rm_anova_mf(cs1, cs2, cs3, subj = subj, data = example_data, time = FALSE, group = group, between = NULL))
})

test_that("rm_banova_mf works", {
  expect_snapshot(rm_banova_mf(cs1, cs2, subj = subj, data = example_data))
})

test_that("rm_banova_mf for groups works", {
  expect_snapshot(rm_banova_mf(cs1, cs2, subj = subj, group = group, data = example_data))
})

#test_that("lm works", {
#  expect_snapshot(lm_mf(cs1, cs2, subj = subj, data = bf_data))
#})

#test_that("lm for groups works", {
#  expect_snapshot(lm_mf(cs1, cs2, subj = subj, group = group, data = #bf_data))
#})

test_that("chop cs works", {
  expect_snapshot(chop_cs(cs = cs1, data = bf_data, subj = subj))
})

test_that("chop cs for groups works", {
  expect_snapshot(chop_cs(cs = cs1, data = bf_data, subj = subj, group = group))
})

test_that("chop css works", {
  expect_snapshot(chop_css(cs1 = cs1, cs2 = cs2, data = bf_data, subj = subj, group = group))
})

test_that("combine cs works", {
  expect_snapshot(combine_cs(cs1 = cs1, cs2 = cs2, data = bf_data))
})

test_that("universe works", {
  expect_snapshot(universe_cs(cs1, cs2, subj = subj, data = example_data, include_bayes = FALSE))
})

test_that("universe works mixed", {
  expect_snapshot(universe_cs(cs1, cs2, subj = subj, data = example_data, include_bayes = FALSE, include_mixed = TRUE))
})

test_that("universe works with groups", {
  expect_snapshot(universe_cs(cs1, cs2, subj = subj, data = example_data, group = group, include_bayes = FALSE))
})

test_that("multiverse works", {
  expect_snapshot(multiverse_cs(cs1, cs2, subj = subj, data = example_data, include_bayes = FALSE))
})

test_that("multiverse works with groups", {
  expect_snapshot(multiverse_cs(cs1, cs2, subj = subj, data = example_data, group = group, include_bayes = FALSE))
})

#test_that("plots", {
#  tmp <- universe_cs(cs1, cs2, subj = subj, data = example_data, include_bayes = FALSE)
#  vdiffr::expect_doppelganger("forestplot", forestplot_mf(tmp, new_page = #FALSE))
#})x
