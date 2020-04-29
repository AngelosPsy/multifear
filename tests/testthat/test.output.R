data("example_data")
cs1 <- paste0("CSP", 1:10)
cs2 <- paste0("CSM", 1:10)
subj = "id"
time = TRUE
group = "group"
bf_data <- example_data %>% dplyr::filter(id %in% c(1, 2, 3, 4))

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

# This one takes a lot of time.
#test_that("rm_banova_mf works", {
#  expect_known_output(rm_banova_mf(cs1, cs2, subj = subj, data = bf_data), tmp)
#})

# This one takes a lot of time.
#test_that("rm_banova_mf for groups works", {
#  expect_known_output(rm_banova_mf(cs1, cs2, subj = subj, group = group, data = bf_data), tmp)
#})
