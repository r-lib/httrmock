
context("status")

test_that("mocking_status", {

  stop_recording()
  stop_replaying()
  expect_equal(mocking_status(), c(recording = FALSE, replaying = FALSE))

  start_recording()
  expect_equal(mocking_status(), c(recording = TRUE, replaying = FALSE))

  start_replaying()
  expect_equal(mocking_status(), c(recording = TRUE, replaying = TRUE))

  stop_recording()
  expect_equal(mocking_status(), c(recording = FALSE, replaying = TRUE))

  stop_replaying()
  expect_equal(mocking_status(), c(recording = FALSE, replaying = FALSE))
})
