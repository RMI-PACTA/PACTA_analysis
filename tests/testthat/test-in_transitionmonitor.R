test_that("outputs FALSE if using misspelling 'bound' as 'bond'", {
  expect_false(in_transitionmonitor("/bond/is/wrong/it/should/be/bound"))
})

test_that("outputs FALSE if in  a path close but not like transition monitor", {
  expect_false(in_transitionmonitor("/elsewhere"))
  expect_false(in_transitionmonitor("bound"))
  expect_false(in_transitionmonitor("/BOUND"))
})

test_that("outputs TRUE if in a path like transition monitor's docker", {
  expect_true(in_transitionmonitor("/bound"))
  expect_true(in_transitionmonitor("/bound/"))
  expect_true(in_transitionmonitor("/bound/a/"))
  expect_true(in_transitionmonitor("/bound/a/b.txt"))
})
