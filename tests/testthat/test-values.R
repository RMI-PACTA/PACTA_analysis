test_that("default_dirpath is a character vector", {
  expect_true(
    inherits(default_dirpath, "character")
  )
})

test_that("default_dirpath has only one element", {
  expect_true(
    length(default_dirpath) == 1
  )
})


test_that("data_object_names is a character vector", {
  expect_true(
    inherits(data_object_names, "character")
  )
})


test_that("default_filenames_sans_ext is a character vector", {
  expect_true(
    inherits(default_filenames_sans_ext, "character")
  )
})

test_that("default_filenames_sans_ext is a named vector", {
  expect_false(
    is.null(attr(default_filenames_sans_ext, "names"))
  )
})

test_that("default_filenames_sans_ext uses names that correspond with data_object_names", {
  expect_true(
    all(names(default_filenames_sans_ext) %in% data_object_names)
  )
})


test_that("common_filenames_sans_ext is a list", {
  expect_true(
    inherits(common_filenames_sans_ext, "list")
  )
})

test_that("common_filenames_sans_ext is a list of only character vectors", {
  expect_true(
    all(sapply(common_filenames_sans_ext, inherits, "character"))
  )
})

test_that("common_filenames_sans_ext only has elements that are named accordingly to data_object_names", {
  expect_true(
    all(names(common_filenames_sans_ext) %in% data_object_names)
  )
})

test_that("common_filenames_sans_ext contains the default filename for for each object name", {
  expect_true(
    all(
      sapply(names(common_filenames_sans_ext), function(x) {
        default_filenames_sans_ext[[x]] %in% common_filenames_sans_ext[[x]]
      })
    )
  )
})


test_that("sector_list is a character vector", {
  expect_true(
    inherits(sector_list, "character")
  )
})


test_that("other_sector_list is a character vector", {
  expect_true(
    inherits(other_sector_list, "character")
  )
})


test_that("cb_groups is a character vector", {
  expect_true(
    inherits(cb_groups, "character")
  )
})


test_that("sb_groups is a character vector", {
  expect_true(
    inherits(sb_groups, "character")
  )
})
