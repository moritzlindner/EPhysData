replace_hdf_field <- function(path, field, value) {
  handle <- hdf5r::H5File$new(path, mode = "r+")
  on.exit(handle$close_all(), add = TRUE)
  handle$link_delete(field)
  handle[[field]] <- value
}

test_that("HDF5 round trips load data safely and restore callbacks only by opt-in", {
  path <- tempfile(fileext = ".h5")
  on.exit(unlink(path), add = TRUE)
  x <- legacy_fixture(n = 5L, trials = 1L)
  FilterFunction(x) <- function(values) values * 2
  Save(x, path)
  expect_error(Save(x, path), "file already exists", class = "ephys_io_error")
  expect_warning(safe <- Load(path), "built-in defaults.*filter.fx", class = "ephys_io_warning")
  expect_equal(dim(safe), c(5L, 1L))
  expect_equal(as.numeric(GetData(safe)), as.numeric(x@Data))
  restored <- Load(path, restore_callbacks = TRUE)
  expect_equal(as.numeric(GetData(restored)), as.numeric(GetData(x)))

  marker <- tempfile()
  on.exit(unlink(marker), add = TRUE)
  replace_hdf_field(path, "average.fx", paste0(
    "file.create(", encodeString(marker, quote = '"'), "); function(x) x"
  ))
  expect_warning(safe <- Load(path), "built-in defaults.*average.fx", class = "ephys_io_warning")
  expect_false(file.exists(marker))
  expect_warning(restored <- Load(path, restore_callbacks = TRUE),
                 "Could not restore callback.*average.fx.*Expected a single function",
                 class = "ephys_io_warning")
  expect_false(file.exists(marker))
  expect_true(is.function(AverageFunction(restored)))
  replace_hdf_field(path, "average.fx", "function(")
  expect_warning(restored <- Load(path, restore_callbacks = TRUE),
                 "Could not restore callback.*average.fx", class = "ephys_io_warning")
  expect_true(is.function(AverageFunction(restored)))
})

test_that("failed saves preserve existing HDF5 data and load errors name the field", {
  path <- tempfile(fileext = ".h5")
  on.exit(unlink(path), add = TRUE)
  x <- legacy_fixture(n = 4L)
  Save(x, path)
  broken <- x
  broken@TimeTrace <- broken@TimeTrace[1L]
  expect_error(Save(broken, path, overwrite = TRUE), "Save.*failed", class = "ephys_io_error")
  set <- newEPhysSet(list(x, broken), data.frame(recording = c("a", "b")))
  expect_error(Save(set, path, overwrite = TRUE), "Save.*Recording 2", class = "ephys_io_error")
  expect_equal(as.numeric(Load(path)@Data), as.numeric(x@Data))
  replace_hdf_field(path, "Type", "Unknown")
  expect_error(Load(path), "Unsupported HDF5 Type.*Unknown", class = "ephys_io_error")
  replace_hdf_field(path, "Type", "EPhysData")
  expect_s4_class(Load(path), "EPhysData")
})

test_that("HDF5 sets retain recording order and empty stimulus traces", {
  path <- tempfile(fileext = ".h5")
  on.exit(unlink(path), add = TRUE)
  a <- legacy_fixture(n = 4L)
  a@StimulusTrace <- units::as_units(numeric(), "1")
  b <- legacy_fixture(n = 6L)
  x <- newEPhysSet(list(a, b), data.frame(recording = c("a", "b")))
  Save(x, path)
  out <- Load(path)
  expect_equal(Metadata(out), Metadata(x))
  expect_equal(dim(out[[1L]]), c(4L, 2L))
  expect_equal(dim(out[[2L]]), c(6L, 2L))
  expect_length(out[[1L]]@StimulusTrace, 0)
})
