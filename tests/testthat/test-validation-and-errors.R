test_that("event channel validation and selection follow names", {
  x <- event_fixture(list(B = 1.5, A = 0.5))
  expect_identical(Subset(x, Channels = "A")@Data[[1L]], list(A = 0.5))
  expect_named(lapply(x, identity, progress = FALSE), "21")
  expect_identical(lapply(x, identity, progress = FALSE)[["21"]], list(A = 0.5, B = 1.5))
  expect_error(Subset(x, Channels = c("A", "missing")), "missing.*Matched 1 of 2")
  expect_error(event_fixture(list(A = 0.5, A = 1.5)), "RunUID 21.*Missing.*B.*Duplicated.*A")
  expect_error(event_fixture(list(A = 0.5)), "RunUID 21.*Missing.*B")
})

test_that("channel metadata aligns keyed rows and rejects inconsistent keys", {
  x <- event_fixture()
  ChannelMetadata(x) <- data.frame(Channel = c("B", "A"), tag = c(20, 10))
  expect_identical(ChannelMetadata(x, "tag"), c(10, 20))
  expect_identical(ChannelMetadata(Subset(x, Channels = "B"), "tag"), 20)
  expect_error(ChannelMetadata(x) <- data.frame(Channel = c("A", "other")),
               "Missing.*B.*Unexpected.*other", class = "ephys_invalid_argument")
  ChannelMetadata(x) <- data.frame(tag = c(1, 2))
  expect_named(setNames(ChannelMetadata(x, "tag"), rownames(ChannelMetadata(x))), c("A", "B"))
})

test_that("subsetting validates bounds and preserves an absent stimulus", {
  x <- event_fixture()
  expect_false(HasStimulus(x))
  expect_error(Subset(x, TimeRange = c(2, 1)), "ordered TimeRange bounds")
  expect_error(Subset(x, TimeRange = c(NA, 1)), "non-missing bounds")
  expect_error(Subset(x, Repeat = 1), "Metadata\\$Repeat")

  timed <- event_fixture(time = seq(0, 2, 0.5))
  out <- Subset(timed, TimeRange = units::as_units(c(500, 1500), "ms"))
  expect_equal(as.numeric(TimeTrace(out)), c(0, 0.5, 1))
  expect_length(StimulusTrace(out), 0)
  expect_false(HasStimulus(out))
  expect_identical(out@Data[[1L]], list(A = 0, B = 1))

  stimulus <- timed
  StimulusTrace(stimulus) <- 1:5
  expect_true(HasStimulus(stimulus))
  expect_equal(as.numeric(StimulusTrace(Subset(stimulus, TimeRange = c(0.5, 1.5)))), 2:4)
  StimulusTrace(stimulus) <- numeric()
  expect_false(HasStimulus(stimulus))

  with_stop <- timed
  Metadata(with_stop) <- transform(Metadata(with_stop), Stop = 2)
  expect_warning(out <- Subset(with_stop, TimeRange = c(0, 1.5)),
                 "preserved Metadata.*Stop.*Start is absent", class = "ephys_metadata_warning")
  expect_equal(Metadata(out)$Stop, 2)
})

test_that("callback failures keep run and channel positions in both modes", {
  x <- event_fixture()
  callback <- function(values) {
    if (is.list(values)) values <- values[[1L]]
    if (values > 1) stop("synthetic callback failure")
    values
  }
  old_plan <- future::plan(future::sequential)
  on.exit(future::plan(old_plan), add = TRUE)
  for (parallel in c(FALSE, TRUE)) {
    expect_warning(by_run <- lapply(x, callback, parallel = parallel, error = "warn", progress = FALSE),
                   "RunUID 21.*RecordingID 7.*channel.*B.*synthetic callback failure",
                   class = "ephys_callback_warning")
    expect_identical(by_run, list("21" = list(A = 0.5, B = NULL)))
    expect_warning(by_recording <- group_apply(x, callback, parallel = parallel, error = "warn", progress = FALSE),
                   "RecordingID 7.*channel.*B.*synthetic callback failure",
                   class = "ephys_callback_warning")
    expect_identical(by_recording, list("7" = list(A = 0.5, B = NULL)))

    err <- tryCatch(lapply(x, callback, parallel = parallel, progress = FALSE),
                    ephys_callback_error = identity)
    expect_s3_class(err, "ephys_callback_error")
    expect_match(conditionMessage(err), "RunUID 21.*RecordingID 7.*channel.*B")
    expect_match(conditionMessage(err$parent), "synthetic callback failure")
  }
})

test_that("continuous callbacks have the same error contract", {
  events <- event_fixture(time = c(0, 0.5, 1, 1.5))
  x <- newEPhysContinuous(
    array(c(1, 2, 3, 4), c(2, 1, 2)), c(0, 0.5), Metadata(events),
    Channels(events), ChannelMetadata(events)
  )
  callback <- function(values) {
    if (values[1L] > 2) stop("synthetic continuous failure")
    sum(values)
  }
  expect_warning(res <- lapply(x, callback, error = "warn", progress = FALSE),
                 "RunUID 21.*channel.*B", class = "ephys_callback_warning")
  expect_identical(res, list("21" = list(A = 3, B = NULL)))
  expect_warning(res <- group_apply(x, callback, error = "warn", progress = FALSE),
                 "RecordingID 7.*channel.*B", class = "ephys_callback_warning")
  expect_identical(res, list("7" = list(A = 3, B = NULL)))
})

test_that("legacy setters reject invalid callbacks before assignment", {
  x <- legacy_fixture(n = 4L)
  expect_error(FilterFunction(x) <- function(values) values[-1L],
               "trial 1.*Expected 4.*received 3", class = "ephys_callback_error")
  expect_error(FilterFunction(x) <- function(values) stop("filter failed"),
               "trial 1", class = "ephys_callback_error")
  expect_error(Rejected(x) <- function(values) FALSE,
               "Expected 2 non-missing logical", class = "ephys_callback_error")
  expect_error(AverageFunction(x) <- function(values) c(1, 2),
               "sample 1.*Expected one numeric", class = "ephys_callback_error")
  expect_s4_class(x, "EPhysData")
  expect_equal(as.numeric(GetData(x)), as.numeric(x@Data))
  FilterFunction(x) <- function(values) values * 2
  AverageFunction(x) <- mean
  expect_equal(as.numeric(GetData(x)), 2 * rowMeans(units::drop_units(x@Data)))

  broken <- x
  broken@TimeTrace <- broken@TimeTrace[1L]
  problems <- methods::validObject(broken, test = TRUE)
  expect_true(any(grepl("TimeTrace.*1 samples.*Expected 4", problems)))
  expect_true(any(grepl("StimulusTrace.*4 samples.*Expected 0 or 1", problems)))
  expect_error(Downsample(broken), "TimeTrace.*Expected 4")
})

test_that("downsampling uses one set of sample indices for all traces", {
  x <- legacy_fixture()
  out <- Downsample(x, n = 11)
  expect_identical(dim(out), c(11L, 2L))
  expect_equal(length(TimeTrace(out)), 11)
  indices <- match(as.numeric(TimeTrace(out)), as.numeric(TimeTrace(x)))
  expect_equal(as.numeric(StimulusTrace(out)), as.numeric(StimulusTrace(x))[indices])
  expect_equal(as.numeric(out@Data), as.numeric(x@Data[indices, ]))
  expect_equal(dim(Downsample(x, n = 1)), c(1L, 2L))
  expect_error(Downsample(x, n = 0), "positive whole number")
})
