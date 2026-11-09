event_fixture <- function(data = list(A = 0.5, B = 1.5), time = numeric(),
                          stimulus = numeric()) {
  md <- data.frame(Experiment = factor("test"), Step = ordered("1"),
                   RecordingID = 7L, RunUID = 21L)
  newEPhysEvents(list(data), time, md, c("A", "B"),
                 data.frame(row.names = c("A", "B")), StimulusTrace = stimulus)
}

legacy_fixture <- function(n = 101L, trials = 2L) {
  time <- 0.003 + seq_len(n) / 100
  newEPhysData(
    Data = units::as_units(matrix(seq_len(n * trials), nrow = n, ncol = trials), "mV"),
    TimeTrace = units::as_units(time, "s"),
    StimulusTrace = units::as_units(seq_len(n), "mV"),
    TimeUnit = "s"
  )
}
