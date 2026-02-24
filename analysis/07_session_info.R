writeLines(
  capture.output(sessionInfo()),
  here::here("analysis", "session-info.txt")
)
