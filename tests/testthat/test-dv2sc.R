context("dv2sc converter")

test_that("converter runs on example file", {
    x <- read_dv(dv_example_file(2))
    tmp <- dv2sc(x)

    ## this should have no instances, because it has no video_time entries
    expect_true(grepl("<ALL_INSTANCES></ALL_INSTANCES>", readLines(tmp), fixed = TRUE))

    x$plays$video_time <- seq_len(nrow(x$plays))
    tmp <- dv2sc(x)
    expect_false(grepl("<ALL_INSTANCES></ALL_INSTANCES>", readLines(tmp), fixed = TRUE))
})
