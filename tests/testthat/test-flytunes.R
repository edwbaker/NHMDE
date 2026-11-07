flytunesData <- function(created_at) {
  categories <- c(
    "Road vehicles e.g. cars/vans",
    "Other modes of transport e.g. trains/planes",
    "Birds",
    "Insects",
    "Humans e.g. walking/talking",
    "Other/I don't know"
  )
  data <- data.frame(
    user_name=c("a.volunteer", "not-logged-in-1234"),
    created_at=created_at,
    file=c("site1_20230601.wav", "site2_20230602.wav"),
    segment.number=c(0, 2),
    segment.length=c(10, 10),
    subject.offset=c(0, 3),
    stringsAsFactors=FALSE
  )
  for (category in categories) {
    data[[category]] <- category %in% c("Birds")
  }
  data[["Insects"]] <- c(TRUE, FALSE)
  return(data)
}

test_that("a FlyTunes annotation is credited to the volunteer who made it, when they made it", {
  a <- flytunes_annotations(flytunesData(c("2023-06-14 09:27:32 UTC", "2023-06-15 18:00:05 UTC")))
  expect_length(a, 3)
  # The volunteer was the source, where the source is where the annotation
  # came from.
  expect_identical(vapply(a, function(x) x@identifiedBy, character(1)),
                   c("a.volunteer", "a.volunteer", "not-logged-in-1234"))
  expect_identical(unique(vapply(a, function(x) x@source, character(1))), "Zooniverse")
  expect_identical(vapply(a, function(x) x@dateIdentified, character(1)),
                   c("2023-06-14T09:27:32Z", "2023-06-14T09:27:32Z", "2023-06-15T18:00:05Z"))
  expect_identical(vapply(a, function(x) x@value, character(1)), c("Birds", "Insects", "Birds"))
  expect_equal(c(a[[3]]@start, a[[3]]@end), c(23, 33))
})

test_that("a classification time in another form is kept as it is given, and none as none", {
  a <- flytunes_annotations(flytunesData(c("2023-06-14T09:27:32Z", NA)))
  expect_identical(a[[1]]@dateIdentified, "2023-06-14T09:27:32Z")
  expect_identical(a[[3]]@dateIdentified, "")

  data <- flytunesData(c("", ""))
  data$created_at <- NULL
  a <- flytunes_annotations(data)
  expect_identical(unique(vapply(a, function(x) x@dateIdentified, character(1))), "")
})
