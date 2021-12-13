# Oddities in input data --------------------------------------------------

test_that("Character vectors in data do not trigger warnings", {
  d <- data.frame(
    x1=rep(letters[1:2], 2),
    x2=rep(letters[1:2], each=2),
    freq=1:4
  )
  expect_silent({
    pdf(NULL)
    alluvial(d[,1:2], freq=d$freq)
    dev.off()
  } )
})



# Output ------------------------------------------------------------------

test_that("alluvial() returns proper value", {
  d <- data.frame(
    x1=rep(letters[1:2], 2),
    x2=rep(letters[1:2], each=2),
    freq=1:4
  )
  pdf(NULL)
  expect_snapshot_value(alluvial(d[,1:2], freq=d$freq), style = "deparse")
  dev.off()
})

test_that("alluvial() produces a proper plot", {
  skip_on_cran()
  skip_on_ci()
  d <- data.frame(
    x1=rep(letters[1:2], 2),
    x2=rep(letters[1:2], each=2),
    freq=1:4
  )
  expect_snapshot_file({
    tfile <- tempfile()
    png(tfile)
    alluvial(d[,1:2], freq=d$freq)
    dev.off()
    tfile
  },
    "alluvial.png"
  )
})
