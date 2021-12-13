# Oddities in input data --------------------------------------------------

test_that("Character vectors in data do not trigger warnings", {
  d <- data.frame(
    x1=rep(letters[1:2], 2),
    x2=rep(letters[1:2], each=2),
    freq=1:4
  )
  expect_silent( alluvial(d[,1:2], freq=d$freq) )
})

