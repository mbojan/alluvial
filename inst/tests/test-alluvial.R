context("Oddities in input data")

test_that("Character vectors are accepted",
          {
            d <- data.frame(x1=rep(letters[1:2], 2),
                            x2=rep(letters[1:2], each=2),
                            freq=1:4, 
                            stringsAsFactors=FALSE)
            # TODO test that this does not trigger a warning
            # op <- options(warn=2)
            alluvial( d[,1:2], freq=d$freq)
            # options(op)
          } )

