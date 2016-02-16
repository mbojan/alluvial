# Testing issue18: `hide` and `layer`

library(alluvial)

d <- data.frame(
  row = 1:4,
  x = letters[c(4,1,2,3)],
  y = letters[c(4,3,1,2)],
  z = letters[c(4,3,2,1)]
)
d$freq <- rep(2, nrow(d))
d

# defaults
alluvial(d[,-ncol(d)], freq=d$freq, col=1:nrow(d), alpha=0.8)

# hiding a row
alluvial(d[,-ncol(d)], freq=d$freq, col=1:nrow(d), alpha=0.8,
         hide = d$row == 3 )

# hiding row=2 and reversing the layering
# (was a bug)
alluvial(d[,-ncol(d)], freq=d$freq, col=1:nrow(d), alpha=0.8,
         hide = d$row == 3,
         layer = 4:1 )

