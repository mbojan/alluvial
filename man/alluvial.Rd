\name{alluvial}
\alias{alluvial}
\title{Alluvial diagrams}
\usage{
  alluvial(..., freq, col = "gray", border = 0, layer,
    alpha = 0.5, gap.width = 0.05, xw = 0.1, cw = 0.1)
}
\arguments{
  \item{...}{vectors or data frames with the same number of
  observations}

  \item{freq}{numeric vector with frequencies}

  \item{col}{colors of the stripes}

  \item{border}{border colors}

  \item{layer}{layer numbers determining plotting order}

  \item{alpha}{numeric, alpha transparency factor}

  \item{gap.width}{numeric, widith of the inter-category
  gap}

  \item{xw}{numeric, location of the control points
  relative to axes}

  \item{cw}{numeric, width of the category blocks}
}
\value{
  Nothing of interest but the plot is plotted.
}
\description{
  Creating alluvial diagrams.
}
\examples{
# Built-in Titanic data as a data frame
tit <- as.data.frame(Titanic)

alluvial( tit[,1:4], freq=tit$Freq )

# highlighting passangers who survived
alluvial( tit[,1:4], freq=tit$Freq, border=NA,
         col=ifelse( tit$Survived == "Yes", "lightskyblue", "violet") ) 
}

