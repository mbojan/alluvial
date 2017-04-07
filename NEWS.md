# 0.2-0

## New features

- `alluvial()` exposes `mar` argument that is passed to `par()` so the user can customize the plot margins (which are rather narrow by default) (#31).
- `alluvial()` has two new arguments `xlim_offset` and `ylim_offset` which are passed to `xlim` and `ylim` of `plot()`. This enables adjusting the size of the plotting region and avoid label clipping in some cirumstances (#32)
- The vignette has a new section on reordering the alluvia, and some other improvements. 

## Minor updates

- The vignette dependency on RColorBrewer has been removed.




# alluvial 0.1-2

## New features

- There is a vignette `vignette("alluvial", package="alluvial")` illustrating basic usage of `alluvial()`. The vignette needs `dplyr` package so it is now `Suggested`.

## Minor updates

- README has been updated. It is dynamically generated from an associated `.Rmd` file. Some typos fixed.


# alluvial 0.1-1

First release.