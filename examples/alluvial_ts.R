# d <- refugees
# get data from 'https://dl.dropboxusercontent.com/u/46043231/data/refugees.csv'
head(d)
reshape2::dcast(d, country ~ year, value.var = 'refugees')

set.seed(39) # for nice colours
cols <- hsv(h = sample(1:10/10), s = sample(3:12)/15, v = sample(3:12)/15)

alluvial_ts(d)
alluvial_ts(d, wave = .2, ygap = 5, lwd = 3)
alluvial_ts(d, wave = .3, ygap = 5, col = cols)
alluvial_ts(d, wave = .3, ygap = 5, col = cols, rankup = TRUE)
alluvial_ts(d, wave = .3, ygap = 5, col = cols, plotdir = 'down')
alluvial_ts(d, wave = .3, ygap = 5, col = cols, plotdir = 'centred', grid = TRUE, grid.lwd = 5)
alluvial_ts(d, wave =  0, ygap = 0, col = cols, alpha = .9, border = 'white', grid = TRUE, grid.lwd = 5)
alluvial_ts(d, wave = .3, ygap = 5, col = cols, xmargin = 0.4)
alluvial_ts(d, wave = .3, ygap = 5, col = cols, xmargin = 0.3, lab.cex = .7)
alluvial_ts(d, wave = .3, ygap = 5, col = cols, xmargin = 0.3, lab.cex = .7, leg.cex = .7, leg.col = 'white')
alluvial_ts(d, wave = .3, ygap = 5, col = cols, leg.mode = FALSE, leg.x = .1, leg.y = .7, leg.max = 3e6)
alluvial_ts(d, wave = .3, ygap = 5, col = cols, plotdir = 'centred', alpha = .9, grid = TRUE, grid.lwd = 5, xmargin = 0.2, 
         lab.cex = .7, xlab = '', ylab = '', border = NA, axis.cex = .8, leg.cex = .7, leg.col = 'white', 
         title = "UNHCR-recognised refugees\nTop 10 countries (2003-13)\n")

# non time-series example - Virginia deaths dataset
d = reshape2::melt(data.frame(age=row.names(VADeaths), VADeaths), id.vars='age')[,c(2,1,3)]
names(d) = c('pop_group','age_group','deaths')
alluvial_ts(d)
