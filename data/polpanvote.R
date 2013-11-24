library(memisc)

f <- spss.system.file("~/Data/polpan/POLPAN1988_2008-spss70.sav")

dset <- subset(f, select=c(polpanid=POLPANID, yrbirth=YRBIRTH, w1988=WAVE1988,
                           w1993=WAVE1993, w1998=WAVE1998, w2003=WAVE2003,
                           w2008=WAVE2008, vote1991=YW23, vote1993=XW10,
                           vote1997=XW12, vote2001=WW10, vote2005=VW16,
                           vote2007=VW18),
               stringsAsFactors=FALSE)
d <- as.data.frame(dset, stringsAsFactors=FALSE)
d$yrbirth <- as.numeric(dset$yrbirth)
waves <- c(1988, 1993, 1998, 2003, 2008)
d[ paste("w", waves, sep="") ] <- lapply(d[paste("w", waves, sep="")], function(x) x=="Yes")
chvars <- grep("vote", names(d), value=TRUE)
d[chvars] <- lapply(d[chvars], as.character)


polpanvote <- d
save(polpanvote, file="polpanvote.rda")
