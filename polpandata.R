library(memisc)

f <- spss.system.file("~/Data/polpan/POLPAN1988_2008-spss70.sav")

dset <- subset(f, select=c(polpanid=POLPANID, yrbirth=YRBIRTH, gender=GENDER,
                           w1988=WAVE1988, w1993=WAVE1993, w1998=WAVE1998,
                           w2003=WAVE2003, w2008=WAVE2008,
                           par1989vote=YW18, par1989choice=YW19, # 1989 parlament
                           pr1990vote=YW20, pr1990choice1=YW21_1, pr1990choice2=YW21_2, # 1990 president
                           par1991vote=YW22, par1991choice=YW23, # 1991 parlament
                           par1993vote=XW09, par1993choice=XW10, # 1993 parlament
                           par1997vote=XW11, par1997choice=XW12, # 1997 parlament
                           xw14a=XW14A, # ?  
                           par2001vote=WW09, par2001choice=WW10, # 2001 parlament
                           par2005vote=WW15, par2005choice=WW16, # 2005 parlament
                           par2007vote=VW17, par2007choice=VW18 # 2007 parlament
                           ),
               stringsAsFactors=FALSE)
d <- as.data.frame(dset, stringsAsFactors=FALSE)
d$yrbirth <- as.numeric(dset$yrbirth)
waves <- c(1988, 1993, 1998, 2003, 2008)
d[ paste("w", waves, sep="") ] <- lapply(d[paste("w", waves, sep="")], function(x) x=="Yes")
chvars <- c("polpanid", grep("vote", names(d), value=TRUE))
d[chvars] <- lapply(d[chvars], as.character)

table(d$vote1991, exclude=NULL)

polpanvote <- d
save(polpanvote, file="polpanvote.rda")
