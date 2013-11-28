library(memisc)

f <- spss.system.file("~/Data/polpan/POLPAN1988_2008-spss70.sav")

# variable labels to file
if(FALSE)
{
sink("polpan.txt")
print(description(f))
sink()
}

dset <- subset(f, select=c(polpanid=POLPANID, yrbirth=YRBIRTH, gender=GENDER,
                           # Wave indicators
                           w1988=WAVE1988, w1993=WAVE1993, w1998=WAVE1998,
                           w2003=WAVE2003, w2008=WAVE2008,
                           # Election vars
                           par1989vote=YW18, par1989choice=YW19, # 1989 parlament
                           # pr1990vote=YW20, pr1990choice1=YW21_1, pr1990choice2=YW21_2, # 1990 president
                           par1991vote=YW22, par1991choice=YW23, # 1991 parlament
                           par1993vote=XW09, par1993choice=XW10, # 1993 parlament
                           par1997vote=XW11, par1997choice=XW12, # 1997 parlament
                           # xw14a=XW14A, # president 1995
                           par2001vote=WW09, par2001choice=WW10, # 2001 parlament
                           par2005vote=VW15, par2005choice=VW16, # 2005 parlament
                           par2007vote=VW17, par2007choice=VW18 # 2007 parlament
                           ),
               stringsAsFactors=FALSE)
d <- as.data.frame(dset, stringsAsFactors=FALSE)

# polpanid as character
d$polpanid <- as.character(d$polpanid)

# Year of birth as numeric
d$yrbirth <- as.numeric(dset$yrbirth)
d$yrbirth[ d$yrbirth == 9999 ] <- NA

# Gender "female"
d$female <- d$gender == "Female"
d$female[d$gender == "ND"] <- NA

# Wave indicators as logical
waves <- c(1988, 1993, 1998, 2003, 2008)
vnames <- paste("w", waves, sep="")
d[vnames] <- lapply(d[vnames], function(x) x=="Yes")

# Voting in elections (yes, no, dont know)
vnames <- grep("vote", names(d), value=TRUE)
d[vnames] <- lapply(d[vnames], as.character)
lapply(d[vnames], unique)

# Election choice variables
vnames <- grep("choice", names(d), value=TRUE)
d[vnames] <- lapply(d[vnames], as.character)
lapply(d[vnames], table, exclude=NULL)
unames <- sort(unique(unlist(d[vnames])))



if(FALSE)
{
polpanvote <- d
save(polpanvote, file="polpanvote.rda")
}
