# Titanic data
tit <- as.data.frame(Titanic)

# 2d
tit2d <- aggregate( Freq ~ Class + Survived, data=tit, sum)
alluvial( tit2d[,1:2], freq=tit2d$Freq, xw=0.0, alpha=0.8,
         gap.width=0.1, col= "steelblue", border="white",
         layer = tit2d$Survived != "Yes" )

alluvial( tit2d[,1:2], freq=tit2d$Freq, 
         hide=tit2d$Freq < 150,
         xw=0.0, alpha=0.8,
         gap.width=0.1, col= "steelblue", border="white",
         layer = tit2d$Survived != "Yes" )

# 3d
tit3d <- aggregate( Freq ~ Class + Sex + Survived, data=tit, sum)

alluvial(tit3d[,1:3], freq=tit3d$Freq, alpha=1, xw=0.2,
         col=ifelse( tit3d$Survived == "No", "red", "gray"),
         layer = tit3d$Sex != "Female",
         border="white")


# 4d
alluvial( tit[,1:4], freq=tit$Freq, border=NA,
         hide = tit$Freq < quantile(tit$Freq, .50),
         col=ifelse( tit$Class == "3rd" & tit$Sex == "Male", "red", "gray") )

# 3d example with custom ordering
# Reorder "Sex" axis according to survival status
ord <- list(NULL, with(tit3d, order(Sex, Survived)), NULL)
alluvial(tit3d[,1:3], freq=tit3d$Freq, alpha=1, xw=0.2,
         col=ifelse( tit3d$Survived == "No", "red", "gray"),
         layer = tit3d$Sex != "Female",
         border="white", ordering=ord)

# Possible blocks options
for (blocks in c(TRUE, FALSE, "bookends")) {
    
    # Elaborate alluvial diagram from main examples file
    alluvial( tit[, 1:4], freq = tit$Freq, border = NA,
              hide = tit$Freq < quantile(tit$Freq, .50),
              col = ifelse( tit$Class == "3rd" & tit$Sex == "Male",
                            "red", "gray" ),
              blocks = blocks )
}


# Data returned
x <- alluvial( tit2d[,1:2], freq=tit2d$Freq, xw=0.0, alpha=0.8,
          gap.width=0.1, col= "steelblue", border="white",
          layer = tit2d$Survived != "Yes" )
points( rep(1, 16), x$endpoints[[1]], col="green")
points( rep(2, 16), x$endpoints[[2]], col="blue")


# Segment-level aesthetics
col_mat <- cbind(
  as.numeric(tit$Class),
  as.numeric(tit$Sex) + 4,
  as.numeric(tit$Age) + 6
)
alluvial( tit[,1:4], freq=tit$Freq, border=NA, col=col_mat )
