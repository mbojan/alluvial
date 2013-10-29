# Built-in Titanic data as a data frame
tit <- as.data.frame(Titanic)

alluvial( tit[,1:4], freq=tit$Freq )

# highlighting passangers who survived
alluvial( tit[,1:4], freq=tit$Freq, border=NA,
         col=ifelse( tit$Survived == "Yes", "lightskyblue", "violet") ) 
