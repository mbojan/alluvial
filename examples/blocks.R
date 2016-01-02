# Titanic data
tit <- as.data.frame(Titanic)

# Possible blocks options
for (blocks in c(TRUE, FALSE, "bookends")) {
    
    # Elaborate alluvial diagram from main examples file
    alluvial( tit[, 1:4], freq = tit$Freq, border = NA,
              hide = tit$Freq < quantile(tit$Freq, .50),
              col = ifelse( tit$Class == "3rd" & tit$Sex == "Male",
                            "red", "gray" ),
              blocks = blocks )
}
