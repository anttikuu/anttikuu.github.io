
#library(ggplot2)
library(gridExtra)
library(reshape2)



longdata <- read.table("RSHINYresults.txt", sep = ",", header = T, stringsAsFactors = T)

# HELPER FUNCTION:
source('confumatHelper.R')

halls = c('AC', 'MH', 'BP', 'CP');

# OVERALL SAME:
hallmat1 <- matrix(0,nrow = 4, ncol = 4);
rownames(hallmat1) <- halls
colnames(hallmat1) <- halls
samedata <- subset(longdata, SAMEDIFF == 'same')
for(i in 1:nrow(samedata)){
  hallmat1[as.character(samedata$REF[i]), as.character(samedata$ANS[i])] <- hallmat1[as.character(samedata$REF[i]), as.character(samedata$ANS[i])] + 1
}

# OVERALL DIFF: 
hallmat2 <- matrix(0,nrow = 4, ncol = 4);
rownames(hallmat2) <- halls
colnames(hallmat2) <- halls
diffdata <- subset(longdata, SAMEDIFF == 'diff')
for(i in 1:nrow(samedata)){
  hallmat2[as.character(diffdata$REF[i]),as.character(diffdata$ANS[i])] <- hallmat2[as.character(diffdata$REF[i]),as.character(diffdata$ANS[i])] + 1
}

# PER MUSIC, SAME
samemats <- list()
for(s in levels(longdata$MUS_REF)) {
  hallmat <- matrix(0,nrow = 4, ncol = 4);
  rownames(hallmat) <- halls
  colnames(hallmat) <- halls
  data <- subset(longdata, SAMEDIFF == 'same' & MUS_REF == as.character(s))
  for(i in 1:nrow(data)){
    hallmat[as.character(data$REF[i]),as.character(data$ANS[i])] <- hallmat[as.character(data$REF[i]), as.character(data$ANS[i])] + 1
  }
  samemats[[as.character(s)]] <- hallmat
}

# PER MUSIC, DIFF
diffmats <- list()
for(s in levels(longdata$MUS_REF)) {
  hallmat <- matrix(0,nrow = 4, ncol = 4);
  rownames(hallmat) <- halls
  colnames(hallmat) <- halls
  data <- subset(longdata, SAMEDIFF == 'diff' & MUS_REF == as.character(s))
  for(i in 1:nrow(data)){
    hallmat[as.character(data$REF[i]),as.character(data$ANS[i])] <- hallmat[as.character(data$REF[i]), as.character(data$ANS[i])] + 1
  }
  diffmats[[as.character(s)]] <- hallmat
}


#########################
# ALL DATA:
#tab0 <- confumatHelper(hallmat1+hallmat2)
# OVERALL SAME:
#tab1 <- confumatHelper(hallmat1)
# OVERALL DIFFERENT:
#tab2 <- confumatHelper(hallmat2)

##################
# PER MUSIC
SSbeetab <- confumatHelper(samemats[[1]] + samemats[[2]])
DDbeetab <- confumatHelper(diffmats[[1]] + diffmats[[2]])
Sviolintab <- confumatHelper(samemats[[3]])
Dviolintab <- confumatHelper(diffmats[[3]])

# PLOTTING THE CONFUSIOMATRICES:
plotHelper <- function(xtab,mytext){
  xtab <- xtab[4:1,]
  titletext <- mytext
  normvalue <- sum(xtab[,1])
  melted_xtab <- melt(round(xtab/normvalue,2))
  g <- ggplot(data = melted_xtab, aes(x=Reference, y=Response, fill=value),show.legend = F) + 
    geom_tile(show.legend = F) + theme_minimal(base_size = 8) +
     scale_fill_gradient(low = "white", high = "darkgreen") +
    geom_text(aes(Reference,Response, label = value), color = "black", size = 3, show.legend = F) +
    labs(title = titletext) + theme(axis.title = element_text(size = 9))
    return(g)
}

SSbee_plot <- plotHelper(SSbeetab,'a) Beethoven, same')
DDbee_plot <- plotHelper(DDbeetab,'b) Beethoven, diff')
Sviolin_plot <- plotHelper(Sviolintab,'c) Violin, same')
Dviolin_plot <- plotHelper(Dviolintab,'d) Violin, diff')
grid.arrange(SSbee_plot,DDbee_plot,Sviolin_plot,Dviolin_plot,nrow = 1)

