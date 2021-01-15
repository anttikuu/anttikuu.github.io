library(ggplot2)
library(gridExtra)
library(reshape2)

load('summary_table_RSHINY.Rdata')


summary_table2$totalsameperc <- summary_table2$totalsame / 16
summary_table2$totaldiffperc <- summary_table2$totaldiff / 16
summary_table2$BEE.S.perc <- summary_table2$BEE.S / 8
summary_table2$BEE.D.perc <- summary_table2$BEE.D / 8
summary_table2$VIO.S.perc <- summary_table2$VIO.S / 8
summary_table2$VIO.D.perc <- summary_table2$VIO.D / 8


#head(summary_table2)

# PLOT SAMPLE MEAN and standard deviations





# SAMPLE MEAN ESTIMATE AND CONFIDENCE INTERVALS WITH BINOMIAL TEST
binstatHelper <- function(n_success, n_tot){
  myres <- c()
  p <- n_success / n_tot
  q <- 1 - p
  myres['Mean'] <- p # estimate of mean success rate is the proportion of correct answers
  myres['SE'] <- sqrt(p * q / n_tot) #standard error of mean for binomial variables, i.e., standard deviation of the mean estimate
  myres['U'] <- myres['Mean'] + myres['SE']*1.96 #  upper limit of confidence interval, normal approximation, 95 %
  myres['L'] <- myres['Mean'] - myres['SE']*1.96 #  lower limit of confidence interval, normal approximation, 95 %
  return(myres)
}


################################################
nsub <- nrow(summary_table2) - 1
totN <- nsub *16
same_corN <- summary_table2[nsub +1 ,'totalsame']
diff_corN <- summary_table2[nsub +1 ,'totaldiff']
same_stats <- binstatHelper(same_corN,totN)
diff_stats <- binstatHelper(diff_corN,totN)

stats <- rbind(same_stats,diff_stats)
stats <- data.frame(stats)
stats['G'] <- factor(c('same', 'different'))
stats$G <- factor(stats$G, levels =  c('same', 'different'))
#str(stats)


BEEtotN <- nsub *8
BEEsame_corN <- summary_table2[nsub +1 ,'BEE.S']
BEEdiff_corN <- summary_table2[nsub +1 ,'BEE.D']
BEEsame_stats <- binstatHelper(BEEsame_corN,BEEtotN)
BEEdiff_stats <- binstatHelper(BEEdiff_corN,BEEtotN)

VIOtotN <- nsub *8
VIOsame_corN <- summary_table2[nsub +1 ,'VIO.S']
VIOdiff_corN <- summary_table2[nsub +1 ,'VIO.D']
VIOsame_stats <- binstatHelper(VIOsame_corN,VIOtotN)
VIOdiff_stats <- binstatHelper(VIOdiff_corN,VIOtotN)

MUSstats <- rbind(BEEsame_stats,BEEdiff_stats,VIOsame_stats,VIOdiff_stats)

MUSstats <- data.frame(MUSstats)
MUSstats['G'] <- factor(c('same', 'different','same', 'different'))

MUSstats$G <- factor(MUSstats$G, levels =  c('same', 'different'))
MUSstats['Signal'] <- factor(c('Beethoven', 'Beethoven','Violin', 'Violin'))

pd <- position_dodge(1)


# PLOT OVERALL:
g0 <- ggplot(data = stats, aes(y = Mean*100, x = G), show.legend = T) +  
  geom_errorbar(data = stats, aes(x = G, ymin = L*100, ymax = U*100), size =0.5, width = 0.1, show.legend = F) +
  geom_point(data = stats, aes(y = Mean*100, x = G), size = 2, show.legend = F,shape=21, fill = 'black') +
  geom_errorbar(data = MUSstats, aes(x = G, ymin = L*100, ymax = U*100, group = Signal),position = pd, size =0.5, width = 0.2, show.legend = F) +
  geom_point(data = MUSstats, aes(y = Mean*100, x = G, shape = Signal, fill = Signal), position = pd, size = 2, show.legend = T) + 
  scale_shape_manual(values=c(22,23)) + scale_fill_manual(values = c('white','white'),aesthetics = "fill") +
  coord_cartesian(ylim = c(0,100)) + labs(title =  '', x = '', y = '% of correct answers', color = 'black') +
  theme_bw(base_size = 12) + 
  theme(axis.title.y = element_text(size = 12)) +
  theme(legend.title=element_blank(), 
        legend.position = c(0.8,0.88), 
        legend.background = element_rect(color = 'black', size = 0.1),
        legend.margin = margin(t = 0, r = 8, l = 3, b = 5)) 
grid.arrange(g0)
