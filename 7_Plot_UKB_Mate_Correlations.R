library(ggplot2)
library(data.table)
library(dplyr)
library(gridExtra)

########################################################
# Big Plot
#######################################################
results <- fread('~/Documents/Projects/AM_MetaAnalysis/Revision_April2023/Mate_Correlations_All_Updated.txt', h=T)

plot.new()
ukb_plot <- ggplot(data=results, aes(x=Correlation, y=reorder(results$Phenotype2, -results$Correlation), xmin=results$CI_Lower, xmax=results$CI_Upper)) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) + 
  geom_linerange(size = .5) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) +   
  guides(color = guide_legend(title = "Type of Correlation")) + 
  scale_colour_manual(values=c("dodgerblue", "firebrick1","lightgreen")) + 
  geom_vline(xintercept = 0, lty = 3) +  # add a dotted line at x=1 after flip
  #coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("") + xlab("Partner Correlation (Adjusted 95% CI)\n") +
  theme_light() +
  theme(axis.line = element_blank(),
        axis.text.y = element_text(angle = 0, vjust = 1, hjust=1, color = rep(c('Black', 'Gray50'), nrow(results))),
        panel.border = element_blank(),
        panel.grid.major.y =  element_line(color = rep(c('Grey78', 'Grey92'), nrow(results))),
        panel.background = element_blank(),
        legend.position = c(.74,.93),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15), 
        legend.background = element_blank()) +
  scale_y_discrete() 

ggsave("UKB_Plot_AllTraits.eps", ukb_plot, width = 9, height = 20)

########################################################
# Within-Category Plots
#######################################################
results <- fread('~/Documents/Projects/AM_MetaAnalysis/Revision_April2023/Mate_Correlations_All_Updated.txt', h=T)
results <- results[results$Phenotype2 != 'Num Same Sex Partners',] # CI is too massive

results1 <- results[results$Category == 'Behavioral',]
results2 <- results[results$Category == 'Health',]
results3 <- results[results$Category == 'Psychological',]
results4 <- results[results$Category == 'Anthropometric',]
results5 <- results[results$Category == 'Demographic/ Family',]
results6 <- results[results$Category == 'Substance Use',]

plot.new()

health <- ggplot(data=results2, aes(x=Correlation, y=reorder(results2$Phenotype2, -results2$Correlation), xmin=results2$CI_Lower, xmax=results2$CI_Upper)) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) + 
  xlim(-.2, .88) +
  geom_linerange(size = .5) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) +   
  scale_colour_manual(values=c("dodgerblue", "firebrick1","lightgreen")) + 
  geom_vline(xintercept = 0, lty = 3) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("") + xlab("Partner Correlation (Adj. 95% CI)       \n") +
  theme_light() +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = rep(c('Black', 'Gray50'), nrow(results2))),
        panel.border = element_blank(),
        panel.grid.major.x =  element_line(color = rep(c('Grey78', 'Grey92'), nrow(results2))),
        panel.background = element_blank()) + 
  ggtitle("Health-Related Traits") + 
  scale_y_discrete() 

psych <- ggplot(data=results3, aes(x=Correlation, y=reorder(results3$Phenotype2, -results3$Correlation), xmin=results3$CI_Lower, xmax=results3$CI_Upper)) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) + 
  xlim(-.2, .88) +
  geom_linerange(size = .5) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) +   
  guides(color = guide_legend(title = "Type of Correlation")) + 
  scale_colour_manual(values=c("dodgerblue", "firebrick1","lightgreen")) + 
  geom_vline(xintercept = 0, lty = 3) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("") + xlab("\n\n") +
  theme_light() +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = rep(c('Black', 'Gray50'), nrow(results3))),
        panel.border = element_blank(),
        panel.grid.major.x =  element_line(color = rep(c('Grey78', 'Grey92'), nrow(results3))),
        panel.background = element_blank()) + 
  ggtitle("Psychological Traits") + 
  scale_y_discrete() 

demog <- ggplot(data=results5, aes(x=Correlation, y=reorder(results5$Phenotype2, -results5$Correlation), xmin=results5$CI_Lower, xmax=results5$CI_Upper)) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) + 
  xlim(-.2, .88) +
  geom_linerange(size = .5) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) +   
  scale_colour_manual(values=c("dodgerblue", "firebrick1","lightgreen")) + 
  geom_vline(xintercept = 0, lty = 3) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("") + xlab("Partner Correlation (Adj. 95% CI)       \n") +
  theme_light() +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = rep(c('Black', 'Gray50'), nrow(results5))),
        panel.border = element_blank(),
        panel.grid.major.x =  element_line(color = rep(c('Grey78', 'Grey92'), nrow(results5))),
        panel.background = element_blank()) + 
  ggtitle("Demographic/ Family Traits") + 
  scale_y_discrete() 

sub_use <- ggplot(data=results6, aes(x=Correlation, y=reorder(results6$Phenotype2, -results6$Correlation), xmin=results6$CI_Lower, xmax=results6$CI_Upper)) +
  xlim(-.2, .88) +
  geom_linerange(size = .5) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) + 
  guides(color = guide_legend(title = "Type of Correlation")) + 
  scale_colour_manual(values=c("dodgerblue", "firebrick1","lightgreen")) + 
  geom_vline(xintercept = 0, lty = 3) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("") + xlab("\n\n") +
  theme_light() +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = rep(c('Black', 'Gray50'), nrow(results6))),
        panel.border = element_blank(),
        panel.grid.major.x =  element_line(color = rep(c('Grey78', 'Grey92'), nrow(results6))),
        panel.background = element_blank()) + 
  ggtitle("Substance Use Traits") + 
  scale_y_discrete() 

anthro <- ggplot(data=results4, aes(x=Correlation, y=reorder(results4$Phenotype2, -results4$Correlation), xmin=results4$CI_Lower, xmax=results4$CI_Upper)) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) + 
  xlim(-.2, .88) +
  geom_linerange(size = .5) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) +   
  scale_colour_manual(values=c("dodgerblue", "firebrick1","lightgreen")) + 
  geom_vline(xintercept = 0, lty = 3) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("") + xlab("Partner Correlation (Adj. 95% CI)\n") +
  theme_light() +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = rep(c('Black', 'Gray50'), nrow(results4))),
        panel.border = element_blank(),
        panel.grid.major.x =  element_line(color = rep(c('Grey78', 'Grey92'), nrow(results4))),
        panel.background = element_blank()) + 
  ggtitle("Anthropometric Traits") + 
  scale_y_discrete() 

behav <- ggplot(data=results1, aes(x=Correlation, y=reorder(results1$Phenotype2, -results1$Correlation), xmin=results1$CI_Lower, xmax=results1$CI_Upper)) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) + 
  xlim(-.2, .88) +
  geom_linerange(size = .5) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) +   
  guides(color = guide_legend(title = "Type of Correlation")) + 
  scale_colour_manual(values=c("dodgerblue", "firebrick1","lightgreen")) + 
  geom_vline(xintercept = 0, lty = 3) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("") + xlab("\n\n") +
  theme_light() +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = rep(c('Black', 'Gray50'), nrow(results1))),
        panel.border = element_blank(),
        panel.grid.major.x =  element_line(color = rep(c('Grey78', 'Grey92'), nrow(results1))),
        panel.background = element_blank()) + 
  ggtitle("Behavioral Traits") + 
  scale_y_discrete() 

health  <- health + theme(legend.position="none")
demog   <- demog  + theme(legend.position="none")
anthro  <- anthro + theme(legend.position="none")

cat_plot <- grid.arrange(health, psych, demog, sub_use, anthro, behav, nrow = 3, widths = c(.82,1)) # Sorted by number of traits 
ggsave("UKB_Plot_Categories.eps", cat_plot, width = 18, height = 10)

########################################################
# Meta-Analysis + UKB Plot
#######################################################

results <- fread("~/Documents/Projects/AM_MetaAnalysis/Revision_April2023/MetaAnalysis_Results_Updated.csv", head=T)[,1:4]
results <- results[order(-results$Correlation),]

# Combine it with the UKB Results
UKB <- fread('~/Documents/Projects/AM_MetaAnalysis/Revision_April2023/Mate_Correlations_All_Updated.txt', h=T)
UKB <- UKB[UKB$Priority==0, c('Trait', 'Correlation', 'CI_Lower', 'CI_Upper')]

results$Source <- 'Meta-Analysis'
UKB$Source <- "UK Biobank"
results2 <- rbind(results, UKB)

plot.new()
ukb_ma_plot <- ggplot() +
  geom_linerange(data = results, aes(x = Correlation, y = reorder(Trait, -Correlation), xmin=CI_Lower, xmax=CI_Upper, color = "Meta-Analysis"), size = .4) +
  geom_linerange(data = UKB, aes(x = Correlation, y = Trait, xmin=CI_Lower, xmax=CI_Upper, color = "UK Biobank"), size = .4) +  
  geom_point(data = results, aes(x = Correlation, y = reorder(Trait, -Correlation), fill = "Meta-Analysis"), color = "black", shape = 21, size = 2.3) +
  geom_point(data = UKB, aes(x = Correlation, y = Trait, fill = "UK Biobank"), color = "black", shape = 21, size = 2.3) + 
  scale_color_manual(name='Source of Correlation',
                     breaks=c('Meta-Analysis', 'UK Biobank'),
                     labels=c('Meta-Analysis'='Meta-Analysis', 'UK Biobank'='UK Biobank'),  # Set label for Meta-Analysis to 'xx'
                     values=c('Meta-Analysis'='blue4', 'UK Biobank'='red4')) +
  scale_fill_manual(name='Source of Correlation',
                    breaks=c('Meta-Analysis', 'UK Biobank'),
                    labels=c('Meta-Analysis'='Meta-Analysis', 'UK Biobank'='UK Biobank'),  # Set label for Meta-Analysis to 'xx'
                    values=c('Meta-Analysis'='blue3', 'UK Biobank'='red2')) +
  scale_shape_manual(name='Source of Correlation',
                     breaks=c('Meta-Analysis', 'UK Biobank'),
                     values=c('Meta-Analysis'=21, 'UK Biobank'=21)) +
  scale_size_manual(name='Source of Correlation',
                    breaks=c('Meta-Analysis', 'UK Biobank'),
                    values=c('Meta-Analysis'=2.7, 'UK Biobank'=2.7)) +
  geom_vline(xintercept = 0, lty = 3) +  
  coord_flip() +  
  ylab("") + xlab("\nPartner Correlation (Adjusted 95% CI)\n\n") +
  theme_light() +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.95, 0.93),
        legend.justification = c(1, 1),
        legend.box.just = "right",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 11))

ggsave("~/Desktop/UKB_MetaAnalysis_Plot.eps", ukb_ma_plot, width = 12, height = 6)
