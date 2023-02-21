library(ggplot2)
library(data.table)
library(dplyr)
library(gridExtra)

########################################################
# Big Plot
#######################################################
results <- fread('~/Documents/Projects/AM_MetaAnalysis/Mate_Correlations_All.txt', h=T)

plot.new()
ggplot(data=results, aes(x=Correlation, y=reorder(results$Phenotype2, -results$Correlation), xmin=results$CI_Lower, xmax=results$CI_Upper)) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) + 
  geom_linerange(size = .5) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) +   
  guides(color = guide_legend(title = "Type of Correlation")) + 
  scale_colour_manual(values=c("dodgerblue", "firebrick1","lightgreen")) + 
  geom_vline(xintercept = 0, lty = 3) +  # add a dotted line at x=1 after flip
  ylab("") + xlab("Partner Correlation (95% CI)\n") +
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

########################################################
# Within-Category Plots
#######################################################
results <- fread('~/Documents/Projects/AM_MetaAnalysis/Mate_Correlations_All.txt', h=T)
results <- results[results$Phenotype2 != 'Num Same Sex Partners',] # CI is too massive

results1 <- results[results$Category == 'Behavioral',]
results2 <- results[results$Category == 'Health',]
results3 <- results[results$Category == 'Psychological',]
results4 <- results[results$Category == 'Anthropometric',]
results5 <- results[results$Category == 'Demographic/ Family',]
results6 <- results[results$Category == 'Substance Use',]

plot.new()
g1 <- ggplot(data=results1, aes(x=Correlation, y=reorder(results1$Phenotype2, -results1$Correlation), xmin=results1$CI_Lower, xmax=results1$CI_Upper)) +
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

g2 <- ggplot(data=results2, aes(x=Correlation, y=reorder(results2$Phenotype2, -results2$Correlation), xmin=results2$CI_Lower, xmax=results2$CI_Upper)) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) + 
  xlim(-.2, .88) +
  geom_linerange(size = .5) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) +   
  guides(color = guide_legend(title = "Type of Correlation")) + 
  scale_colour_manual(values=c("dodgerblue", "firebrick1","lightgreen")) + 
  geom_vline(xintercept = 0, lty = 3) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("") + xlab("Partner Correlation (95% CI)\n") +
  theme_light() +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = rep(c('Black', 'Gray50'), nrow(results2))),
        panel.border = element_blank(),
        panel.grid.major.x =  element_line(color = rep(c('Grey78', 'Grey92'), nrow(results2))),
        panel.background = element_blank()) + 
  ggtitle("Health-Related Traits") + 
  scale_y_discrete() 

g3 <- ggplot(data=results3, aes(x=Correlation, y=reorder(results3$Phenotype2, -results3$Correlation), xmin=results3$CI_Lower, xmax=results3$CI_Upper)) +
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

g4 <- ggplot(data=results4, aes(x=Correlation, y=reorder(results4$Phenotype2, -results4$Correlation), xmin=results4$CI_Lower, xmax=results4$CI_Upper)) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) + 
  xlim(-.2, .88) +
  geom_linerange(size = .5) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) +   
  guides(color = guide_legend(title = "Type of Correlation")) + 
  scale_colour_manual(values=c("dodgerblue", "firebrick1","lightgreen")) + 
  geom_vline(xintercept = 0, lty = 3) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("") + xlab("Partner Correlation (95% CI)\n") +
  theme_light() +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = rep(c('Black', 'Gray50'), nrow(results4))),
        panel.border = element_blank(),
        panel.grid.major.x =  element_line(color = rep(c('Grey78', 'Grey92'), nrow(results4))),
        panel.background = element_blank()) + 
  ggtitle("Anthropometric Traits") + 
  scale_y_discrete() 

g5 <- ggplot(data=results5, aes(x=Correlation, y=reorder(results5$Phenotype2, -results5$Correlation), xmin=results5$CI_Lower, xmax=results5$CI_Upper)) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) + 
  xlim(-.2, .88) +
  geom_linerange(size = .5) +
  geom_point(size = 2.5, aes(color=Correlation_Type)) +   
  guides(color = guide_legend(title = "Type of Correlation")) + 
  scale_colour_manual(values=c("dodgerblue", "firebrick1","lightgreen")) + 
  geom_vline(xintercept = 0, lty = 3) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("") + xlab("Partner Correlation (95% CI)\n") +
  theme_light() +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = rep(c('Black', 'Gray50'), nrow(results5))),
        panel.border = element_blank(),
        panel.grid.major.x =  element_line(color = rep(c('Grey78', 'Grey92'), nrow(results5))),
        panel.background = element_blank()) + 
  ggtitle("Demographic/ Family Traits") + 
  scale_y_discrete() 

g6 <- ggplot(data=results6, aes(x=Correlation, y=reorder(results6$Phenotype2, -results6$Correlation), xmin=results6$CI_Lower, xmax=results6$CI_Upper)) +
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

grid.arrange(g2, g3, g5, g6, g4, g1, nrow = 3) # Sorted by number of traits 

########################################################
# Meta-Analysis + UKB Plot
#######################################################

results <- fread("~/Documents/Projects/AM_MetaAnalysis/AM_MetaAnalysis_Results.txt", head=T)[,1:4]
results <- results[order(-results$Correlation),]

# Combine it with the UKB Results
UKB <- fread('~/Documents/Projects/AM_MetaAnalysis/Mate_Correlations_All.txt', h=T)
UKB <- UKB[UKB$Priority==0, c('Trait', 'Correlation', 'CI_Lower', 'CI_Upper')]

results$Source <- 'Meta-Analysis'
UKB$Source <- "UK Biobank"
results2 <- rbind(results, UKB)

plot.new()
ggplot() +
  geom_linerange(data = results, aes(x = Correlation, y = reorder(Trait, -Correlation), xmin=CI_Lower, xmax=CI_Upper), color = "blue4", size = .5) +
  geom_linerange(data = UKB, aes(x = Correlation, y = Trait, xmin=CI_Lower, xmax=CI_Upper), color = "red4", size = .5) +  
  geom_point(data = results, aes(x = Correlation, y = reorder(Trait, -Correlation)), color = "black", shape = 21, fill = 'blue3', size = 2.7) +
  geom_point(data = UKB, aes(x = Correlation, y = Trait), color = "black", shape = 21, fill = 'red2', size = 2.7) + 
  scale_color_manual(name='Source of Correlation',
                     breaks=c('Meta-Analysis', 'UK Biobank'),
                     values=c('Meta-Analysis'='navy', 'UK Biobank'='red3')) +
  geom_vline(xintercept = 0, lty = 3) +  
  coord_flip() +  
  ylab("") + xlab("\nPartner Correlation (95% CI)\n\n") +
  theme_light() +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        panel.border = element_blank(),
        panel.background = element_blank())

# The above plot is what we used, and the below was used to generate the legend
# Note: I had to use photoshop to fix the points in the legend and to combine it with the plot above
ggplot(data=results2, aes(x=Correlation, y=reorder(Trait, -Correlation), xmin=CI_Lower, xmax=CI_Upper)) +
  geom_linerange(size = .5, aes(color=Source), position =position_dodge(width=0)  ) +
  geom_point(size = 2.5, aes(color=Source), position = position_dodge(width=0)) + 
  #geom_linerange(size = .5, aes(color=Source), position =position_dodge(width=0.001)  ) +
  #geom_point(size = 2.5, aes(color=Source), position = position_dodge(width=0.001)) + 
  guides(color = guide_legend(title = "Source of Correlation")) + 
  scale_colour_manual(values=c("blue3", "red2")) + 
  geom_vline(xintercept = 0, lty = 3) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("") + xlab("\n\nPartner Correlation (95% CI)\n") +
  theme_light() +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        panel.border = element_blank(),
        panel.background = element_blank(), 
        legend.position = c(.88,.80),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12), 
        legend.background = element_blank()) + 
  scale_y_discrete() 

############################
# Run WLS Regression
############################

UKB <- fread('Mate_Correlations_All.txt', h=T)

UKB$Category1 <- as.numeric(factor(UKB$Category))
UKB$Correlation_Type1 <- as.numeric(factor(UKB$Correlation_Type))


WLS <- lm(Correlation ~ Category1 + Correlation_Type1, data = UKB, weights = UKB$Weight)
summary(WLS)

summary(lm(Correlation ~ Category + Correlation_Type, data = UKB, weights = Weight))
summary(lm(Correlation ~ as.factor(Category) + as.factor(Correlation_Type), data = UKB, weights = Weight))
summary(lm(Correlation ~ as.factor(Category) + as.factor(Correlation_Type), data = UKB, weights = Weight))


UKB %>% 
  group_by(Category) %>% 
  summarize(Mean = mean(Correlation, na.rm=TRUE))

summary(lm(Correlation ~ Category, data = UKB, weights = UKB$Weight))
summary(lm(Correlation ~ Correlation_Type, data = UKB, weights = UKB$Weight))

summary(aov(Correlation ~ Correlation_Type + Category, data = UKB))
