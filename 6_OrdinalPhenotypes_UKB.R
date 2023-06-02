library(data.table)

setwd("/pl/active/KellerLab/jared/misc/AM_Meta_Analysis")
Ord <- fread("/pl/active/KellerLab/jared/misc/AM_Meta_Analysis/UKB_SpouseData_Other_AllEthnicities.txt", h=T)

# Merge the phenotype file with the PC file:
PCs <- fread("/pl/active/KellerLab/jared/misc/AM_Meta_Analysis/PCs_Birthplace_Spouses.txt", h=T)
OrdinalPhen <- merge(Ord, PCs, by='PairID') 

Ordinal <- c("Hair_Color", "Facial_Aging", "Tried_to_Quit", "Smoking_Status", 
             "Insomnia", "Narcolepsy", "Current_Smoker", "Past_Smoker", 
             "Alcohol_Freq", "Comparative_Body_Size_Age_10", "Dep_2Weeks", 
             "Disinterest_2Weeks", "Restlessness_2Weeks", "Lethargy_2weeks", 
             "Able_to_Confide", "Overall_Health_Rating", "Plays_Computer_Games", 
             "Time_to_First_Cig", "Difficulty_Not_Smoking", "Want_to_Quit", 
             "Happiness", "Job_Satisfaction", "Health_Satisfaction", 
             "Family_Relationship_Satisfaction", "Friendship_Satisfaction", 
             "Financial_Satisfaction", "Freq_FriendFamily_Visits")

# Prepare/ Run the Correlations
Partial = FALSE # Choose partial or zero order correlation: 

# Based on this choice, load the appropriate covariates:
if(Partial == TRUE){
  Covariates <- c(
    paste0('PC', 1:10, '_M'), paste0('PC', 1:10, '_F'), 
    "Born_in_British_Isles_M", "Born_in_British_Isles_F",
    "Year_of_Birth_M", "Year_of_Birth_F"
  )
}else{
  Covariates <- c()
}

# Function to get the correct SE for each correlation:
myse.spear <- function(cor,n){
  num <- ((1-cor^2)^2) * (1+(cor^2)/2)
  den <- n-3
  se <- sqrt(num/den)
  return(se)
}

N_Tests = 133 # Change based on the number of tests in the final publication

# Run the analysis for each ordinal trait: 
results = data.frame()
for (Trait in Ordinal){
  Temp = OrdinalPhen
  
  # Split data by sex:
  Temp$Trait_M <- eval(parse(text = paste0('Temp$', Trait, '_M')))
  Temp$Trait_F <- eval(parse(text = paste0('Temp$', Trait, '_F')))
  
  # Get sex-specific means and sample size:
  Mean_M  <- round(mean(Temp$Trait_M, na.rm = T),3)
  Mean_F  <- round(mean(Temp$Trait_F, na.rm = T),3)
  
  Temp <- Temp[complete.cases(Temp[,c('Trait_M', 'Trait_F')])]
  Num_Pairs <- nrow(Temp)
  
  # Run the correlation:
  # Important: Rank variables BEFORE residualizing. Results may be biased otherwise
  M_Resid <- lm(rank(V1) ~ ., data = cbind(rank(Temp$Trait_M), Temp[,..Covariates]))
  F_Resid <- lm(rank(V1) ~ ., data = cbind(Temp$Trait_F, Temp[,..Covariates]))
  cor_result <- cor.test(resid(M_Resid), resid(F_Resid), method="pearson", exact=F)
  
  # Gather the relevant statistics:
  cor <- round(cor_result$estimate, 3)
  se  <- round(myse.spear(cor=cor, n=Num_Pairs),3)
  CI_Lower <- round(cor - (2*se), 3)
  CI_Upper <- round(cor + (2*se), 3)
  p_value <- format.pval(cor_result$p.value, digits = 3)
  
  # Bonferroni p-value adjustment:
  if(p_value == '<0.0000000000000002' | p_value == '<2e-16'){
    p_value_adj <- p.adjust(2e-16, method = "bonferroni", n = N_Tests)
    p_value_adj <- format.pval(p_value_adj, 3)
    
  }else{
    p_value_adj <- p.adjust(p_value, method = "bonferroni", n = N_Tests)
    p_value_adj <- format.pval(p_value_adj, 3)
  }
  
  # Confidence interval adjustment:
  alpha_bonf      <- .05 / N_Tests # Bonferroni corrected alpha level
  CI_Lower_adj    <- round(cor - qnorm(1 - alpha_bonf/2) * se, 3)
  CI_Upper_adj    <- round(cor + qnorm(1 - alpha_bonf/2) * se, 3)
  
  # Collect the output:
  Trait_Result <- data.frame(cbind(Trait, Mean_M, Mean_F, cor, se, CI_Lower, 
                                   CI_Upper, p_value, Num_Pairs, p_value_adj, 
                                   CI_Lower_adj, CI_Upper_adj))
  
  results <- rbind(results, Trait_Result)
}

# Save Output:
if(Partial == TRUE){
  fwrite(results, "/pl/active/KellerLab/jared/misc/AM_Meta_Analysis/Results/UKB_Spouse_AllEthnicities_OrdinalPhen_PartialCorrelation_Results.txt", sep=",")
}else{
  fwrite(results, "/pl/active/KellerLab/jared/misc/AM_Meta_Analysis/Results/UKB_Spouse_AllEthnicities_OrdinalPhen_ZeroOrderCorrelation_Results.txt", sep=",")
}


