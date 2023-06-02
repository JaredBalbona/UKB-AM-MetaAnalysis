library(data.table)
library(tidyverse)
library(polycor)
library(lavaan)

setwd("/pl/active/KellerLab/jared/misc/AM_Meta_Analysis")

# Read in binary phenotype data:
BinaryPhen <- fread("/pl/active/KellerLab/jared/misc/AM_Meta_Analysis/UKB_SpouseData_Binary_AllEthnicities.txt", h=T)

# Merge the phenotype file with the PC file:
PCs <- fread("/pl/active/KellerLab/jared/misc/AM_Meta_Analysis/PCs_Birthplace_Spouses.txt", sep=",")
BinaryPhen <- merge(BinaryPhen, PCs, by='PairID') 

# Full list of dichotomous traits:
Binary <- c("Chronotype", "Breastfed", "Adopted", "Multiple_Birth", "MSP", 
            "Father_Alive", "Mother_Alive", "Mood_Swings", "Miserableness", 
            "Irritability", "Hurt_Feelings", "FedUp_Feelings", "Nervous_Feelings", 
            "Anxious_Feelings", "High_Strung", "Worry_After_Embarrassment", 
            "Nerves", "Loneliness", "Guilt", "Risk_Taking", "Doctor_Anx_Dep", 
            "Same_Sex_Intercourse", "Longterm_Illness", "Wears_Glasses", 
            "Chest_Pain", "Diabetes_Diagnosed", "Cancer_Diagnosed", 
            "Smoked_100_Times", "Ever_Quit_Smoking", "Pain_for_3Months", 
            "Pacemaker", "Former_Drinker", "NonAccidental_Family_Death", 
            "Ever_Dep_1Week", "Ever_Manic_2Days", "Ever_Irritable_2Days", 
            "Tinnitus", "Ever_Smoked", "HayFever_Rhinitis_Eczema_Diag", 
            "Asthma_Diag", "Heart_Attack_Diag", "Emphysema_ChronicBronchitis_Diag", 
            "Drinking_Status", "Hearing_Difficulty", "Dental_Problems", 
            "Heart_Problems", "Smoking_Status_NvFC", "Smoking_Status_FvCN", 
            "Smoking_Status_CvFN", "Alc_Add", "Sub_Behav_Addict", "Ill_Rec_Addict", 
            "OTC_Addict", "Major_Dep", 'Bipolar', "Handedness")

# Prepare/ Run the Correlations
Partial = FALSE # Choose partial or zero order correlation: 

# Based on this choice, load the appropriate covariates:
if(Partial == TRUE){
  Covariates <- c(paste0('PC', 1:10, '_M'), paste0('PC', 1:10, '_F'), 
                  "Born_in_British_Isles_M", "Born_in_British_Isles_F", 
                  "Year_of_Birth_M", "Year_of_Birth_F")
}else{
  Covariates <- c()
}

N_Tests = 133 # Change based on the number of tests in the final publication

results = data.frame()
for (Trait in Binary){
  Temp = BinaryPhen
  
  # Split the data by sex:
  Temp$Trait_M <- eval(parse(text = paste0('Temp$', Trait, '_M')))
  Temp$Trait_F <- eval(parse(text = paste0('Temp$', Trait, '_F')))
  
  # Ensure that there are enough pairs in each group:
  Freq <- table(Temp$Trait_M, Temp$Trait_F)
  Total_Sum <- sum(Freq)
  M1_F1 <- Freq[2,2]
  M1_F0 <- Freq[2,1]
  M0_F1 <- Freq[1,2]
  M0_F0 <- Freq[1,1]
  
  Sufficient_Freq <-    (sum(M1_F1, M1_F0) * (sum(M1_F1, M0_F1) / Total_Sum) > 5) &
    (sum(M1_F1, M1_F0) * (sum(M1_F0, M0_F1) / Total_Sum) > 5) &
    (sum(M1_F1, M0_F1) * (sum(M0_F1, M0_F0) / Total_Sum) > 5) &
    (sum(M0_F0, M0_F1) * (sum(M1_F0, M0_F0) / Total_Sum) > 5)
  
  # Calculate statistics:
  cor_result  <- polychor(t(Freq), std.err = T)
  cor       <- round(cor_result$rho, 3)
  var       <- cor_result$var[1]
  se        <- round(sqrt(var), 3)
  zscore    <- cor / se
  p_value   <- format.pval(2 * (1 - pnorm(abs(zscore))), digits = 3)
  
  CI_Lower <- round(cor - (2*se), 3)
  CI_Upper <- round(cor + (2*se), 3)
  
  FemPrev    <- paste0(round(((M0_F1 + M1_F1) / Total_Sum) * 100, 2), "%")
  MalePrev <- paste0(round(((M1_F0 + M1_F1) / Total_Sum) * 100, 2), "%")
  
  OR <- round((M0_F0 / M1_F0) / (M0_F1 / M1_F1), 3)
  
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
  
  # Put it all together:
  Trait_Result <- data.frame(cbind(Trait, M1_F1, M1_F0, M0_F1, M0_F0, 
                                   Sufficient_Freq, cor, se, CI_Lower, CI_Upper, 
                                   FemPrev, MalePrev, Total_Sum, OR, p_value, 
                                   p_value_adj, CI_Lower_adj, CI_Upper_adj))
  results <- rbind(results, Trait_Result)
}

# Save Output:
if(Partial == TRUE){
  fwrite(results, "/pl/active/KellerLab/jared/misc/AM_Meta_Analysis/Results/UKB_Spouse_AllEthnicities_BinaryPhen_PartialCorrelation_Results.txt", sep=",")
}else{
  fwrite(results, "/pl/active/KellerLab/jared/misc/AM_Meta_Analysis/Results/UKB_Spouse_AllEthnicities_BinaryPhen_ZeroOrderCorrelation_Results.txt", sep=",")
}

#############################################################
#############################################################

# I needed to use Lavaan to get the binary partial correlations
# NOTE: This is much slower than what's above, and will produce the same results 
# when Partial = FALSE. So I'd recommend only using the code below 
# when including covariates

results = data.frame()
for (Trait in Binary){
  Temp = BinaryPhen
  
  # Split the data by sex:
  Temp$Trait_M <- eval(parse(text = paste0('Temp$', Trait, '_M')))
  Temp$Trait_F <- eval(parse(text = paste0('Temp$', Trait, '_F')))
  
  # Ensure that there are enough pairs in each group:
  Freq <- table(Temp$Trait_M, Temp$Trait_F)
  Total_Sum <- Num_Pairs <- sum(Freq)
  M1_F1 <- Freq[2,2]
  M1_F0 <- Freq[2,1]
  M0_F1 <- Freq[1,2]
  M0_F0 <- Freq[1,1]
  
  Sufficient_Freq <-    (sum(M1_F1, M1_F0) * (sum(M1_F1, M0_F1) / Total_Sum) > 5) &
    (sum(M1_F1, M1_F0) * (sum(M1_F0, M0_F1) / Total_Sum) > 5) &
    (sum(M1_F1, M0_F1) * (sum(M0_F1, M0_F0) / Total_Sum) > 5) &
    (sum(M0_F0, M0_F1) * (sum(M1_F0, M0_F0) / Total_Sum) > 5)
  
  # Create the Model:
  if(Partial == TRUE){
    BinaryModel <-'
            Trait_M + Trait_F ~ PC1_M + PC2_M + PC3_M + PC4_M + PC5_M + PC6_M + 
            PC7_M + PC8_M + PC9_M + PC10_M + PC1_F + PC2_F + PC3_F + PC4_F + 
            PC5_F + PC6_F + PC7_F + PC8_F + PC9_F + PC10_F + 
            Born_in_British_Isles_M + Born_in_British_Isles_F + 
            Year_of_Birth_M + Year_of_Birth_F
            Trait_M ~~ Trait_F'
  }else{
    BinaryModel <-'Trait_M ~~ Trait_F'
  }
  
  # Fit/ Estiamte the model:
  Fit_BinaryModel <- cfa(BinaryModel, data=Temp, ordered=c("Trait_M","Trait_F"))
  Param_Est <- data.frame(parameterEstimates(Fit_BinaryModel))
  res <- Param_Est[Param_Est$lhs == "Trait_M" & Param_Est$rhs == "Trait_F",]
  
  # Gather Relevant Statistics: 
  cor <- round(res$est, 3)
  se <- round(res$se, 3)
  zscore <- round(res$z, 3)
  p_value <- format.pval(res$pvalue, digits = 3)
  
  CI_Lower <- round(cor - (2*se), 3)
  CI_Upper <- round(cor + (2*se), 3)
  
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
  
  # Collect the final statistics for our table: 
  FemPrev    <- paste0(round(((M0_F1 + M1_F1) / Total_Sum) * 100, 2), "%")
  MalePrev <- paste0(round(((M1_F0 + M1_F1) / Total_Sum) * 100, 2), "%")
  OR <- round((M0_F0 / M1_F0) / (M0_F1 / M1_F1), 3)
  
  # Gather all of the output:
  Trait_Result <- data.frame(cbind(Trait, M1_F1, M1_F0, M0_F1, M0_F0, 
                                   Sufficient_Freq, cor, se, CI_Lower, CI_Upper, 
                                   FemPrev, MalePrev, Num_Pairs, OR, p_value, 
                                   p_value_adj, CI_Lower_adj, CI_Upper_adj))
  results <- rbind(results, Trait_Result)
}

# Save Output:
if(Partial == TRUE){
  fwrite(results, "/pl/active/KellerLab/jared/misc/AM_Meta_Analysis/Results/UKB_Spouse_AllEthnicities_BinaryPhen_PartialCorrelation_Results.txt", sep=",")
}else{
  fwrite(results, "/pl/active/KellerLab/jared/misc/AM_Meta_Analysis/Results/UKB_Spouse_AllEthnicities_BinaryPhen_ZeroOrderCorrelation_Results.txt", sep=",")
}



