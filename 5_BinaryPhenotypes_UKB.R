library(data.table)
library(tidyverse)
library(polycor)
library(lavaan)

setwd("/file/to/path")

# Load/ prep the data:
    # Read in binary phenotype data:
    Binary_Phen <- fread("UKB_Mate_Phenotypes_Binary.txt", h=T)

    # Merge the phenotype file with the PC file:
    PCs <- fread("Mate_PCs.txt", h=T)
    BinaryPhen <- merge(BinaryPhen, PCs, by='PairID') 

    # Full list of dichotomous traits:
    Binary <- c("Chronotype", "Breastfed", "Adopted", "Multiple_Birth", "MSP", "Father_Alive", "Mother_Alive", "Mood_Swings", "Miserableness", "Irritability", "Hurt_Feelings", "FedUp_Feelings", "Nervous_Feelings", "Anxious_Feelings", "High_Strung", "Worry_After_Embarrassment", "Nerves", "Loneliness", "Guilt", "Risk_Taking", "Doctor_Anx_Dep", "Same_Sex_Intercourse", "Longterm_Illness", "Wears_Glasses", "Chest_Pain", "Diabetes_Diagnosed", "Cancer_Diagnosed", "Smoked_100_Times", "Ever_Quit_Smoking", "Pain_for_3Months", "Pacemaker", "Former_Drinker", "NonAccidental_Family_Death", "Ever_Dep_1Week", "Ever_Manic_2Days", "Ever_Irritable_2Days", "Tinnitus", "Ever_Smoked", "HayFever_Rhinitis_Eczema_Diag", "Asthma_Diag", "Heart_Attack_Diag", "Emphysema_ChronicBronchitis_Diag", "Drinking_Status", "Hearing_Difficulty", "Dental_Problems", "Heart_Problems", "Smoking_Status_NvFC", "Smoking_Status_FvCN", "Smoking_Status_CvFN", "Alc_Add", "Sub_Behav_Addict", "Ill_Rec_Addict", "OTC_Addict", "Major_Dep", 'Bipolar', "Handedness")

###############
# Run the Zero-Order Correlations:
###############

    # Run the analysis for each binary trait:
    results <- data.frame()
    for (Trait in Binary){
        Temp <- BinaryPhen

        # Split the data by sex:
        Temp$Trait_M <- eval(parse(text = paste0('Temp$', Trait, '_M')))
        Temp$Trait_F <- eval(parse(text = paste0('Temp$', Trait, '_F')))

        # Get the frequency of each outcome:
        Freq <- table(Temp$Trait_M, Temp$Trait_F)
        Total_Sum <- sum(Freq)
        M1_F1 <- Freq[2,2]
        M1_F0 <- Freq[2,1]
        M0_F1 <- Freq[1,2]
        M0_F0 <- Freq[1,1]

        # Check that there are enough of each outcome:
        Sufficient_Freq <-  (sum(M1_F1, M1_F0) * (sum(M1_F1, M0_F1) / Total_Sum) > 5) &
        (sum(M1_F1, M1_F0) * (sum(M1_F0, M0_F1) / Total_Sum) > 5) &
        (sum(M1_F1, M0_F1) * (sum(M0_F1, M0_F0) / Total_Sum) > 5) &
        (sum(M0_F0, M0_F1) * (sum(M1_F0, M0_F0) / Total_Sum) > 5)

        # Run the correlation and gather relevant statistics:
        cor_result <- polychor(t(Freq), std.err = T)
        cor     <- round(cor_result$rho, 3)
        var     <- cor_result$var
        se      <- round(sqrt(var), 3)
        zscore  <- cor / se
        p_value <- format.pval(2 * (1 - pnorm(abs(zscore))), digits = 3)

        CI_Lower <- round(cor - (2*se), 3)
        CI_Upper <- round(cor + (2*se), 3)

        # Calculate sex-specific prevalence rates:
        FemPrev  <- paste0(round(((M0_F1 + M1_F1) / Total_Sum) * 100, 2), "%")
        MalePrev <- paste0(round(((M1_F0 + M1_F1) / Total_Sum) * 100, 2), "%")
        
        # Calculate odds ratio: 
        OR <- round((M0_F0 / M1_F0) / (M0_F1 / M1_F1), 3)

        # Collect the output:
        Trait_Result <- data.frame(cbind(Trait, M1_F1, M1_F0, M0_F1, M0_F0, Sufficient_Freq, cor, se, CI_Lower, CI_Upper, FemPrev, MalePrev, Total_Sum, OR, p_value,))

        results <- rbind(results, Trait_Result)
    }

    # Save Output:
    if(Partial == TRUE){
        fwrite(results, "UKB_Spouse_AllEthnicities_BinaryPhen_PartialCorrelation_Results.txt", sep=",")
    }else{
        fwrite(results, "UKB_Spouse_AllEthnicities_BinaryPhen_ZeroOrderCorrelation_Results.txt", sep=",")
    }

###############
# Run the Partial: Correlations:
###############

# Use Lavaan to get the binary partial correlations
    # Note that thsi is much slower than the loop above and produces the same results for the zero-order correlations.
    # Thus, I'd recommend only using the code below when including covariates.

    # Choose Partial or Zero-Order Correlation:
    Partial <- FALSE

    # Run the analysis for each binary trait:
    results <- data.frame()
    for (Trait in Binary){
        Temp <- BinaryPhen

        # Split the data by sex:
        Temp$Trait_M <- eval(parse(text = paste0('Temp$', Trait, '_M')))
        Temp$Trait_F <- eval(parse(text = paste0('Temp$', Trait, '_F')))

        # Get the frequency of each outcome:
        Freq <- table(Temp$Trait_M, Temp$Trait_F)
        Total_Sum <- sum(Freq)
        M1_F1 <- Freq[2,2]
        M1_F0 <- Freq[2,1]
        M0_F1 <- Freq[1,2]
        M0_F0 <- Freq[1,1]

        # Check that there are enough of each outcome:
        Sufficient_Freq <-  (sum(M1_F1, M1_F0) * (sum(M1_F1, M0_F1) / Total_Sum) > 5) &
        (sum(M1_F1, M1_F0) * (sum(M1_F0, M0_F1) / Total_Sum) > 5) &
        (sum(M1_F1, M0_F1) * (sum(M0_F1, M0_F0) / Total_Sum) > 5) &
        (sum(M0_F0, M0_F1) * (sum(M1_F0, M0_F0) / Total_Sum) > 5)

        # Create SEM for both types of correlations:
        if(Partial == TRUE){
            BinaryModel <-'
            Trait_M + Trait_F ~ PC1_M + PC2_M + PC3_M + PC4_M + PC5_M + PC6_M + PC7_M + PC8_M + PC9_M + PC10_M + PC1_F + PC2_F + PC3_F + PC4_F + PC5_F + PC6_F + PC7_F + PC8_F + PC9_F + PC10_F + Born_in_British_Isles_M + Born_in_British_Isles_F + Year_of_Birth_M + Year_of_Birth_F
            Trait_M ~~ Trait_F'

        }else{
            BinaryModel <-'Trait_M ~~ Trait_F'
        }

        # Fit the model and get parameter estimates:
        Fit_BinaryModel <- cfa(BinaryModel, data=Temp, ordered=c("Trait_M","Trait_F"))
        Param_Est <- data.frame(parameterEstimates(Fit_BinaryModel))

        # Gather relevant statistics:
        res <- Param_Est[Param_Est$lhs == "Trait_M" & Param_Est$rhs == "Trait_F",]
        cor <- round(res$est, 3)
        se <- round(res$se, 3)
        zscore <- round(res$z, 3)
        p_value <- format.pval(res$pvalue, digits = 3)

        CI_Lower <- round(res$ci.lower, 3)
        CI_Upper <- round(res$ci.upper, 3)

        # Calculate sex-specific prevalence rates:
        FemPrev  <- paste0(round(((M0_F1 + M1_F1) / Total_Sum) * 100, 2), "%")
        MalePrev <- paste0(round(((M1_F0 + M1_F1) / Total_Sum) * 100, 2), "%")

        # Calculate odds ratio:
        OR <- round((M0_F0 / M1_F0) / (M0_F1 / M1_F1), 3)

        # Collect the output: 
        Trait_Result <- data.frame(cbind(Trait, M1_F1, M1_F0, M0_F1, M0_F0, Sufficient_Freq, cor, CI_Lower, CI_Upper, FemPrev, MalePrev, Total_Sum, OR, p_value))

        results <- rbind(results, Trait_Result)
    }

    # Save Output:
    if(Partial == TRUE){
        fwrite(results, "UKB_Spouse_AllEthnicities_BinaryPhen_PartialCorrelation_Results.txt", sep=",")
    }else{
        fwrite(results, "UKB_Spouse_AllEthnicities_BinaryPhen_ZeroOrderCorrelation_Results.txt", sep=",")
    }
