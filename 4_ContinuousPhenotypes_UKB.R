library(data.table)
library(tidyverse)
library(ppcor)

setwd("/file/to/path")

# Load/ prep the data:
    # Load the continuous phenotype data:
    Cont <- fread("UKB_Mate_Phenotypes_Continuous.txt", h=T)

    # Add in a couple phenotypes that weren't included before:
    Cont$WHR_F <- Cont$Waist_Circumference_F/Cont$Hip_Circumference_F
    Cont$WHR_M <- Cont$Waist_Circumference_M/Cont$Hip_Circumference_M

    Cont$BMI_F <- Cont$Weight_F / ((Cont$Height_F/100)^2)
    Cont$BMI_M <- Cont$Weight_M / ((Cont$Height_M/100)^2)

    # Merge the phenotype file with the PC file:
    PCs <- fread("Mate_PCs.txt", h=T)
    Cont_Phen <- merge(Cont, PCs, by='PairID') 

    # Full list of continuous traits:
    Continuous <- c("Year_of_Birth", "HandGrip", "Waist_Circumference", "Hip_Circumference", "Height", "Heel_BMD", "SBP", "DBP", "Pulse", "Birth_Weight", "Birthplace_North", "Birthplace_East", "Num_Cancers", "Num_Illnesses", "Num_Operations", "Num_Medications", "Age_Completed_Edu", "Time_TV", "Time_Computer", "Sleep_Duration", "Water_Intake", "Father_Age_at_Death", "Mother_Age", "Num_Sisters", "Num_Brothers", "Age_First_Intercourse", "Num_Sexual_Partners", "Num_Children", "AI", "CPD", "Age_Stopped_Smoking", "Num_Quitting_Attempts", "Father_Age", "FVC", "FEV1", "PEF", "Weight", "Mother_Age_at_Death", "Num_Same_Sex_Partners", "Longest_Dep_Period", "Num_Dep_Episodes", "Num_Older_Sibs", "EA", "fIQ", "Neuroticism", "FEV1_Pred_Percentage", "Pack_Years", "Pack_Years_Lifespan_Proportion", "Basal_Metabolic_Rate", "Whole_Body_Impedance", "WBC_Count", "RBC_Count", "Cholesterol", "CRP", "HDL", "LDL", "Num_Siblings", "Current_CPD_AllParticipants", "BMI", "WHR")

# Run the correlations

    # Choose Partial or Zero-Order Correlation:
    Partial <- FALSE 

    # Based on this choice, load the appropriate covariates:
    if(Partial == TRUE){
        Covariates <- c(paste0('PC', 1:10, '_M'), paste0('PC', 1:10, '_F'), "Born_in_British_Isles_M", "Born_in_British_Isles_F", "Year_of_Birth_M", "Year_of_Birth_F")
    }else{
        Covariates <- c()
    }

    # Run the analysis for each continuous trait:
    results <- data.frame()
    for (Trait in Continuous){
        Temp <- Cont_Phen

        # Split the data by sex:
        Temp$Trait_M <- eval(parse(text = paste0('Temp$', Trait, '_M')))
        Temp$Trait_F <- eval(parse(text = paste0('Temp$', Trait, '_F')))

        # Get sex-specific descriptive statistics:
        Mean_M  <- mean(Temp$Trait_M, na.rm = T)
        Mean_F  <- mean(Temp$Trait_F, na.rm = T)
        SD_M    <- sd(Temp$Trait_M, na.rm = T)
        SD_F    <- sd(Temp$Trait_F, na.rm = T)
        Temp$Z_M <- abs((Temp$Trait_M - Mean_M) / SD_M)
        Temp$Z_F <- abs((Temp$Trait_F - Mean_F) / SD_F)

        # Remove outliers:
        Temp <- Temp[complete.cases(Temp[,c('Trait_M', 'Trait_F', 'Z_M', 'Z_F', ..Covariates)])]
        Temp <- Temp[Temp$Z_M < 4 & Temp$Z_F < 4, ]

        # Run the Correlation:
        M_Resid <- lm(V1 ~ ., data = cbind(Temp$Trait_M, Temp[,..Covariates]))
        F_Resid <- lm(V1 ~ ., data = cbind(Temp$Trait_F, Temp[,..Covariates]))
        Pearson_Results <- cor.test(resid(M_Resid), resid(F_Resid))
        
        # Gather the relevant statistics:
        Pearson_R   <- round(Pearson_Results$estimate, 3)
        CI_Lower    <- round(Pearson_Results$conf.int[1], 3)
        CI_Upper    <- round(Pearson_Results$conf.int[2], 3)
        p_Value     <- format.pval(Pearson_Results$p.value, 3)
        Spearman_Rho <- round(cor.test(resid(M_Resid), resid(F_Resid), method= "spearman", exact=F)$estimate, 3)

        # Calculate new descriptives:
        Mean_M  <- round(mean(Temp$Trait_M, na.rm = T), 3)
        Mean_F  <- round(mean(Temp$Trait_F, na.rm = T), 3)
        SD_M    <- round(sd(Temp$Trait_M, na.rm = T), 3)
        SD_F    <- round(sd(Temp$Trait_F, na.rm = T), 3)
        Num_Pairs <- nrow(Temp)

        # Collect the output:
        Trait_Result <- data.frame(cbind(Trait, Mean_M, Mean_F, SD_M, SD_F, Pearson_R, CI_Lower, CI_Upper, p_Value, Num_Pairs, Spearman_Rho))
        results <- rbind(results, Trait_Result)
    }

    # Save Output:
    if(Partial == TRUE){
        fwrite(results, "UKB_Spouse_AllEthnicities_ContinuousPhen_PartialCorrelation_Results.txt", sep=",")
    }else{
        fwrite(results, "UKB_Spouse_AllEthnicities_ContinuousPhen_ZeroOrderCorrelation_Results.txt", sep=",")
    }

