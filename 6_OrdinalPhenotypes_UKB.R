library(data.table)

setwd("/file/to/path")

# Load/ prep the data:

    # Load the ordinal phenotype data:
    Ord <- fread("UKB_Mate_Phenotypes_Ordinal.txt", h=T)

    # Merge the phenotype file with the PC file:
    PCs <- fread("Mate_PCs.txt", h=T)
    OrdinalPhen <- merge(Ord, PCs, by='PairID') 

    # Full list of ordinal traits:
    Ordinal <- c("Hair_Color", "Facial_Aging", "Tried_to_Quit", "Smoking_Status", "Insomnia", "Narcolepsy", "Current_Smoker", "Past_Smoker", "Alcohol_Freq", "Comparative_Body_Size_Age_10", "Dep_2Weeks", "Disinterest_2Weeks", "Restlessness_2Weeks", "Lethargy_2weeks", "Able_to_Confide", "Overall_Health_Rating", "Plays_Computer_Games", "Time_to_First_Cig", "Difficulty_Not_Smoking", "Want_to_Quit", "Happiness", "Job_Satisfaction", "Health_Satisfaction", "Family_Relationship_Satisfaction", "Friendship_Satisfaction", "Financial_Satisfaction", "Freq_FriendFamily_Visits")

# Run the correlations:

    # Choose Partial or Zero-Order Correlation:
    Partial <- FALSE

    # Based on this choice, load the appropriate covariates:
    if(Partial == TRUE){
        Covariates <- c(paste0('PC', 1:10, '_M'), paste0('PC', 1:10, '_F'), "Born_in_British_Isles_M", "Born_in_British_Isles_F", "Year_of_Birth_M", "Year_of_Birth_F")
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
        N_Pairs <- nrow(Temp)

        # Run the correlation:
        # Important: Rank variables BEFORE residualizing. Results may be biased otherwise
        M_Resid <- lm(rank(V1) ~ ., data = cbind(rank(Temp$Trait_M), Temp[,..Covariates]))
        F_Resid <- lm(rank(V1) ~ ., data = cbind(Temp$Trait_F, Temp[,..Covariates]))
        cor_result <- cor.test(resid(M_Resid), resid(F_Resid), method="pearson", exact=F)

        # Gather the relevant statistics:
        cor <- round(cor_result$estimate, 3)
        se  <- round(myse.spear(cor=cor, n=N_Pairs),3)
        CI_Lower <- round(cor - (2*se), 3)
        CI_Upper <- round(cor + (2*se), 3)
        p_value <- format.pval(cor_result$p.value, digits = 3)

        # Collect the output:
        Trait_Result <- data.frame(cbind(Trait, Mean_M, Mean_F, cor, se, CI_Lower, CI_Upper, p_value, N_Pairs))

        results <- rbind(results, Trait_Result)
    }

    # Save Output:
    if(Partial == TRUE){
        fwrite(results, "UKB_Spouse_AllEthnicities_OrdinalPhen_PartialCorrelation_Results.txt", sep=",")
    }else{
        fwrite(results, "UKB_Spouse_AllEthnicities_OrdinalPhen_ZeroOrderCorrelation_Results.txt", sep=",")
    }