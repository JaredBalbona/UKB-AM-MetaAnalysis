
#############################################################
#    SCRIPT TO GET ID NUMBERS OF MATE PAIRS IN THE UKB      #
#############################################################

library(data.table)
library(tidyverse)

options(stringsAsFactors=F)
setwd("/path/to/file/")

# Import the full UKB data file:
    UKB_Whole <- fread("Full_UKB_Data.tab", h=T) [, columns_of_interest]

# Limit the data to the fields we're interested in:
    Columns_of_Interest <- fread("UKB_Mate_Correlation_Columns.txt", h=T)
    UKB <- UKB_Whole[,colnames(UKB_Whole) %in% Columns_of_Interest$fids]

    names(UKB) <- c(Columns_of_Interest$Variable_Names)

# Take out people who have asked to be removed from the UKB:
    # Before: 502,481 | After: 502,414
    Redacted_FIDs <- fread("Redacted_Participants.csv", h=F)
    UKB_Original <- UKB_Original[!(UKB_Original$f.eid %in% Redacted_FIDs$V1),]

# Keep only people who reported living with their mate, and remove people who live with an unrelated roommate:
    # Before: 502,414 | After: 359,189
    # Colocation is based on living arrangements at baseline, so we don't want to look at the other waves.
    Mates1 <- UKB_Whole[,c("f.eid", "f.6141.0.0", "f.6141.0.1", "f.6141.0.2", "f.6141.0.3", "f.6141.0.4", )]
    Mates <- Mates1[apply(Mates1, 1, function(x) any(grepl("Husband, wife or partner", x))), ] # People who live with their mate
    Roomates<- Mates1[apply(Mates1, 1, function(x) any(grepl("Other unrelated", x))), ] # People who live with their mate

    UKB1 <- UKB_Original[(UKB_Original$f.eid %in% Mates$f.eid),] # 363,019 rows
    UKB1 <- UKB1[!(UKB1$f.eid %in% Roomates$f.eid),] # 359,189 rows
	
##########################################
### Remove people based on relatedness ###
##########################################

# Read in the data on which participants cohabitate:
    # Before: 359,189 | After: 200,707
   colocation1 <- fread("UKB_Cohabitation.csv", sep=",") # This contains a unique ID for all households
    colnames(colocation1)[1] <- "f.eid"
    colocation <- colocation1[colocation1$f.eid %in% UKB1$f.eid,] 
    UKB <- merge(UKB1, colocation, by='f.eid') # 200,707 rows

# Remove cohabitating pairs that are related (pihat >= .05):
    # Before: 200,707 | After: 200,481
    NonEur_Rels <- fread("NonEuro_Related_Pairs.txt", h=T) # 22,341
    NonEur_Rels <- NonEur_Rels[NonEur_Rels$ID_1 %in% UKB$f.eid & NonEur_Rels$ID_2 %in% UKB$f.eid,] # 1,889
    
    Eur_Rels 	<- fread("Euro_Related_Pairs.txt", h=F) # 164,880
    Eur_Rels 	<- Eur_Rels[Eur_Rels$V1 %in% UKB$f.eid & Eur_Rels$V3 %in% UKB$f.eid,]  # 27,104

# Combine IDs 
    NonEur_RelIDs 	<- c(paste(NonEur_Rels$ID_1, NonEur_Rels$ID_2, sep="_"), paste(NonEur_Rels$ID_2, NonEur_Rels$ID_1, sep="_")) 
    Eur_RelIDs 		<- c(paste(Eur_Rels$V1, Eur_Rels$V3, sep="_"), paste(Eur_Rels$V3, Eur_Rels$V1, sep="_")) # Combine IDs 
    RelIDs <- c(NonEur_RelIDs, Eur_RelIDs)

    UKB <- data.frame(UKB %>%
        group_by(group) %>%
        mutate(BothIDs = paste0(f.eid, collapse="_"))) # Combine IDs for the UKB

    UKB <- UKB[!(UKB$BothIDs %in% RelIDs),] # Keep non-overlapping pairs | 200,481
    
#Keep households with 2 UKB participants (there are very very few with >2):		
    # Before: 200,481 | After: 194,115
    Group_Frequencies <- data.frame(table(UKB$group)) # 103,473

    colnames(Group_Frequencies)[1] <- 'group'
    Group_Frequencies <- Group_Frequencies[Group_Frequencies$Freq == 2,] # 97,107 rows
    UKB <- UKB[UKB$group %in% Group_Frequencies$group,] # 193,764 rows

# Remove cross-ancestry relationships
    # Before: 193,764 | After: 175,250
    NE_IDs <- fread("NonEuro_Ancestry.txt", h=T)
    CrossAncestry <- merge(NE_IDs, UKB_Original, by='f.eid', all=T)

    CrossAncestry[!(CrossAncestry$f.eid %in% NE_IDs$f.eid),'Ancestry_Group'] <- 11
    CrossAncestry <- merge(CrossAncestry[,c('f.eid', 'Ancestry_Group')], UKB[,c('f.eid', 'group')], by='f.eid')

    equals <- function(x) x[1] == x[2] 
    CrossAncestry <- data.frame(CrossAncestry %>%
        group_by(group) %>%
        mutate(Ancestry_Match = equals(Ancestry_Group)))

    CrossAncestry_Pairs <- CrossAncestry[CrossAncestry$Ancestry_Match == FALSE, ] 

    UKB <- UKB[!(UKB$group %in% CrossAncestry_Pairs$group),] # 175,250 rows (11,700 Non-European)

#################################################
## Spousal Detection using the Colocation file ##
#################################################

# Create Pair ID's based on matches in multiple columns (Colocation IDs)
    UKB_Colocation <- data.frame(UKB %>%
		group_by(group, NumInHouse, OwnRent, TDI) %>%
		mutate(PairID =cur_group_id()))

# Remove Colocation ID's that are shared by more/ fewer than two people:
    # Before: Before: 175,250 rows | After: # 160,738
    Coloc_Table <- data.frame(table(UKB_Colocation$PairID))
    colnames(Coloc_Table)[1] <- 'PairID'
    Coloc_Table <- Coloc_Table[Coloc_Table$Freq == 2, ]
    UKB_Colocation <- UKB_Colocation[UKB_Colocation$PairID %in% Coloc_Table$PairID,] # 160,738

# Create "PairSex" ID's based on the Pair ID (created above) and Sex:
    UKB_Colocation <- data.frame(UKB_Colocation %>%
		group_by(PairID, Sex) %>%
		mutate(PairSex =cur_group_id()))

# Remove Same-Sex Pairs (i.e., rows with PairSex IDs that belong to more than one person):
    # Before: 160,738 | After: 159,998
    PairSex_Frequency <- data.frame(table(UKB_Colocation$PairSex))
    colnames(PairSex_Frequency)[1] <- 'PairSex'
    PairSex_Frequency <- PairSex_Frequency[PairSex_Frequency$Freq==1, ]
    UKB_Colocation <- UKB_Colocation[UKB_Colocation$PairSex %in% PairSex_Frequency$PairSex,]

# Include couples that chose adjacent household income categories, or where at least one said "I don't know"/ "Prefer not to answer". 
# We're keeping people with a difference of 0 or 1 (because they're close enough), and people with a difference >= 1000 (because that means one didn't answer)
    # In other words, we're removing anybody with a difference of 2, 3, or 4. 
    # Before: 159,998 | After: 158,350

    # Change 'I don't know" and "Prefer not to answer" to -999
    UKB_Colocation$HouseholdIncome[UKB_Colocation$HouseholdIncome == -3] <- -999
    UKB_Colocation$HouseholdIncome[UKB_Colocation$HouseholdIncome == -1] <- -999

    # Create a subtraction function and apply it within each pair.
    minus <- function(x) x[1] - x[2] 
    UKB_Colocation <- data.frame(UKB_Colocation %>%
            group_by(PairID) %>%
            mutate(Income_Difference = abs(minus(HouseholdIncome))))
    UKB_Colocation <- UKB_Colocation[!(UKB_Colocation$Income_Difference %in% c(2,3,4)),]

# Use the subtraction function to remove based on age differences
    # Before: 158,350 | After: 158,148
    UKB_Colocation <- data.frame(UKB_Colocation %>%
            group_by(PairID) %>%
            mutate(Age_Difference = abs(minus(Age))))
    UKB_Colocation <- UKB_Colocation[UKB_Colocation$Age_Difference < 20,]

# Save Output:
    fwrite(UKB_Colocation, 'UKB_Cohabitation_Updated.txt', sep=',')

    UKB_Mates_All_Ethnicities <- UKB_Colocation[,c('f.eid', 'Sex', 'PairID')]
    fwrite(UKB_Mates_All_Ethnicities, 'UKB_Mate_Pairs_All.txt', sep=',')

#########################
##    Check Results    ##
#########################

# Here, we're comparing our sample to the sample derived by Yengo et al. 2018, published in Nature Genetics.
# Their sample is much smaller than ours, so want to check that the patterns between our samples line up. 

# The authors of Yengo et al. were kind enough to send us their code to make sure we are replicating their sample as closely as possible.

# Function to obtain mate-pair correlations:
    mate_correlations <- function(Dataset) {
        Full_Results <- data.frame()
        for (Trait in names(Phenotypes)[-1]){
            Trait_M <- eval(parse(text = paste0(Dataset, '$', Trait, '_M')))
            Trait_F <- eval(parse(text = paste0(Dataset, '$', Trait, '_F')))

            Mean_M  <- mean(Trait_M, na.rm = T)
            Mean_F  <- mean(Trait_F, na.rm = T)
            SD_M    <- sd(Trait_M, na.rm = T)
            SD_F    <- sd(Trait_F, na.rm = T)
            Z_M     <- abs((Trait_M - Mean_M) / SD_M)
            Z_F     <- abs((Trait_F - Mean_F) / SD_F)

            Pearson_R   <- round(cor.test(Trait_M, Trait_F)$estimate, 3)
            CI_Lower    <- round(cor.test(Trait_M, Trait_F)$conf.int[1], 3)
            CI_Upper    <- round(cor.test(Trait_M, Trait_F)$conf.int[2], 3)
            p_Value     <- format.pval(cor.test(Trait_M, Trait_F)$p.value, 3)
            Spearman_Rho <- round(cor.test(Trait_M, Trait_F, method= "spearman", exact=F)$estimate, 3)

            r_prime <- atanh(cor.test(Trait_M, Trait_F)$estimate)
            
            Mean_M  <- round(mean(Trait_M, na.rm = T), 3)
            Mean_F  <- round(mean(Trait_F, na.rm = T), 3)
            SD_M    <- round(sd(Trait_M, na.rm = T), 3)
            SD_F    <- round(sd(Trait_F, na.rm = T), 3)
            Num_Pairs <- nrow(Trait_F)

            Trait_Result <- data.frame(cbind(Trait, Mean_M, Mean_F, SD_M, SD_F, Z_M, Z_F, Pearson_R, CI_Lower, CI_Upper, p_Value, Num_Pairs, Spearman_Rho, r_prime))
            Full_Results <- rbind(Full_Results, Trait_Result)
        } 
        return(Full_Results)
    }

# Convert data from long to wide:
    long_to_wide <- function(Dataset) {
        as.data.frame(Dataset %>%
            select(c('f.eid','Sex','PairID', names(Phenotypes)[-1])) %>%
            pivot_wider(
            names_from = Sex,
            values_from = c('f.eid','Sex', all_of(names(Phenotypes)[-1]))))
    }

# Import the phenotype data:
    Continuous <- c("f.eid", "Year_of_Birth", "HandGrip", "Waist_Circumference", "Hip_Circumference", "Height", "Heel_BMD", "SBP", "DBP", "Pulse", "Birth_Weight", "Birthplace_North", "Birthplace_East", "Num_Cancers", "Num_Illnesses", "Num_Operations", "Num_Medications", "Age_Completed_Edu", "Time_TV", "Time_Computer", "Sleep_Duration", "Water_Intake", "Father_Age_at_Death", "Mother_Age", "Num_Sisters", "Num_Brothers", "Age_First_Intercourse", "Num_Sexual_Partners", "Num_Children", "AI", "CPD", "Age_Stopped_Smoking", "Num_Quitting_Attempts", "Father_Age", "PEF", "Weight", "Mother_Age_at_Death", "Num_Same_Sex_Partners", "Longest_Dep_Period", "Num_Dep_Episodes", "Num_Older_Sibs", "EA", "fIQ", "Neuroticism", "FEV1", "FVC", "FEV1_Pred_Percentage", "Pack_Years", "Pack_Years_Lifespan_Proportion", "Basal_Metabolic_Rate", "Whole_Body_Impedance", "WBC_Count", "RBC_Count", "Cholesterol", "CRP", "HDL", "LDL", "Num_Siblings", "Current_CPD_AllParticipants")

    Phenotypes <- fread("UKB_Phenotypes_QC.txt", h=T)[,..Continuous]

# Load the mate pairs from Yengo et al.: 
    YengoPairs <- fread('UKB_Coordinates_Final.txt', h=T)[,c('IIDm', 'IIDf')]
    YengoPairs$PairID <- 1:nrow(YengoPairs)
    YengoPairs <- data.frame(rbind(cbind(YengoPairs$IIDm, YengoPairs$PairID, 'M'), cbind(YengoPairs$IIDf, YengoPairs$PairID, 'F')))
    colnames(YengoPairs) <- c('f.eid', 'PairID', 'Sex')

# Load the pairs we derived above:
    JaredPairs <- fread('UKB_Mate_Pairs_All.txt', h=T)
    # names(JaredPairs)[names(JaredPairs) == 'PairID'] <- 'PairID_Jared'
    JaredPairs$Sex <- recode(as.character(JaredPairs$Sex), '1' = 'M', '0' = 'F')

##################
# Create Subsets #
##################

# Create datasets:
    Yengo_Phen  <- merge(YengoPairs, Phenotypes, by='f.eid')
    Jared_Phen  <- merge(JaredPairs, Phenotypes, by='f.eid')

    Only_Jared  <- merge(JaredPairs[!(JaredPairs$f.eid %in% YengoPairs$f.eid),], Phenotypes, by='f.eid')
    Only_Yengo  <- merge(YengoPairs[!(YengoPairs$f.eid %in% JaredPairs$f.eid),], Phenotypes, by='f.eid')
    In_Both     <- merge(YengoPairs[ (YengoPairs$f.eid %in% JaredPairs$f.eid),], Phenotypes, by='f.eid')

# Long to Wide:
    Jared_Phen  <- long_to_wide(Jared_Phen)
    Yengo_Phen  <- long_to_wide(Yengo_Phen)
    Only_Jared  <- long_to_wide(Only_Jared)
    Only_Yengo  <- long_to_wide(Only_Yengo)
    In_Both     <- long_to_wide(In_Both)

# Get correlation results:
    Jared_Results <- mate_correlations('Jared_Phen')
    Yengo_Results <- mate_correlations('Yengo_Phen')
    Only_Jared_Results <- mate_correlations('Only_Jared')
    Only_Yengo_Results <- mate_correlations('Only_Yengo')
    In_Both_Results    <- mate_correlations('In_Both')

# Compare Results:
# This approach is based on the third most upvoted post in this stackexchange thread: 
# https://stats.stackexchange.com/questions/278751/how-do-i-determine-whether-two-correlations-are-significantly-different
    n1 <- nrow(Yengo_Phen)
    n2 <- nrow(Only_Jared)

    S <- sqrt(1/(n1 - 3) + 1/(n2 - 3))

    z = (as.numeric(Yengo_Results$r_prime) - as.numeric(Only_Jared_Results$r_prime)) / S

    Cor_Comparison <- data.frame(cbind(names(Phenotypes)[-1], round(z, 4)))
    colnames(Cor_Comparison) <- c('Trait', 'z')
    Cor_Comparison$pval <- round(2 * (1 - pnorm(abs(as.numeric(Cor_Comparison$z)))), 5)
    Cor_Comparison$Bonferroni_Sig <- Cor_Comparison$pval <= .05/nrow(Cor_Comparison)

    # Looks good!!
