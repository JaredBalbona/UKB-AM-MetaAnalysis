
library(data.table)
library(dplyr)
library(tidyr)

setwd("/path/to/file/")

##########################
##      LOAD DATA       ##
##########################

# Read in the full UKB data file:
    UKB_Whole <- fread("UKB_Full.tab", h=T)[,]

# Limit the data to the fields we're interested in:
    Columns_of_Interest <- fread("UKB_Mate_Correlation_Columns.txt", h=T)
    UKB <- UKB_Whole[,colnames(UKB_Whole) %in% Columns_of_Interest$fids]

    names(UKB) <- c(Columns_of_Interest$Variable_Names)

# Take out people who have asked to be removed from the UKB:
	Redacted_FIDs <- fread("Redacted_Participants.csv", h=F)	
    UKB <- UKB[!(UKB$f.eid %in% Redacted_FIDs$V1),]

# Save the output:
    # fwrite(UKB, "UKB_Raw.txt", sep=",")

###################################################################
###################################################################

##########################
##       QC DATA        ##
##########################

# Read in the data file created above:
    UKB <- data.frame(fread("UKB_Raw.txt", h=T))
    FieldIDs <- names(UKB)

# Each column name with its corresponding field ID so that you can look up any columns online to see how they're coded/ what they mean:
    (Key <- cbind(FieldIDs,Column_Names))

# Change columns where -10 and -7 should be coded as zero-- Typically, this is because -10 and -7 indicate 'None of the Above' or 'Less than [the minimum option]', which is valuable information and is not something I want to remove/ set to NA
    UKB[,c("Time_TV", "Time_Computer", "Water_Intake", "CPD_FormerSmokers", "CPD_CurrSmokers")][UKB[,c("Time_TV", "Time_Computer", "Water_Intake", "CPD_FormerSmokers", "CPD_CurrSmokers")] == -10] <- 0
   
    UKB[,c("Dental_Problems", "Heart_Problems")][UKB[,c("Dental_Problems", "Heart_Problems")] == -7] <- 0

# For these other columns, set negative values to NA-- These are pretty much all "I don't know" or "Prefer not to answer" responses
    Remove_Negatives <- c("Birthplace_North", "Birthplace_East", "Freq_FriendFamily_Visits", "Time_TV", "Time_Computer", "Sleep_Duration", "Chronotype", "Insomnia", "Narcolepsy", "Current_Smoker", "Past_Smoker", "Water_Intake", "Alcohol_Freq", "Breastfed", "Comparative_Body_Size_Age_10", "Handedness", "Skin_Color", "Hair_Color", "Facial_Aging", "Adopted", "Multiple_Birth", "MSP", "Father_Alive", "Father_Age_at_Death", "Mother_Alive", "Mother_Age", "Num_Sisters", "Num_Brothers", "Mood_Swings", "Miserableness", "Irritability", "Hurt_Feelings", "FedUp_Feelings", "Nervous_Feelings" ,"Anxious_Feelings", "High_Strung", "Worry_After_Embarrassment", "Nerves", "Loneliness", "Guilt", "Risk_Taking", "Dep_2Weeks", "Disinterest_2Weeks", "Restlessness_2Weeks", "Lethargy_2weeks", "GP_Anx_Dep", "Psychiatrist_Anx_Dep", "Able_to_Confide", "Age_First_Intercourse", "Num_Sexual_Partners", "Same_Sex_Intercourse", "Overall_Health_Rating", "Longterm_Illness", "Wears_Glasses", "Plays_Computer_Games", "Hearing_Difficulty", "Chest_Pain", "Diabetes_Diagnosed", "Smoked_100_Times", "Num_Children_Fathered", "Num_Live_Births", "AI_CurrSmokers", "CPD_CurrSmokers", "AI_FormerSmokers", "CPD_FormerSmokers", "Age_Stopped_Smoking", "Ever_Quit_Smoking", "Num_Quitting_Attempts", "Father_Age", "Pain_for_3Months", "Time_to_First_Cig", "Difficulty_Not_Smoking", "Tried_to_Quit", "Want_to_Quit", "Mother_Age_at_Death", "Num_Same_Sex_Partners", "Former_Drinker", "Age_HayFever_Rhinitis_Eczema", "Age_Asthma", "Age_Heart_Attack", "Age_Emphysema_ChronicBronchitis", "NonAccidental_Family_Death", "Happiness", "Job_Satisfaction", "Health_Satisfaction", "Family_Relationship_Satisfaction", "Friendship_Satisfaction", "Financial_Satisfaction", "Ever_Dep_1Week", "Longest_Dep_Period", "Num_Dep_Episodes", "Ever_Manic_2Days", "Ever_Irritable_2Days", "Tinnitus", "Num_Older_Sibs", "Smoking_Status", "Drinking_Status", "Age_Completed_Edu", "Cancer_Diagnosed", "Dental_Problems", "Heart_Problems", "Alc_Add", "Sub_Behav_Addict", "Ill_Rec_Addict", "OTC_Addict")

    UKB[,names(UKB) %in% Remove_Negatives][UKB[names(UKB) %in% Remove_Negatives] < 0] <- NA

# Use EA4 coding of educational attainment:
    UKB <- UKB %>% mutate(EA = recode(EA,
                                '1'  = 20,
                                '2'  = 13,
                                '3'  = 10,
                                '4'  = 10,
                                '5'  = Age_Completed_Edu - 5,
                                '6'  = 15,
                                '-7' =  7))
    UKB$EA[UKB$EA < 7] <- NA

# Only 130 people reported being completely deaf, so for simplicity, we'll remove them from the hearing difficulty question to keep it binary:
    UKB$Hearing_Difficulty[UKB$Hearing_Difficulty == 99] <- NA

# Job satisfaction rating of 7 us 'I am unemployed', so we'll also remove that:
    UKB$Job_Satisfaction[UKB$Job_Satisfaction == 7] <- NA

# Matt wanted to remove ambidextrous people, so we'll do that:
    UKB$Handedness[UKB$Handedness == 3] <- NA
    UKB$Handedness <- UKB$Handedness - 1 # I prefer 0-1 to 1-2

# The Tinnitus question has 4 yes choices: Most/ all of the time (11), a lot of the time (12), some of the time (13), or in the past (14). I am going to condense those all to 1 for simplicity:
    UKB$Tinnitus[UKB$Tinnitus >= 11] <- 1

# The chronotype question has options for 'definitely' a morning/ evening person (1/4), and 'mostly' a morning/evening person (2/3). I'll combine those into two groups:
    UKB$Chronotype[UKB$Chronotype <= 2] <- 0
    UKB$Chronotype[UKB$Chronotype >= 3] <- 1

# The ordering for Comparative_Body_Size_Age_10 is weird (it goes 'Thinner' (1), 'Plumper' (2), and 'Average' (3)), so I'm just going to reorder it so that it makes sense as an ordinal variable (if you end up choosing to analyze it that way):
    UKB <- UKB %>% mutate(Comparative_Body_Size_Age_10 = recode(Comparative_Body_Size_Age_10,
                                '1'  = -1,
                                '2'  = 1,
                                '3'  = 0))

# Same for facial aging (it currently goes 'Younger' (1), 'Older' (2), and 'About my age' (3)), so I'm just going to reorder it so that it makes sense as an ordinal variable (again, if you choose to analyze it that way):
    UKB <- UKB %>% mutate(Facial_Aging = recode(Facial_Aging,
                                '1'  = -1,
                                '2'  = 1,
                                '3'  = 0))

# Look at the columns with negative values in our dataset to make sure that they're what we'd expect:
    head(Filter(function(x) any(x < 0), UKB)) # They are!

# Num_Brothers and Num_Sisters are mixed up in the UKB file (my mistake) so let's correct that:
    num_bro = which(colnames(UKB) == "Num_Brothers")
    num_sis = which(colnames(UKB) == "Num_Sisters")

    colnames(UKB)[num_bro] <- "Num_Sisters"
    colnames(UKB)[num_sis] <- "Num_Brothers"

# We decided not to use skin color as a phenotype:
    UKB$Skin_Color <- NULL

# For the following variables, we'll combine across columns to have a more meaningful measure:
    # Note: pmax stands for 'parallel maxima'
    UKB$HandGrip     <- pmax(UKB$HandGrip_L, UKB$HandGrip_R, na.rm=T)                 # Uses strongest hand
    UKB$AI           <- pmax(UKB$AI_FormerSmokers, UKB$AI_CurrSmokers, na.rm=T)       # Combines across former/ current smokers
    UKB$CPD          <- pmax(UKB$CPD_FormerSmokers, UKB$CPD_CurrSmokers, na.rm=T)     # Combines across former/ current smokers
    UKB$Num_Children <- pmax(UKB$Num_Children_Fathered, UKB$Num_Live_Births, na.rm=T) # Combines across parental sexes

    UKB$Num_Siblings   <- apply(UKB[,c('Num_Sisters','Num_Brothers')], 1, sum, na.rm=T) # Combines across sibling sexes
    UKB$Doctor_Anx_Dep <- (UKB$GP_Anx_Dep == 1 | UKB$Psychiatrist_Anx_Dep == 1) * 1     # Combines across doctor type

# Obtain yes/ no variables using the 'Age at ____ Diagnosis' variables:
    UKB$HayFever_Rhinitis_Eczema_Diag    <- (!is.na(UKB$Age_HayFever_Rhinitis_Eczema))*1
    UKB$Asthma_Diag                      <- (!is.na(UKB$Age_Asthma))*1
    UKB$Heart_Attack_Diag                <- (!is.na(UKB$Age_Heart_Attack))*1
    UKB$Emphysema_ChronicBronchitis_Diag <- (!is.na(UKB$Age_Emphysema_ChronicBronchitis))*1

# Remove the now unecessary columns (I'm still keeping the number of sisters/ brothers bc I think that's still meaningful):
    UKB <- subset(UKB, select = -c(HandGrip_L,
                                   HandGrip_R,
                                   AI_FormerSmokers,
                                   AI_CurrSmokers,
                                   CPD_FormerSmokers,
                                   CPD_CurrSmokers,
                                   Num_Children_Fathered,
                                   Num_Live_Births,
                                   GP_Anx_Dep,
                                   Psychiatrist_Anx_Dep,
                                   Age_HayFever_Rhinitis_Eczema,
                                   Age_Asthma,
                                   Age_Heart_Attack,
                                   Age_Emphysema_ChronicBronchitis))

# Create binary smoking status variables:
    UKB$Smoking_Status_NvFC <- (UKB$Smoking_Status == 0)*1 # Never vs. Former/ Current
    UKB$Smoking_Status_FvCN <- (UKB$Smoking_Status == 1)*1 # Former vs. Current/ Never
    UKB$Smoking_Status_CvFN <- (UKB$Smoking_Status == 2)*1 # Current vs. Former/ Never

# Create a variable that captures how much people are currently smoking:
    UKB$Current_CPD_AllParticipants <- UKB$CPD
    UKB$Current_CPD_AllParticipants[UKB$Smoking_Status == 0] <- 0
    UKB$Current_CPD_AllParticipants[UKB$Smoking_Status == 1] <- 0

# Make Drinking Status Binary:
    UKB$Drinking_Status[UKB$Drinking_Status == 1] <- 0
    UKB$Drinking_Status[UKB$Drinking_Status == 2] <- 1

# Make Dental Problems Binary:
    UKB$Dental_Problems <- (UKB$Dental_Problems > 0) * 1

# Make Heart Problems Binary: 
    UKB$Heart_Problems <- (UKB$Heart_Problems > 0) * 1

# Get binary measures for Major Depression and Bipolar
    UKB$Major_Dep   <- (UKB$Major_Dep >= 3) * 1
    UKB$Bipolar     <- (UKB$Bipolar == 1 | UKB$Bipolar == 2) * 1

# Finally, we'll assign zeros for substance addictions for people who have never been addicted to a substance
    UKB$Alc_Add[UKB$Sub_Behav_Addict == 0]  <- 0
    UKB$OTC_Addict[UKB$Sub_Behav_Addict == 0] <- 0
    UKB$Ill_Rec_Addict[UKB$Sub_Behav_Addict == 0] <- 0

# For ease of analysis, separate phenotypes into Binary, Continuous, and Other (i.e., ordinal and nominal)
    Binary <- c("Chronotype", "Breastfed", "Adopted", "Multiple_Birth", "MSP", "Father_Alive", "Mother_Alive", "Mood_Swings", "Miserableness", "Irritability", "Hurt_Feelings", "FedUp_Feelings", "Nervous_Feelings", "Anxious_Feelings", "High_Strung", "Worry_After_Embarrassment", "Nerves", "Loneliness", "Guilt", "Risk_Taking", "Doctor_Anx_Dep", "Same_Sex_Intercourse", "Longterm_Illness", "Wears_Glasses", "Chest_Pain", "Diabetes_Diagnosed", "Cancer_Diagnosed", "Smoked_100_Times", "Ever_Quit_Smoking", "Pain_for_3Months", "Pacemaker", "Former_Drinker", "NonAccidental_Family_Death", "Ever_Dep_1Week", "Ever_Manic_2Days", "Ever_Irritable_2Days", "Tinnitus", "Ever_Smoked", "HayFever_Rhinitis_Eczema_Diag", "Asthma_Diag", "Heart_Attack_Diag", "Emphysema_ChronicBronchitis_Diag", "Drinking_Status", "Hearing_Difficulty", "Dental_Problems", "Heart_Problems", "Smoking_Status_NvFC", "Smoking_Status_FvCN", "Smoking_Status_CvFN", "Alc_Add", "Sub_Behav_Addict", "Ill_Rec_Addict", "OTC_Addict", "Major_Dep", 'Bipolar', 'Handedness')

    Continuous <- c("Year_of_Birth", "HandGrip", "Waist_Circumference", "Hip_Circumference", "Height", "Heel_BMD", "SBP", "DBP", "Pulse", "Birth_Weight", "Birthplace_North", "Birthplace_East", "Num_Cancers", "Num_Illnesses", "Num_Operations", "Num_Medications", "Age_Completed_Edu", "Time_TV", "Time_Computer", "Sleep_Duration", "Water_Intake", "Father_Age_at_Death", "Mother_Age", "Num_Sisters", "Num_Brothers", "Age_First_Intercourse", "Num_Sexual_Partners", "Num_Children", "AI", "CPD", "Age_Stopped_Smoking", "Num_Quitting_Attempts", "Father_Age", "PEF", "Weight", "Mother_Age_at_Death", "Num_Same_Sex_Partners", "Longest_Dep_Period", "Num_Dep_Episodes", "Num_Older_Sibs", "EA", "fIQ", "Neuroticism", "FEV1", "FVC", "FEV1_Pred_Percentage", "Pack_Years", "Pack_Years_Lifespan_Proportion", "Basal_Metabolic_Rate", "Whole_Body_Impedance", "WBC_Count", "RBC_Count", "Cholesterol", "CRP", "HDL", "LDL", "Num_Siblings", "Current_CPD_AllParticipants")

    Other <- c("Hair_Color", "Facial_Aging", "Tried_to_Quit", "Smoking_Status", "Insomnia", "Narcolepsy", "Current_Smoker", "Past_Smoker", "Alcohol_Freq", "Comparative_Body_Size_Age_10", "Dep_2Weeks", "Disinterest_2Weeks", "Restlessness_2Weeks", "Lethargy_2weeks", "Able_to_Confide", "Overall_Health_Rating", "Plays_Computer_Games", "Time_to_First_Cig", "Difficulty_Not_Smoking", "Want_to_Quit", "Happiness", "Job_Satisfaction", "Health_Satisfaction", "Family_Relationship_Satisfaction", "Friendship_Satisfaction", "Financial_Satisfaction", "Freq_FriendFamily_Visits")

#  fwrite(UKB, "UKB_Phenotypes_QC.txt", sep=",")


# Load Mates:
Mates <- fread("UKB_Mates_All_Ethnicities_UPDATED.txt", h=T)
		
# Merge the two:
    UKB_Mates <- merge(UKB, Mates, by='f.eid')
    UKB_Mates$Sex[UKB_Mates$Sex==0] <- "F"
    UKB_Mates$Sex[UKB_Mates$Sex==1] <- "M"

# Convert from long to wide:
    UKB_All <- as.data.frame(UKB_Mates %>%
      pivot_wider(
        names_from = Sex,
        values_from = names(UKB)))

    UKB_Binary <- as.data.frame(UKB_Mates %>%
      select('f.eid','Sex','PairID', all_of(Binary)) %>%
      pivot_wider(
        names_from = Sex,
        values_from = c('f.eid','Sex', all_of(Binary))))

    UKB_Continuous <- as.data.frame(UKB_Mates %>%
      select(c('f.eid','Sex','PairID', all_of(Continuous))) %>%
      pivot_wider(
        names_from = Sex,
        values_from = c('f.eid','Sex', all_of(Continuous))))

    UKB_Other <- as.data.frame(UKB_Mates %>%
      select(c('f.eid','Sex','PairID', all_of(Other))) %>%
      pivot_wider(
        names_from = Sex,
        values_from = c('f.eid','Sex', all_of(Other))))

# Save Output:
    fwrite(UKB_Binary, "UKB_MateData_Binary_AllEthnicities.txt", sep=",")
    fwrite(UKB_All, "UKB_MateData_AllPhen_AllEthnicities.txt", sep=",")
    fwrite(UKB_Continuous, "UKB_MateData_Continuous_AllEthnicities.txt", sep=",")
    fwrite(UKB_Other, "UKB_MateData_Other_AllEthnicities.txt", sep=",")

# Take a look at the missingness for each column:
    # Number of Missing values in each column:
    sort(colSums(is.na(UKB_All)), decreasing=T)

    # Proportion of rows missing data in each column:
    sort(round(colSums(is.na(UKB_All))/nrow(UKB_All),3), decreasing=T)

    # Columns that are more than 90% missing:
    which(colSums(is.na(UKB_All))/nrow(UKB_All) > .9)

###########################
## Create Covariate File ##
###########################

# Get Principal Components and Country of Birth:
    UKB_Whole <- fread("UKB_Full.tab", h=T)[,]

    UKB <- UKB_Whole[, c("f.eid", "f.22009.0.1", "f.22009.0.2", "f.22009.0.3", "f.22009.0.4", "f.22009.0.5", "f.22009.0.6", "f.22009.0.7", "f.22009.0.8", "f.22009.0.9", "f.22009.0.10", "f.22009.0.11", "f.22009.0.12", "f.22009.0.13", "f.22009.0.14", "f.22009.0.15", "f.22009.0.16", "f.22009.0.17", "f.22009.0.18", "f.22009.0.19", "f.22009.0.20", "f.22009.0.21", "f.22009.0.22", "f.22009.0.23", "f.22009.0.24", "f.22009.0.25", "f.22009.0.26", "f.22009.0.27", "f.22009.0.28", "f.22009.0.29", "f.22009.0.30", "f.22009.0.31", "f.22009.0.32", "f.22009.0.33", "f.22009.0.34", "f.22009.0.35", "f.22009.0.36", "f.22009.0.37", "f.22009.0.38", "f.22009.0.39", "f.22009.0.40", 'f.1647.0.0')]

    PC_Names <- paste0('PC', 1:40)
    colnames(UKB) <- c('f.eid', PC_Names, 'Country_of_Birth')

    UKB$Country_of_Birth[UKB$Country_of_Birth < 0] <- NA
    UKB$Born_in_British_Isles <- (UKB$Country_of_Birth != 6)*1 # 1 = Born in British Isles, 0 = Not born in British Isles
    UKB$Country_of_Birth <- NULL

    fwrite(UKB, "UKB_PCs_Birthplace.txt", sep=",")

# Merge the PC/ Covariate file with the Mate Pair file:
    UKB <- fread("UKB_PCs_Birthplace.txt", h=T)

    # NOTE: The code here leaves out year of birth, so I'm adding that here:
        YOB <- data.frame(fread("UKB_Raw.txt", h=T))[,c('f.eid', 'Year_of_Birth')] 
        UKB <- merge(UKB, YOB, by='f.eid')

    Mates <- fread('/UKB_Mates_All_Ethnicities_UPDATED.txt', h=T)
    Mates$Sex[Mates$Sex==0] <- "F"
    Mates$Sex[Mates$Sex==1] <- "M"

    PCs_Mates <- merge(UKB, Mates, by = 'f.eid')

    PCs_Mates_Final <- as.data.frame(PCs_Mates %>%
        pivot_wider(
        names_from = Sex,
        values_from = names(UKB)))

    fwrite(PCs_Mates_Final, "PCs_Birthplace_Mates.txt", sep=",")


#######
