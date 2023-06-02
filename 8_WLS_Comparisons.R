library(data.table)
library(psych)
library(car)
library(survey)

UKB <- fread('~/Documents/Projects/AM_MetaAnalysis/Revision_April2023/Mate_Correlations_All_Updated.txt', h=T)

UKB2 <- UKB[UKB$Phenotype2 != 'Num Same Sex Partners',] # Huge SE
UKB2 <- UKB2[order(UKB2$Category),]
UKB2[order(UKB2$Correlation),]

#####################################################
## Test 1: Looking at the effect of Trait Category ##
#####################################################

# Levene's Test:
cov_model <- lm(Correlation ~ Correlation_Type, data = UKB2)
cov_residuals <- resid(cov_model)
leveneTest(cov_residuals ~ UKB2$Category) 
#Levene's test result: F(5, 127) = 9.75, p = 6e-08, suggesting that the assumption of equal variances may be violated. 

# Welch's test on residuals:
pmod2   <- lm(Correlation ~ Correlation_Type, data=UKB2)
rp2     <- resid(pmod2)

(pmod1r <- lm(rp2 ~ UKB2$Category, weights = UKB2$SE^-2))
(Welch_Res1 <- anova(pmod1r))
# Welch Results: F(5, 127) = 24.37, p < 2e-16

# Obtain the SS and DF from the ANOVA table
SS_Category <- Welch_Res1$`Sum Sq`[1]
SS_total    <- sum(Welch_Res1$`Sum Sq`)
df_Category <- Welch_Res1$Df[1]
df_error    <- Welch_Res1$Df[2]

# Calculate the effect size (eta-squared):
(eta_squared = SS_Category/ sum(Welch_Res1$`Sum Sq`)) # 0.4896429

# Calculate the CI for eta squared
n <- length(pmod1r$residuals)
k <- length(pmod1r$coefficients)

SE <- sqrt((2 * eta_squared^2) / ((n - k - 1) * (1 - eta_squared)^2))
CI_lower <- eta_squared - qt(1 - .05/2, df = n - k - 1) * SE
CI_upper <- eta_squared + qt(1 - .05/2, df = n - k - 1) * SE

####################################################################
## Test 2: Looking at the effect of Trait Category (Dichotomized) ##
####################################################################

# Now make two category groups to see if they're significantly different:
UKB2$Dichotomized_Category <- 'A'
UKB2$Dichotomized_Category[UKB2$Category %in% c("Anthropomorphic","Health","Psychological")] <- 'B'

# Levene's Test:
leveneTest(cov_residuals ~ UKB2$Dichotomized_Category) 
#Levene's test result: F(1, 131) = 31.52, p = 1e-07, suggesting that the assumption of equal variances may be violated. 

# Weighted Welch's T test: 
# Create a survey design object
design <- svydesign(ids = ~1, weights = ~(1/UKB2$SE^2), data = UKB2)

# Perform weighted Welch's t-test
(svyttest(Correlation ~ Dichotomized_Category, design, vartype = "ci", var.equal = FALSE)) # t(131) = 4.854, p < .001

group_means <- tapply(UKB2$Correlation, UKB2$Dichotomized_Category, mean)
group_sds   <- tapply(UKB2$Correlation, UKB2$Dichotomized_Category, sd)
group_ns    <- tapply(UKB2$Correlation, UKB2$Dichotomized_Category, length)

pooled_sd <- sqrt(((group_ns[1] - 1) * group_sds[1]^2 + (group_ns[2] - 1) * group_sds[2]^2) / (group_ns[1] + group_ns[2] - 2))
(cohens_d <- (group_means[1] - group_means[2]) / pooled_sd)

# Calculate CI: 
standard_error <- sqrt((group_ns[1] + group_ns[2]) /
                         (group_ns[1] * group_ns[2]) + 
                         (cohens_d^2 / (2 * (group_ns[1] + group_ns[2]))))
confidence_level <- 0.95  # Adjust as needed
critical_value  <- qnorm(1 - (1 - confidence_level) / 2)
lower_bound <- cohens_d - critical_value * standard_error
upper_bound <- cohens_d + critical_value * standard_error

#######################################################
## Test 3: Looking at the effect of Correlation Type ##
#######################################################

cov_model <- lm(Correlation ~ Category, data = UKB2)
resids <- resid(cov_model)
leveneTest(resids ~ UKB2$Correlation_Type) 
# The Levene's test for homogeneity of variance, using the median as the center, 
# indicated no significant differences in variance across the groups (F(2, 130) = 0.7417, p = 0.4783).

# Finally, let's get the means & SEs:
tapply(UKB2$Correlation, UKB2$Category, mean)
tapply(UKB2$Correlation, UKB2$Category, function(x) sd(x)/sqrt(length(x)))

summary(mod1 <- lm(Correlation ~ Category + Correlation_Type, data = UKB2, weights = UKB2$SE^-2)) 
summary(mod2 <- lm(Correlation ~ Category, data = UKB2, weights = UKB2$SE^-2))
anova(mod1,mod2) 
