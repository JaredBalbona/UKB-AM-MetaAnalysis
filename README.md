## UK Biobank (UKB) Mate Pair Analysis

----

### Broad Summary:

These scripts contain nearly all of the code for [a recent publication in Nature Human Behavior](https://www.nature.com/articles/s41562-023-01672-z) that I worked on with Tanya Horwitz, Katie Paulich, and Matt Keller. Taken as a whole, the majority of what's included here serves to get the data in the appropriate form to be analyzed— the analysis themselves make up a relatively small portion of the code. My goal is that these scripts will be useful for researchers who are curious about how exactly are study was performed, and interested in building upon it themselves.


Please feel free to reach out to me at jared.v.balbona@gmail.com with any quesitons— Thanks!

----

### Specific Steps:

 1. #### Detect_Related_Pairs_NonEuropean.R:  ####
   - The goal of this script is to identify which pairs of individuals in the UKB sample are related so that they can be removed (as their inclusion can potentially bias results). A member of our lab had already been completed this for the participants of European ancestry, and thus I only needed to complete it for the non-European participants.
   - Prior to running this script, we used k-means clustering with the non-European participants' first 130 genetic principal components to identify 10 non-European ancestry groups. Then, within each cluster, I created a genomic relationship matrix (GRM; done using [a technique](https://gcta.freeforums.net/thread/175/gcta-estimating-genetic-relationship-using?page=1&scrollTo=576) from Dr. Jian Yang) that provides the relatedness coefficient ($\hat{\pi}$) for all pairs of individuals. 
     - From here, I identified the IDs of pairs with $\hat{\pi} \ge$ .05 and saved those to an output file. 
     - IMPORTANT NOTE: Because GRMs cannot be calculated cross-ancestrally (at least not without potentially significant bias), I could only identify related pairs within-ancestry. Thus, to minimize bias, our study does not include any cross-ancestry mate pairs. 
     
 2. #### Deriving_Mate_Pairs.R: ####
   - This script seeks to identify which pairs of participants in the UK Biobank are mates/ partners/ spouses, etc. based on a variety of demographic questions. While our approach is similar those used in past studies of assortment, ours manages to identify 79K mate pairs— Substantially more than nearly all existant studies of AM.
     - As described in our manuscript, I used used colocation/ cohabitation information provided by the UK Biobank to identify which participants reside at the same address. Importantly, this does not necessarily mean that they live in the same *residence*, as it could include apartment complexes, assisted living faciliites, etc.
     - It is also important to note that, while the colocation/ cohabitation file is no longer available from the UKB, this information can be approximated quite closely using other variables such as latitude, longitude, and measures of measures of physical proximity (see Border et al. 2022). 
   -  Once pairs were identified, I compared the similarity of mates in our sample to those identified in [a previous study by Yengo et al. (2018)](https://www.nature.com/articles/s41562-018-0476-3). For context, I was able to obtain the pairs used in the Yengo et al. study (prior to running this script) by using code that was kindly sent to me by the authors.
   
 3. #### UKB_Mate_Correlations.R: ####
   - Here, I am taking the full UKB phenotype file (which contains >500K people and >13K traits) and getting it into the proper format for our analyses. This process mostly involves going trait-by-trait and recoding variables— Largely to make them more concordant with the variables used in past studies, but also to make them more appropriate for the analyses used in this study. For convenience, I then divided the varaibles into three groups— continuous, ordinal, and binary— which will be useful in upcoming scripts. 
   - Once the phenotypes were all in order, I merged them with the mate pair files created in step 2 and then created a covariate file that included the individuals' first 40 genetic PCs and their country of origin (which I also dichotomized as Britain vs. Elsewhere). 
   
 4-6. **(Continuous/ Binary/ Ordinal) Phenotypes_UKB.R:**
   - In these three scripts, I am separately analyzing the mate-pair correlations (both partial and zero-order) for each type of trait. Notably, this could have been done within one script, but I personally prefer breaking it up for the sake of clarity. 
     - The correlations run in the ordinal variable script might look a little weird, so here's a quick explanation: There is [evidence from past studies](https://www.nature.com/articles/s41431-018-0159-6) showing that normalization (specifically rank-based inverse normal transformation) of dependent variables can re-introduce covariate effects. While our study did not involve rank-based INT, I did observe this issue with the Spearman correlations (which makes sense, given that a Spearman is just a Pearson correlation on ranks). Thus, to correct the issue, I followed the suggestions from that paper and ranked the dependnet variables *prior* to residualizing them, and then ran a Pearson's correlation on the residualized variables. 
     - It should also be noted that for the tetrachoric correlations (used for binary traits), I used the *lavaan* package to calculate the partial correlations— This is akin to estimating the path coefficients for a structural equation model. As such it is slow— Therefore, I included a much faster implemetation that can be used for the zero-order tetrachoric correlations above the lavaan code. 

 7. #### Plot_UKB_Mate_Correlations.R: ####
   - Not a ton to unpack here! This is just the code I used to create the plots in our manuscript (made using ggplot2). 
