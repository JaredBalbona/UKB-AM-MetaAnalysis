
# File to detect related pairs among the non-European UKB participants
# Code is based on a GCTA message board post from Jian Yang: 
# https://gcta.freeforums.net/thread/175/gcta-estimating-genetic-relationship-using?page=1&scrollTo=576

library(data.table)

setwd("/path/to/file")

ReadGRMBin=function(prefix, use_all_n=F, size=4){

    sum_i = function(i){
        return((1+i)*i/2)
    }

    bin_file_name <- paste(prefix,".grm.bin",sep="")
    n_file_name   <- paste(prefix,".grm.N.bin",sep="")
    id_file_name  <- paste(prefix,".grm.id",sep="")

    id      <- read.table(id_file_name)
    n       <- dim(id)[1]
    n_file   <- file(n_file_name, "rb");

    bin_file <- file(bin_file_name, "rb");
    grm <- readBin(bin_file, n=n*(n+1)/2, what=numeric(0), size=size)

    if(use_all_n){
        N <- readBin(n_file, n=n*(n+1)/2, what=numeric(0), size=size)
    }
    else N <- readBin(n_file, n=1, what=numeric(0), size=size)

    i <- sapply(1:n, sum_i)

    return(list(diag=grm[i], off=grm[-i], id=id, N=N))
}

file_names  <-  c("keep_iid_snps_merged.k1",
                "keep_iid_snps_merged.k2",
                "keep_iid_snps_merged.k3",
                "keep_iid_snps_merged.k4",
                "keep_iid_snps_merged.k5",
                "keep_iid_snps_merged.k6",
                "keep_iid_snps_merged.k7",
                "keep_iid_snps_merged.k8",
                "keep_iid_snps_merged.k9",
                "keep_iid_snps_merged.k10")

relatives <- data.frame(ID_1 = numeric(),
                        ID_2 = numeric(),
                        pi_hat = numeric(),
                        group = numeric())

non_eurs_all <- data.frame(f.eid = numeric(), group = numeric())

for(i in 1:length(file_names)){
    print(i)
    GRM <- ReadGRMBin(file_names[i])
    grm_full <- matrix(NA, nrow = length(GRM$diag), ncol = length(GRM$diag))
    rownames(grm_full) <- colnames(grm_full) <- GRM$id[,1]
    
    grm_full[lower.tri(grm_full)] <- GRM$off
    
    RelsID_1 <- rownames(grm_full)[which(grm_full >= .05, arr.ind = TRUE)[, 1]]
    RelsID_2 <- colnames(grm_full)[which(grm_full >= .05, arr.ind = TRUE)[, 2]]
    pi_hat <- grm_full[which(grm_full >= .05)]
   
    Temp_Rels <- cbind(RelsID_1, RelsID_2, pi_hat, i)
    Rels <- rbind(relatives, Temp_Rels)

    Temp_NonEurs_All <- cbind(GRM$id[,1], i)
    NonEurs_All <- rbind(non_eurs_all, Temp_NonEurs_All)
}

relatives$pi_hat <- round(as.numeric(as.character(relatives$pi_hat)),4)
colnames(relatives) <- c("ID_1", "ID_2", "pi_hat", "Ancestry_Group")
colnames(non_eurs_all) <- c("f.eid", "Ancestry_Group")
fwrite(relatives, "NonEuro_Related_Pairs.txt", sep = ",")
fwrite(non_eurs_all, "NonEuro_Ancestry.txt", sep = ",")
