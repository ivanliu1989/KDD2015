setwd('Google Drive/KDD2015')
rm(list = ls()); gc()
require(data.table);require(caret);require(doMC);require(ROCR)
registerDoMC(core=3)
options(scipen=3)

### min(<0.5)|max(>0.5)
ensem_prob <- matrix(0, nrow = 24108, ncol = 2, dimnames = list(NULL, NULL))
datadirectory <- 'results/' # 'results/best'
files <- list.files(datadirectory,full.names = T)

for (file in files){
    result <- as.matrix(fread(file,header = T, stringsAsFactor = F, data.table=F))
    ensem_prob <- ensem_prob + result 
}
# ensem_prob[,2:10] <- prop.table(ensem_prob[,2:10], 1) 
ensem_prob[,1:2] <- ensem_prob[,1:2]/length(files)
score <- auc(ensem_prob, target_val);print(score)
