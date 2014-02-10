library(data.table)
rawData = fread("train_v2.csv")
#total 771 vars
colVar = apply(rawData, 2, var)
zeroVarCols = names(which(colVar < 1e-8))
#zero variance (19):
#f33  f34  f35  f37  f38 f336 f382 f418 f465 f477 f480 f530 f659 f700 f701 f702 f732 f736 f764

for(i in zeroVarCols){
  rawData[,paste0(i):=NULL]
}
#752 vars left

library(Hmisc)
rawM = as.matrix(rawData)
#drop "id" and "loss" column
rawM = rawM[,-c(1,752)]
corrM = rcorr(rawM)$r

perfectCorrEntries = which(corrM > 1-1e-6, arr.in=TRUE)
colDiffVec = perfectCorrEntries[,2] - perfectCorrEntries[,1]
perfectCorrEntries = cbind(perfectCorrEntries, colDiffVec)
perfectCorrPairs = pecrfectCorrEntries[which(perfectCorrEntries[,3]>0),]
uniqColNrs = unique(perfectCorrPairs[,2])
colNames = colnames(corrM)
col2rm = colNames[uniqColNrs]

#vars to be removed:

#  [1] "f58"  "f86"  "f87"  "f88"  "f96"  "f97"  "f98"  "f106" "f107" "f108"
# [11] "f116" "f117" "f118" "f126" "f127" "f128" "f155" "f156" "f157" "f165"
# [21] "f166" "f167" "f175" "f176" "f177" "f185" "f186" "f187" "f195" "f196"
# [31] "f197" "f225" "f226" "f227" "f235" "f236" "f237" "f245" "f246" "f247"
# [41] "f255" "f256" "f257" "f265" "f266" "f267" "f294" "f295" "f296" "f302"
# [51] "f303" "f304" "f310" "f311" "f312" "f318" "f319" "f320" "f326" "f327"
# [61] "f328" "f345" "f354" "f362" "f371" "f379" "f408" "f417" "f427" "f439"
# [71] "f452" "f453" "f457" "f467" "f478" "f483" "f484" "f488" "f493" "f494"
# [81] "f498" "f503" "f504" "f508" "f527" "f528" "f532" "f538" "f539" "f543"
# [91] "f548" "f549" "f553" "f558" "f559" "f563" "f568" "f569" "f573" "f577"
#[101] "f578" "f582" "f599" "f722" "f729" "f741" "f763" "f765" "f768" "f770"

for(i in col2rm){
    rawData[,paste0(i):=NULL]
}

write.table(rawData,file="train_v2_filtered.csv",sep=",",row.names=F)
