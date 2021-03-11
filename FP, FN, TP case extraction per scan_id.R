install.packages("qpcR")
library(qpcR)

install.packages("tidyr")
library(tidyr)

install.packages("plyr")
library(plyr)

install.packages("dplyr")
library(dplyr)

############### Data prep #########
#Add "-m" to the end of each of the parasite names in the manula doc





############### Manual only count #############

A_C2 = read.csv("manual_data_minus_dump_202102261212.csv", head=TRUE, sep=",")

### Sums
#oxyuris_equi
Individual_pieces <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(oxyuris_equi))
write.csv(Individual_pieces, file = "Manual count Oxyuris_equi.csv")

# Stronglye
Individual_pieces <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyle))
write.csv(Individual_pieces, file = "Manual count Stronglye.csv")

#anoplocephala
Individual_pieces <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(anoplocephala))
write.csv(Individual_pieces, file = "Manual count Anoplocephala.csv")

#parascaris_equorum
Individual_pieces <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(parascaris_equorum ))
write.csv(Individual_pieces, file = "Manual count Parascaris_equorum.csv")

#strongyloides_westeri
Individual_pieces <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyloides_westeri))
write.csv(Individual_pieces, file = "Manual count Strongyloides_westeri.csv")

############### Auto count only ##########

A_C2 = read.csv("auto_data_minus_dump.csv", head=TRUE, sep=",")

### Sums
#oxyuris_equi
Individual_pieces <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(oxyuris_equi))
write.csv(Individual_pieces, file = "Auto count Oxyuris_equi.csv")

# Stronglye
Individual_pieces <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyle))
write.csv(Individual_pieces, file = "Auto count Stronglye.csv")

#anoplocephala
Individual_pieces <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(anoplocephala))
write.csv(Individual_pieces, file = "Auto  count Anoplocephala.csv")

#parascaris_equorum
Individual_pieces <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(parascaris_equorum ))
write.csv(Individual_pieces, file = "Auto count Parascaris_equorum.csv")

#strongyloides_westeri
Individual_pieces <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyloides_westeri))
write.csv(Individual_pieces, file = "Auto count Strongyloides_westeri.csv")


############### FP SUM ###############

A_C = read.csv("Auto and Manual combined minus dumps -HSI.csv", head=TRUE, sep=",")


## subset scan_images where there is a difference(+ or -) between respective columns and writing them to a file. 
SS2 <- subset(A_C, subset = c(A_C$oxyuris_equi != A_C$oxyuris_equi_m | A_C$strongyle != A_C$strongyle_m | 
                                A_C$anoplocephala != A_C$anoplocephala_m | A_C$parascaris_equorum != A_C$parascaris_equorum_m |
                                A_C$strongyloides_westeri != strongyloides_westeri_m))

## False positive case extraction
## working on the all differences file, we want to extract where a specific parasite's 
## Auto columns have values that are greater than the manual
## columns (false positive) they are subsetted.


A_C1 = SS2

#All FP cases extracted
FP_all <- subset(A_C1, subset = c(A_C1$oxyuris_equi > A_C1$oxyuris_equi_m | A_C1$strongyle > A_C1$strongyle_m | 
                                    A_C1$anoplocephala > A_C1$anoplocephala_m | A_C1$parascaris_equorum > A_C1$parascaris_equorum_m |
                                    A_C1$strongyloides_westeri > A_C1$strongyloides_westeri_m ))
write.csv(FP_all, file = "false positive all.csv")

# extraction of cases false positive cases for each parasite and saving to individual variables
FP_oxyuris_equi <- subset(A_C1, subset = c(A_C1$oxyuris_equi > A_C1$oxyuris_equi_m)) 
FP_strongyle <- subset(A_C1, subset = c(A_C1$strongyle > A_C1$strongyle_m)) 
FP_anoplocephala <- subset(A_C1, subset = c(A_C1$anoplocephala > A_C1$anoplocephala_m)) 
FP_parascaris_equorum <- subset(A_C1, subset = c(A_C1$parascaris_equorum  > A_C1$parascaris_equorum_m)) 
FP_strongyloides_westeri <- subset(A_C1, subset = c(A_C1$strongyloides_westeri  > A_C1$strongyloides_westeri_m)) 


### Sums
#oxyuris_equi
FP_sum_OE <- FP_oxyuris_equi %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(oxyuris_equi - oxyuris_equi_m))

# Stronglye
FP_sum_S <- FP_strongyle %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyle - strongyle_m))

#anoplocephala
FP_sum_A <- FP_anoplocephala %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(anoplocephala - anoplocephala_m))

#parascaris_equorum
FP_sum_PE <- FP_parascaris_equorum %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(parascaris_equorum - parascaris_equorum_m))

#strongyloides_westeri
FP_sum_SW  <- FP_strongyloides_westeri %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyloides_westeri - strongyloides_westeri_m))

# Write all the data to one spreadsheet
dta <- qpcR:::cbind.na(FP_sum_OE, FP_sum_S, FP_sum_A, FP_sum_PE, FP_sum_SW)
write.csv(dta, file = "FP per parasite.csv")

### No we get the TP remainder from the FP calculation

#oxyuris_equi
FP_TP_Remainder_OE <- FP_oxyuris_equi %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(oxyuris_equi_m))

# Stronglye
FP_TP_Remainder_S <- FP_strongyle %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyle_m))

#anoplocephala
FP_TP_Remainder_A <- FP_anoplocephala %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(anoplocephala_m))

#parascaris_equorum
FP_TP_Remainder_PE <- FP_parascaris_equorum %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(parascaris_equorum_m))

#strongyloides_westeri
FP_TP_Remainder_SW <- FP_strongyloides_westeri %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyloides_westeri_m))

# Write all the data to one spreadsheet
dta <- qpcR:::cbind.na(FP_TP_Remainder_OE, FP_TP_Remainder_S, FP_TP_Remainder_A, FP_TP_Remainder_PE, FP_TP_Remainder_SW)
write.csv(dta, file = "FP TP Remainder per parasite.csv")






############### FN ##############
A_C = read.csv("Auto and Manual combined minus dumps -HSI.csv", head=TRUE, sep=",")

## subset scan_images where there is a difference(+ or -) between respective columns and writing them to a file. 
SS2 <- subset(A_C, subset = c(A_C$oxyuris_equi != A_C$oxyuris_equi_m | A_C$strongyle != A_C$strongyle_m | 
                                A_C$anoplocephala != A_C$anoplocephala_m | A_C$parascaris_equorum != A_C$parascaris_equorum_m |
                                A_C$strongyloides_westeri != strongyloides_westeri_m))
A_C1= SS2


#Pulling all FN cases. 
FP_all <- subset(A_C1, subset = c(A_C1$oxyuris_equi < A_C1$oxyuris_equi_m | A_C1$strongyle < A_C1$strongyle_m | 
                                    A_C1$anoplocephala < A_C1$anoplocephala_m | A_C1$parascaris_equorum < A_C1$parascaris_equorum_m |
                                    A_C1$strongyloides_westeri < A_C1$strongyloides_westeri_m ))
write.csv(FP_all, file = "false negative all.csv")

#Extraction of False Negative files and saving to individual variables

FN_oxyuris_equi <- subset(A_C1, subset = c(A_C1$oxyuris_equi < A_C1$oxyuris_equi_m)) 
FN_strongyle <- subset(A_C1, subset = c(A_C1$strongyle < A_C1$strongyle_m)) 
FN_anoplocephala <- subset(A_C1, subset = c(A_C1$anoplocephala < A_C1$anoplocephala_m)) 
FN_parascaris_equorum <- subset(A_C1, subset = c(A_C1$parascaris_equorum  < A_C1$parascaris_equorum_m)) 
FN_strongyloides_westeri <- subset(A_C1, subset = c(A_C1$strongyloides_westeri  < A_C1$strongyloides_westeri_m)) 

### Sums
#oxyuris_equi
FN_sum_OE <- FN_oxyuris_equi %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(oxyuris_equi_m - oxyuris_equi))

# Stronglye
FN_sum_S <- FN_strongyle %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyle_m - strongyle))

#anoplocephala
FN_sum_A <- FN_anoplocephala %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(anoplocephala_m - anoplocephala))

#parascaris_equorum
FN_sum_PE  <- FN_parascaris_equorum %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(parascaris_equorum_m - parascaris_equorum))

#strongyloides_westeri
FN_sum_SW  <- FN_strongyloides_westeri %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyloides_westeri_m - strongyloides_westeri))


# Write all the data to one spreadsheet
dta <- qpcR:::cbind.na(FN_sum_OE, FN_sum_S, FN_sum_A, FN_sum_PE, FN_sum_SW)
write.csv(dta, file = "FN per parasite.csv")


#### Picking up the TP that are left behind from the FN calculation

#oxyuris_equi
FN_TP_Remainder_O <- FN_oxyuris_equi %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(oxyuris_equi))

# Stronglye
FN_TP_Remainder_S  <- FN_strongyle %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyle))

#anoplocephala
FN_TP_Remainder_A  <- FN_anoplocephala %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(anoplocephala))

#parascaris_equorum
FN_TP_Remainder_PE  <- FN_parascaris_equorum %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(parascaris_equorum))

#strongyloides_westeri
FN_TP_Remainder_SW  <- FN_strongyloides_westeri %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyloides_westeri))

dta <- qpcR:::cbind.na(FN_TP_Remainder_O, FN_TP_Remainder_S, FN_TP_Remainder_A, FN_TP_Remainder_PE, FN_TP_Remainder_SW )
write.csv(dta, file = "FN TP Remainder per parasite.csv")









############### TP#######

A_C = read.csv("Auto and Manual combined minus dumps -HSI.csv", head=TRUE, sep=",")

TP_oxyuris_equi <- subset(A_C, subset = c(A_C$oxyuris_equi == A_C$oxyuris_equi_m & A_C$oxyuris_equi>=1)) 
TP_strongyle <- subset(A_C, subset = c(A_C$strongyle == A_C$strongyle_m & A_C$strongyle>=1)) 
TP_anoplocephala <- subset(A_C, subset = c(A_C$anoplocephala == A_C$anoplocephala_m & A_C$anoplocephala>=1)) 
TP_parascaris_equorum <- subset(A_C, subset = c(A_C$parascaris_equorum  == A_C$parascaris_equorum_m & A_C$parascaris_equorum>=1)) 
TP_strongyloides_westeri <- subset(A_C, subset = c(A_C$strongyloides_westeri  == A_C$strongyloides_westeri_m & A_C$strongyloides_westeri>1)) 

#Pulling all FP cases
FP_all <- subset(A_C, subset = c(A_C$oxyuris_equi == A_C$oxyuris_equi_m & A_C$oxyuris_equi>=1 | 
                                   A_C$strongyle == A_C$strongyle_m & A_C$strongyle>=1 | 
                                    A_C$anoplocephala == A_C$anoplocephala_m & A_C$anoplocephala>=1 | 
                                   A_C$parascaris_equorum  == A_C$parascaris_equorum_m & A_C$parascaris_equorum>=1 |
                                    A_C$strongyloides_westeri  == A_C$strongyloides_westeri_m & A_C$strongyloides_westeri>=1 ))
write.csv(FP_all, file = "True positive all.csv")


### Sums of eggs per scan_image group by scan_id

#oxyuris_equi
TP_sum_OE <- TP_oxyuris_equi %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(oxyuris_equi))

# Stronglye
TP_sum_S <- TP_strongyle %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyle))

#anoplocephala
TP_sum_A <- TP_anoplocephala %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(anoplocephala))

#parascaris_equorum
TP_sum_PE <- TP_parascaris_equorum %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(parascaris_equorum))

#strongyloides_westeri
TP_sum_SW <- TP_strongyloides_westeri %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyloides_westeri))


dta <- qpcR:::cbind.na(TP_sum_OE, TP_sum_S, TP_sum_A, TP_sum_PE, TP_sum_SW )
write.csv(dta, file = "TP per parasite.csv")




###All species egg sums
#Optional

#Individual_pieces <- TP_all %>% 
#  group_by(scan_id) %>% 
#  summarise(Frequency = sum(oxyuris_equi + strongyle + anoplocephala + parascaris_equorum + strongyloides_westeri))

#write.csv(Individual_pieces, file = "TP Individual_pieces_sum - All.csv")



### Count rows per scan_id that post dump
A_Css = data.frame(A_C)

A_Cssagr = aggregate(cbind(count = scanimage_id) ~ scan_id, 
                     data = A_Css, 
                     FUN = function(x){NROW(x)})

write.csv(A_Cssagr, file = "Rows per scan_id original files combined all col.csv")

