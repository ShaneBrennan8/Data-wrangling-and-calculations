install.packages("tidyr")
library(tidyr)

install.packages("plyr")
library(plyr)

install.packages("dplyr")
library(dplyr)

############ Manual only count #############

A_C2 = read.csv("manual_data.csv", head=TRUE, sep=",")

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

######## Auto count only ##########

A_C2 = read.csv("auto_data.csv", head=TRUE, sep=",")

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


####################FP SUM ###############

A_C = read.csv("auto_&_manual_combined_minus_dumps.csv", head=TRUE, sep=",")


## subset scan_images where there is a difference(+ or -) between respective columns and writing them to a file. 
SS2 <- subset(A_C, subset = c(A_C$oxyuris_equi != A_C$oxyuris_equi_m | A_C$strongyle != A_C$strongyle_m | 
                                A_C$anoplocephala != A_C$anoplocephala_m | A_C$parascaris_equorum != A_C$parascaris_equorum_m |
                                A_C$strongyloides_westeri != strongyloides_westeri_m))

### False positive case extraction
## working on the all differences file, we want to extract where a specific parasite's 
## Auto columns have values that are greater than the manual
## columns (false positive) they are subsetted.

### SS2 is reassigned to A_C1. Remember that this is just a subset where there is a difference
A_C1 = SS2

#All FP cases
FP_all <- subset(A_C1, subset = c(A_C1$oxyuris_equi > A_C1$oxyuris_equi_m | A_C1$strongyle > A_C1$strongyle_m | 
                                    A_C1$anoplocephala > A_C1$anoplocephala_m | A_C1$parascaris_equorum > A_C1$parascaris_equorum_m |
                                    A_C1$strongyloides_westeri > A_C1$strongyloides_westeri_m ))
#write.csv(FP_all, file = "false positive all.csv")

# extraction of cases false positive cases for each paracite and saving to individual variables
FP_oxyuris_equi <- subset(A_C1, subset = c(A_C1$oxyuris_equi > A_C1$oxyuris_equi_m)) 
FP_strongyle <- subset(A_C1, subset = c(A_C1$strongyle > A_C1$strongyle_m)) 
FP_anoplocephala <- subset(A_C1, subset = c(A_C1$anoplocephala > A_C1$anoplocephala_m)) 
FP_parascaris_equorum <- subset(A_C1, subset = c(A_C1$parascaris_equorum  > A_C1$parascaris_equorum_m)) 
FP_strongyloides_westeri <- subset(A_C1, subset = c(A_C1$strongyloides_westeri  > A_C1$strongyloides_westeri_m)) 



### Sums
#oxyuris_equi
Individual_pieces <- FP_oxyuris_equi %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(oxyuris_equi - oxyuris_equi_m))
write.csv(Individual_pieces, file = "FP Individual_pieces_sum - Oxyuris_equi.csv")

# Stronglye
Individual_pieces <- FP_strongyle %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyle - strongyle_m))
write.csv(Individual_pieces, file = "FP Individual_pieces_sum - Stronglye.csv")

#anoplocephala
Individual_pieces <- FP_anoplocephala %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(anoplocephala - anoplocephala_m))
write.csv(Individual_pieces, file = "FP Individual_pieces_sum - Anoplocephala.csv")

#parascaris_equorum
Individual_pieces <- FP_parascaris_equorum %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(parascaris_equorum - parascaris_equorum_m))
write.csv(Individual_pieces, file = "FP Individual_pieces_sum - Parascaris_equorum.csv")

#strongyloides_westeri
Individual_pieces <- FP_strongyloides_westeri %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyloides_westeri - strongyloides_westeri_m))
write.csv(Individual_pieces, file = "FP Individual_pieces_sum - Strongyloides_westeri.csv")

###No we get the TP remainder from the FP calculation
### Sums
#oxyuris_equi
Individual_pieces <- FP_oxyuris_equi %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(oxyuris_equi_m))
write.csv(Individual_pieces, file = "FP leftover(TP) Oxyuris_equi.csv")

# Stronglye
Individual_pieces <- FP_strongyle %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyle_m))
write.csv(Individual_pieces, file = "FP leftover(TP) Stronglye.csv")

#anoplocephala
Individual_pieces <- FP_anoplocephala %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(anoplocephala_m))
write.csv(Individual_pieces, file = "FP leftover(TP) Anoplocephala.csv")

#parascaris_equorum
Individual_pieces <- FP_parascaris_equorum %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(parascaris_equorum_m))
write.csv(Individual_pieces, file = "FP leftover(TP) Parascaris_equorum.csv")

#strongyloides_westeri
Individual_pieces <- FP_strongyloides_westeri %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyloides_westeri_m))
write.csv(Individual_pieces, file = "FP leftover(TP) Strongyloides_westeri.csv")


############### FN ##############
A_C = read.csv("auto_&_manual_combined_minus_dumps.csv", head=TRUE, sep=",")

#All FN cases
FN_all <- subset(A_C1, subset = c(A_C1$oxyuris_equi < A_C1$oxyuris_equi_m | A_C1$strongyle < A_C1$strongyle_m | 
                                    A_C1$anoplocephala < A_C1$anoplocephala_m | A_C1$parascaris_equorum < A_C1$parascaris_equorum_m |
                                    A_C1$strongyloides_westeri < A_C1$strongyloides_westeri_m))
write.csv(FN_all, file = "false negative all.csv")


## subset scan_images where there is a difference(+ or -) between respective columns and writing them to a file. 
SS2 <- subset(A_C, subset = c(A_C$oxyuris_equi != A_C$oxyuris_equi_m | A_C$strongyle != A_C$strongyle_m | 
                                A_C$anoplocephala != A_C$anoplocephala_m | A_C$parascaris_equorum != A_C$parascaris_equorum_m |
                                A_C$strongyloides_westeri != strongyloides_westeri_m))
A_C1= SS2
write.csv(A_C1, file = "A_C1.csv")
#Extraction of False Negative files and saving to individual variables

FN_oxyuris_equi <- subset(A_C1, subset = c(A_C1$oxyuris_equi < A_C1$oxyuris_equi_m)) 
FN_strongyle <- subset(A_C1, subset = c(A_C1$strongyle < A_C1$strongyle_m)) 
FN_anoplocephala <- subset(A_C1, subset = c(A_C1$anoplocephala < A_C1$anoplocephala_m)) 
FN_parascaris_equorum <- subset(A_C1, subset = c(A_C1$parascaris_equorum  < A_C1$parascaris_equorum_m)) 
FN_strongyloides_westeri <- subset(A_C1, subset = c(A_C1$strongyloides_westeri  < A_C1$strongyloides_westeri_m)) 



### Sums
#oxyuris_equi
Individual_pieces <- FN_oxyuris_equi %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(oxyuris_equi_m - oxyuris_equi))
write.csv(Individual_pieces, file = "FN Individual_pieces_sum - Oxyuris_equi.csv")

# Stronglye
Individual_pieces <- FN_strongyle %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyle_m - strongyle))
write.csv(Individual_pieces, file = "FN Individual_pieces_sum - Stronglye.csv")

#anoplocephala
Individual_pieces <- FN_anoplocephala %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(anoplocephala_m - anoplocephala))
write.csv(Individual_pieces, file = "FN Individual_pieces_sum - Anoplocephala.csv")

#parascaris_equorum
Individual_pieces <- FN_parascaris_equorum %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(parascaris_equorum_m - parascaris_equorum))
write.csv(Individual_pieces, file = "FN Individual_pieces_sum - Parascaris_equorum.csv")

#strongyloides_westeri
Individual_pieces <- FN_strongyloides_westeri %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyloides_westeri_m - strongyloides_westeri))
write.csv(Individual_pieces, file = "FN Individual_pieces_sum - Strongyloides_westeri.csv")

#### Picking up the TP that are left behind from the FN calculation
### Basically the lower value is what is the shared TP. 
#oxyuris_equi
Individual_pieces <- FN_oxyuris_equi %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(oxyuris_equi))
write.csv(Individual_pieces, file = "FN leftover(TP) Oxyuris_equi.csv")

# Stronglye
Individual_pieces <- FN_strongyle %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyle))
write.csv(Individual_pieces, file = "FN leftover(TP) Stronglye.csv")

#anoplocephala
Individual_pieces <- FN_anoplocephala %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(anoplocephala))
write.csv(Individual_pieces, file = "FN leftover(TP) Anoplocephala.csv")

#parascaris_equorum
Individual_pieces <- FN_parascaris_equorum %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(parascaris_equorum))
write.csv(Individual_pieces, file = "FN leftover(TP)Parascaris_equorum.csv")

#strongyloides_westeri
Individual_pieces <- FN_strongyloides_westeri %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyloides_westeri))
write.csv(Individual_pieces, file = "FN leftover(TP) Strongyloides_westeri.csv")

############TP#######

A_C = read.csv("auto_&_manual_combined_minus_dumps.csv", head=TRUE, sep=",")


## subset scan_images where both columns have the same value writing them to a file. 
#SS2 <- subset(A_C, subset = c(A_C$oxyuris_equi == A_C$oxyuris_equi_m | A_C$strongyle == A_C$strongyle_m | 
                                #A_C$anoplocephala == A_C$anoplocephala_m | A_C$parascaris_equorum == A_C$parascaris_equorum_ |
                                #A_C$strongyloides_westeri == strongyloides_westeri_m))


#write.csv(SS2, file = "True Positives.csv")

TP_oxyuris_equi <- subset(A_C, subset = c(A_C$oxyuris_equi == A_C$oxyuris_equi_m)) 
TP_strongyle <- subset(A_C, subset = c(A_C$strongyle == A_C$strongyle_m)) 
TP_anoplocephala <- subset(A_C, subset = c(A_C$anoplocephala == A_C$anoplocephala_m)) 
TP_parascaris_equorum <- subset(A_C, subset = c(A_C$parascaris_equorum  == A_C$parascaris_equorum_m)) 
TP_strongyloides_westeri <- subset(A_C, subset = c(A_C$strongyloides_westeri  == A_C$strongyloides_westeri_m)) 

write.csv(TP_oxyuris_equi, file = "TP_oxyuris_equi.csv")
write.csv(TP_strongyle, file = "TP_strongyle.csv")
write.csv(TP_anoplocephala, file = "TP_anoplocephala.csv")
write.csv(TP_parascaris_equorum, file = "TP_parascaris_equorum.csv")
write.csv(TP_strongyloides_westeri, file = "TP_strongyloides_westeri.csv")


# Here we need to filter the 0==0 values (true negative) to get the true positives. All the above will now be saved
# saved with a 1 behind their names
#################
TP_oxyuris_equi1 = read.csv("TP_oxyuris_equi1.csv", head=TRUE, sep=",")
TP_strongyle1 = read.csv("TP_strongyle1.csv", head=TRUE, sep=",")
TP_anoplocephala1 = read.csv("TP_anoplocephala1.csv", head=TRUE, sep=",")
TP_parascaris_equorum1 = read.csv("TP_parascaris_equorum1.csv", head=TRUE, sep=",")
TP_strongyloides_westeri1= read.csv("TP_strongyloides_westeri1.csv", head=TRUE, sep=",")

### Sums of eggs per scan_image group by scan_id

#oxyuris_equi
Individual_pieces <- TP_oxyuris_equi1 %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(oxyuris_equi))
write.csv(Individual_pieces, file = "TP Individual_pieces_sum - Oxyuris_equi.csv")

# Stronglye
Individual_pieces <- TP_strongyle1 %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyle))
write.csv(Individual_pieces, file = "TP Individual_pieces_sum - Stronglye.csv")

#anoplocephala
Individual_pieces <- TP_anoplocephala1 %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(anoplocephala))
write.csv(Individual_pieces, file = "TP Individual_pieces_sum - Anoplocephala.csv")

#parascaris_equorum
Individual_pieces <- TP_parascaris_equorum1 %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(parascaris_equorum))
write.csv(Individual_pieces, file = "TP Individual_pieces_sum - Parascaris_equorum.csv")

#strongyloides_westeri
Individual_pieces <- TP_strongyloides_westeri1 %>% 
  group_by(scan_id) %>% 
  summarise(Frequency = sum(strongyloides_westeri))
write.csv(Individual_pieces, file = "TP Individual_pieces_sum - Strongyloides_westeri.csv")

###All species egg sums
#Optional

#Individual_pieces <- TP_all %>% 
#  group_by(scan_id) %>% 
#  summarise(Frequency = sum(oxyuris_equi + strongyle + anoplocephala + parascaris_equorum + strongyloides_westeri))

#write.csv(Individual_pieces, file = "TP Individual_pieces_sum - All.csv")







