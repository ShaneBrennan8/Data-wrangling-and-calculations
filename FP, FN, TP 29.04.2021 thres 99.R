install.packages("qpcR")
library(qpcR)

install.packages("tidyr")
library(tidyr)

install.packages("plyr")
library(plyr)

install.packages("dplyr")
library(dplyr)

############### Data prep #########
# Ensure the scan_ids are sorted lowest to highest
# Remove dumps from manual and Auto. Ensure both spreadsheets are the same length(Some scanIDs might appear in one and not the other) (this may have to be done 
# after the manual and dump spreadsheets are processed)
# Combine the auto and manual files on the scan_image column. Make sure not to mess this up by using the scan_id column erroneously
# Occasionally the manual's data type may have to be changed to numeric so that that a vlookup can be performed on it
# The truth (manual) is always on the right (think 'the truth is never wrong')
# Ensure all spreadsheets are saved in .CSV format
# Add "-m" to the end of each of the parasite names in the manual doc AFTER merging
# Set WD



############### Manual only count #############
# Remember to remove dumps 
A_C2 = read.csv("manual_data_minus_dump_202104261245.csv", head=TRUE, sep=",")

### Sums
#oxyuris_equi
OE_ManualCount <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(oxyuris_equi = sum(oxyuris_equi))

# Stronglye
S_ManualCount <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(strongyle = sum(strongyle))

#anoplocephala
A_ManualCount <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(anoplocephala = sum(anoplocephala))

#parascaris_equorum
PE_ManualCount <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(parascaris_equorum = sum(parascaris_equorum))

#strongyloides_westeri
SW_ManualCount <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(strongyloides_westeri = sum(strongyloides_westeri))

# Write all individual manual counts to one spreadsheet
dta1 <- qpcR:::cbind.na(OE_ManualCount, S_ManualCount, A_ManualCount, PE_ManualCount, SW_ManualCount)
write.csv(dta1, file = "1.Manual Count All.csv")



############### Auto count only ##########
# Remember to remove dumps
A_C2 = read.csv("Apr2021_22_2_Thres99_minus_dumps.csv", head=TRUE, sep=",")

### Sums
#oxyuris_equi
OE_AutoCount <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(oxyuris_equi = sum(oxyuris_equi))

# Stronglye
S_AutoCount <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(strongyle = sum(strongyle))

#anoplocephala
A_AutoCount <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(anoplocephala = sum(anoplocephala))

#parascaris_equorum
PE_AutoCount <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(parascaris_equorum = sum(parascaris_equorum))

#strongyloides_westeri
SW_AutoCount <- A_C2 %>% 
  group_by(scan_id) %>% 
  summarise(strongyloides_westeri = sum(strongyloides_westeri))

# Write all individual manual counts to one spreadsheet
dta2 <- qpcR:::cbind.na(OE_AutoCount, S_AutoCount, A_AutoCount, PE_AutoCount, SW_AutoCount)
write.csv(dta2, file = "2.Auto Count All.csv")
a


#Individual_pieces <- TP_all %>% 
#  group_by(scan_id) %>% 
#  summarise(Frequency = sum(oxyuris_equi + strongyle + anoplocephala + parascaris_equorum + strongyloides_westeri))

#write.csv(Individual_pieces, file = "TP Individual_pieces_sum - All.csv")


### Count rows per scan_id that post dump
#A_Css = data.frame(A_C)

#A_Cssagr = aggregate(cbind(count = scanimage_id) ~ scan_id, 
                     #data = A_Css, 
                     #FUN = function(x){NROW(x)})

#write.csv(A_Cssagr, file = "Rows per scan_id original files combined all col.csv")


