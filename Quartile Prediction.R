install.packages("qpcR")
library(qpcR)

install.packages("tidyr")
library(tidyr)

install.packages("plyr")
library(plyr)

install.packages("dplyr")
library(dplyr)

install.packages("RcppRoll")
library(RcppRoll)

install.packages("zoo")
library(zoo)

install.packages("microbenchmark")
library(microbenchmark)

### The objective here is to do the cumulative sum data of each parasite 
### then the quartiles are calculated so we have data which we will use to make future predictions on egg counts
### Hopefully github is updated with this

# First read in the data set and change it to a data frame
A_C2 = read.csv("manual_data_minus_dump_quartile_work_14122021.csv", head=TRUE, sep=",")
A_C2 = as.data.frame(A_C2)

# Use of the mutate function in dplyr to add the cumulative sums as columns to our database
# it is then written to rolled.csv for inspection 
pdf = A_C2 %>% group_by(scan_id)  %>% mutate(cumsum_oxyuris_equi = cumsum(oxyuris_equi), cumsum_strongyle = cumsum(strongyle), 
                                             cumsum_anoplocephala = cumsum(anoplocephala), cumsum_parascaris_equorum = cumsum(parascaris_equorum),
                                             cumsum_strongyloides_westeri = cumsum(strongyloides_westeri))
write.csv(pdf, file = "rolled.csv") 

# use of the quantile function to get the quartile results which are written to csvs for inspection
oxyuris_equi_quartiles <- ddply(pdf, .(scan_id), function(pdf) quantile(pdf$cumsum_oxyuris_equi))
write.csv(oxyuris_equi_quartiles, file = "oxyuris_equi_quartiles.csv")

strongly_quartiles <- ddply(pdf, .(scan_id), function(pdf) quantile(pdf$cumsum_strongyle))
write.csv(strongly_quartiles, file = "strongly_quartiles.csv")

anoplocephala_quartiles <- ddply(pdf, .(scan_id), function(pdf) quantile(pdf$cumsum_anoplocephala))
write.csv(anoplocephala_quartiles, file = "anoplocephala_quartiles.csv")

parascaris_equorum_quartiles <- ddply(pdf, .(scan_id), function(pdf) quantile(pdf$cumsum_parascaris_equorum))
write.csv(parascaris_equorum_quartiles, file = "parascaris_equorum_quartiles.csv")

strongyloides_westeri_quartiles <- ddply(pdf, .(scan_id), function(pdf) quantile(pdf$cumsum_strongyloides_westeri))
write.csv(strongyloides_westeri_quartiles, file = "strongyloides_westeri_quartiles.csv")


### Count rows per scan_id to give us an idea of how many post dump scan_images there are per scan_id
# first up we calculate the post dump removal number of images per scan_id

A_Css = data.frame(A_C2)

A_Cssagr = aggregate(cbind(count = scanimage_id) ~ scan_id, 
                     data = A_Css, 
                     FUN = function(x){NROW(x)})

write.csv(A_Cssagr, file = "Rows per scan_id post dump combined all col.csv")

# secondly we calculate the number of images pre dump
A_C2 = read.csv("manual_data_with_dump_quartile_work_14122021.csv", head=TRUE, sep=",")

A_Css = data.frame(A_C2)

A_Cssagr = aggregate(cbind(count = scanimage_id) ~ scan_id, 
                     data = A_Css, 
                     FUN = function(x){NROW(x)})

write.csv(A_Cssagr, file = "Rows per scan_id pre dump combined all col.csv")

