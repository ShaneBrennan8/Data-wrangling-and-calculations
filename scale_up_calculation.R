
## for testing purposes
### Required packages
install.packages("tidyr")
library(tidyr)

install.packages("plyr")
library(plyr)

install.packages("dplyr")
library(dplyr)


### Purpose: Compare the total number of strongyle eggs found between Auto and McMaster, FlowTACT. 
### To do this here we will get the strongyle eggs per scan_id will have to be scaled up to EPG.
### A graph and statistical analysis can then be performed on Excel

### The example we are using here is stronglye but it can be performed on any paracite in either the auto
### or manual

### Call in file and prepare it for iteration
A_C = read.csv("manual_data_minus_dump.csv", head=TRUE, sep=",")


### Count it to get numberOfImages per scan_id
A_Css = data.frame(A_C)
A_Cssagr = aggregate(cbind(count = scanimage_id) ~ scan_id, 
                     data = A_Css, 
                     FUN = function(x){NROW(x)})
write.csv(A_Cssagr, file = "Rows per scan_id original files combined all col.csv")


### Subset out scan_image column as its not needed
A_C <- data.frame(A_C)
A_C1 <- subset(A_C, select = - scanimage_id)

### Run a sum and group by scan_id an assign it to a variable "by_scan_id"
by_scan_id <- A_C1 %>% group_by(scan_id) %>% summarise_all(sum)


### Now we count scan_images per scan_Id.  
### (Dumps removed in original CSV file). 
A_Css = data.frame(A_C)
A_Cssagr = aggregate(cbind(count = scanimage_id) ~ scan_id, 
                     data = A_Css, 
                     FUN = function(x){NROW(x)})


### We bind number of eggs variable column to our no.of images
by_scan_id = data.frame(by_scan_id)
A_Cssagr = data.frame(A_Cssagr)
E_I <- cbind(by_scan_id, "numberOfImages" = A_Cssagr$count)


### Set scale up function for
concentration_factor = 0.066667
volume_per_image =     0.008317
calculation_factor <- function(number_of_eggs, number_of_images)
{number_of_eggs * (1/(volume_per_image*number_of_images*concentration_factor))}


### Now with our eggs per scan_id and image totals per scan_id we have the required input to scale up. 
### Pass both columns into "calculation function"
scale_up <- calculation_factor(E_I$anoplocephala, E_I$numberOfImages) 


### Merge all the information we have obtained above into one file and save it in a CSV format.
EPG_per_image_a <- cbind("anoplocephala" = E_I$scan_id, "Eggs_Per_Scan_ID" =E_I$anoplocephala, scale_up,"Images_Per_Scan_id" =E_I$numberOfImages)

# Stronglye
scale_up <- calculation_factor(E_I$strongyle, E_I$numberOfImages) 
EPG_per_image_s <- cbind("strongyle" = E_I$scan_id, "Eggs_Per_Scan_ID" =E_I$strongyle, scale_up,"Images_Per_Scan_id" =E_I$numberOfImages)

# parascaris_equorum
scale_up <- calculation_factor(E_I$parascaris_equorum, E_I$numberOfImages) 
EPG_per_image_pe <- cbind("parascaris_equorum" = E_I$scan_id, "Eggs_Per_Scan_ID" =E_I$parascaris_equorum, scale_up,"Images_Per_Scan_id" =E_I$numberOfImages)

# strongyloides_westeri
scale_up <- calculation_factor(E_I$strongyloides_westeri, E_I$numberOfImages) 
EPG_per_image_sw <- cbind("strongyloides_westeri" = E_I$scan_id, "Eggs_Per_Scan_ID" =E_I$strongyloides_westeri, scale_up,"Images_Per_Scan_id" =E_I$numberOfImages)



# Write all individual manual counts to one spreadsheet
dta <- qpcR:::cbind.na(EPG_per_image_a, EPG_per_image_s, EPG_per_image_pe, EPG_per_image_sw)
write.csv(dta, file = "EPG combined.csv")

### Extra coloumns can be removed in Excel. 
### You can now use vlookup in excel to bring across McMaster and MiniFLOTACT
### before comparing them both



