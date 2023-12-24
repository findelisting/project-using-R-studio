
####################################
'libraries '
#####################################

library(readxl)
library (tidyr)
library (dplyr)
library(graphics)
library (ggplot2)
library (tibble)
library(stringr)
library (openxlsx)
library(dplyr)
library(stringr)
library(tidyverse)


################################################################################
'data loading, preparation and data cleaning'
###############################################################################

my_data <- read.csv("data_SalimS2.csv")

unique(my_data$user_login)
unique(my_data$case_name)

View (my_data)


###############################################################
'Filter for rows that start with "details: '
###############################################################
df <- my_data %>%
  filter(str_starts(data, "details: "))



################################################################
' CREATING ROWS FOR THE CARD_ID, Mandatory_Results (MR) AND 
Adequately_Justified (AJ)'
###############################################################


df1 <- df %>%
  mutate(card_id = str_extract(data, "(?<=card_id:)[0-9]+"),
         mandatory_result = str_extract(data, "(?<=mandatory_result:)[^\\s]+"),
         adequately_justified = str_extract(data, "(?<=adequately_justified:)[^\\s]+"))

View (df1)
#################################################################
'first instace of each unique card_id specific to case name
and User_logins'
#################################################################

df_unique_id <- df1 %>%
  group_by(case_name, card_id, user_login) %>%
  arrange(user_login, case_name, card_id, timestamp) %>%
  mutate(instance_number = row_number()) %>%
  ungroup() %>%
  mutate(unique_id = paste(user_login, case_name, card_id, sep = ""))
'The unique_id creates a special id for every user_login, 
per a card_id and case_name'

# Printing the resulting dataset with unique IDs
View(df_unique_id)

'filtering for only the unique_ids that had an error 
based on they having either MR & AJ as false'
df_filtered <- df_unique_id %>%
  group_by(unique_id) %>%
  filter(!(instance_number == 1 & mandatory_result 
           == "true" & adequately_justified == "true")) %>%
  ungroup()

# Printing the resulting filtered dataset
View(df_filtered)


###############################################################
' Due to differences in bef size for the various case_name
we had to filter for specific case study and extract bef/diags
separately'
###############################################################

################
'START Freundorf'
###############
FR <-  filter(df_filtered, case_name == "Freundorf")


#extracting the befs values into columns


FR <- FR %>%
  mutate(bef1 = coalesce(str_extract(data, "(?<=bef1_)[0-9]+"), "0"),
         bef2 = coalesce(str_extract(data, "(?<=bef2_)[0-9]+"), "0"),
         bef3 = coalesce(str_extract(data, "(?<=bef3_)[0-9]+"), "0"),
         bef4 = coalesce(str_extract(data, "(?<=bef4_)[0-9]+"), "0"),
         bef5 = coalesce(str_extract(data, "(?<=bef5_)[0-9]+"), "0"),
         bef6 = coalesce(str_extract(data, "(?<=bef6_)[0-9]+"), "0"),
         bef7 = coalesce(str_extract(data, "(?<=bef7_)[0-9]+"), "0"),
         bef8 = coalesce(str_extract(data, "(?<=bef8_)[0-9]+"), "0"),
         bef9 = coalesce(str_extract(data, "(?<=bef9_)[0-9]+"), "0"),
         bef10 = coalesce(str_extract(data, "(?<=bef10_)[0-9]+"), "0"),
         bef11 = coalesce(str_extract(data, "(?<=bef11_)[0-9]+"), "0"),
         bef12 = coalesce(str_extract(data, "(?<=bef12_)[0-9]+"), "0"),
         bef13 = coalesce(str_extract(data, "(?<=bef13_)[0-9]+"), "0"))

#extracting the diag values into columns
FR <- FR %>%
  mutate(fdiag1 = coalesce(str_extract(data, "(?<=fdiag1-)[diag0-9]+"), "0"),
         fdiag2 = coalesce(str_extract(data, "(?<=fdiag2-)[diag0-9]+"), "0"),
         fdiag3 = coalesce(str_extract(data, "(?<=fdiag3-)[diag0-9]+"), "0"),
         fdiag4 = coalesce(str_extract(data, "(?<=fdiag4-)[diag0-9]+"), "0"),
         fdiag5 = coalesce(str_extract(data, "(?<=fdiag5-)[diag0-9]+"), "0"),
         fdiag6 = coalesce(str_extract(data, "(?<=fdiag6-)[diag0-9]+"), "0"),
         fdiag7 = coalesce(str_extract(data, "(?<=fdiag7-)[diag0-9]+"), "0"),
         fdiag8 = coalesce(str_extract(data, "(?<=fdiag8-)[diag0-9]+"), "0"),
         fdiag9 = coalesce(str_extract(data, "(?<=fdiag9-)[diag0-9]+"), "0"),
         fdiag10 = coalesce(str_extract(data, "(?<=fdiag10-)[diag0-9]+"), "0"))


#viewing the dataset
View (FR)

###############
'END Freundorf'
##############

#################
'START Goettlich'
################
GH <-  filter(df_filtered, case_name == "Goettlich")


#extracting the befs values into columns


GH <- GH %>%
  mutate(bef1 = coalesce(str_extract(data, "(?<=bef1_)[0-9]+"), "0"),
         bef2 = coalesce(str_extract(data, "(?<=bef2_)[0-9]+"), "0"),
         bef3 = coalesce(str_extract(data, "(?<=bef3_)[0-9]+"), "0"),
         bef4 = coalesce(str_extract(data, "(?<=bef4_)[0-9]+"), "0"),
         bef5 = coalesce(str_extract(data, "(?<=bef5_)[0-9]+"), "0"),
         bef6 = coalesce(str_extract(data, "(?<=bef6_)[0-9]+"), "0"),
         bef7 = coalesce(str_extract(data, "(?<=bef7_)[0-9]+"), "0"),
         bef8 = coalesce(str_extract(data, "(?<=bef8_)[0-9]+"), "0"),
         bef9 = coalesce(str_extract(data, "(?<=bef9_)[0-9]+"), "0"),
         bef10 = coalesce(str_extract(data, "(?<=bef10_)[0-9]+"), "0"),
         bef11 = coalesce(str_extract(data, "(?<=bef11_)[0-9]+"), "0"),
         bef12 = coalesce(str_extract(data, "(?<=bef12_)[0-9]+"), "0"),
         bef13 = coalesce(str_extract(data, "(?<=bef13_)[0-9]+"), "0"))

#extracting the diag values into columns
GH <- GH %>%
  mutate(fdiag1 = coalesce(str_extract(data, "(?<=fdiag1-)[diag0-9]+"), "0"),
         fdiag2 = coalesce(str_extract(data, "(?<=fdiag2-)[diag0-9]+"), "0"),
         fdiag3 = coalesce(str_extract(data, "(?<=fdiag3-)[diag0-9]+"), "0"),
         fdiag4 = coalesce(str_extract(data, "(?<=fdiag4-)[diag0-9]+"), "0"),
         fdiag5 = coalesce(str_extract(data, "(?<=fdiag5-)[diag0-9]+"), "0"),
         fdiag6 = coalesce(str_extract(data, "(?<=fdiag6-)[diag0-9]+"), "0"),
         fdiag7 = coalesce(str_extract(data, "(?<=fdiag7-)[diag0-9]+"), "0"),
         fdiag8 = coalesce(str_extract(data, "(?<=fdiag8-)[diag0-9]+"), "0"),
         fdiag9 = coalesce(str_extract(data, "(?<=fdiag9-)[diag0-9]+"), "0"),
         fdiag10 = coalesce(str_extract(data, "(?<=fdiag10-)[diag0-9]+"), "0"))


#viewing the dataset
View (GH)



###############
'END Goettlich'
##############

#################
'START Binder'
################
BR <-  filter(df_filtered, case_name == "Binder")


#extracting the befs values into columns


BR <- BR %>%
  mutate(bef1 = coalesce(str_extract(data, "(?<=bef1_)[0-9]+"), "0"),
         bef2 = coalesce(str_extract(data, "(?<=bef2_)[0-9]+"), "0"),
         bef3 = coalesce(str_extract(data, "(?<=bef3_)[0-9]+"), "0"),
         bef4 = coalesce(str_extract(data, "(?<=bef4_)[0-9]+"), "0"),
         bef5 = coalesce(str_extract(data, "(?<=bef5_)[0-9]+"), "0"),
         bef6 = coalesce(str_extract(data, "(?<=bef6_)[0-9]+"), "0"),
         bef7 = coalesce(str_extract(data, "(?<=bef7_)[0-9]+"), "0"),
         bef8 = coalesce(str_extract(data, "(?<=bef8_)[0-9]+"), "0"),
         bef9 = coalesce(str_extract(data, "(?<=bef9_)[0-9]+"), "0"),
         bef10 = coalesce(str_extract(data, "(?<=bef10_)[0-9]+"), "0"),
         bef11 = coalesce(str_extract(data, "(?<=bef11_)[0-9]+"), "0"),
         bef12 = coalesce(str_extract(data, "(?<=bef12_)[0-9]+"), "0"),
         bef13 = coalesce(str_extract(data, "(?<=bef13_)[0-9]+"), "0"))

#extracting the diag values into columns
BR <- BR %>%
  mutate(fdiag1 = coalesce(str_extract(data, "(?<=fdiag1-)[diag0-9]+"), "0"),
         fdiag2 = coalesce(str_extract(data, "(?<=fdiag2-)[diag0-9]+"), "0"),
         fdiag3 = coalesce(str_extract(data, "(?<=fdiag3-)[diag0-9]+"), "0"),
         fdiag4 = coalesce(str_extract(data, "(?<=fdiag4-)[diag0-9]+"), "0"),
         fdiag5 = coalesce(str_extract(data, "(?<=fdiag5-)[diag0-9]+"), "0"),
         fdiag6 = coalesce(str_extract(data, "(?<=fdiag6-)[diag0-9]+"), "0"),
         fdiag7 = coalesce(str_extract(data, "(?<=fdiag7-)[diag0-9]+"), "0"),
         fdiag8 = coalesce(str_extract(data, "(?<=fdiag8-)[diag0-9]+"), "0"),
         fdiag9 = coalesce(str_extract(data, "(?<=fdiag9-)[diag0-9]+"), "0"),
         fdiag10 = coalesce(str_extract(data, "(?<=fdiag10-)[diag0-9]+"), "0"))


#viewing the dataset
View (BR)
#str (BR$mandatory_result)


###############
'END BINDER'
##############


#################
'START Schenker'
################
SR <-  filter(df_filtered, case_name == "Schenker")


#extracting the befs values into columns


SR <- SR %>%
  mutate(bef1 = coalesce(str_extract(data, "(?<=bef1_)[0-9]+"), "0"),
         bef2 = coalesce(str_extract(data, "(?<=bef2_)[0-9]+"), "0"),
         bef3 = coalesce(str_extract(data, "(?<=bef3_)[0-9]+"), "0"),
         bef4 = coalesce(str_extract(data, "(?<=bef4_)[0-9]+"), "0"),
         bef5 = coalesce(str_extract(data, "(?<=bef5_)[0-9]+"), "0"),
         bef6 = coalesce(str_extract(data, "(?<=bef6_)[0-9]+"), "0"),
         bef7 = coalesce(str_extract(data, "(?<=bef7_)[0-9]+"), "0"),
         bef8 = coalesce(str_extract(data, "(?<=bef8_)[0-9]+"), "0"),
         bef9 = coalesce(str_extract(data, "(?<=bef9_)[0-9]+"), "0"),
         bef10 = coalesce(str_extract(data, "(?<=bef10_)[0-9]+"), "0"),
         bef11 = coalesce(str_extract(data, "(?<=bef11_)[0-9]+"), "0"),
         bef12 = coalesce(str_extract(data, "(?<=bef12_)[0-9]+"), "0"),
         bef13 = coalesce(str_extract(data, "(?<=bef13_)[0-9]+"), "0"))

#extracting the diag values into columns
SR <- SR %>%
  mutate(fdiag1 = coalesce(str_extract(data, "(?<=fdiag1-)[diag0-9]+"), "0"),
         fdiag2 = coalesce(str_extract(data, "(?<=fdiag2-)[diag0-9]+"), "0"),
         fdiag3 = coalesce(str_extract(data, "(?<=fdiag3-)[diag0-9]+"), "0"),
         fdiag4 = coalesce(str_extract(data, "(?<=fdiag4-)[diag0-9]+"), "0"),
         fdiag5 = coalesce(str_extract(data, "(?<=fdiag5-)[diag0-9]+"), "0"),
         fdiag6 = coalesce(str_extract(data, "(?<=fdiag6-)[diag0-9]+"), "0"),
         fdiag7 = coalesce(str_extract(data, "(?<=fdiag7-)[diag0-9]+"), "0"),
         fdiag8 = coalesce(str_extract(data, "(?<=fdiag8-)[diag0-9]+"), "0"),
         fdiag9 = coalesce(str_extract(data, "(?<=fdiag9-)[diag0-9]+"), "0"),
         fdiag10 = coalesce(str_extract(data, "(?<=fdiag10-)[diag0-9]+"), "0"))


#viewing the dataset
View (SR)



###############
'END Schenker'
##############

#################
'START Winkler'
################
WR <-  filter(df_filtered, case_name == "Winkler")


#extracting the befs values into columns


WR <- WR %>%
  mutate(bef1 = coalesce(str_extract(data, "(?<=bef1_)[0-9]+"), "0"),
         bef2 = coalesce(str_extract(data, "(?<=bef2_)[0-9]+"), "0"),
         bef3 = coalesce(str_extract(data, "(?<=bef3_)[0-9]+"), "0"),
         bef4 = coalesce(str_extract(data, "(?<=bef4_)[0-9]+"), "0"),
         bef5 = coalesce(str_extract(data, "(?<=bef5_)[0-9]+"), "0"),
         bef6 = coalesce(str_extract(data, "(?<=bef6_)[0-9]+"), "0"),
         bef7 = coalesce(str_extract(data, "(?<=bef7_)[0-9]+"), "0"),
         bef8 = coalesce(str_extract(data, "(?<=bef8_)[0-9]+"), "0"),
         bef9 = coalesce(str_extract(data, "(?<=bef9_)[0-9]+"), "0"),
         bef10 = coalesce(str_extract(data, "(?<=bef10_)[0-9]+"), "0"),
         bef11 = coalesce(str_extract(data, "(?<=bef11_)[0-9]+"), "0"),
         bef12 = coalesce(str_extract(data, "(?<=bef12_)[0-9]+"), "0"),
         bef13 = coalesce(str_extract(data, "(?<=bef13_)[0-9]+"), "0"))

#extracting the diag values into columns
WR <- WR %>%
  mutate(fdiag1 = coalesce(str_extract(data, "(?<=fdiag1-)[diag0-9]+"), "0"),
         fdiag2 = coalesce(str_extract(data, "(?<=fdiag2-)[diag0-9]+"), "0"),
         fdiag3 = coalesce(str_extract(data, "(?<=fdiag3-)[diag0-9]+"), "0"),
         fdiag4 = coalesce(str_extract(data, "(?<=fdiag4-)[diag0-9]+"), "0"),
         fdiag5 = coalesce(str_extract(data, "(?<=fdiag5-)[diag0-9]+"), "0"),
         fdiag6 = coalesce(str_extract(data, "(?<=fdiag6-)[diag0-9]+"), "0"),
         fdiag7 = coalesce(str_extract(data, "(?<=fdiag7-)[diag0-9]+"), "0"),
         fdiag8 = coalesce(str_extract(data, "(?<=fdiag8-)[diag0-9]+"), "0"),
         fdiag9 = coalesce(str_extract(data, "(?<=fdiag9-)[diag0-9]+"), "0"),
         fdiag10 = coalesce(str_extract(data, "(?<=fdiag10-)[diag0-9]+"), "0"))


#viewing the dataset
View (WR)

##############
'END Winkler'
##############

#################
'START Fomin'
################
FN <-  filter(df_filtered, case_name == "Fomin")


#extracting the befs values into columns


FN <- FN %>%
  mutate(bef1 = coalesce(str_extract(data, "(?<=bef1_)[0-9]+"), "0"),
         bef2 = coalesce(str_extract(data, "(?<=bef2_)[0-9]+"), "0"),
         bef3 = coalesce(str_extract(data, "(?<=bef3_)[0-9]+"), "0"),
         bef4 = coalesce(str_extract(data, "(?<=bef4_)[0-9]+"), "0"),
         bef5 = coalesce(str_extract(data, "(?<=bef5_)[0-9]+"), "0"),
         bef6 = coalesce(str_extract(data, "(?<=bef6_)[0-9]+"), "0"),
         bef7 = coalesce(str_extract(data, "(?<=bef7_)[0-9]+"), "0"),
         bef8 = coalesce(str_extract(data, "(?<=bef8_)[0-9]+"), "0"),
         bef9 = coalesce(str_extract(data, "(?<=bef9_)[0-9]+"), "0"),
         bef10 = coalesce(str_extract(data, "(?<=bef10_)[0-9]+"), "0"),
         bef11 = coalesce(str_extract(data, "(?<=bef11_)[0-9]+"), "0"),
         bef12 = coalesce(str_extract(data, "(?<=bef12_)[0-9]+"), "0"),
         bef13 = coalesce(str_extract(data, "(?<=bef13_)[0-9]+"), "0"))

#extracting the diag values into columns
FN <- FN %>%
  mutate(fdiag1 = coalesce(str_extract(data, "(?<=fdiag1-)[diag0-9]+"), "0"),
         fdiag2 = coalesce(str_extract(data, "(?<=fdiag2-)[diag0-9]+"), "0"),
         fdiag3 = coalesce(str_extract(data, "(?<=fdiag3-)[diag0-9]+"), "0"),
         fdiag4 = coalesce(str_extract(data, "(?<=fdiag4-)[diag0-9]+"), "0"),
         fdiag5 = coalesce(str_extract(data, "(?<=fdiag5-)[diag0-9]+"), "0"),
         fdiag6 = coalesce(str_extract(data, "(?<=fdiag6-)[diag0-9]+"), "0"),
         fdiag7 = coalesce(str_extract(data, "(?<=fdiag7-)[diag0-9]+"), "0"),
         fdiag8 = coalesce(str_extract(data, "(?<=fdiag8-)[diag0-9]+"), "0"),
         fdiag9 = coalesce(str_extract(data, "(?<=fdiag9-)[diag0-9]+"), "0"),
         fdiag10 = coalesce(str_extract(data, "(?<=fdiag10-)[diag0-9]+"), "0"))


#viewing the dataset
View (FN)



##############
'END Fomin'
##############

#################
'START Forster'
################
FORS <-  filter(df_filtered, case_name == "Forster")


#extracting the befs values into columns


FORS <- FORS %>%
  mutate(bef1 = coalesce(str_extract(data, "(?<=bef1_)[0-9]+"), "0"),
         bef2 = coalesce(str_extract(data, "(?<=bef2_)[0-9]+"), "0"),
         bef3 = coalesce(str_extract(data, "(?<=bef3_)[0-9]+"), "0"),
         bef4 = coalesce(str_extract(data, "(?<=bef4_)[0-9]+"), "0"),
         bef5 = coalesce(str_extract(data, "(?<=bef5_)[0-9]+"), "0"),
         bef6 = coalesce(str_extract(data, "(?<=bef6_)[0-9]+"), "0"),
         bef7 = coalesce(str_extract(data, "(?<=bef7_)[0-9]+"), "0"),
         bef8 = coalesce(str_extract(data, "(?<=bef8_)[0-9]+"), "0"),
         bef9 = coalesce(str_extract(data, "(?<=bef9_)[0-9]+"), "0"),
         bef10 = coalesce(str_extract(data, "(?<=bef10_)[0-9]+"), "0"),
         bef11 = coalesce(str_extract(data, "(?<=bef11_)[0-9]+"), "0"),
         bef12 = coalesce(str_extract(data, "(?<=bef12_)[0-9]+"), "0"),
         bef13 = coalesce(str_extract(data, "(?<=bef13_)[0-9]+"), "0"))


FORS <- FORS %>%
  mutate(fdiag1 = coalesce(str_extract(data, "(?<=fdiag1-)[diag0-9]+"), "0"),
         fdiag2 = coalesce(str_extract(data, "(?<=fdiag2-)[diag0-9]+"), "0"),
         fdiag3 = coalesce(str_extract(data, "(?<=fdiag3-)[diag0-9]+"), "0"),
         fdiag4 = coalesce(str_extract(data, "(?<=fdiag4-)[diag0-9]+"), "0"),
         fdiag5 = coalesce(str_extract(data, "(?<=fdiag5-)[diag0-9]+"), "0"),
         fdiag6 = coalesce(str_extract(data, "(?<=fdiag6-)[diag0-9]+"), "0"),
         fdiag7 = coalesce(str_extract(data, "(?<=fdiag7-)[diag0-9]+"), "0"),
         fdiag8 = coalesce(str_extract(data, "(?<=fdiag8-)[diag0-9]+"), "0"),
         fdiag9 = coalesce(str_extract(data, "(?<=fdiag9-)[diag0-9]+"), "0"),
         fdiag10 = coalesce(str_extract(data, "(?<=fdiag10-)[diag0-9]+"), "0"))


#viewing the dataset
View (FORS)

##################################################
'Second stage of data preparation was to determine:
if changes had been made or not (error adjustments) and 
the type of changes and the number of times changes were made'
####################################################


######################################################
'Determining if changes occured within the fdigs
and befs for Freundorf dataset'
#######################################################
changes_FR <- data.frame(card_id = character(),
                         user_login = character(),
                         instance_number = integer(),
                         unique_id = character(),
                         mandatory_result = character(),
                         adequately_justified = character(),
                         changed = integer(),
                         deleted = integer(),
                         added = integer(),
                         stringsAsFactors = FALSE)

distinct_card_ids <- unique(FR$card_id)

for (card_id in distinct_card_ids) {
  # Subset the data for the current card_id
  card_data <- FR[FR$card_id == card_id, ]
  
  # Check the number of occurrences for the current card_id
  num_occurrences <- nrow(card_data)
  
  # Ignore card_id with only one occurrence
  if (num_occurrences > 1) {
    # Get the first instance of the card_id
    first_instance <- card_data[1, ]
    
    # Loop through subsequent instances
    for (i in 2:num_occurrences) {
      current_instance <- card_data[i, ]
      
      # Check if any changes, deletions, or additions were made for bef columns
      bef_columns <- paste0("bef", 1:13)
      bef_changes <- colSums(current_instance[, bef_columns] != first_instance[, bef_columns])
      bef_deletions <- sum(colSums(current_instance[, bef_columns] == "0") > 0)
      bef_additions <- sum(colSums(current_instance[, bef_columns] != "0") > 0) - 13
      
      # Check if any changes, deletions, or additions were made for fdiag columns
      fdiag_columns <- paste0("fdiag", 1:10)
      fdiag_changes <- colSums(current_instance[, fdiag_columns] != first_instance[, fdiag_columns])
      fdiag_deletions <- sum(colSums(current_instance[, fdiag_columns] == "0") > 0)
      fdiag_additions <- sum(colSums(current_instance[, fdiag_columns] != "0") > 0) - 10
      
      # Create a row for the current card_id instance with the calculated values
      card_changes <- data.frame(card_id = card_id,
                                 user_login = current_instance$user_login,
                                 instance_number = current_instance$instance_number,
                                 mandatory_result = current_instance$mandatory_result,
                                 adequately_justified = current_instance$adequately_justified,
                                 unique_id = current_instance$unique_id,
                                 changed = as.integer(any(bef_changes > 0) || any(fdiag_changes > 0)),
                                 deleted = as.integer(bef_deletions > 0 || fdiag_deletions > 0),
                                 added = as.integer(bef_additions > 0 || fdiag_additions > 0))
      
      # Append the row to the changes data frame
      changes_FR <- rbind(changes_FR, card_changes)
    }
  }
}

####adding the case_names to it
changes_FR = changes_FR %>%
  add_column(case_name = rep(c('Freundorf')), .after = 'user_login')

View(changes_FR)


##########################################################
'Determining if changes occured within the fdigs and befs 
for Goettlich dataset'
########################################################## 

changes_GH <- data.frame(card_id = character(),
                         user_login = character(),
                         instance_number = integer(),
                         unique_id = character(),
                         mandatory_result = character(),
                         adequately_justified = character(),
                         changed = integer(),
                         deleted = integer(),
                         added = integer(),
                         stringsAsFactors = FALSE)

distinct_card_ids <- unique(GH$card_id)

for (card_id in distinct_card_ids) {
  # Subset the data for the current card_id
  card_data <- GH[GH$card_id == card_id, ]
  
  # Check the number of occurrences for the current card_id
  num_occurrences <- nrow(card_data)
  
  # Ignore card_id with only one occurrence
  if (num_occurrences > 1) {
    # Get the first instance of the card_id
    first_instance <- card_data[1, ]
    
    # Loop through subsequent instances
    for (i in 2:num_occurrences) {
      current_instance <- card_data[i, ]
      
      # Check if any changes, deletions, or additions were made for bef columns
      bef_columns <- paste0("bef", 1:13)
      bef_changes <- colSums(current_instance[, bef_columns] != first_instance[, bef_columns])
      bef_deletions <- sum(colSums(current_instance[, bef_columns] == "0") > 0)
      bef_additions <- sum(colSums(current_instance[, bef_columns] != "0") > 0) - 13
      
      # Check if any changes, deletions, or additions were made for fdiag columns
      fdiag_columns <- paste0("fdiag", 1:10)
      fdiag_changes <- colSums(current_instance[, fdiag_columns] != first_instance[, fdiag_columns])
      fdiag_deletions <- sum(colSums(current_instance[, fdiag_columns] == "0") > 0)
      fdiag_additions <- sum(colSums(current_instance[, fdiag_columns] != "0") > 0) - 10
      
      # Create a row for the current card_id instance with the calculated values
      card_changes <- data.frame(card_id = card_id,
                                 user_login = current_instance$user_login,
                                 instance_number = current_instance$instance_number,
                                 mandatory_result = current_instance$mandatory_result,
                                 adequately_justified = current_instance$adequately_justified,
                                 unique_id = current_instance$unique_id,
                                 changed = as.integer(any(bef_changes > 0) || any(fdiag_changes > 0)),
                                 deleted = as.integer(bef_deletions > 0 || fdiag_deletions > 0),
                                 added = as.integer(bef_additions > 0 || fdiag_additions > 0))
      
      # Append the row to the changes data frame
      changes_GH <- rbind(changes_GH, card_changes)
    }
  }
}

####adding the case_names to it
changes_GH = changes_GH %>%
  add_column(case_name = rep(c('Goettlich ')), .after = 'user_login')

View(changes_GH)


##########################################################
'Determining if changes occured within the fdigs and befs
for Binder dataset'
##########################################################
changes_BR <- data.frame(card_id = character(),
                         user_login = character(),
                         instance_number = integer(),
                         unique_id = character(),
                         mandatory_result = character(),
                         adequately_justified = character(),
                         changed = integer(),
                         deleted = integer(),
                         added = integer(),
                         stringsAsFactors = FALSE)

distinct_card_ids <- unique(BR$card_id)

for (card_id in distinct_card_ids) {
  # Subset the data for the current card_id
  card_data <- BR[BR$card_id == card_id, ]
  
  # Check the number of occurrences for the current card_id
  num_occurrences <- nrow(card_data)
  
  # Ignore card_id with only one occurrence
  if (num_occurrences > 1) {
    # Get the first instance of the card_id
    first_instance <- card_data[1, ]
    
    # Loop through subsequent instances
    for (i in 2:num_occurrences) {
      current_instance <- card_data[i, ]
      
      # Check if any changes, deletions, or additions were made for bef columns
      bef_columns <- paste0("bef", 1:13)
      bef_changes <- colSums(current_instance[, bef_columns] != first_instance[, bef_columns])
      bef_deletions <- sum(colSums(current_instance[, bef_columns] == "0") > 0)
      bef_additions <- sum(colSums(current_instance[, bef_columns] != "0") > 0) - 13
      
      # Check if any changes, deletions, or additions were made for fdiag columns
      fdiag_columns <- paste0("fdiag", 1:10)
      fdiag_changes <- colSums(current_instance[, fdiag_columns] != first_instance[, fdiag_columns])
      fdiag_deletions <- sum(colSums(current_instance[, fdiag_columns] == "0") > 0)
      fdiag_additions <- sum(colSums(current_instance[, fdiag_columns] != "0") > 0) - 10
      
      # Create a row for the current card_id instance with the calculated values
      card_changes <- data.frame(card_id = card_id,
                                 user_login = current_instance$user_login,
                                 instance_number = current_instance$instance_number,
                                 unique_id = current_instance$unique_id,
                                 mandatory_result = current_instance$mandatory_result,
                                 adequately_justified = current_instance$adequately_justified,
                                 changed = as.integer(any(bef_changes > 0) || any(fdiag_changes > 0)),
                                 deleted = as.integer(bef_deletions > 0 || fdiag_deletions > 0),
                                 added = as.integer(bef_additions > 0 || fdiag_additions > 0))
      
      # Append the row to the changes data frame
      changes_BR <- rbind(changes_BR, card_changes)
    }
  }
}

####adding the case_names to it
changes_BR = changes_BR %>%
  add_column(case_name = rep(c('Binder')), .after = 'user_login')

View(changes_BR)


############################################################
'Determining if changes occured within the fdigs and befs
for Schenker dataset'
###########################################################

changes_SR <- data.frame(card_id = character(),
                         user_login = character(),
                         instance_number = integer(),
                         unique_id = character(),
                         mandatory_result = character(),
                         adequately_justified = character(),
                         changed = integer(),
                         deleted = integer(),
                         added = integer(),
                         stringsAsFactors = FALSE)

distinct_card_ids <- unique(SR$card_id)

for (card_id in distinct_card_ids) {
  # Subset the data for the current card_id
  card_data <- SR[SR$card_id == card_id, ]
  
  # Check the number of occurrences for the current card_id
  num_occurrences <- nrow(card_data)
  
  # Ignore card_id with only one occurrence
  if (num_occurrences > 1) {
    # Get the first instance of the card_id
    first_instance <- card_data[1, ]
    
    # Loop through subsequent instances
    for (i in 2:num_occurrences) {
      current_instance <- card_data[i, ]
      
      # Check if any changes, deletions, or additions were made for bef columns
      bef_columns <- paste0("bef", 1:13)
      bef_changes <- colSums(current_instance[, bef_columns] != first_instance[, bef_columns])
      bef_deletions <- sum(colSums(current_instance[, bef_columns] == "0") > 0)
      bef_additions <- sum(colSums(current_instance[, bef_columns] != "0") > 0) - 13
      
      # Check if any changes, deletions, or additions were made for fdiag columns
      fdiag_columns <- paste0("fdiag", 1:10)
      fdiag_changes <- colSums(current_instance[, fdiag_columns] != first_instance[, fdiag_columns])
      fdiag_deletions <- sum(colSums(current_instance[, fdiag_columns] == "0") > 0)
      fdiag_additions <- sum(colSums(current_instance[, fdiag_columns] != "0") > 0) - 10
      
      # Create a row for the current card_id instance with the calculated values
      card_changes <- data.frame(card_id = card_id,
                                 user_login = current_instance$user_login,
                                 instance_number = current_instance$instance_number,
                                 unique_id = current_instance$unique_id,
                                 mandatory_result = current_instance$mandatory_result,
                                 adequately_justified = current_instance$adequately_justified,
                                 changed = as.integer(any(bef_changes > 0) || any(fdiag_changes > 0)),
                                 deleted = as.integer(bef_deletions > 0 || fdiag_deletions > 0),
                                 added = as.integer(bef_additions > 0 || fdiag_additions > 0))
      
      # Append the row to the changes data frame
      changes_SR <- rbind(changes_SR, card_changes)
    }
  }
}

####adding the case_names to it
changes_SR = changes_SR %>%
  add_column(case_name = rep(c('Schenker')), .after = 'user_login')

View(changes_SR)


##############################################################
'Determining if changes occured within the fdigs and befs
for Winkler dataset'
#############################################################

changes_WR <- data.frame(card_id = character(),
                         user_login = character(),
                         instance_number = integer(),
                         unique_id = character(),
                         mandatory_result = character(),
                         adequately_justified = character(),
                         changed = integer(),
                         deleted = integer(),
                         added = integer(),
                         stringsAsFactors = FALSE)

distinct_card_ids <- unique(WR$card_id)

for (card_id in distinct_card_ids) {
  # Subset the data for the current card_id
  card_data <- WR[WR$card_id == card_id, ]
  
  # Check the number of occurrences for the current card_id
  num_occurrences <- nrow(card_data)
  
  # Ignore card_id with only one occurrence
  if (num_occurrences > 1) {
    # Get the first instance of the card_id
    first_instance <- card_data[1, ]
    
    # Loop through subsequent instances
    for (i in 2:num_occurrences) {
      current_instance <- card_data[i, ]
      
      # Check if any changes, deletions, or additions were made for bef columns
      bef_columns <- paste0("bef", 1:13)
      bef_changes <- colSums(current_instance[, bef_columns] != first_instance[, bef_columns])
      bef_deletions <- sum(colSums(current_instance[, bef_columns] == "0") > 0)
      bef_additions <- sum(colSums(current_instance[, bef_columns] != "0") > 0) - 13
      
      # Check if any changes, deletions, or additions were made for fdiag columns
      fdiag_columns <- paste0("fdiag", 1:10)
      fdiag_changes <- colSums(current_instance[, fdiag_columns] != first_instance[, fdiag_columns])
      fdiag_deletions <- sum(colSums(current_instance[, fdiag_columns] == "0") > 0)
      fdiag_additions <- sum(colSums(current_instance[, fdiag_columns] != "0") > 0) - 10
      
      # Create a row for the current card_id instance with the calculated values
      card_changes <- data.frame(card_id = card_id,
                                 user_login = current_instance$user_login,
                                 instance_number = current_instance$instance_number,
                                 unique_id = current_instance$unique_id,
                                 mandatory_result = current_instance$mandatory_result,
                                 adequately_justified = current_instance$adequately_justified,
                                 changed = as.integer(any(bef_changes > 0) || any(fdiag_changes > 0)),
                                 deleted = as.integer(bef_deletions > 0 || fdiag_deletions > 0),
                                 added = as.integer(bef_additions > 0 || fdiag_additions > 0))
      
      # Append the row to the changes data frame
      changes_WR <- rbind(changes_WR, card_changes)
    }
  }
}

####adding the case_names to it
changes_WR = changes_WR %>%
  add_column(case_name = rep(c('Winkler')), .after = 'user_login')

View(changes_WR)


############################################################
'Determining if changes occured within the fdigs and befs 
for Fomin dataset'
###########################################################

changes_FN <- data.frame(card_id = character(),
                         user_login = character(),
                         instance_number = integer(),
                         unique_id = character(),
                         mandatory_result = character(),
                         adequately_justified = character(),
                         changed = integer(),
                         deleted = integer(),
                         added = integer(),
                         stringsAsFactors = FALSE)

distinct_card_ids <- unique(FN$card_id)

for (card_id in distinct_card_ids) {
  # Subset the data for the current card_id
  card_data <- FN[FN$card_id == card_id, ]
  
  # Check the number of occurrences for the current card_id
  num_occurrences <- nrow(card_data)
  
  # Ignore card_id with only one occurrence
  if (num_occurrences > 1) {
    # Get the first instance of the card_id
    first_instance <- card_data[1, ]
    
    # Loop through subsequent instances
    for (i in 2:num_occurrences) {
      current_instance <- card_data[i, ]
      
      # Check if any changes, deletions, or additions were made for bef columns
      bef_columns <- paste0("bef", 1:13)
      bef_changes <- colSums(current_instance[, bef_columns] != first_instance[, bef_columns])
      bef_deletions <- sum(colSums(current_instance[, bef_columns] == "0") > 0)
      bef_additions <- sum(colSums(current_instance[, bef_columns] != "0") > 0) - 13
      
      # Check if any changes, deletions, or additions were made for fdiag columns
      fdiag_columns <- paste0("fdiag", 1:10)
      fdiag_changes <- colSums(current_instance[, fdiag_columns] != first_instance[, fdiag_columns])
      fdiag_deletions <- sum(colSums(current_instance[, fdiag_columns] == "0") > 0)
      fdiag_additions <- sum(colSums(current_instance[, fdiag_columns] != "0") > 0) - 10
      
      # Create a row for the current card_id instance with the calculated values
      card_changes <- data.frame(card_id = card_id,
                                 user_login = current_instance$user_login,
                                 instance_number = current_instance$instance_number,
                                 unique_id = current_instance$unique_id,
                                 mandatory_result = current_instance$mandatory_result,
                                 adequately_justified = current_instance$adequately_justified,
                                 changed = as.integer(any(bef_changes > 0) || any(fdiag_changes > 0)),
                                 deleted = as.integer(bef_deletions > 0 || fdiag_deletions > 0),
                                 added = as.integer(bef_additions > 0 || fdiag_additions > 0))
      
      # Append the row to the changes data frame
      changes_FN <- rbind(changes_FN, card_changes)
    }
  }
}

####adding the case_names to it
changes_FN = changes_FN %>%
  add_column(case_name = rep(c('Fomin')), .after = 'user_login')
View(changes_FN)


###########################################################
'Determining if changes occured within the fdigs and befs
for Forster dataset'
##########################################################

changes_FORS <- data.frame(card_id = character(),
                           user_login = character(),
                           instance_number = integer(),
                           unique_id = character(),
                           mandatory_result = character(),
                           adequately_justified = character(),
                           changed = integer(),
                           deleted = integer(),
                           added = integer(),
                           stringsAsFactors = FALSE)

distinct_card_ids <- unique(FORS$card_id)

for (card_id in distinct_card_ids) {
  # Subset the data for the current card_id
  card_data <- FORS[FORS$card_id == card_id, ]
  
  # Check the number of occurrences for the current card_id
  num_occurrences <- nrow(card_data)
  
  # Ignore card_id with only one occurrence
  if (num_occurrences > 1) {
    # Get the first instance of the card_id
    first_instance <- card_data[1, ]
    
    # Loop through subsequent instances
    for (i in 2:num_occurrences) {
      current_instance <- card_data[i, ]
      
      # Check if any changes, deletions, or additions were made for bef columns
      bef_columns <- paste0("bef", 1:13)
      bef_changes <- colSums(current_instance[, bef_columns] != first_instance[, bef_columns])
      bef_deletions <- sum(colSums(current_instance[, bef_columns] == "0") > 0)
      bef_additions <- sum(colSums(current_instance[, bef_columns] != "0") > 0) - 13
      
      # Check if any changes, deletions, or additions were made for fdiag columns
      fdiag_columns <- paste0("fdiag", 1:10)
      fdiag_changes <- colSums(current_instance[, fdiag_columns] != first_instance[, fdiag_columns])
      fdiag_deletions <- sum(colSums(current_instance[, fdiag_columns] == "0") > 0)
      fdiag_additions <- sum(colSums(current_instance[, fdiag_columns] != "0") > 0) - 10
      
      # Create a row for the current card_id instance with the calculated values
      card_changes <- data.frame(card_id = card_id,
                                 user_login = current_instance$user_login,
                                 instance_number = current_instance$instance_number,
                                 unique_id = current_instance$unique_id,
                                 mandatory_result = current_instance$mandatory_result,
                                 adequately_justified = current_instance$adequately_justified,
                                 changed = as.integer(any(bef_changes > 0) || any(fdiag_changes > 0)),
                                 deleted = as.integer(bef_deletions > 0 || fdiag_deletions > 0),
                                 added = as.integer(bef_additions > 0 || fdiag_additions > 0))
      
      # Append the row to the changes data frame
      changes_FORS <- rbind(changes_FORS, card_changes)
    }
  }
}

####adding the case_names to it
changes_FORS = changes_FORS %>%
  add_column(case_name = rep(c('Forster')), .after = 'user_login')

View(changes_FORS)


########################################################
'Combining the separate datasets created based 
on the casenames'
#######################################################

combined_datasets <- rbind(changes_BR,changes_FN, changes_FORS, changes_FR, 
                           changes_GH, changes_SR, changes_WR)


str (combined_datasets)



'Selecting only the instances where error adjustments
were made after an impasse and we using instance > 1'
combined_datasetsabove1 <- combined_datasets %>%
  filter(instance_number > 1)

View(combined_datasetsabove1)


####################################################
' defining accuracy as when MR and AJ = True
determining the max count for error adjustement'
####################################################

data <- combined_datasetsabove1 %>%
  group_by(unique_id) %>%
  summarize(
    count = n(),
    max_instance_number = max(instance_number),
    accuracy = as.integer(all(mandatory_result == "true" & adequately_justified == "true")),
    card_id = first(card_id),
    user_login = first(user_login),
    case_name = first(case_name)
  )

View(data) 

###############################################################################
'reading in the accuracy dataset'
###############################################################################

accuracy <- read_excel("Accuracy2.xlsx")

#deleting unnecessary columns
accuracy <- accuracy[, c("user_login", "case_name", "pt_lsg2")]

###############################
'Merging'
###############################


final_data <- merge(accuracy, data, by = c("user_login", "case_name"))

'although I added the column for the experiments original measure of accuracy

which was captured in the column pt_lsg2, I didnt not use that to determine
accuracy in my analysis, i rather used MR and AJ = True: 
becuase that was the initial criteria for determing those who got it wrong
and also becuase the pt_lsg2 column had some missing values and 0.5 which 
I didnt know how to deal with'

###############################################################
'peparing the variables for analysis'
##############################################################

#coding low and high error adjustments counts
final_data$low_high <- as.factor(as.numeric(final_data$low_high))

final_data$low_high <- ifelse(final_data$count %in% c(1), "low", "high")

final_data$dumlow_high <- as.factor(as.numeric(final_data$dumlow_high))

final_data$dumlow_high <- ifelse(final_data$count %in% c(1), "0", "1")
final_data$dumlow_high <- (as.numeric(final_data$dumlow_high))

View(final_data)




###############################################################################
'creating another data with the necessary averages (accurarcy, error adjustments
per a user, for scatter plot analysis'
###############################################################################

####creating the dataset with mean accuracy
data2 = final_data %>%
  group_by(user_login) %>%  
  summarise(mean_accuracy = mean(accuracy))
View (data2)

#####creating mean changes

ErAdj = final_data %>%
  group_by(user_login) %>%  
  summarise(mean_errorA = mean(dumlow_high))

View (ErAdj)

#######creating grouping variable instnace == 0 is low and above is high

data2 <- merge(data2, ErAdj, by = "user_login", all.x = TRUE)


View (data2)







###############################################################################
'ANALYSIS: BOTH DESCRIPTIVES AND INFERENTIAL STATISTIS'
###############################################################################


##############################################################
'Descriptive statistic'
##############################################################

'PLOTS'

# Create scatter plot for changes and accuracy
plot(data2$mean_errorA, data2$mean_accuracy, main = "Scatter Plot", 
     xlab = "Error Adjustment", ylab = " Accuracy", pch = 16, col = "black")

fit <- lm(mean_errorA ~ mean_accuracy, data = data2)  # Perform linear regression
abline(fit, col = "black")


'Frequency, percentages and Proportions tables'

#########error adjustment and accuracy#########


# Calculate frequencies
count_freq <- table(final_data$low_high, final_data$accuracy)

# Calculate percentages
count_perc <- prop.table(count_freq, margin = 2) * 100

# Calculate proportions
count_prop <- prop.table(count_freq, margin = 1) * 100

# Combine frequencies, percentages, and proportions into a table
result_table <- cbind(Frequencies = count_freq, Percentages = count_perc, Proportions = count_prop)

# Print the result table
print(result_table)


##########################accuracy and case_name#################

# Calculate frequencies
count_freq <- table(final_data$case_name, final_data$accuracy)

# Calculate percentages
count_perc <- prop.table(count_freq, margin = 2) * 100

# Combine frequencies and percentages into a table
result_table <- cbind(Frequencies = count_freq, Percentages = count_perc)

# Print the result table
print(result_table)



########################################################
'inreferential statistics'
#######################################################



###############################
'correlation between changes 
and accuracy'
###############################

'this was done using data'

cor (final_data$accuracy, final_data$dumlow_high, method = 'pearson')
p_value <- cor.test(final_data$accuracy, final_data$dumlow_high)$p.value
mean(data$accuracy)
mean(data$dumlow_high)
n_high <- sum(data$dumlow_high == 1)
n_low <- sum(data$dumlow_high == 0)
print (n_high)
print (n_low)

# Print the correlation coefficient and its significance
print(p_value)

# Perform independent t-test
result <- t.test(accuracy ~ low_high, var.equal = TRUE, data = final_data, method = 't.test')

# View the result
print(result)


# Calculate mean and standard deviation from the original dataset
mean_high <- mean(final_data$dumlow_high[final_data$dumlow_high == 1])
mean_low <- mean(final_data$dumlow_high[final_data$dumlow_high == 0])
sd_high <- sd(final_data$dumlow_high[final_data$dumlow_high == 1])
sd_low <- sd(final_data$dumlow_high[final_data$dumlow_high == 0])
n_high <- sum(final_data$dumlow_high == 1)
n_low <- sum(final_data$dumlow_high == 0)

# Print the mean, standard deviation, and n
cat("Mean in group high:", mean_high, "\n")
cat("Mean in group low:", mean_low, "\n")
cat("Standard deviation in group high:", sd_high, "\n")
cat("Standard deviation in group low:", sd_low, "\n")
cat("Sample size (n) in group high:", n_high, "\n")
cat("Sample size (n) in group low:", n_low, "\n")


