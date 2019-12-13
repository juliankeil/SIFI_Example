#### Analysis Script for the SIFI (Basic offline) by Merle Marie Schuckart
####
#### This file is a basic analyis script for the lab.js-based example experiment for the sound-induced flash illusion.
#### The experiment can be found here: https://github.com/juliankeil/SIFI_Example
#### This file can also be found at https://github.com/MMarieSchuckart/R-scripts/blob/master/SIFI/SIFI_Basic_offline
#### This is work in progress, please let me know if I can improve this. :-)

### 1. Read in the data

## 1.1 Set Working Directory
setwd('/Users/merle/Desktop/SIFI Basic/SIFI Basic Daten')

## 1.2 Get the list of files in the Directory
file_list <- list.files(pattern='.csv')

## 1.3 Loop through files to read in data
# (This might take some time.)
subject_data <- list() # Create placeholder for the main list

# Subject loop starts here
for (i in 1:length(file_list)) {

	# 1.3.1. Get data for one participant
	subject <- data.frame((read.csv(file_list[i])), stringsAsFactors = F)

	# 1.3.2 Get information concerning the experiment (Demographics)
	position_code <-  which(subject$sender == "participant's ID")
	Demographics_Value <- as.character(subject[position_code, "id_participant"])
	Demographics_Label <- "id_participant"

	# 1.3.2.1 Find Row with the demographics (for id_gender, id_birth,...)
	position_demographics <-  which(subject$sender == "demographics")
	dem_start <- which(colnames(subject) == "id_birth")
	dem_end <- which(colnames(subject) == "id_neuro")

	for (j in dem_start:dem_end) {
  		tmp <- as.character(subject[position_demographics, colnames(subject)[j] ])
  		Demographics_Value <- c(Demographics_Value, tmp)
  		Demographics_Label <- c(Demographics_Label, colnames(subject)[j])
	} # End Demographics

	age <- as.numeric(substr(subject$timestamp[1],1,4)) - as.numeric(substr(Demographics_Value[2],1,4))
	Demographics_Value <- c(Demographics_Value, age)
	Demographics_Label <- c(Demographics_Label, "id_age")

	# 1.3.2.2 Combine Demographics to Data Frame and Clean up
	Demographics <- data.frame("Label" = Demographics_Label,
                           "Value" = Demographics_Value, stringsAsFactors = F)
	Demographics[Demographics ==""] <- NA
	Demographics <- na.omit(Demographics)

	# 1.3.3. Get information concerning the experiment (Feedback)
	Feedback_Value <- NULL
	Feedback_Label <- NULL

	# 1.3.3.1 Find Row with the feedback
	position_feedback <-  which(subject$sender == "last questions")
	exp_start <- which(colnames(subject) == "exp_auditory")
	exp_end <- which(colnames(subject) == "exp_visual")

	for (j in exp_start:exp_end) {
  		tmp <- as.character(subject[position_feedback, colnames(subject)[j] ])
  		Feedback_Value <- c(Feedback_Value, tmp)
  		Feedback_Label <- c(Feedback_Label, colnames(subject)[j])
	} # End Feedback

	Feedback <- data.frame("Label" = Feedback_Label,
                       "Value" = Feedback_Value, stringsAsFactors = F)

	# 1.3.4. Get the Data
	# 1.3.4.1 Change Event-IDs to Numeric
	subject$sender_id <- as.numeric(sub("_.*", "", subject$sender_id))

	# 1.3.4.2 List and Find the Rows with the names of the blocks?
	#row_names <- c("training block","main block") # including training block
	row_names <- c("main block") # excluding training block
	
	pos <- list()
	for (k in 1:length(row_names)) {
  		pos[k] <- which(subject$sender == row_names[k])
	} # End Row

	# 1.3.4.3 Find the Event ID Indicating the Block Onset
	block_onset <- list()
	for (k in 1:length(pos)) {
  		block_onset[k] <- as.numeric(subject[as.numeric(pos[k]), "sender_id"])
	} # End Blocks

	# 1.3.4.4 SIFI Loop through the data to find the single blocks
	SIFI <- data.frame()
	for (l in 1:length(block_onset)) {
 		tmppos <- which((subject$sender == "SIFI response") &
                    (subject$sender_id == block_onset[l])) #&
  		#(subject$ended_on == "response")) # I could use this one to filter the responses

  		Label <- as.factor(subject[tmppos, "SIFIlabel"])
  		Value <- as.numeric(subject[tmppos, "duration"])
  		Response <- as.factor(subject[tmppos, "response"])
  		Block <- as.factor(row_names[l])

  		tmp_frame <-  data.frame("Label" = Label,
                           "Value" = Value,
                           "Response" = Response,
                           "Block" = Block,
                           stringsAsFactors = F)

  		# And Add to big list
  		SIFI <- rbind(SIFI, tmp_frame)
	} # End SIFI

	# 1.3.5. Put lists into list:
	tmplist <- list(Demographics,Feedback,SIFI)
	subject_data[i] <- list(tmplist)
	names(subject_data)[i] <- paste0("subject_", as.character(i))
	
}  # Subject-Loop End here

# Clean Environment: Remove all variables except for the nested list
rm(list=setdiff(ls(), "subject_data"))

### 2. Save data if you want to
# Write data back to disk in RDA format.
#save(subject_data, file='subject_data')


### 3. Preprocessing: Clean the dataset
subject_data_raw <- subject_data # Save from harm :-)

## 3.1 Exclude datasets of participants with poor eyesight, impaired hearing, etc.

# Criteria for exclusion: Remove if...
# ... id_eyesight, id_hearing, id_neuro, id_drugs and exp_delay is "yes"
# ... exp_auditory, exp_letters and exp_visual is "no"
# ... exp_delayfrequency, exp_delayintensity????? <-- criteria for exclusion still a bit unclear

subject_clean <- list() # Placeholder for the Cleaned Datasets

# 3.1.2 Impaired Demographics
dem_select <- c("id_drugs","id_eyesight","id_hearing","id_neuro")

# Subject loop starts here
for(i in 1:length(subject_data)) {
	
  	# Selection-Loop
	for (j in 1:length(dem_select)) {
  		tmppos <- which(subject_data$subject_1[[1]]$Label == dem_select[j])

    	subject <- subject_data[[i]]

    	if (subject[[1]]$Value[[tmppos]] == "yes"){
    		subject <-  NA
      		subject_clean[[i]] <- subject

    	} else {
      		subject_clean[[i]] <- subject_data[[i]]
      		names(subject_clean)[i] <- paste0("subject_", as.character(i))
    	} # IF
    	
  	} # Selection
} # subject

#	# 3.1.3 Impaired Experiment
#	exp_select <- c("exp_delay","exp_auditory","exp_visual",
#                 "exp_letters","exp_delayfrequency","exp_delayintensity")
#	exp_pass <- c("no","yes","yes","yes","never","zero") # Which values are OK to let pass?
#	# These could also be negative statements -> which values to exclude
#
#	# subject-Loop
#	for(i in 1:length(subject_data)) {
#		# Selection-Loop
#		for (j in 1:length(exp_select)) {
#			tmppos <- which(subject_data$subject_1[[2]]$Label == exp_select[j])
#
#		  subject <- subject_data[[i]]
#
#			if (is.null(subject[[2]]$Value[[tmppos]])) {
#   			subject[[2]]$Value[[tmppos]] <- NA
#  			subject <-  NA
# 			subject_clean[[i]] <- subject
#
#  			} else if (subject[[2]]$Value[[tmppos]] != exp_pass[j]){
#    			subject <-  NA
#    			subject_clean[[i]] <- subject
#
#  			} else {
#    		next()
#  			} # IF
#		} # Selection
#  	}  # subject


# 3.1.4 Remove NAs
if (length(which(is.na(subject_clean))) != 0){
	subject_clean <- subject_clean[-which(is.na(subject_clean))]
}

### 3.2. Save data if you want to
# Write data back to disk in RDA format.
# save(subject_clean, file='subject_clean')


### 4. Post-Processing
# 1. Compute Response Rates and RTs
# 2. Remove Participants if
#	- SIFI rates below 10% or above 90% in 2 out of 4 Blocks

subject_clean_1 <- subject_clean # Save from harm
subject_post <- list() # Placeholder

# Subject loop starts here
for (i in 1:length(subject_clean)){

	subject <- subject_clean[[i]]

  	# 4.2. SIFI Response Rates
  	tr_count <- NULL # trials
  	ill_count <- NULL # illusions
	
	# Loop Blocks
    for (b in 1:length(levels(subject[3][[1]]$Block))){
    
    	# How many A2V1 trials are there?
    	tr_count[b] <- length(which(subject[3][[1]]$Label == 'A2V1' &
                                  subject[3][[1]]$Block == levels(subject[3][[1]]$Block)[b]))
    	# How many illusions were perceived?
    	ill_count[b] <- length(which(subject[3][[1]]$Label == 'A2V1' &
                                   subject[3][[1]]$Block == levels(subject[3][[1]]$Block)[b] &
                                   subject[3][[1]]$Response == "2"))
  	} # end SIFI Blocks

	# Exclude Participants with less than 10% illusions or more than 90% illusions
  	Ill <- ill_count/tr_count  # illusion rates
  	low <- Ill < .1  # minimum
  	hi <- Ill > .9  # maximum

  	if (sum(low) >= 2 | sum(hi) >= 2) {
    	subject_post[[i]] <- NA
  	} else {
    	subject_post[[i]] <- subject
  	}# IF

  	# 4.3. A0V2 Perception -> Can participants see 2 flashes
  	# How many A0V2 trials are there?
  	tr_count <- NULL
  	resp_count <- NULL
  	
  	for (b in 1:length(levels(subject[3][[1]]$Block))){
    
    	# How many A0V2 trials are there?
    	tr_count[b] <- length(which(subject[3][[1]]$Label == 'A0V2' &
                                  subject[3][[1]]$Block == levels(subject[3][[1]]$Block)[b]))
    	# How many correctly perceived?
    	resp_count[b] <- length(which(subject[3][[1]]$Label == 'A0V2' &
                                   subject[3][[1]]$Block == levels(subject[3][[1]]$Block)[b] &
                                   subject[3][[1]]$Response == "2"))
  	} # end A0V2 Blocks

	# Exclude Participants who had more than 10% errors in the 2-flash condition
  	A0V2 <- resp_count/tr_count
  	if (A0V2 < .1) {
    	subject_post[[i]] <- NA
  	} else {
    	subject_post[[i]] <- subject
  	}# IF

}# subject loop ends here

# 4.4 Remove NAs
if (length(which(is.na(subject_post))) != 0){
  subject_post <- subject_post[-which(is.na(subject_post))]
}

# Clean up again:
rm(list=setdiff(ls(), c("subject_data","subject_data_raw",
                        "subject_clean","subject_clean_1",
                        "subject_post")))

#### 4. Save data
# Write data back to disk in RDA format.
# save(subject_post, file='subject_post')

# -------------------------------------------

# 5. Stats:
