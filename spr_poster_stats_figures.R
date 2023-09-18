library(tidyverse) 
library(patchwork) #for combining plots
library(plotrix) #for calculating standard error

# Andrew's last know location of data in sabat lab
sabat_data_folder <- "/Volumes/startech3TB_bkup/research_data/Pic_Vid"
andrew_data_folder <- "/home/andrewf/Research_data/EEG/Pic_Vid"

if (dir.exists(sabat_data_folder)) {
  parent_directory <- sabat_data_folder
  batch_file_directory <- paste0(sabat_data_folder, 
                                 "/batch_files")
  picture_by_cat_ar_directory <- paste0(sabat_data_folder, 
                                        "/avg_files/pics/by_category")
  cat_hamps_directory <- paste0(sabat_data_folder, 
                                "/hamp_files/videos/category")
  
} else if(dir.exists(andrew_data_folder)) {
  parent_directory <- andrew_data_folder
  batch_file_directory <- paste0(andrew_data_folder, 
                                 "/batch_files")
  picture_by_cat_ar_directory <- paste0(andrew_data_folder, 
                                        "/average_files/pics/by_category")
  cat_hamps_directory <- paste0(andrew_data_folder, 
                                "/hamp_files/videos/by_category")
} else{
  stop("Something is wrong, are you working on the right computer with access to the data")
}


pic_vid_data <- data.frame("par_id" = 1:50)

# Bad participants
no_pictures <- c(9)
no_videos <- c(38)

## no picture participants were missing more than 50% for any valence
# more_than_50_percent_of_video_trials_missing <- c(4, 22, 24, 25, 39, 45, 49, 50) # this is total trials, not per valence

pic_vid_data <- pic_vid_data %>% 
  filter(!par_id %in% c(no_pictures, no_videos))

# Number of good trials
number_of_good_trials <- tribble(
  ~ par_id, ~number_of_good_trials_pictures, ~number_of_good_trials_video,
  1, 77,69,
  2, 67,70,
  3, 86,77,
  4, 82,73,
  5, 77,67,
  6, 70,79,
  7, 69,78,
  8, 72,73,
  9,  0,61,
  10,66,76,
  11,79,75,
  12,83,67,
  13,75,75,
  14,55,80,
  15,82,77,
  16,82,78,
  17,70,80,
  18,80,75,
  19,67,72,
  20,77,77,
  21,61,76,
  22,80,49,
  23,82,80,
  24,85,63,
  25,73,68,
  26,66,73,
  27,84,83,
  28,82,81,
  29,82,79,
  30,75,74,
  31,82,72,
  32,82,78,
  33,79,83,
  34,77,54,
  35,78,75,
  36,83,82,
  37,82,75,
  38,84, 0,
  39,65,50,
  40,75,71,
  41,70,84,
  42,83,81,
  43,83,76,
  44,78,81,
  45,82,79,
  46,76,71,
  47,74,73,
  48,81,75,
  49,77,41,
  50,70,54)

# write.csv(number_of_good_trials,
#           file = paste0(parent_directory, "/misc/pic_vid_good_trials_out_of_90.csv"),
#           quote = F,row.names = F)

pic_vid_data <- merge(x = pic_vid_data, y = number_of_good_trials,
                      by.x = "par_id", by.y = "par_id", all.x = T)

# Demographics
# get from Matt by par id


# Valence Arousal
# get from Matt by par id

# Load in video hamp and scene LPP data

lpp_chans <- c(26, #FCC1H top left
               30, #C1
               34, #CCP1H
               39, #CP1 bottom left
               95, #CZ
               40, #CPZ
               90, #FCC2H top right
               96, #C2
               109, #CCP2H
               104) #CP2 bottom right


occipital_chans_many <- c(48, # P3
                          52, # PPO5h
                          54, # PO7
                          58, # POO9h
                          61, # O9/I1
                          49, # P1
                          53, # PPO1h
                          55, # PO3
                          60, # POO1
                          59, # O1
                          62, # OI1h
                          50, # Pz
                          56, # POZ
                          63, # OZ
                          64, # IZ
                          113,# P2
                          118,# PPO2h
                          121,# PO4
                          124,# POO2
                          125,# O2
                          127,# OI2h
                          114,# P4
                          119,# PPO6h
                          122,# PO8
                          126,# POO10h,
                          128)# O10/I2


occipital_chans_few <- c(58, # POO9h
                         61, # O9/I1
                         59, # O1
                         62, # OI1h
                         63, # OZ
                         64, # IZ
                         125,# O2
                         127,# OI2h
                         126,# POO10h,
                         128)# O10/I2


#lpp 400ms 279pt - 900ms 526pt

lpp_cat_dat <- EMEGShelper::read_ar_files(data_folders = picture_by_cat_ar_directory,
                                      patterns = ".ar$",
                                      baseline_pts = c(14:65),
                                      select_time_points = c(279:526),
                                      average_channels = T,
                                      average_timepoints = T,
                                      extract_channels = lpp_chans) %>% 
  reframe(file_name = list.files(path = picture_by_cat_ar_directory, 
                                 pattern = ".ar$"),
          amp = V1)


#ssvep 1000ms 1537pt - 9000ms 5633pt

ssvep_cat_many_sensors_dat <- EMEGShelper::read_ar_files(data_folders = cat_hamps_directory,
                                                         patterns = ".hamp8$",
                                                         select_time_points = c(1537:5633),
                                                         average_channels = T,
                                                         average_timepoints = T,
                                                         extract_channels = occipital_chans_many) %>% 
  reframe(file_name = list.files(path = cat_hamps_directory, 
                                 pattern = ".hamp8$"),
          amp = V1)

ssvep_cat_few_sensors_dat <- EMEGShelper::read_ar_files(data_folders = cat_hamps_directory,
                                                         patterns = ".hamp8$",
                                                         select_time_points = c(1537:5633),
                                                         average_channels = T,
                                                         average_timepoints = T,
                                                         extract_channels = occipital_chans_few) %>% 
  reframe(file_name = list.files(path = cat_hamps_directory, 
                                 pattern = ".hamp8$"),
          amp = V1)

# Add necessary information
lpp_cat_dat <- lpp_cat_dat %>% 
  reframe(par_id = stringr::str_extract(lpp_cat_dat$file_name, "\\d+") %>% as.numeric(),
          cat_id = stringr::str_extract(lpp_cat_dat$file_name, "(\\d+)(?!.*\\d)") %>% as.numeric(),
          lpp_amp = amp) %>% 
  mutate("category" = case_when(
    cat_id == 1 ~ "pleasant",
    cat_id == 2 ~ "neutral",
    cat_id == 3 ~ "unpleasant"),
    .before = lpp_amp) %>% 
  group_by(par_id) %>% 
  mutate("zscore_lpp_amp" = as.numeric(scale(lpp_amp)))

ssvep_cat_few_sensors_dat %>% 
  reframe(par_id = stringr::str_extract(ssvep_cat_few_sensors_dat$file_name, "\\d+") %>% as.numeric(),
          cat_id = stringr::str_extract(ssvep_cat_few_sensors_dat$file_name, pattern = "at[1-3].hamp8$") %>% 
            stringr::str_extract("\\d+") %>% as.numeric(),
          ssvep_few_sensors_amp = amp) %>% 
  mutate("category" = case_when(
    cat_id == 1 ~ "pleasant",
    cat_id == 2 ~ "neutral",
    cat_id == 3 ~ "unpleasant"),
    .before = ssvep_few_sensors_amp) %>% 
  group_by(par_id) %>% 
  mutate("zscore_ssvep_few_sensors_amp" = as.numeric(scale(ssvep_few_sensors_amp)))


all_numbers <- str_extract_all(str, "\\d+")[[1]]
second_to_last_number <- as.numeric(all_numbers[length(all_numbers) - 1])

stringr::str_extract(ssvep_cat_few_sensors_dat$file_name, "\\d+") %>% as.numeric()
stringr::str_extract(ssvep_cat_few_sensors_dat$file_name, "(\\d+)(?!.*\\d)") %>% as.numeric()




lpp_cat_dat <- data.frame()