library(tidyverse)
library(patchwork)
# library(cmdstanr)
library(plotrix)
library(ggbeeswarm)
# library(gganimate)
# library(ggthemes)


# Load and tidy data ####
## All this data prep should be shorted and put in its own script

# Andrew's last know location of data in sabat lab
sabat_data_folder <- "/Volumes/startech3TB_bkup/research_data/Pic_Vid"
andrew_data_folder <- "/home/andrewf/Research_data/EEG/Pic_Vid"
andrew_work_data_folder <- "/Users/andrewfarkas/nfs-share/Research_data/EEG/Pic_Vid"

if (dir.exists(sabat_data_folder)) {
  parent_directory <- sabat_data_folder
  batch_file_directory <- paste0(sabat_data_folder, 
                                 "/batch_files")
  picture_by_cat_ar_directory <- paste0(sabat_data_folder, 
                                        "/avg_files/pics/by_category")
  picture_by_cat_wave_directory <- paste0(sabat_data_folder, 
                                          "/stats_folder/picture_by_cat_49pars/CAVG")
  picture_by_scene_ar_directory <- paste0(sabat_data_folder, 
                                          "/avg_files/pics/by_scene")
  video_by_cat_directory <- paste0(sabat_data_folder, 
                                   "/hamp_files/videos/category")
  video_by_cat_wave_directory <- paste0(sabat_data_folder, 
                                        "/stats_folder/video_by_cat_49pars_not_same_as_picture/CAVG")
  misc_directory <- paste0(sabat_data_folder, 
                           "/misc")
} else if(dir.exists(andrew_data_folder)) {
  parent_directory <- andrew_data_folder
  batch_file_directory <- paste0(andrew_data_folder, 
                                 "/batch_files")
  picture_by_cat_ar_directory <- paste0(andrew_data_folder, 
                                        "/average_files/pics/by_category")
  picture_by_cat_wave_directory <- paste0(andrew_data_folder, 
                                          "/stats_folder/picture_by_cat_49pars/CAVG")
  picture_by_scene_ar_directory <- paste0(andrew_data_folder, 
                                          "/average_files/pics/by_scene")
  video_by_cat_directory <- paste0(andrew_data_folder, 
                                   "/hamp_files/videos/by_category")
  video_by_cat_wave_directory <- paste0(andrew_data_folder, 
                                        "/stats_folder/video_by_cat_49pars_not_same_as_picture/CAVG")
  misc_directory <- paste0(andrew_data_folder, 
                           "/misc")
  pic_vid_repository <- "/home/andrewf/Repositories/Pic_Vid"
} else if(dir.exists(andrew_work_data_folder)) {
  parent_directory <- andrew_work_data_folder
  batch_file_directory <- paste0(andrew_work_data_folder, 
                                 "/batch_files")
  picture_by_cat_ar_directory <- paste0(andrew_work_data_folder, 
                                        "/average_files/pics/by_category")
  picture_by_cat_wave_directory <- paste0(andrew_work_data_folder, 
                                          "/stats_folder/picture_by_cat_49pars/CAVG")
  picture_by_scene_ar_directory <- paste0(andrew_work_data_folder, 
                                          "/average_files/pics/by_scene")
  video_by_cat_directory <- paste0(andrew_work_data_folder, 
                                   "/hamp_files/videos/by_category")
  video_by_cat_wave_directory <- paste0(andrew_work_data_folder, 
                                        "/stats_folder/video_by_cat_49pars_not_same_as_picture/CAVG")
  misc_directory <- paste0(andrew_work_data_folder, 
                           "/misc")
  pic_vid_repository <- "/Users/andrewfarkas/nfs-share/Repositories/Pic_Vid"
} else{
  stop("Something is wrong, are you working on the right computer with access to the data")
}

# For time saving
# load(paste0(misc_directory,"/temp_time_saving_data.RData")

pic_vid_trial_data <- data.frame("par_id" = 1:50)

# Bad participants
no_pictures <- c(9)
no_videos <- c(38)
bad_participants <- c(9,38)

# Video or picture order based on participant number

saw_vids_first <- seq(1, 50, by = 2) 

saw_pics_first <- seq(2, 50, by = 2)

## no picture participants were missing more than 50% for any valence
# more_than_50_percent_of_video_trials_missing <- c(4, 22, 24, 25, 39, 45, 49, 50) # this is total trials, not per valence

pic_vid_trial_data <- pic_vid_trial_data %>% 
  filter(!par_id %in% c(no_pictures, no_videos))

# Number of good trials ####
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

pic_vid_trial_data <- merge(x = pic_vid_trial_data, y = number_of_good_trials,
                            by.x = "par_id", by.y = "par_id", all.x = T)

pic_vid_trial_data %>% 
  summarise(mean_pic = mean(number_of_good_trials_pictures),
            mean_vid = mean(number_of_good_trials_video))

# write.csv(pic_vid_trial_data,
#           file = paste0(parent_directory, "/misc/pic_vid_good_trials_out_of_90.csv"),
#           quote = F,row.names = F)


# Demographics ####
demographic_information <- read.csv(paste0(parent_directory, "/misc/Participant_data.csv")) %>% 
  mutate(par_id = 1:50, .before = 1) %>% 
  select(-Par_id) %>% 
  filter(!par_id %in% bad_participants)

demographic_information %>% pull(age) %>% summary()
demographic_information %>% pull(age) %>% sd(na.rm = T)

demographic_information %>% pull(sex) %>% table()

demographic_information %>% pull(race.ethnicity) %>% table()

# Ratings data ####
ratings_data <- read.csv(paste0(parent_directory, "/misc/ratings_by_par_by_scene.csv")) %>% 
  mutate(par_id = stringr::str_extract(Par_id, "\\d+") %>% as.numeric(),
         .before = 1) %>% 
  select(-Par_id) %>% 
  filter(!par_id %in% bad_participants) %>% 
  mutate(Stim_cat = factor(Stim_cat,
                           levels = c("Pleasant",
                                      "Neutral",
                                      "Unpleasant")))


# Load in ssVEP LPP ####

## Sensors used ####
# 9 channels more parietal 
lpp_chans <- c(34, #CCP1H
               39, #CP1
               95, #CZ
               44, #CPP1H
               40, #CPZ
               109, # CPP2H
               104, #CP2
               100, #CCP2H
               50) #Pz



# nine channels
occipital_chans <- c(56, # POz
                     61, # O9/I1
                     59, # O1
                     62, # OI1h
                     63, # OZ
                     64, # IZ
                     125,# O2
                     127,# OI2h
                     128)# O10/I2

## Load in category ERPs ####
# lpp 400ms 279pt - 900ms 526pt
start_time_ms <- -125
number_of_time_points <- 1089
sample_rate_hz <- 512

lpp_time_key <- data.frame(time_pt = 1:number_of_time_points) %>% 
  mutate(time_ms = rep(start_time_ms,number_of_time_points) + 
           ((((1:number_of_time_points)-1) * (1000/sample_rate_hz))),
         V_time_pt = paste0("V", time_pt))

lpp_cat_dat <- EMEGShelper::read_ar_files(data_folders = picture_by_cat_ar_directory,
                                          patterns = ".ar$",
                                          baseline_pts = c(14:65),
                                          select_time_points = c(279:526),
                                          average_channels = T,
                                          average_timepoints = T,
                                          include_file_name = T,
                                          extract_channels = lpp_chans) %>% 
  rename(amp = V1)

lpp_cat_wave <- EMEGShelper::read_ar_files(data_folders = picture_by_cat_wave_directory,
                                           patterns = ".ar$",
                                           baseline_pts = c(14:65),
                                           average_channels = T,
                                           include_file_name = T,
                                           extract_channels = lpp_chans)
lpp_cat_wave <- lpp_cat_wave %>% 
  mutate(category = case_when(
    file_name == "CAVG.cat3.pleasant.at.ar" ~ "pleasant",
    file_name == "CAVG.cat3.neutral.at.ar" ~ "neutral",
    file_name == "CAVG.cat3.unpleasant.at.ar" ~ "unpleasant"), .before = V1) %>%
  mutate(category = factor(category,
                           levels = c("pleasant", "neutral", "unpleasant"))) %>% 
  select(-file_name) %>% 
  pivot_longer(cols = starts_with("V"), 
               values_to = "amp",
               names_to = "time_point") %>% 
  merge(y = lpp_time_key, by.x = "time_point", by.y = "V_time_pt", all.x = T) %>% 
  select(time_ms, category, amp) %>% 
  arrange(category, time_ms)




# ssVEP 1000ms 1537pt - 9000ms 5633pt
start_time_ms <- -2000
number_of_time_points <- 6146
sample_rate_hz <- 512

ssvep_time_key <- data.frame(time_pt = 1:number_of_time_points) %>% 
  mutate(time_ms = rep(start_time_ms,number_of_time_points) + 
           ((((1:number_of_time_points)-1) * (1000/sample_rate_hz))),
         V_time_pt = paste0("V", time_pt))

ssvep_cat_dat <- EMEGShelper::read_ar_files(data_folders = video_by_cat_directory,
                                            patterns = ".hamp8$",
                                            select_time_points = c(1537:5633),
                                            average_channels = T,
                                            average_timepoints = T,
                                            include_file_name = T,
                                            extract_channels = occipital_chans) %>% 
  rename(amp = V1)

ssvep_cat_wave <- EMEGShelper::read_ar_files(data_folders = video_by_cat_wave_directory,
                                             patterns = ".ar$",
                                             average_channels = T,
                                             include_file_name = T,
                                             extract_channels = occipital_chans)

ssvep_cat_wave <- ssvep_cat_wave %>% 
  mutate(category = case_when(
    file_name == "CAVG.cat3.pleasant.at.ar" ~ "pleasant",
    file_name == "CAVG.cat3.neutral.at.ar" ~ "neutral",
    file_name == "CAVG.cat3.unpleasant.at.ar" ~ "unpleasant"), .before = V1) %>%
  mutate(category = factor(category,
                           levels = c("pleasant", "neutral", "unpleasant"))) %>% 
  select(-file_name) %>% 
  pivot_longer(cols = starts_with("V"), 
               values_to = "amp",
               names_to = "time_point") %>% 
  merge(y = ssvep_time_key, by.x = "time_point", by.y = "V_time_pt", all.x = T) %>% 
  select(time_ms, category, amp) %>% 
  arrange(category, time_ms)

# Baselined ssVEP 1000ms 1537pt - 9000ms 5633pt
## Baseline -1000ms 513pt - 0ms 1025pt
ssvep_cat_dat_base <- EMEGShelper::read_ar_files(data_folders = video_by_cat_directory,
                                                 patterns = ".hamp8$",
                                                 select_time_points = c(1537:5633),
                                                 baseline_pts = c(513:1025),
                                                 average_channels = T,
                                                 average_timepoints = T,
                                                 include_file_name = T,
                                                 extract_channels = occipital_chans) %>% 
  rename(amp = V1)

ssvep_cat_wave_base <- EMEGShelper::read_ar_files(data_folders = video_by_cat_wave_directory,
                                             patterns = ".ar$",
                                             baseline_pts = c(513:1025),
                                             average_channels = T,
                                             include_file_name = T,
                                             extract_channels = occipital_chans)

ssvep_cat_wave_base <- ssvep_cat_wave_base %>% 
  mutate(category = case_when(
    file_name == "CAVG.cat3.pleasant.at.ar" ~ "pleasant",
    file_name == "CAVG.cat3.neutral.at.ar" ~ "neutral",
    file_name == "CAVG.cat3.unpleasant.at.ar" ~ "unpleasant"), .before = V1) %>%
  mutate(category = factor(category,
                           levels = c("pleasant", "neutral", "unpleasant"))) %>% 
  select(-file_name) %>% 
  pivot_longer(cols = starts_with("V"), 
               values_to = "amp",
               names_to = "time_point") %>% 
  merge(y = ssvep_time_key, by.x = "time_point", by.y = "V_time_pt", all.x = T) %>% 
  select(time_ms, category, amp) %>% 
  arrange(category, time_ms)

## Load by video/scene ERPs####
# ssVEP by video

load(paste0(parent_directory,"/misc/by_video.RData"))

by_video_ssvep_amp <- by_scene_info %>% 
  filter(channel_names %in% occipital_chans) %>% 
  select(!any_of(c("category", "scene_id_con_num", "file_name", "channel_names"))) %>% 
  group_by(par_id, scene) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  mutate(., ssvep_amp = rowMeans(as.matrix(select(., paste0("V", c(1537:5633))))),
         .after = "scene") %>% 
  group_by(par_id) %>% 
  mutate(zscore_ssvep_amp = as.numeric(scale(ssvep_amp), .after = "ssvep_amp")) %>% 
  select(par_id, scene, ssvep_amp, zscore_ssvep_amp) %>% 
  ungroup() %>% 
  mutate(par_id = stringr::str_extract(par_id, "\\d+") %>% as.numeric()) %>% 
  filter(!par_id %in% bad_participants)

by_video_ssvep_baseline_values <- by_scene_info %>% 
  filter(channel_names %in% occipital_chans) %>% 
  mutate(., baseline_amp = rowMeans(as.matrix(select(., 
                                                     paste0("V", 
                                                            c(513:1025)))))) %>% 
  pull(baseline_amp)

by_video_ssvep_amp_base <- by_scene_info %>% 
  filter(channel_names %in% occipital_chans) %>%
  mutate(across(paste0("V",1:6145), ~ . - by_video_ssvep_baseline_values)) %>% 
  select(!any_of(c("category", "scene_id_con_num", "file_name", "channel_names"))) %>% 
  group_by(par_id, scene) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  mutate(., ssvep_amp = rowMeans(as.matrix(select(., paste0("V", c(1537:5633))))),
         .after = "scene") %>% 
  group_by(par_id) %>% 
  mutate(zscore_ssvep_amp = as.numeric(scale(ssvep_amp), .after = "ssvep_amp")) %>% 
  select(par_id, scene, ssvep_amp, zscore_ssvep_amp) %>% 
  ungroup() %>% 
  mutate(par_id = stringr::str_extract(par_id, "\\d+") %>% as.numeric()) %>% 
  filter(!par_id %in% bad_participants)

# lpp by scene
lpp_scene_dat <- EMEGShelper::read_ar_files(data_folders = picture_by_scene_ar_directory,
                                            patterns = ".ar$",
                                            baseline_pts = c(14:65),
                                            select_time_points = c(279:526),
                                            average_channels = T,
                                            average_timepoints = T,
                                            include_file_name = T,
                                            extract_channels = lpp_chans) %>% 
  rename(amp = V1)


lpp_scene_dat <- lpp_scene_dat %>% 
  reframe(par_id = stringr::str_extract(file_name, "\\d+") %>% as.numeric(),
          scene_id = stringr::str_extract(file_name, "(\\d+)(?!.*\\d)") %>% as.numeric(),
          lpp_amp = amp) %>% 
  group_by(par_id) %>% 
  mutate(zscore_lpp_amp = as.numeric(scale(lpp_amp)))

pic_id_key <- read.csv(paste0(parent_directory, "/misc/Picture_id_number.csv"))

lpp_scene_dat <- merge(x = lpp_scene_dat, 
                       y = pic_id_key, 
                       by.x = "scene_id", 
                       by.y = "con_id", 
                       all.x = T)


# Consolidate and merge data####
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
  mutate("zscore_lpp_amp" = as.numeric(scale(lpp_amp))) %>% 
  ungroup() %>% 
  filter(!par_id %in% bad_participants)


ssvep_cat_dat <- ssvep_cat_dat %>% 
  reframe(par_id = stringr::str_extract(ssvep_cat_dat$file_name, "\\d+") %>% as.numeric(),
          cat_id = stringr::str_extract(ssvep_cat_dat$file_name, pattern = "at[1-3].hamp8$") %>% 
            stringr::str_extract("\\d+") %>% as.numeric(),
          ssvep_amp = -amp) %>% 
  mutate("category" = case_when(
    cat_id == 1 ~ "pleasant",
    cat_id == 2 ~ "neutral",
    cat_id == 3 ~ "unpleasant"),
    .before = ssvep_amp) %>% 
  group_by(par_id) %>% 
  mutate("zscore_ssvep_amp" = as.numeric(scale(ssvep_amp))) %>% 
  ungroup() %>% 
  filter(!par_id %in% bad_participants)


ssvep_lpp_dat_by_participant <- lpp_cat_dat %>% 
  full_join(ssvep_cat_dat,  by = c("par_id","cat_id", "category")) %>% 
  mutate(cat_id = factor(cat_id,
                         levels = c(1,2,3)),
         category = factor(category,
                           levels = c("pleasant", "neutral", "unpleasant")))

gm_by_video_ssvep_amp <- by_video_ssvep_amp %>% 
  group_by(scene) %>% 
  select(-par_id) %>% 
  summarise_all(mean) %>% 
  mutate(Stim_type = factor("Video",
                            levels = c("Pics",
                                       "Video")),
         .before = 1)

gm_lpp_scene_dat <- lpp_scene_dat %>% 
  reframe(scene = picture,
          lpp_amp = lpp_amp,
          zscore_lpp_amp = zscore_lpp_amp) %>% 
  group_by(scene) %>% 
  summarise_all(mean) %>% 
  mutate(Stim_type = factor("Pics",
                            levels = c("Pics",
                                       "Video")),
         .before = 1)


gm_erp_by_scene <- rbind.data.frame(rename(gm_by_video_ssvep_amp,
                                           "amp" = ssvep_amp,
                                           "zscore_amp" = zscore_ssvep_amp),
                                    rename(gm_lpp_scene_dat,
                                           "amp" = lpp_amp,
                                           "zscore_amp" = zscore_lpp_amp))

by_scene_ratings_with_path <- ratings_data %>% 
  group_by(Stim_name, Stim_type, Stim_cat) %>% 
  summarise(mean_aro = mean(arousal),
            mean_val = mean(valence)) %>% 
  mutate(path = paste0("/home/andrewf/Research_data/EEG/Pic_Vid/Stimuli/matt_diss/Pics/", 
                       Stim_name, ".jpg"))  

ratings_erps_path_by_scene <- merge(gm_erp_by_scene, y = by_scene_ratings_with_path,
                                    by.x = c("Stim_type", "scene"), by.y = c("Stim_type", "Stim_name"))

lpp_scene_dat_ratings <- merge(x = lpp_scene_dat, y = ratings_data[ratings_data$Stim_type == "Pics",], 
                               by.x = c("picture","par_id"), by.y = c("Stim_name", "par_id"))

by_video_ssvep_amp_ratings <- merge(x = by_video_ssvep_amp, y = ratings_data[ratings_data$Stim_type == "Video",], 
                                    by.x = c("scene","par_id"), by.y = c("Stim_name", "par_id"))

integer_key_for_stan <- lpp_scene_dat_ratings$picture %>%
  unique() %>% 
  sort() %>% 
  data.frame(stim_name_alphabetic = .,
             stim_int_alphabetic = as.integer(factor(.)))