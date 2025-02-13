library(tidyverse)
library(cmdstanr)

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
no_pictures <- c(9) # corrupted file
no_videos <- c(38) # pulled out of study, felt ill
missing_over_50perc_from_valence_cat <- c(22,39,49)
bad_participants <- c(no_pictures, no_videos, missing_over_50perc_from_valence_cat) 

# Video or picture order based on participant number

saw_vids_first <- seq(1, 50, by = 2) 

saw_pics_first <- seq(2, 50, by = 2)

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

# Baselined ssVEP 1000ms 1537pt - 9000ms 5633pt
## Baseline -1000ms 513pt - 0ms 1025pt
# ssvep_cat_dat_base <- EMEGShelper::read_ar_files(data_folders = video_by_cat_directory,
#                                                  patterns = ".hamp8$",
#                                                  select_time_points = c(1537:5633),
#                                                  baseline_pts = c(513:1025),
#                                                  average_channels = T,
#                                                  average_timepoints = T,
#                                                  include_file_name = T,
#                                                  extract_channels = occipital_chans) %>% 
#   rename(amp = V1)


##
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

data_for_stan_df <- data.frame(
  rbind.data.frame(
    lpp_scene_dat_ratings %>% 
      merge(x = ., y = integer_key_for_stan, 
            by.x = "picture", by.y = "stim_name_alphabetic") %>% 
      mutate(cate_int = case_when(
        Stim_cat == "Pleasant" ~ 1,
        Stim_cat == "Neutral" ~ 2,
        Stim_cat == "Unpleasant" ~ 3)) %>% 
      select(par_id, stim_int_alphabetic, 
             cate_int, lpp_amp, picture, valence, arousal) %>% 
      reframe(par = par_id,
              type = as.integer(1),
              cate = cate_int,
              stim = stim_int_alphabetic,
              amp = lpp_amp,
              stim_name = picture,
              valence = valence,
              arousal = arousal),
    by_video_ssvep_amp_ratings %>% 
      merge(x = ., y = integer_key_for_stan, 
            by.x = "scene", by.y = "stim_name_alphabetic") %>% 
      mutate(cate_int = case_when(
        Stim_cat == "Pleasant" ~ 1,
        Stim_cat == "Neutral" ~ 2,
        Stim_cat == "Unpleasant" ~ 3)) %>% 
      select(par_id, stim_int_alphabetic, cate_int, 
             ssvep_amp, scene, valence, arousal) %>% 
      reframe(par = par_id,
              type = as.integer(2),
              cate = cate_int,
              stim = stim_int_alphabetic,
              amp = ssvep_amp,
              stim_name = scene,
              valence = valence,
              arousal = arousal))) %>% 
  mutate(.before = 2,
         vids_first_is_one = case_when(par %in% saw_vids_first ~ 1,
                                       par %in% saw_pics_first ~ 2))


data_for_stan_df$par <- data_for_stan_df$par %>% 
  as.factor() %>% 
  as.integer()

data_list_for_stan_lpp <- list()
data_list_for_stan_ssvep <- list()

data_list_for_stan_lpp$amp_mean_prior <-
  data_for_stan_df %>%
  filter(type == 1) %>% 
  summarise("mean_amp" = mean(amp)) %>% 
  pull(mean_amp)
data_list_for_stan_ssvep$amp_mean_prior <-
  data_for_stan_df %>%
  filter(type == 2) %>% 
  summarise("mean_amp" = mean(amp)) %>% 
  pull(mean_amp)

data_list_for_stan_lpp$amp_sd_prior <-
  data_for_stan_df %>% 
  filter(type == 1) %>% 
  summarise(sd_amp = sd(amp)*2) %>% 
  pull(sd_amp)
data_list_for_stan_ssvep$amp_sd_prior <-
  data_for_stan_df %>% 
  filter(type == 2) %>% 
  summarise(sd_amp = sd(amp)*2) %>% 
  pull(sd_amp)

data_list_for_stan_lpp$nobs <- nrow(data_for_stan_df[data_for_stan_df$type == 1,])
data_list_for_stan_lpp$npar <- data_for_stan_df[data_for_stan_df$type == 1,]$par %>% unique() %>% length()
data_list_for_stan_lpp$ncate <- as.integer(3)
# data_list_for_stan_lpp$ntype <- as.integer(2)
data_list_for_stan_lpp$nstim <- as.integer(90)
data_list_for_stan_lpp$par = data_for_stan_df[data_for_stan_df$type == 1,]$par %>% as.factor() %>% as.integer()
# data_list_for_stan_lpp$type = data_for_stan_df[data_for_stan_df$type == 1,]$type
data_list_for_stan_lpp$cate = data_for_stan_df[data_for_stan_df$type == 1,]$cate
data_list_for_stan_lpp$stim = data_for_stan_df[data_for_stan_df$type == 1,]$stim %>% as.factor() %>% as.integer()
data_list_for_stan_lpp$amp = data_for_stan_df[data_for_stan_df$type == 1,]$amp
data_list_for_stan_lpp$arousal = data_for_stan_df[data_for_stan_df$type == 1,]$arousal

data_list_for_stan_ssvep$nobs <- nrow(data_for_stan_df[data_for_stan_df$type == 2,])
data_list_for_stan_ssvep$npar <- data_for_stan_df[data_for_stan_df$type == 2,]$par %>% unique() %>% length()
data_list_for_stan_ssvep$ncate <- as.integer(3)
# data_list_for_stan_ssvep$ntype <- as.integer(2)
data_list_for_stan_ssvep$nstim <- as.integer(90)
data_list_for_stan_ssvep$par = data_for_stan_df[data_for_stan_df$type == 2,]$par %>% as.factor() %>% as.integer()
# data_list_for_stan_ssvep$type = data_for_stan_df[data_for_stan_df$type == 2,]$type
data_list_for_stan_ssvep$cate = data_for_stan_df[data_for_stan_df$type == 2,]$cate
data_list_for_stan_ssvep$stim = data_for_stan_df[data_for_stan_df$type == 2,]$stim %>% as.factor() %>% as.integer()
data_list_for_stan_ssvep$amp = data_for_stan_df[data_for_stan_df$type == 2,]$amp
data_list_for_stan_ssvep$arousal = data_for_stan_df[data_for_stan_df$type == 2,]$arousal



# Data used for paper_stats_figures_11 ####
save(data_for_stan_df,
     data_list_for_stan_lpp,
     data_list_for_stan_ssvep,
     file = paste0(parent_directory,
                   "/paper_data_models/data/pic_vid_paper.RData"))

# Fit models ####
# fit ssvep and lpp separately, but use same model for each

## Model sampling options ####
posterior_samples_per_chain <- 10000
number_of_chains <- 8 # 8 performance cores on mac
number_of_parallel_chains <- ifelse(parallel::detectCores() > 8, 8, 4)


## 1 par intercept ####
model_name <- "model001_par_intercept"

model001_path <- paste0(pic_vid_repository,
                        "/stan_models/", 
                        model_name,
                        ".stan")

model001 <- cmdstan_model(model001_path, force_recompile = T)

# Clear previous chains
list.files(path = paste0(parent_directory,"/paper_data_models/models/chains/"),
           pattern = model_name,
           full.names = T) %>% 
  file.remove()

model001_lpp_fit <- model001$sample(
  data = data_list_for_stan_lpp,
  refresh = 200,
  seed = 2,
  iter_warmup = 10000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model001_ssvep_fit <- model001$sample(
  data = data_list_for_stan_ssvep,
  refresh = 200,
  seed = 2, 
  iter_warmup = 10000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model001_lpp_fit_meta_data <- model001_lpp_fit$metadata()

model001_ssvep_fit_meta_data <- model001_ssvep_fit$metadata()


model001_lpp_fit_relevant_parameters <- model001_lpp_fit_meta_data$model_params[
  !str_detect(model001_lpp_fit_meta_data$model_params, "log_lik")]

model001_ssvep_fit_relevant_parameters <- model001_ssvep_fit_meta_data$model_params[
  !str_detect(model001_ssvep_fit_meta_data$model_params, "log_lik")]


model001_lpp_fit_summary <- model001_lpp_fit$summary(
  variables = model001_lpp_fit_relevant_parameters)

model001_ssvep_fit_summary <- model001_ssvep_fit$summary(
  variables = model001_ssvep_fit_relevant_parameters)


## 2 par and cate intercepts ####

model_name <- "model002_par_cate_intercepts"

model002_path <- paste0(pic_vid_repository,
                        "/stan_models/", 
                        model_name,
                        ".stan")

model002 <- cmdstan_model(model002_path, force_recompile = T)

# Clear previous chains
list.files(path = paste0(parent_directory,"/paper_data_models/models/chains/"),
           pattern = model_name,
           full.names = T) %>% 
  file.remove()

model002_lpp_fit <- model002$sample(
  data = data_list_for_stan_lpp,
  refresh = 200,
  seed = 2, 
  iter_warmup = 60000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)

model002_ssvep_fit <- model002$sample(
  data = data_list_for_stan_ssvep,
  refresh = 200,
  seed = 2, 
  iter_warmup = 60000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model002_lpp_fit_meta_data <- model002_lpp_fit$metadata()

model002_ssvep_fit_meta_data <- model002_ssvep_fit$metadata()


model002_lpp_fit_relevant_parameters <- model002_lpp_fit_meta_data$model_params[
  !str_detect(model002_lpp_fit_meta_data$model_params, "log_lik")]

model002_ssvep_fit_relevant_parameters <- model002_ssvep_fit_meta_data$model_params[
  !str_detect(model002_ssvep_fit_meta_data$model_params, "log_lik")]


model002_lpp_fit_summary <- model002_lpp_fit$summary(
  variables = model002_lpp_fit_relevant_parameters)

model002_ssvep_fit_summary <- model002_ssvep_fit$summary(
  variables = model002_ssvep_fit_relevant_parameters)


## 3 par and stim intercepts ####
model_name <- "model003_par_stim_intercepts"

model003_path <- paste0(pic_vid_repository,
                        "/stan_models/", 
                        model_name,
                        ".stan")

model003 <- cmdstan_model(model003_path, force_recompile = T)

# Clear previous chains
list.files(path = paste0(parent_directory,"/paper_data_models/models/chains/"),
           pattern = model_name,
           full.names = T) %>% 
  file.remove()

model003_lpp_fit <- model003$sample(
  data = data_list_for_stan_lpp,
  refresh = 200,
  seed = 2, 
  iter_warmup = 50000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)



model003_ssvep_fit <- model003$sample(
  data = data_list_for_stan_ssvep,
  refresh = 200,
  seed = 2, 
  iter_warmup = 50000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)



model003_lpp_fit_meta_data <- model003_lpp_fit$metadata()

model003_ssvep_fit_meta_data <- model003_ssvep_fit$metadata()


model003_lpp_fit_relevant_parameters <- model003_lpp_fit_meta_data$model_params[
  !str_detect(model003_lpp_fit_meta_data$model_params, "log_lik|mu_pred")]

model003_ssvep_fit_relevant_parameters <- model003_ssvep_fit_meta_data$model_params[
  !str_detect(model003_ssvep_fit_meta_data$model_params, "log_lik|mu_pred")]


model003_lpp_fit_summary <- model003_lpp_fit$summary(
  variables = model003_lpp_fit_relevant_parameters)

model003_ssvep_fit_summary <- model003_ssvep_fit$summary(
  variables = model003_ssvep_fit_relevant_parameters)


## 4 par intercept, arousal slope ####

model_name <- "model004_par_intercept_arousal_slope"

model004_path <- paste0(pic_vid_repository,
                        "/stan_models/", 
                        model_name,
                        ".stan")

model004 <- cmdstan_model(model004_path, force_recompile = T)

# Clear previous chains
list.files(path = paste0(parent_directory,"/paper_data_models/models/chains/"),
           pattern = model_name,
           full.names = T) %>% 
  file.remove()

model004_lpp_fit <- model004$sample(
  data = data_list_for_stan_lpp,
  refresh = 200,
  seed = 2, 
  iter_warmup = 50000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model004_ssvep_fit <- model004$sample(
  data = data_list_for_stan_ssvep,
  refresh = 200,
  seed = 2, 
  iter_warmup = 50000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model004_lpp_fit_meta_data <- model004_lpp_fit$metadata()

model004_ssvep_fit_meta_data <- model004_ssvep_fit$metadata()


model004_lpp_fit_relevant_parameters <- model004_lpp_fit_meta_data$model_params[
  !str_detect(model004_lpp_fit_meta_data$model_params, "log_lik|mu_pred")]

model004_ssvep_fit_relevant_parameters <- model004_ssvep_fit_meta_data$model_params[
  !str_detect(model004_ssvep_fit_meta_data$model_params, "log_lik|mu_pred")]


model004_lpp_fit_summary <- model004_lpp_fit$summary(
  variables = model004_lpp_fit_relevant_parameters)

model004_ssvep_fit_summary <- model004_ssvep_fit$summary(
  variables = model004_ssvep_fit_relevant_parameters)


## 5 par and cate intercepts, arousal slope ####

model_name <- paste0(pic_vid_repository,
                        "/stan_models/model005_par_cate_intercepts_arousal_slope.stan")

model005 <- cmdstan_model(model_name, force_recompile = T)

# Clear previous chains
list.files(path = paste0(parent_directory,"/paper_data_models/models/chains/"),
           pattern = model_name,
           full.names = T) %>% 
  file.remove()

model005_lpp_fit <- model005$sample(
  data = data_list_for_stan_lpp,
  refresh = 200,
  seed = 2, 
  iter_warmup = 40000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)



model005_ssvep_fit <- model005$sample(
  data = data_list_for_stan_ssvep,
  refresh = 200,
  seed = 2, 
  iter_warmup = 25000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model005_lpp_fit_meta_data <- model005_lpp_fit$metadata()

model005_ssvep_fit_meta_data <- model005_ssvep_fit$metadata()


model005_lpp_fit_relevant_parameters <- model005_lpp_fit_meta_data$model_params[
  !str_detect(model005_lpp_fit_meta_data$model_params, "log_lik")]

model005_ssvep_fit_relevant_parameters <- model005_ssvep_fit_meta_data$model_params[
  !str_detect(model005_ssvep_fit_meta_data$model_params, "log_lik")]


model005_lpp_fit_summary <- model005_lpp_fit$summary(
  variables = model005_lpp_fit_relevant_parameters)

model005_ssvep_fit_summary <- model005_ssvep_fit$summary(
  variables = model005_ssvep_fit_relevant_parameters)


## 6 par and stim intercepts, arousal slope ####

model_name <- paste0(pic_vid_repository,
                        "/stan_models/model006_par_stim_intercepts_arousal_slope.stan")

model006 <- cmdstan_model(model_name, force_recompile = T)

# Clear previous chains
list.files(path = paste0(parent_directory,"/paper_data_models/models/chains/"),
           pattern = model_name,
           full.names = T) %>% 
  file.remove()

model006_lpp_fit <- model006$sample(
  data = data_list_for_stan_lpp,
  refresh = 200,
  seed = 2, 
  iter_warmup = 25000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model006_ssvep_fit <- model006$sample(
  data = data_list_for_stan_ssvep,
  refresh = 200,
  seed = 2, 
  iter_warmup = 25000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model006_lpp_fit_meta_data <- model006_lpp_fit$metadata()

model006_ssvep_fit_meta_data <- model006_ssvep_fit$metadata()


model006_lpp_fit_relevant_parameters <- model006_lpp_fit_meta_data$model_params[
  !str_detect(model006_lpp_fit_meta_data$model_params, "log_lik")]

model006_ssvep_fit_relevant_parameters <- model006_ssvep_fit_meta_data$model_params[
  !str_detect(model006_ssvep_fit_meta_data$model_params, "log_lik")]


model006_lpp_fit_summary <- model006_lpp_fit$summary(
  variables = model006_lpp_fit_relevant_parameters)

model006_ssvep_fit_summary <- model006_ssvep_fit$summary(
  variables = model006_ssvep_fit_relevant_parameters)

## 7 multi-level par intercept arousal slope ####

model_name <- "model007_par_intercept_arousal_slope_ML"

model007_path <- paste0(pic_vid_repository,
                        "/stan_models/", 
                        model_name,
                        ".stan")

# Clear previous chains
list.files(path = paste0(parent_directory,"/paper_data_models/models/chains/"),
           pattern = model_name,
           full.names = T) %>% 
  file.remove()

model007 <- cmdstan_model(model007_path, force_recompile = T)

model007_lpp_fit <- model007$sample(
  data = data_list_for_stan_lpp,
  refresh = 200,
  seed = 3, 
  iter_warmup = 50000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model007_ssvep_fit <- model007$sample(
  data = data_list_for_stan_ssvep,
  refresh = 200,
  seed = 2, 
  iter_warmup = 50000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model007_lpp_fit_meta_data <- model007_lpp_fit$metadata()

model007_ssvep_fit_meta_data <- model007_ssvep_fit$metadata()


model007_lpp_fit_relevant_parameters <- model007_lpp_fit_meta_data$model_params[
  !str_detect(model007_lpp_fit_meta_data$model_params, "log_lik|mu_pred")]

model007_ssvep_fit_relevant_parameters <- model007_ssvep_fit_meta_data$model_params[
  !str_detect(model007_ssvep_fit_meta_data$model_params, "log_lik|mu_pred")]


model007_lpp_fit_summary <- model007_lpp_fit$summary(
  variables = model007_lpp_fit_relevant_parameters)

model007_ssvep_fit_summary <- model007_ssvep_fit$summary(
  variables = model007_ssvep_fit_relevant_parameters)

## 8 par cat intercepts and ml arousal slope ####
model_name <- paste0(pic_vid_repository,
                        "/stan_models/model008_par_cate_intercepts_arousal_slope_ML.stan")

model008 <- cmdstan_model(model_name, force_recompile = T)

# Clear previous chains
list.files(path = paste0(parent_directory,"/paper_data_models/models/chains/"),
           pattern = model_name,
           full.names = T) %>% 
  file.remove()

model008_lpp_fit <- model008$sample(
  data = data_list_for_stan_lpp,
  refresh = 200,
  seed = 2, 
  iter_warmup = 50000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model008_ssvep_fit <- model008$sample(
  data = data_list_for_stan_ssvep,
  refresh = 200,
  seed = 2, 
  iter_warmup = 50000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model008_lpp_fit_meta_data <- model008_lpp_fit$metadata()

model008_ssvep_fit_meta_data <- model008_ssvep_fit$metadata()


model008_lpp_fit_relevant_parameters <- model008_lpp_fit_meta_data$model_params[
  !str_detect(model008_lpp_fit_meta_data$model_params, "log_lik")]

model008_ssvep_fit_relevant_parameters <- model008_ssvep_fit_meta_data$model_params[
  !str_detect(model008_ssvep_fit_meta_data$model_params, "log_lik")]


model008_lpp_fit_summary <- model008_lpp_fit$summary(
  variables = model008_lpp_fit_relevant_parameters)

model008_ssvep_fit_summary <- model008_ssvep_fit$summary(
  variables = model008_ssvep_fit_relevant_parameters)

## 9 par stim intercepts ml arousal slope ####
model_name <- paste0(pic_vid_repository,
                        "/stan_models/model009_par_stim_intercepts_arousal_slope_ML.stan")

model009 <- cmdstan_model(model_name, force_recompile = T)

# Clear previous chains
list.files(path = paste0(parent_directory,"/paper_data_models/models/chains/"),
           pattern = model_name,
           full.names = T) %>% 
  file.remove()

model009_lpp_fit <- model009$sample(
  data = data_list_for_stan_lpp,
  refresh = 200,
  seed = 2, 
  iter_warmup = 50000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model009_ssvep_fit <- model009$sample(
  data = data_list_for_stan_ssvep,
  refresh = 200,
  seed = 2, 
  iter_warmup = 50000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model009_lpp_fit_meta_data <- model009_lpp_fit$metadata()

model009_ssvep_fit_meta_data <- model009_ssvep_fit$metadata()


model009_lpp_fit_relevant_parameters <- model009_lpp_fit_meta_data$model_params[
  !str_detect(model009_lpp_fit_meta_data$model_params, "log_lik")]

model009_ssvep_fit_relevant_parameters <- model009_ssvep_fit_meta_data$model_params[
  !str_detect(model009_ssvep_fit_meta_data$model_params, "log_lik")]


model009_lpp_fit_summary <- model009_lpp_fit$summary(
  variables = model009_lpp_fit_relevant_parameters)

model009_ssvep_fit_summary <- model009_ssvep_fit$summary(
  variables = model009_ssvep_fit_relevant_parameters)



## 10 same as 7 rephrased####
model_name <- "model010_007_but_different_standard_beta"

model010_path <- paste0(pic_vid_repository,
                        "/stan_models/", 
                        model_name,
                        ".stan")

# Clear previous chains
list.files(path = paste0(parent_directory,"/paper_data_models/models/chains/"),
           pattern = model_name,
           full.names = T) %>% 
  file.remove()

model010 <- cmdstan_model(model010_path, force_recompile = T)

model010_lpp_fit <- model010$sample(
  data = data_list_for_stan_lpp,
  refresh = 200,
  seed = 3, 
  iter_warmup = 50000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model010_ssvep_fit <- model010$sample(
  data = data_list_for_stan_ssvep,
  refresh = 200,
  seed = 2, 
  iter_warmup = 50000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model010_lpp_fit_meta_data <- model010_lpp_fit$metadata()

model010_ssvep_fit_meta_data <- model010_ssvep_fit$metadata()


model010_lpp_fit_relevant_parameters <- model010_lpp_fit_meta_data$model_params[
  !str_detect(model010_lpp_fit_meta_data$model_params, "log_lik|mu_pred")]

model010_ssvep_fit_relevant_parameters <- model010_ssvep_fit_meta_data$model_params[
  !str_detect(model010_ssvep_fit_meta_data$model_params, "log_lik|mu_pred")]


model010_lpp_fit_summary <- model010_lpp_fit$summary(
  variables = model010_lpp_fit_relevant_parameters)

model010_ssvep_fit_summary <- model010_ssvep_fit$summary(
  variables = model010_ssvep_fit_relevant_parameters)

## 11 Multi-level participant clusters ####
model_name <- "model011_ML_bivariate_normal_amp_arousal"

model011_path <- paste0(pic_vid_repository,
                        "/stan_models/", 
                        model_name,
                        ".stan")

# Clear previous chains
list.files(path = paste0(parent_directory,"/paper_data_models/models/chains/"),
           pattern = model_name,
           full.names = T) %>% 
  file.remove()

model011 <- cmdstan_model(model011_path, force_recompile = T)

model011_lpp_fit <- model011$sample(
  data = data_list_for_stan_lpp,
  init=1,
  refresh = 200, 
  seed = 3, 
  iter_warmup = 5000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model011_ssvep_fit <- model011$sample(
  data = data_list_for_stan_ssvep,
  init=1,
  refresh = 200, 
  seed = 3, 
  iter_warmup = 5000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model011_lpp_fit_meta_data <- model011_lpp_fit$metadata()

model011_ssvep_fit_meta_data <- model011_ssvep_fit$metadata()


model011_lpp_fit_relevant_parameters <- model011_lpp_fit_meta_data$model_params[
  !str_detect(model011_lpp_fit_meta_data$model_params, "log_lik|mu_pred")]

model011_ssvep_fit_relevant_parameters <- model011_ssvep_fit_meta_data$model_params[
  !str_detect(model011_ssvep_fit_meta_data$model_params, "log_lik|mu_pred")]


model011_lpp_fit_summary <- model011_lpp_fit$summary(
  variables = model011_lpp_fit_relevant_parameters)

model011_ssvep_fit_summary <- model011_ssvep_fit$summary(
  variables = model011_ssvep_fit_relevant_parameters)

## 12 Par and Stim predictors were each participant has their own amp SD ####
model_name <- "model012_par_stim_intercepts_MLsd"

model012_path <- paste0(pic_vid_repository,
                        "/stan_models/", 
                        model_name,
                        ".stan")

# Clear previous chains
list.files(path = paste0(parent_directory,"/paper_data_models/models/chains/"),
           pattern = model_name,
           full.names = T) %>% 
  file.remove()

model012 <- cmdstan_model(model012_path, force_recompile = T)

model012_lpp_fit <- model012$sample(
  data = data_list_for_stan_lpp,
  # init=1,
  refresh = 200, 
  seed = 3, 
  iter_warmup = 5000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model012_ssvep_fit <- model012$sample(
  data = data_list_for_stan_ssvep,
  # init=2,
  refresh = 200, 
  seed = 3, 
  iter_warmup = 5000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model012_lpp_fit_meta_data <- model012_lpp_fit$metadata()

model012_ssvep_fit_meta_data <- model012_ssvep_fit$metadata()


model012_lpp_fit_relevant_parameters <- model012_lpp_fit_meta_data$model_params[
  !str_detect(model012_lpp_fit_meta_data$model_params, "log_lik|mu_pred")]

model012_ssvep_fit_relevant_parameters <- model012_ssvep_fit_meta_data$model_params[
  !str_detect(model012_ssvep_fit_meta_data$model_params, "log_lik|mu_pred")]


model012_lpp_fit_summary <- model012_lpp_fit$summary(
  variables = model012_lpp_fit_relevant_parameters)

model012_ssvep_fit_summary <- model012_ssvep_fit$summary(
  variables = model012_ssvep_fit_relevant_parameters)

## 13 Multi-level intercept slope seperate par amp_sd ####
model_name <- "model013_par_intercept_arousal_slope_ML_sd"

model013_path <- paste0(pic_vid_repository,
                        "/stan_models/", 
                        model_name,
                        ".stan")

# Clear previous chains
list.files(path = paste0(parent_directory,"/paper_data_models/models/chains/"),
           pattern = model_name,
           full.names = T) %>% 
  file.remove()

model013 <- cmdstan_model(model013_path, force_recompile = T)

model013_lpp_fit <- model013$sample(
  data = data_list_for_stan_lpp,
  # init=1,
  refresh = 200, 
  seed = 3, 
  iter_warmup = 5000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model013_ssvep_fit <- model013$sample(
  data = data_list_for_stan_ssvep,
  # init=2,
  refresh = 200, 
  seed = 3, 
  iter_warmup = 5000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model013_lpp_fit_meta_data <- model013_lpp_fit$metadata()

model013_ssvep_fit_meta_data <- model013_ssvep_fit$metadata()


model013_lpp_fit_relevant_parameters <- model013_lpp_fit_meta_data$model_params[
  !str_detect(model013_lpp_fit_meta_data$model_params, "log_lik|mu_pred")]

model013_ssvep_fit_relevant_parameters <- model013_ssvep_fit_meta_data$model_params[
  !str_detect(model013_ssvep_fit_meta_data$model_params, "log_lik|mu_pred")]


model013_lpp_fit_summary <- model013_lpp_fit$summary(
  variables = model013_lpp_fit_relevant_parameters)

model013_ssvep_fit_summary <- model013_ssvep_fit$summary(
  variables = model013_ssvep_fit_relevant_parameters)

## 14 Multi-level participant cluster truncated arousal ####
##This would be nice but haven't implemented yet as it would take complex work around

model_name <- "model014__ML_bivariate_normal_amp_arousal_truncated"

model014_path <- paste0(pic_vid_repository,
                        "/stan_models/", 
                        model_name,
                        ".stan")

# Clear previous chains
list.files(path = paste0(parent_directory,"/paper_data_models/models/chains/"),
           pattern = model_name,
           full.names = T) %>% 
  file.remove()

model013 <- cmdstan_model(model013_path, force_recompile = T)

model013_lpp_fit <- model013$sample(
  data = data_list_for_stan_lpp,
  # init=1,
  refresh = 200, 
  seed = 3, 
  iter_warmup = 5000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model013_ssvep_fit <- model013$sample(
  data = data_list_for_stan_ssvep,
  # init=2,
  refresh = 200, 
  seed = 3, 
  iter_warmup = 5000,
  iter_sampling = posterior_samples_per_chain,
  save_warmup = F,
  show_messages = T,
  output_dir = paste0(parent_directory,"/paper_data_models/models/chains"),
  chains = number_of_chains, 
  parallel_chains = number_of_parallel_chains)


model013_lpp_fit_meta_data <- model013_lpp_fit$metadata()

model013_ssvep_fit_meta_data <- model013_ssvep_fit$metadata()


model013_lpp_fit_relevant_parameters <- model013_lpp_fit_meta_data$model_params[
  !str_detect(model013_lpp_fit_meta_data$model_params, "log_lik|mu_pred")]

model013_ssvep_fit_relevant_parameters <- model013_ssvep_fit_meta_data$model_params[
  !str_detect(model013_ssvep_fit_meta_data$model_params, "log_lik|mu_pred")]


model013_lpp_fit_summary <- model013_lpp_fit$summary(
  variables = model013_lpp_fit_relevant_parameters)

model013_ssvep_fit_summary <- model013_ssvep_fit$summary(
  variables = model013_ssvep_fit_relevant_parameters)

# Save data ####
demographic_information$stan_par_id <- 1:45

pic_vid_stan_df <- 
  merge(y = data_for_stan_df, 
        x = demographic_information, 
        by.y = "par", 
        by.x = "stan_par_id",
        all.y = T) %>% 
  select(-par_id) %>% 
  rename(par = stan_par_id,
         type_pics_are_one = type)

save(pic_vid_stan_df,
     file = paste0(parent_directory,"/pic_vid_stan_df.RData"))

# Save model fits and summaries
save(model011_lpp_fit,
     model011_lpp_fit_summary,
     model011_ssvep_fit,
     model011_ssvep_fit_summary,
     model012_lpp_fit,
     model012_lpp_fit_summary,
     model012_ssvep_fit,
     model012_ssvep_fit_summary,
     file = paste0(parent_directory,"/paper_data_models/models/paper_models.RData"))

save(model001_lpp_fit,
     model001_lpp_fit_summary,
     model001_ssvep_fit,
     model001_ssvep_fit_summary,
     model002_lpp_fit,
     model002_lpp_fit_summary,
     model002_ssvep_fit,
     model002_ssvep_fit_summary,
     model003_lpp_fit,
     model003_lpp_fit_summary,
     model003_ssvep_fit,
     model003_ssvep_fit_summary,
     model004_lpp_fit,
     model004_lpp_fit_summary,
     model004_ssvep_fit,
     model004_ssvep_fit_summary,
     model005_lpp_fit,
     model005_lpp_fit_summary,
     model005_ssvep_fit,
     model005_ssvep_fit_summary,
     model006_lpp_fit,
     model006_lpp_fit_summary,
     model006_ssvep_fit,
     model006_ssvep_fit_summary,
     model007_lpp_fit,
     model007_lpp_fit_summary,
     model007_ssvep_fit,
     model007_ssvep_fit_summary,
     model008_lpp_fit,
     model008_lpp_fit_summary,
     model008_ssvep_fit,
     model008_ssvep_fit_summary,
     model009_lpp_fit,
     model009_lpp_fit_summary,
     model009_ssvep_fit,
     model009_ssvep_fit_summary,
     model010_lpp_fit,
     model010_lpp_fit_summary,
     model010_ssvep_fit,
     model010_ssvep_fit_summary,
     model011_lpp_fit,
     model011_lpp_fit_summary,
     model011_ssvep_fit,
     model011_ssvep_fit_summary,
     model012_lpp_fit,
     model012_lpp_fit_summary,
     model012_ssvep_fit,
     model012_ssvep_fit_summary,
     model013_lpp_fit,
     model013_lpp_fit_summary,
     model013_ssvep_fit,
     model013_ssvep_fit_summary,
     file = paste0(parent_directory,"/paper_data_models/models/all_models.RData"))

# Save model cross validation LOO
model011_lpp_fit_loo <- model011_lpp_fit$loo()
model012_lpp_fit_loo <- model012_lpp_fit$loo()
model011_ssvep_fit_loo <- model011_ssvep_fit$loo()
model012_ssvep_fit_loo <- model012_ssvep_fit$loo()

save(model011_lpp_fit_loo,
     model011_ssvep_fit_loo,
     model012_lpp_fit_loo,
     model012_ssvep_fit_loo,
     file = paste0(parent_directory,"/paper_data_models/models/paper_model_loos.RData"))


