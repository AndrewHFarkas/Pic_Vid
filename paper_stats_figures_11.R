library(tidyverse)
library(patchwork)
library(cmdstanr)
library(plotrix)
library(ggbeeswarm)
library(gganimate)
library(ggthemes)
library(ggridges)
library(grid)
library(jpeg)

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


# Load data and models ####

## Good trials, demographics, ratings ####

pic_vid_trial_data <- data.frame("par_id" = 1:50)

# Bad participants
no_pictures <- c(9)
no_videos <- c(38)
bad_participants <- c(9,38)

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

by_scene_ratings_with_path <- ratings_data %>% 
  group_by(Stim_name, Stim_type, Stim_cat) %>% 
  summarise(mean_aro = mean(arousal),
            mean_val = mean(valence)) %>% 
  mutate(path = paste0("/home/andrewf/Research_data/EEG/Pic_Vid/Stimuli/matt_diss/Pics/", 
                       Stim_name, ".jpg"))  

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


gm_ssvep_lpp <- ssvep_lpp_dat_by_participant %>% 
  group_by(category) %>% 
  summarise(mean_zscore_lpp   = mean(zscore_lpp_amp, na.rm = T),
            se_zscore_lpp     = plotrix::std.error(zscore_lpp_amp),
            mean_zscore_ssvep = mean(zscore_ssvep_amp, na.rm = T),
            se_zscore_ssvep   = plotrix::std.error(zscore_ssvep_amp))

gm_amp_long <- gm_ssvep_lpp %>% 
  pivot_longer(
    cols = starts_with("mean_") | starts_with("se_"),
    names_to = c(".value", "erp_type"),
    names_pattern = "(mean_|se_)(.*)"
  ) %>% 
  rename(mean_amp = mean_,
         se_amp = se_) 



## By category data ####

load(paste0(parent_directory,
            "/paper_data_models/data/pic_vid_paper.RData"))



# Models and draws ####
load(paste0(parent_directory,"/paper_data_models/models/models.RData"))
load(paste0(parent_directory,"/paper_data_models/models/paper_models.RData"))

model003_lpp_fit <- model003_lpp_fit$output_files() %>% 
  basename() %>% 
  paste0(parent_directory,
         "/paper_data_models/models/chains/",
         .) %>% 
  as_cmdstan_fit()
model003_ssvep_fit <- model003_ssvep_fit$output_files() %>% 
  basename() %>% 
  paste0(parent_directory,
         "/paper_data_models/models/chains/",
         .) %>% 
  as_cmdstan_fit()

model007_lpp_fit <- model007_lpp_fit$output_files() %>% 
  basename() %>% 
  paste0(parent_directory,
         "/paper_data_models/models/chains/",
         .) %>% 
  as_cmdstan_fit()
model007_ssvep_fit <- model007_ssvep_fit$output_files() %>% 
  basename() %>% 
  paste0(parent_directory,
         "/paper_data_models/models/chains/",
         .) %>% 
  as_cmdstan_fit()

model009_lpp_fit <- model009_lpp_fit$output_files() %>% 
  basename() %>% 
  paste0(parent_directory,
         "/paper_data_models/models/chains/",
         .) %>% 
  as_cmdstan_fit()
model009_ssvep_fit <- model009_ssvep_fit$output_files() %>% 
  basename() %>% 
  paste0(parent_directory,
         "/paper_data_models/models/chains/",
         .) %>% 
  as_cmdstan_fit()

model011_lpp_fit <- model011_lpp_fit$output_files() %>% 
  basename() %>% 
  paste0(parent_directory,
         "/paper_data_models/models/chains/",
         .) %>% 
  as_cmdstan_fit()
model011_ssvep_fit <- model011_ssvep_fit$output_files() %>% 
  basename() %>% 
  paste0(parent_directory,
         "/paper_data_models/models/chains/",
         .) %>% 
  as_cmdstan_fit()

model003_lpp_fit_draws <- model003_lpp_fit$draws(format = "df")
model003_ssvep_fit_draws <- model003_ssvep_fit$draws(format = "df")
model007_lpp_fit_draws <- model007_lpp_fit$draws(format = "df")
model007_ssvep_fit_draws <- model007_ssvep_fit$draws(format = "df")
model009_lpp_fit_draws <- model009_lpp_fit$draws(format = "df")
model009_ssvep_fit_draws <- model009_ssvep_fit$draws(format = "df")
model010_lpp_fit_draws <- model010_lpp_fit$draws(format = "df")
model010_ssvep_fit_draws <- model010_ssvep_fit$draws(format = "df")
model011_lpp_fit_draws <- model011_lpp_fit$draws(format = "df")
model011_ssvep_fit_draws <- model011_ssvep_fit$draws(format = "df")
model012_lpp_fit_draws <- model012_lpp_fit$draws(format = "df")
model012_ssvep_fit_draws <- model012_ssvep_fit$draws(format = "df")


model003_lpp_fit_loo <- model003_lpp_fit$loo()
model003_ssvep_fit_loo <- model003_ssvep_fit$loo()
model007_lpp_fit_loo <- model007_lpp_fit$loo()
model007_ssvep_fit_loo <- model007_ssvep_fit$loo()
model009_lpp_fit_loo <- model009_lpp_fit$loo()
model009_ssvep_fit_loo <- model009_ssvep_fit$loo()
model011_lpp_fit_loo <- model011_lpp_fit$loo()
model011_ssvep_fit_loo <- model011_ssvep_fit$loo()
model012_lpp_fit_loo <- model012_lpp_fit$loo()
model012_ssvep_fit_loo <- model012_ssvep_fit$loo()

loo::loo_compare(model003_lpp_fit_loo,
                 model007_lpp_fit_loo,
                 model009_lpp_fit_loo)

loo::loo_model_weights(list(model003_lpp_fit_loo,
                            model007_lpp_fit_loo,
                            model009_lpp_fit_loo))

loo::loo_compare(model003_ssvep_fit_loo,
                 model007_ssvep_fit_loo,
                 model009_ssvep_fit_loo)

loo::loo_model_weights(list(model003_ssvep_fit_loo,
                            model007_ssvep_fit_loo,
                            model009_ssvep_fit_loo))

loo::loo_compare(model003_lpp_fit_loo,
                 model007_lpp_fit_loo)

loo::loo_model_weights(list(model003_lpp_fit_loo,
                            model007_lpp_fit_loo))

loo::loo_compare(model003_ssvep_fit_loo,
                 model007_ssvep_fit_loo)

loo::loo_model_weights(list(model003_ssvep_fit_loo,
                            model007_ssvep_fit_loo))

model003_lpp_fit_loo
model007_lpp_fit_loo
model009_lpp_fit_loo
model011_lpp_fit_loo
model012_lpp_fit_loo

loo::loo_compare(model003_lpp_fit_loo,
                 model007_lpp_fit_loo,
                 model011_lpp_fit_loo,
                 model012_lpp_fit_loo)

loo::loo_model_weights(list(model003_lpp_fit_loo,
                            model007_lpp_fit_loo,
                            model011_lpp_fit_loo,
                            model012_lpp_fit_loo))

loo::loo_compare(model003_lpp_fit_loo,
                 model011_lpp_fit_loo,
                 model012_lpp_fit_loo)

loo::loo_model_weights(list(#model003_lpp_fit_loo,
                            model011_lpp_fit_loo,
                            model012_lpp_fit_loo))

loo::loo_compare(model003_ssvep_fit_loo,
                 model011_ssvep_fit_loo,
                 model012_ssvep_fit_loo)

loo::loo_model_weights(list(#model003_ssvep_fit_loo,
                            model011_ssvep_fit_loo,
                            model012_ssvep_fit_loo))

loo::loo_compare(model003_lpp_fit_loo,
                 model007_lpp_fit_loo,
                 model009_lpp_fit_loo,
                 model011_lpp_fit_loo)

loo::loo_model_weights(list(model003_lpp_fit_loo,
                            model007_lpp_fit_loo,
                            model009_lpp_fit_loo,
                            model011_lpp_fit_loo))

loo::loo_compare(model001_lpp_fit_loo,
                 model002_lpp_fit_loo,
                 model003_lpp_fit_loo,
                 model004_lpp_fit_loo,
                 model005_lpp_fit_loo,
                 model006_lpp_fit_loo,
                 model007_lpp_fit_loo,
                 model008_lpp_fit_loo,
                 model009_lpp_fit_loo,
                 model010_lpp_fit_loo,
                 model011_lpp_fit_loo,
                 model012_lpp_fit_loo,
                 model013_lpp_fit_loo)

loo::loo_model_weights(list(model001_lpp_fit_loo,
                            model002_lpp_fit_loo,
                            model003_lpp_fit_loo,
                            model004_lpp_fit_loo,
                            model005_lpp_fit_loo,
                            model006_lpp_fit_loo,
                            model007_lpp_fit_loo,
                            model008_lpp_fit_loo,
                            model009_lpp_fit_loo,
                            model010_lpp_fit_loo,
                            model011_lpp_fit_loo,
                            model012_lpp_fit_loo))




model003_ssvep_fit_loo
model007_ssvep_fit_loo
model009_ssvep_fit_loo
model011_ssvep_fit_loo
model012_ssvep_fit_loo

loo::loo_compare(model003_ssvep_fit_loo,
                 model007_ssvep_fit_loo,
                 model011_ssvep_fit_loo)

loo::loo_model_weights(list(model003_ssvep_fit_loo,
                            model007_ssvep_fit_loo,
                            model011_ssvep_fit_loo))

loo::loo_compare(model003_ssvep_fit_loo,
                 model011_ssvep_fit_loo,
                 model012_ssvep_fit_loo)

loo::loo_model_weights(list(#model003_ssvep_fit_loo,
                            model011_ssvep_fit_loo,
                            model012_ssvep_fit_loo))

loo::loo_compare(model003_ssvep_fit_loo,
                 model007_ssvep_fit_loo,
                 model009_ssvep_fit_loo,
                 model011_ssvep_fit_loo)

loo::loo_model_weights(list(model003_ssvep_fit_loo,
                            model007_ssvep_fit_loo,
                            model009_ssvep_fit_loo,
                            model011_ssvep_fit_loo))

loo::loo_compare(model001_ssvep_fit_loo,
                 model002_ssvep_fit_loo,
                 model003_ssvep_fit_loo,
                 model004_ssvep_fit_loo,
                 model005_ssvep_fit_loo,
                 model006_ssvep_fit_loo,
                 model007_ssvep_fit_loo,
                 model008_ssvep_fit_loo,
                 model009_ssvep_fit_loo,
                 model010_ssvep_fit_loo,
                 model011_ssvep_fit_loo,
                 model012_ssvep_fit_loo,
                 model013_ssvep_fit_loo)

loo::loo_model_weights(list(model001_ssvep_fit_loo,
                            model002_ssvep_fit_loo,
                            model003_ssvep_fit_loo,
                            model004_ssvep_fit_loo,
                            model005_ssvep_fit_loo,
                            model006_ssvep_fit_loo,
                            model007_ssvep_fit_loo,
                            model008_ssvep_fit_loo,
                            model009_ssvep_fit_loo,
                            model010_ssvep_fit_loo,
                            model011_ssvep_fit_loo))

# Standard ANOVAs and t-tests####
amp_cate_type_aov <- data_for_stan_df %>%
  select(par,type,cate,amp) %>% 
  group_by(par,type,cate) %>%
  summarise_all(mean) %>% 
  mutate(zamp = as.vector(scale(amp))) %>% 
  ungroup() %>% 
  mutate(zamp = if_else(type == 2, -1*zamp, zamp)) %>% 
  afex::aov_ez(data = ., dv = "zamp", 
               within = c("type", "cate"),
               id = "par")

amp_cate_type_aov
effectsize::eta_squared(amp_cate_type_aov)

aro_cate_type_aov <- data_for_stan_df %>%
  select(par,type,cate,arousal) %>% 
  group_by(par,type,cate) %>%
  summarise_all(mean) %>% 
  ungroup() %>% 
  afex::aov_ez(data = ., dv = "arousal", 
               within = c("type", "cate"),
               id = "par")

aro_cate_type_aov
summary(aro_cate_type_aov)
effectsize::eta_squared(aro_cate_type_aov)

aro_cate_scene_aov <- data_for_stan_df %>%
  select(par,type,cate,arousal) %>% 
  filter(type == 1) %>% 
  group_by(par,type,cate) %>%
  summarise_all(mean) %>% 
  ungroup() %>% 
  afex::aov_ez(data = ., dv = "arousal", 
               within = c("cate"),
               id = "par")

aro_cate_scene_aov
summary(aro_cate_scene_aov)
effectsize::eta_squared(aro_cate_scene_aov)

aro_cate_video_aov <- data_for_stan_df %>%
  select(par,type,cate,arousal) %>% 
  filter(type == 2) %>% 
  group_by(par,type,cate) %>%
  summarise_all(mean) %>% 
  ungroup() %>% 
  afex::aov_ez(data = ., dv = "arousal", 
               within = c("cate"),
               id = "par")

aro_cate_video_aov
summary(aro_cate_video_aov)
effectsize::eta_squared(aro_cate_video_aov)

scene_SAM_cate_avg <- data_for_stan_df %>%
  select(par,type,cate,arousal,valence) %>% 
  filter(type == 1) %>%
  group_by(par,type,cate) %>%
  summarise_all(mean)

video_SAM_cate_avg <- data_for_stan_df %>%
  select(par,type,cate,arousal,valence) %>% 
  filter(type == 2) %>%
  group_by(par,type,cate) %>%
  summarise_all(mean)



t.test(scene_SAM_cate_avg$arousal[scene_SAM_cate_avg$cate == 1],
       video_SAM_cate_avg$arousal[video_SAM_cate_avg$cate == 1],
       paired = T)  

effectsize::cohens_d(scene_SAM_cate_avg$arousal[scene_SAM_cate_avg$cate == 1],
                     video_SAM_cate_avg$arousal[video_SAM_cate_avg$cate == 1],
                     paired = T)  

t.test(scene_SAM_cate_avg$arousal[scene_SAM_cate_avg$cate == 2],
       video_SAM_cate_avg$arousal[video_SAM_cate_avg$cate == 2],
       paired = T)  

effectsize::cohens_d(scene_SAM_cate_avg$arousal[scene_SAM_cate_avg$cate == 2],
                     video_SAM_cate_avg$arousal[video_SAM_cate_avg$cate == 2],
                     paired = T)  

t.test(scene_SAM_cate_avg$arousal[scene_SAM_cate_avg$cate == 3],
       video_SAM_cate_avg$arousal[video_SAM_cate_avg$cate == 3],
       paired = T)  

effectsize::cohens_d(scene_SAM_cate_avg$arousal[scene_SAM_cate_avg$cate == 3],
                     video_SAM_cate_avg$arousal[video_SAM_cate_avg$cate == 3],
                     paired = T)  

scene_SAM_cate_avg %>% 
  group_by(cate) %>% 
  summarise(mean_aro = mean(arousal),
            se_aro = plotrix::std.error(arousal))

video_SAM_cate_avg %>% 
  group_by(cate) %>% 
  summarise(mean_aro = mean(arousal),
            se_aro = plotrix::std.error(arousal))

val_cate_type_aov <- data_for_stan_df %>%
  select(par,type,cate,valence) %>% 
  group_by(par,type,cate) %>%
  summarise_all(mean) %>% 
  ungroup() %>% 
  afex::aov_ez(data = ., dv = "valence", 
               within = c("type", "cate"),
               id = "par")

val_cate_type_aov
summary(val_cate_type_aov)
effectsize::eta_squared(val_cate_type_aov)

aro_cate_scene_aov <- data_for_stan_df %>%
  select(par,type,cate,arousal) %>% 
  filter(type == 1) %>% 
  group_by(par,type,cate) %>%
  summarise_all(mean) %>% 
  ungroup() %>% 
  afex::aov_ez(data = ., dv = "arousal", 
               within = c("cate"),
               id = "par")

aro_cate_scene_aov
summary(aro_cate_scene_aov)
effectsize::eta_squared(aro_cate_scene_aov)

aro_cate_video_aov <- data_for_stan_df %>%
  select(par,type,cate,arousal) %>% 
  filter(type == 2) %>% 
  group_by(par,type,cate) %>%
  summarise_all(mean) %>% 
  ungroup() %>% 
  afex::aov_ez(data = ., dv = "arousal", 
               within = c("cate"),
               id = "par")

aro_cate_video_aov
summary(aro_cate_video_aov)
effectsize::eta_squared(aro_cate_video_aov)

  
## R^2 ####

### R^2 using residuals for full model



model003_lpp_predicted_means <- model003_lpp_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()

model003_ssvep_predicted_means <- model003_ssvep_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()

model_R2_posteriors <- data.frame(model003_lpp_R2 = 
                                    bayes_R2_residuals(
                                      data_list_for_stan_lpp$amp, 
                                      model003_lpp_predicted_means),
                                  model003_ssvep_R2 = 
                                    bayes_R2_residuals(
                                      data_list_for_stan_ssvep$amp, 
                                      model003_ssvep_predicted_means))


model_R2_posteriors %>% 
  ggplot() +
  geom_density(aes(x = model003_lpp_R2)) +
  geom_density(aes(x = model003_ssvep_R2), color = "blue") +
  theme_classic()

### R^2 using residuals for effect of stim 

bayes_R2_residuals <- function(observed_data, 
                               predicted_means, 
                               correct_for_sample_by_obs = NULL) {

  if(!is.null(correct_for_sample_by_obs)){
    
    corrected_observed_data <- -1 * sweep(correct_for_sample_by_obs,
                                          2, observed_data)
    
    bayes_residuals <- corrected_observed_data - predicted_means
    
  } else {
    
    bayes_residuals <- -1 * sweep(predicted_means, 2, 
                                  observed_data)
    
  }
  variance_of_predicted_means <- apply(predicted_means, 1, var)
  
  variance_of_residuals <- apply(bayes_residuals, 1, var)
  
  
  bayesian_R_squared <- variance_of_predicted_means / 
    (variance_of_predicted_means + variance_of_residuals)
  
  return(bayesian_R_squared)
}

model011_lpp_predicted_means_per_obs_minus_par_effect <- 
  model011_lpp_predicted_means - model011_lpp_predicted_par_means_per_obs

model011_lpp_predicted_means[1:5,1:5]
model011_lpp_predicted_par_means_per_obs[1:5,1:5]
model011_lpp_predicted_means_per_obs_minus_par_effect[1:5,1:5]

hold_lpp_corrected_observed_data <- -1 * sweep(model011_lpp_predicted_par_means_per_obs,
                                      2, data_list_for_stan_lpp$amp)

hold_lpp_bayes_residuals <- 
  hold_lpp_corrected_observed_data - model011_lpp_predicted_means_per_obs_minus_par_effect


hold_lpp_variance_of_predicted_means <- 
  apply(model011_lpp_predicted_means_per_obs_minus_par_effect, 1, var)

hold_lpp_variance_of_residuals <- 
  apply(hold_lpp_bayes_residuals, 1, var)


hold_lpp_bayesian_R_squared <- hold_lpp_variance_of_predicted_means / 
  (hold_lpp_variance_of_predicted_means + hold_lpp_variance_of_residuals)

model011_ssvep_predicted_means_per_obs_minus_par_effect <- 
  model011_ssvep_predicted_means - model011_ssvep_predicted_par_means_per_obs

model011_ssvep_predicted_means[1:5,1:5]
model011_ssvep_predicted_par_means_per_obs[1:5,1:5]
model011_ssvep_predicted_means_per_obs_minus_par_effect[1:5,1:5]

hold_ssvep_corrected_observed_data <- -1 * sweep(model011_ssvep_predicted_par_means_per_obs,
                                      2, data_list_for_stan_ssvep$amp)

hold_ssvep_bayes_residuals <- 
  hold_ssvep_corrected_observed_data - model011_ssvep_predicted_means_per_obs_minus_par_effect


hold_ssvep_variance_of_predicted_means <- 
  apply(model011_ssvep_predicted_means_per_obs_minus_par_effect, 1, var)

hold_ssvep_variance_of_residuals <- 
  apply(hold_ssvep_bayes_residuals, 1, var)


hold_ssvep_bayesian_R_squared <- hold_ssvep_variance_of_predicted_means / 
  (hold_ssvep_variance_of_predicted_means + hold_ssvep_variance_of_residuals)



model011_ssvep_predicted_aro_means_per_obs <- 
  model011_ssvep_predicted_means - model011_ssvep_predicted_par_means_per_obs



# Full model predicted means
model003_lpp_predicted_means <- model003_lpp_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()

model003_ssvep_predicted_means <- model003_ssvep_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()

model011_lpp_predicted_means <- model011_lpp_fit_draws %>% 
  select(starts_with("mu_amp_given_aro[")) %>% 
  as.matrix()

model011_ssvep_predicted_means <- model011_ssvep_fit_draws %>% 
  select(starts_with("mu_amp_given_aro[")) %>% 
  as.matrix()

model012_lpp_predicted_means <- model012_lpp_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()

model012_ssvep_predicted_means <- model012_ssvep_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()


# Predict par mean per observation for isolating emotion effects
model003_lpp_predicted_par_means_per_obs <- as.matrix(
  model003_lpp_fit_draws[
    paste0("bpar[",
           data_list_for_stan_lpp$par, "]")])

model003_lpp_predicted_stim_means_per_obs <- as.matrix(
  model003_lpp_fit_draws[
    paste0("bstim[",
           data_list_for_stan_lpp$par, "]")])

model003_ssvep_predicted_par_means_per_obs <- as.matrix(
  model003_ssvep_fit_draws[
    paste0("bpar[",
           data_list_for_stan_ssvep$par, "]")])

model003_ssvep_predicted_stim_means_per_obs <- as.matrix(
  model003_ssvep_fit_draws[
    paste0("bstim[",
           data_list_for_stan_ssvep$par, "]")])



model011_lpp_predicted_par_means_per_obs <- as.matrix(
  model011_lpp_fit_draws[
    paste0("par_mu_amp[",
           data_list_for_stan_lpp$par, "]")])

model011_ssvep_predicted_par_means_per_obs <- as.matrix(
  model011_ssvep_fit_draws[
    paste0("par_mu_amp[",
           data_list_for_stan_ssvep$par, "]")])

  
  
model012_lpp_predicted_par_means_per_obs <- as.matrix(
  model012_lpp_fit_draws[
    paste0("bpar[",
           data_list_for_stan_lpp$par, "]")])

model012_lpp_predicted_stim_means_per_obs <- as.matrix(
  model012_lpp_fit_draws[
    paste0("bstim[",
           data_list_for_stan_lpp$par, "]")])

model012_ssvep_predicted_par_means_per_obs <- as.matrix(
  model012_ssvep_fit_draws[
    paste0("bpar[",
           data_list_for_stan_ssvep$par, "]")])

model012_ssvep_predicted_stim_means_per_obs <- as.matrix(
  model012_ssvep_fit_draws[
    paste0("bstim[",
           data_list_for_stan_ssvep$par, "]")])


model_R2_posteriors <- data.frame(model003_lpp_R2 = 
                                    bayes_R2_residuals(
                                      data_list_for_stan_lpp$amp, 
                                      model003_lpp_predicted_means),
                                  model003_ssvep_R2 = 
                                    bayes_R2_residuals(
                                      data_list_for_stan_ssvep$amp, 
                                      model003_ssvep_predicted_means),
                                  model003_lpp_R2_stim = 
                                    bayes_R2_residuals(
                                      data_list_for_stan_lpp$amp, 
                                      model003_lpp_predicted_stim_means_per_obs,
                                      model003_lpp_predicted_par_means_per_obs),
                                  model003_ssvep_R2_stim = 
                                    bayes_R2_residuals(
                                      data_list_for_stan_ssvep$amp, 
                                      model003_ssvep_predicted_stim_means_per_obs,
                                      model003_ssvep_predicted_par_means_per_obs),
                                  model011_lpp_R2 =
                                    bayes_R2_residuals(
                                      data_list_for_stan_lpp$amp,
                                      model011_lpp_predicted_means),
                                  model011_ssvep_R2 =
                                    bayes_R2_residuals(
                                      data_list_for_stan_ssvep$amp,
                                      model011_ssvep_predicted_means),
                                  # model011_lpp_R2_stim = 
                                  #   bayes_R2_residuals(
                                  #     data_list_for_stan_lpp$amp, 
                                  #     model011_lpp_predicted_stim_means_per_obs,
                                  #     model011_lpp_predicted_par_means_per_obs),
                                  # model011_ssvep_R2_stim = 
                                  #   bayes_R2_residuals(
                                  #     data_list_for_stan_ssvep$amp, 
                                  #     model011_ssvep_predicted_stim_means_per_obs,
                                  #     model011_ssvep_predicted_par_means_per_obs),
                                  model012_lpp_R2 = 
                                    bayes_R2_residuals(
                                      data_list_for_stan_lpp$amp, 
                                      model012_lpp_predicted_means),
                                  model012_ssvep_R2 = 
                                    bayes_R2_residuals(
                                      data_list_for_stan_ssvep$amp, 
                                      model012_ssvep_predicted_means),
                                  model012_lpp_R2_stim = 
                                    bayes_R2_residuals(
                                      data_list_for_stan_lpp$amp, 
                                      model012_lpp_predicted_stim_means_per_obs,
                                      model012_lpp_predicted_par_means_per_obs),
                                  model012_ssvep_R2_stim = 
                                    bayes_R2_residuals(
                                      data_list_for_stan_ssvep$amp, 
                                      model012_ssvep_predicted_stim_means_per_obs,
                                      model012_ssvep_predicted_par_means_per_obs))

# Full model R-squared
model_R2_posteriors %>% 
  ggplot() +
  # geom_density(aes(x = model003_lpp_R2)) +
  geom_density(aes(x = model011_lpp_R2), color = "red") +
  geom_density(aes(x = model012_lpp_R2), color = "purple") +
  # geom_density(aes(x = model003_ssvep_R2), color = "blue") +
  geom_density(aes(x = model011_ssvep_R2), color = "orange") +
  geom_density(aes(x = model012_ssvep_R2), color = "cyan") +
  # geom_density(aes(x = model003_lpp_R2_stim), color = "red") +
  # geom_density(aes(x = model003_ssvep_R2_stim), color = "green") +
  # geom_density(aes(x = model012_lpp_R2_stim), color = "gray") +
  # geom_density(aes(x = model012_ssvep_R2_stim), color = "black") +
  theme_classic()


# Remove effect of Par R-squared

(data.frame(hold_lpp_bayesian_R_squared,
           hold_ssvep_bayesian_R_squared) %>% 
  ggplot() +
  geom_density(aes(x = hold_lpp_bayesian_R_squared)) +
  geom_density(aes(x = hold_ssvep_bayesian_R_squared), color = "blue") +
  coord_cartesian(xlim = c(0,.10)) +
  theme_classic()) /
(model_R2_posteriors %>% 
  ggplot() +
  # geom_density(aes(x = model003_lpp_R2)) +
  # geom_density(aes(x = model003_ssvep_R2), color = "blue") +
  # geom_density(aes(x = model012_lpp_R2), color = "purple") +
  # geom_density(aes(x = model012_ssvep_R2), color = "cyan") +
  # geom_density(aes(x = model003_lpp_R2_stim), color = "red") +
  geom_density(aes(x = model012_lpp_R2_stim), color = "black") +
  # geom_density(aes(x = model003_ssvep_R2_stim), color = "green") +
  geom_density(aes(x = model012_ssvep_R2_stim), color = "blue") +
   coord_cartesian(xlim = c(0,.10)) +
  theme_classic())

# Loo R^2 I can't generalize this to predictors so I am not going to include this.


hold_observed_data <- data_list_for_stan_lpp$amp
  
hold_predicted_means <- model012_lpp_predicted_means

hold_log_likelihood <- model012_lpp_fit_draws %>% 
  select(starts_with("log_lik[")) %>% 
  as.matrix()

hold_num_chains <- 8

hold_iterations_per_chain <- 1000

hold_relative_effective_sample_size <- 
  loo::relative_eff(exp(hold_log_likelihood), 
                    chain_id = rep(1:hold_num_chains, 
                                   each = hold_iterations_per_chain))

hold_psis_results <- 
  loo::psis(log_ratios = -hold_log_likelihood, 
            r_eff = hold_relative_effective_sample_size)


hold_loo_predicted_means <- 
  loo::E_loo(hold_predicted_means, 
             hold_psis_results, 
             log_ratios = -hold_log_likelihood)$value


hold_prediction_errors <- 
  hold_loo_predicted_means - hold_observed_data

hold_num_observations <- length(hold_observed_data)
  
hold_dirichlet_samples <- 
  bayesboot::rudirichlet(8000, hold_num_observations)

hold_observed_variance <- 
  (rowSums(sweep(hold_dirichlet_samples, 2, 
                 hold_observed_data^2, FUN = "*")) -
     rowSums(sweep(hold_dirichlet_samples, 2, 
                   hold_observed_data, FUN = "*"))^2) * 
  (hold_num_observations / (hold_num_observations - 1))


hold_error_variance <- 
  (rowSums(sweep(hold_dirichlet_samples, 2, 
                 hold_prediction_errors^2, FUN = "*")) -
     rowSums(sweep(hold_dirichlet_samples, 2, 
                   hold_prediction_errors, FUN = "*")^2)) * 
  (hold_num_observations / (hold_num_observations - 1))
  
hold_loo_R2_values <- 1 - hold_error_variance / hold_observed_variance

  # Constraint R-squared values to be within the range [-1, 1]
hold_loo_R2_values[hold_loo_R2_values < -1] <- -1
hold_loo_R2_values[hold_loo_R2_values > 1] <- 1


model_R2_posteriors %>% 
  ggplot() +
  # geom_density(aes(x = model003_lpp_R2)) +
  geom_density(aes(x = model011_lpp_R2), color = "red") +
  geom_density(aes(x = model012_lpp_R2), color = "purple") +
  geom_density(data = data.frame(hold_loo_R2_values),
               aes(x = hold_loo_R2_values), color = "blue") +
  # geom_density(aes(x = model003_ssvep_R2), color = "blue") +
  # geom_density(aes(x = model011_ssvep_R2), color = "orange") +
  # geom_density(aes(x = model012_ssvep_R2), color = "cyan") +
  # geom_density(aes(x = model003_lpp_R2_stim), color = "red") +
  # geom_density(aes(x = model003_ssvep_R2_stim), color = "green") +
  # geom_density(aes(x = model012_lpp_R2_stim), color = "gray") +
  # geom_density(aes(x = model012_ssvep_R2_stim), color = "black") +
  theme_classic()




sweep(matrix(c(1:4),ncol = 2), 2, c(1:2))

lpp_amp_minus_eff_of_par <- -1 * sweep(model003_lpp_predicted_par_means_per_obs,2,
                              data_list_for_stan_lpp$amp)
  
model003_lpp_predicted_means_minus_eff_of_par <- 
  model003_lpp_predicted_means - model003_lpp_predicted_par_means_per_obs

hold_residuals <- lpp_amp_minus_eff_of_par - model003_lpp_predicted_means_minus_eff_of_par

hold_res_var <- apply(hold_residuals,1,var)

hold_mean_var <- apply(model003_lpp_predicted_means_minus_eff_of_par,1,var)

hold_mean_var / 
  (hold_mean_var + hold_res_var)
  
data_list_for_stan_lpp$amp[1:5]

model003_lpp_predicted_par_means_per_obs[1:5,1:5]

lpp_amp_minus_eff_of_par[1:5,1:5]

model003_lpp_predicted_means[1:5,1:5]

model003_lpp_predicted_means_minus_eff_of_par[1:5,1:5]

  
model_R2_posteriors %>% 
  mutate(model003_lpp_R2_stim = 
          bayes_R2_residuals(
            lpp_amp_minus_eff_of_par, 
            model003_lpp_predicted_means_minus_eff_of_par))



predicted_means <- model003_lpp_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()

variance_of_predicted_means <- apply(predicted_means, 1, var)

residual_variance <- model003_lpp_fit_draws %>% 
  select(starts_with("amp_sd")) %>% 
  mutate(residual_variance = amp_sd^2) %>% 
  pull() %>% 
  as.matrix()

R2_stim <- data.frame(lpp_R2 = variance_of_predicted_means / 
                        (variance_of_predicted_means + residual_variance))

predicted_means <- model003_ssvep_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()

variance_of_predicted_means <- apply(predicted_means, 1, var)

residual_variance <- model003_ssvep_fit_draws %>% 
  select(starts_with("amp_sd")) %>% 
  mutate(residual_variance = amp_sd^2) %>% 
  pull() %>% 
  as.matrix()

R2_stim <- R2_stim %>% 
  mutate(ssvep_R2 = variance_of_predicted_means / 
           (variance_of_predicted_means + residual_variance))

observed_data <- data_list_for_stan_lpp$amp

predicted_means <- model003_lpp_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()

# par_mean_per_obs <- as.matrix(
#   model003_lpp_fit_draws[
#     paste0("bpar[",
#            data_list_for_stan_lpp$par, "]")])
# 
# predicted_means_minus_bpar <- predicted_means - par_mean_per_obs


log_likelihood <- model003_lpp_fit_draws %>% 
  select(starts_with("log_lik[")) %>% 
  as.matrix()

num_chains <- 8
iterations_per_chain <- 10000

relative_effective_sample_size <- loo::relative_eff(
  exp(log_likelihood), 
  chain_id = rep(1:num_chains, 
                 each = iterations_per_chain))

psis_results <- loo::psis(log_ratios = -log_likelihood, 
                          r_eff = relative_effective_sample_size)

loo_predicted_means <- loo::E_loo(predicted_means, 
                                  psis_results, 
                                  log_ratios = -log_likelihood)$value

# Errors for leave-one-out predictions
prediction_errors <- loo_predicted_means - observed_data

# Number of observations
num_observations <- length(observed_data)

# Generate random Dirichlet distributions
dirichlet_samples <- bayesboot::rudirichlet(4000, num_observations)

# Variance of observed data using random Dirichlet weights
observed_variance <- (rowSums(sweep(dirichlet_samples, 2, observed_data^2, FUN = "*")) -
                        rowSums(sweep(dirichlet_samples, 2, observed_data, FUN = "*"))^2) * (num_observations / (num_observations - 1))

# Variance of leave-one-out prediction errors using random Dirichlet weights
error_variance <- (rowSums(sweep(dirichlet_samples, 2, prediction_errors^2, FUN = "*")) -
                     rowSums(sweep(dirichlet_samples, 2, prediction_errors, FUN = "*")^2)) * (num_observations / (num_observations - 1))

# Calculate leave-one-out R-squared values
loo_R2_values <- 1 - error_variance / observed_variance

# Constraint R-squared values to be within the range [-1, 1]
loo_R2_values[loo_R2_values < -1] <- -1
loo_R2_values[loo_R2_values > 1] <- 1

loo_R2_stim <- data.frame(loo_lpp_R2 = loo_R2_values)

observed_data <- data_list_for_stan_ssvep$amp

predicted_means <- model003_ssvep_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()

# par_mean_per_obs <- as.matrix(
#   model003_lpp_fit_draws[
#     paste0("bpar[",
#            data_list_for_stan_lpp$par, "]")])
# 
# predicted_means_minus_bpar <- predicted_means - par_mean_per_obs


log_likelihood <- model003_ssvep_fit_draws %>% 
  select(starts_with("log_lik[")) %>% 
  as.matrix()

num_chains <- 8
iterations_per_chain <- 10000

relative_effective_sample_size <- loo::relative_eff(
  exp(log_likelihood), 
  chain_id = rep(1:num_chains, 
                 each = iterations_per_chain))

psis_results <- loo::psis(log_ratios = -log_likelihood, 
                          r_eff = relative_effective_sample_size)

loo_predicted_means <- loo::E_loo(predicted_means, 
                                  psis_results, 
                                  log_ratios = -log_likelihood)$value

# Errors for leave-one-out predictions
prediction_errors <- loo_predicted_means - observed_data

# Number of observations
num_observations <- length(observed_data)

# Generate random Dirichlet distributions
dirichlet_samples <- bayesboot::rudirichlet(4000, num_observations)

# Variance of observed data using random Dirichlet weights
observed_variance <- (rowSums(sweep(dirichlet_samples, 2, observed_data^2, FUN = "*")) -
                        rowSums(sweep(dirichlet_samples, 2, observed_data, FUN = "*"))^2) * (num_observations / (num_observations - 1))

# Variance of leave-one-out prediction errors using random Dirichlet weights
error_variance <- (rowSums(sweep(dirichlet_samples, 2, prediction_errors^2, FUN = "*")) -
                     rowSums(sweep(dirichlet_samples, 2, prediction_errors, FUN = "*")^2)) * (num_observations / (num_observations - 1))

# Calculate leave-one-out R-squared values
loo_R2_values <- 1 - error_variance / observed_variance

# Constraint R-squared values to be within the range [-1, 1]
loo_R2_values[loo_R2_values < -1] <- -1
loo_R2_values[loo_R2_values > 1] <- 1

loo_R2_stim <- loo_R2_stim %>% 
  mutate(loo_ssvep_R2 = loo_R2_values)


ggplot() +
  geom_density(data = R2_stim,
               aes(x = lpp_R2)) +
  geom_density(data = R2_stim,
               aes(x = ssvep_R2), 
               color = "blue") +
  # geom_density(data = loo_R2_stim,
  #              aes(x = loo_lpp_R2), 
  #              color = "red") +
  # geom_density(data = loo_R2_stim,
  #              aes(x = loo_ssvep_R2), 
  #              color = "green") +
  # coord_cartesian(xlim = c(0,1)) +
  theme_classic()


predicted_means <- model012_lpp_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()

variance_of_predicted_means <- apply(predicted_means, 1, var)

residual_variance <- model012_lpp_fit_draws %>% 
  select(starts_with("amp_sd")) %>% 
  mutate(residual_variance = amp_sd^2) %>% 
  pull() %>% 
  as.matrix()

R2_stim <- data.frame(lpp_R2 = variance_of_predicted_means / 
                        (variance_of_predicted_means + residual_variance))

predicted_means <- model012_ssvep_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()

variance_of_predicted_means <- apply(predicted_means, 1, var)

residual_variance <- model012_ssvep_fit_draws %>% 
  select(starts_with("amp_sd")) %>% 
  mutate(residual_variance = amp_sd^2) %>% 
  pull() %>% 
  as.matrix()

R2_stim <- R2_stim %>% 
  mutate(ssvep_R2 = variance_of_predicted_means / 
           (variance_of_predicted_means + residual_variance))


observed_data <- data_list_for_stan_lpp$amp

predicted_means <- model012_lpp_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()

# par_mean_per_obs <- as.matrix(
#   model003_lpp_fit_draws[
#     paste0("bpar[",
#            data_list_for_stan_lpp$par, "]")])
# 
# predicted_means_minus_bpar <- predicted_means - par_mean_per_obs


log_likelihood <- model012_lpp_fit_draws %>% 
  select(starts_with("log_lik[")) %>% 
  as.matrix()

num_chains <- 8
iterations_per_chain <- 10000

relative_effective_sample_size <- loo::relative_eff(
  exp(log_likelihood), 
  chain_id = rep(1:num_chains, 
                 each = iterations_per_chain))

psis_results <- loo::psis(log_ratios = -log_likelihood, 
                          r_eff = relative_effective_sample_size)

loo_predicted_means <- loo::E_loo(predicted_means, 
                                  psis_results, 
                                  log_ratios = -log_likelihood)$value

# Errors for leave-one-out predictions
prediction_errors <- loo_predicted_means - observed_data

# Number of observations
num_observations <- length(observed_data)

# Generate random Dirichlet distributions
dirichlet_samples <- bayesboot::rudirichlet(4000, num_observations)

# Variance of observed data using random Dirichlet weights
observed_variance <- (rowSums(sweep(dirichlet_samples, 2, observed_data^2, FUN = "*")) -
                        rowSums(sweep(dirichlet_samples, 2, observed_data, FUN = "*"))^2) * (num_observations / (num_observations - 1))

# Variance of leave-one-out prediction errors using random Dirichlet weights
error_variance <- (rowSums(sweep(dirichlet_samples, 2, prediction_errors^2, FUN = "*")) -
                     rowSums(sweep(dirichlet_samples, 2, prediction_errors, FUN = "*")^2)) * (num_observations / (num_observations - 1))

# Calculate leave-one-out R-squared values
loo_R2_values <- 1 - error_variance / observed_variance

# Constraint R-squared values to be within the range [-1, 1]
loo_R2_values[loo_R2_values < -1] <- -1
loo_R2_values[loo_R2_values > 1] <- 1

loo_R2_stim <- data.frame(loo_lpp_R2 = loo_R2_values)

observed_data <- data_list_for_stan_ssvep$amp

predicted_means <- model012_ssvep_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()

# par_mean_per_obs <- as.matrix(
#   model003_lpp_fit_draws[
#     paste0("bpar[",
#            data_list_for_stan_lpp$par, "]")])
# 
# predicted_means_minus_bpar <- predicted_means - par_mean_per_obs


log_likelihood <- model012_ssvep_fit_draws %>% 
  select(starts_with("log_lik[")) %>% 
  as.matrix()

num_chains <- 8
iterations_per_chain <- 10000

relative_effective_sample_size <- loo::relative_eff(
  exp(log_likelihood), 
  chain_id = rep(1:num_chains, 
                 each = iterations_per_chain))

psis_results <- loo::psis(log_ratios = -log_likelihood, 
                          r_eff = relative_effective_sample_size)

loo_predicted_means <- loo::E_loo(predicted_means, 
                                  psis_results, 
                                  log_ratios = -log_likelihood)$value

# Errors for leave-one-out predictions
prediction_errors <- loo_predicted_means - observed_data

# Number of observations
num_observations <- length(observed_data)

# Generate random Dirichlet distributions
dirichlet_samples <- bayesboot::rudirichlet(4000, num_observations)

# Variance of observed data using random Dirichlet weights
observed_variance <- (rowSums(sweep(dirichlet_samples, 2, observed_data^2, FUN = "*")) -
                        rowSums(sweep(dirichlet_samples, 2, observed_data, FUN = "*"))^2) * (num_observations / (num_observations - 1))

# Variance of leave-one-out prediction errors using random Dirichlet weights
error_variance <- (rowSums(sweep(dirichlet_samples, 2, prediction_errors^2, FUN = "*")) -
                     rowSums(sweep(dirichlet_samples, 2, prediction_errors, FUN = "*")^2)) * (num_observations / (num_observations - 1))

# Calculate leave-one-out R-squared values
loo_R2_values <- 1 - error_variance / observed_variance

# Constraint R-squared values to be within the range [-1, 1]
loo_R2_values[loo_R2_values < -1] <- -1
loo_R2_values[loo_R2_values > 1] <- 1

loo_R2_stim <- loo_R2_stim %>% 
  mutate(loo_ssvep_R2 = loo_R2_values)


ggplot() +
  # geom_density(data = R2_stim,
  #              aes(x = lpp_R2)) +
  geom_density(data = R2_stim,
               aes(x = ssvep_R2), 
               color = "blue") +
  # geom_density(data = loo_R2_stim,
  #              aes(x = loo_lpp_R2), 
  #              color = "red") +
  geom_density(data = loo_R2_stim,
               aes(x = loo_ssvep_R2), 
               color = "green") +
  # coord_cartesian(xlim = c(0,1)) +
  theme_classic()





predicted_means <- model003_lpp_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()

par_mean_per_obs <- as.matrix(
  model003_lpp_fit_draws[
    paste0("bpar[",
           data_list_for_stan_lpp$par, "]")])

predicted_means_minus_bpar <- predicted_means - par_mean_per_obs

variance_of_predicted_means <- apply(predicted_means_minus_bpar, 1, var)

residual_variance <- model003_lpp_fit_draws %>% 
  select(starts_with("amp_sd")) %>% 
  mutate(residual_variance = amp_sd^2) %>% 
  pull() %>% 
  as.matrix()

R2_stim <- data.frame(lpp_R2 = variance_of_predicted_means / 
                        (variance_of_predicted_means + residual_variance)) 


ggplot() +
  geom_density(data = R2_stim,
               aes(x = lpp_R2)) +
  geom_density(data = R2_stim,
               aes(x = ssvep_R2), 
               color = "blue") +
  geom_density(data = loo_R2_stim,
               aes(x = loo_lpp_R2), 
               color = "red") +
  theme_classic()

predicted_means <- model003_ssvep_fit_draws %>% 
  select(starts_with("mu_pred[")) %>% 
  as.matrix()

par_mean_per_obs <- as.matrix(
  model003_ssvep_fit_draws[
    paste0("bpar[",
           data_list_for_stan_ssvep$par, "]")])

predicted_means_minus_bpar <- predicted_means - par_mean_per_obs

variance_of_predicted_means <- apply(predicted_means_minus_bpar, 1, var)


residual_variance <- model003_ssvep_fit_draws %>% 
  select(starts_with("amp_sd")) %>% 
  mutate(residual_variance = amp_sd^2) %>% 
  pull() %>% 
  as.matrix()

R2_stim <- R2_stim %>% 
  mutate(ssvep_R2 = variance_of_predicted_means / 
           (variance_of_predicted_means + residual_variance))

R2_stim %>% 
  ggplot() +
  geom_density(aes(x = lpp_R2)) +
  geom_density(aes(x = ssvep_R2), color = "blue") +
  theme_classic()





loo_R2 <- function(model_fit) {
  # Extract observed data from the model fit
  observed_data <- get_y(model_fit)
  
  # Predicted values based on the posterior distribution
  predicted_means <- posterior_epred(model_fit)
  
  # Log-likelihood for each observation across all posterior samples
  log_likelihood <- log_lik(model_fit)
  
  # Number of chains and iterations per chain from the model fit
  num_chains <- length(model_fit$stanfit@sim$n_save)
  iterations_per_chain <- model_fit$stanfit@sim$n_save[[1]]
  
  # Effective sample size adjustment for each log-likelihood point
  relative_effective_sample_size <- relative_eff(exp(log_likelihood), chain_id = rep(1:num_chains, each = iterations_per_chain))
  
  # Pareto smoothed importance sampling diagnostics
  psis_results <- psis(log_ratios = -log_likelihood, r_eff = relative_effective_sample_size)
  
  # Expected values of predictions using leave-one-out cross-validation
  loo_predicted_means <- E_loo(predicted_means, 
                               psis_results, 
                               log_ratios = -log_likelihood)$value
  
  # Errors for leave-one-out predictions
  prediction_errors <- loo_predicted_means - observed_data
  
  # Number of observations
  num_observations <- length(observed_data)
  
  # Generate random Dirichlet distributions
  dirichlet_samples <- bayesboot::rudirichlet(4000, num_observations)
  
  # Variance of observed data using random Dirichlet weights
  observed_variance <- (rowSums(sweep(dirichlet_samples, 2, observed_data^2, FUN = "*")) -
                          rowSums(sweep(dirichlet_samples, 2, observed_data, FUN = "*"))^2) * (num_observations / (num_observations - 1))
  
  # Variance of leave-one-out prediction errors using random Dirichlet weights
  error_variance <- (rowSums(sweep(dirichlet_samples, 2, prediction_errors^2, FUN = "*")) -
                       rowSums(sweep(dirichlet_samples, 2, prediction_errors, FUN = "*")^2)) * (num_observations / (num_observations - 1))
  
  # Calculate leave-one-out R-squared values
  loo_R2_values <- 1 - error_variance / observed_variance
  
  # Constraint R-squared values to be within the range [-1, 1]
  loo_R2_values[loo_R2_values < -1] <- -1
  loo_R2_values[loo_R2_values > 1] <- 1
  
  return(loo_R2_values)
}





# Function to compute Bayesian R^2 for each posterior sample
posterior_r2 <- function(mu_pred, observed) {
  var_y <- var(observed)
  var_mu_pred <- 
    mu_mean <- mean(observed)
  
  # Computing explained variance as the variance of the predicted values
  explained_variance <- var_mu_pred
  
  # Total variance is the variance of the observed data
  total_variance <- var_y
  
  # R^2 is the explained variance divided by the total variance
  r2 <- explained_variance / total_variance
  return(r2)
}

# Extract posterior predictions for mu
mu_pred <- model003_lpp_fit$draws(variables = "mu_pred",format = "matrix")
sweep(mu_pred,2,data_list_for_stan_lpp$amp, FUN = "-")

bayes_R2_res <- function(fit) {
  y <- rstanarm::get_y(fit)
  ypred <- rstanarm::posterior_epred(fit)
  if (family(fit)$family == "binomial" && NCOL(y) == 2) {
    trials <- rowSums(y)
    y <- y[, 1]
    ypred <- ypred %*% diag(trials)
  }
  e <- -1 * sweep(ypred, 2, y)
  var_ypred <- apply(ypred, 1, var)
  var_e <- apply(e, 1, var)
  var_ypred / (var_ypred + var_e)
}

bayes_R2_res(model003_lpp_fit)

# Apply the function across all posterior samples
r2_posterior <- apply(mu_pred, 1, posterior_r2, observed = data_list_for_stan_lpp$amp)



model003_lpp_fit_draws %>%
  select(starts_with("par_mu_amp[")) %>%
  mutate(sample = 1:nrow(.), .before = 1) %>%
  pivot_longer(cols = starts_with("par_mu_amp["), names_to = "name", values_to = "value") %>%
  mutate(par_id = sub(".*\\[(.*)\\]", "\\1", name),
         amp = value) %>%
  select(sample, par_id, amp)




# Figure 1 is the paradigm already made ####



# Figure 2 Category rating dot plot####
dot_size <- 7
dodge_size <- .7
text_size <- 25
legend_tite_text_size <- 30
axis_line_thickness <- 1
valence_colors <- c("blue1","black", "red1", "white")
color_blind_valence_colors <- c("#0072B2","#009E73","#D55E00","ivory4")


gm_ratings_long <- ratings_data %>% 
  group_by(Stim_cat,Stim_type) %>% 
  summarise(mean_val = mean(valence),
            se_val = plotrix::std.error(valence),
            mean_aro = mean(arousal),
            se_aro = plotrix::std.error(arousal)) %>% 
  pivot_longer(
    cols = starts_with("mean_") | starts_with("se_"),
    names_to = c(".value", "rating"),
    names_pattern = "(mean_|se_)(.*)"
  ) %>% 
  rename(mean_rating = mean_,
         se_rating = se_) 

ratings_cat_breaks <- seq(1, 9, by = 1)
ratings_cat_limits <- c(1,9)

arousal_plot <- gm_ratings_long %>% 
  filter(rating == "aro") %>% 
  ggplot() +
  # geom_line(aes(x = Stim_cat,
  #               y = mean_rating, 
  #               group = Stim_type),
  #           linetype = "dotted",
  #           position = position_dodge(width = dodge_size)) + 
  geom_point(aes(x = Stim_cat, shape = Stim_type,
                 y = mean_rating),
             color = "black",
             position = position_dodge(width = dodge_size),
             size = dot_size + 1) +
  geom_point(aes(x = Stim_cat, shape = Stim_type,
                 y = mean_rating,
                 color = Stim_cat),
             position = position_dodge(width = dodge_size),
             size = dot_size) +
  geom_text(aes(x =2, y = 8.5), 
            label = "Arousal",
            fontface = "bold", 
            family = "Arial",
            size = 10) +
  geom_text(aes(x = 2, y = 1), 
            label = "Unpleasant",
            fontface = "bold", 
            family = "Arial",
            color = "red1",
            size = 10) +
  geom_text(aes(x = 2, y = 1.5), 
            label = "Neutral",
            fontface = "bold", 
            family = "Arial",
            color = "black",
            size = 10) +
  geom_text(aes(x = 2, y = 2), 
            label = "Pleasant",
            fontface = "bold", 
            family = "Arial",
            color = "blue1",
            size = 10) +
  scale_y_continuous(breaks = ratings_cat_breaks,
                     limits = ratings_cat_limits,
                     name = "SAM Rating",) +
  scale_shape_manual(name = "Stimulus Modality", values = c(15,19), labels = c("Scenes", "Videos")) +
  scale_color_manual(values = c(valence_colors),) +
  scale_x_discrete(name = "Category", 
                   labels = c("Pleasant", "Neutral", "Unpleasant    "),
                   guide = guide_axis(n.dodge = 2)) +
  guides(color = "none") +
  theme_classic() +
  # ggtitle("Arousal") +
  theme(#legend.position = c(.5,.97), 
    legend.title.align = .5,
    # plot.title = element_text(hjust = .5, vjust = .5,
    #                           size = legend_tite_text_size, 
    #                           family = "Arial", 
    #                           face = "bold"),
    legend.text = element_text(size = legend_tite_text_size, 
                               family = "Arial", 
                               face = "bold"),
    #legend.justification = c(.5,1)
    #,legend.key.height = unit(.5, "cm")
    legend.box.margin = margin(-20, 0, 0, 0),
    text = element_text(size = text_size, family = "Arial", face = "bold"),
    axis.line = element_line(size = axis_line_thickness,
                             lineend = "square"),
    axis.ticks = element_blank(),
    axis.text = element_text(size = text_size, 
                             family = "Arial", 
                             face = "bold",
                             color = "black"),
    axis.text.x = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = legend_tite_text_size, family = "Arial", face = "bold")) +
  guides(shape = guide_legend(direction = "horizontal", title.position = "top"))

valence_plot <- gm_ratings_long %>% 
  filter(rating == "val") %>% 
  ggplot() +
  # geom_line(aes(x = Stim_cat,
  #               y = mean_rating, 
  #               group = Stim_type),
  #           linetype = "dotted",
  #           position = position_dodge(width = 0.4)) + 
  geom_point(aes(x = Stim_cat, shape = Stim_type,
                 y = mean_rating),
             color = "black",
             position = position_dodge(width = dodge_size),
             size = dot_size + 1) + 
  geom_point(aes(x = Stim_cat, shape = Stim_type,
                 y = mean_rating,
                 color = Stim_cat),
             position = position_dodge(width = dodge_size),
             size = dot_size) + 
  geom_text(aes(x =2, y = 8.5), 
            label = "Valence",
            fontface = "bold", 
            family = "Arial",
            size = 10) +
  # annotate("text", x = 2, y = 0.5, label = "Stimulus X Valence\nF(2,94) = 2.23, p = .12;\nηg2 = .023", size = 8) +
  scale_y_continuous(breaks = ratings_cat_breaks,
                     limits = ratings_cat_limits) +
  scale_shape_manual(name = "Ratings by Stimulus Modality", values = c(15,19), labels = c("Scenes", "Videos")) +
  scale_color_manual(values = c(valence_colors),) +
  scale_x_discrete(name = "Category", 
                   labels = c("Pleasant", "Neutral", "Unpleasant    "),
                   guide = guide_axis(n.dodge = 2)) +
  guides(color = "none") +
  theme_classic() +
  # ggtitle("Valence") +
  theme(legend.position = c(.5,.5), 
        legend.justification = c(0.5, 3.1),
        legend.box.just = "center",
        # legend.title.align = .5,
        legend.text = element_text(size = legend_tite_text_size, 
                                   family = "Arial", 
                                   face = "bold"),
        legend.text.align = .5,
        legend.title = element_blank(),
        # plot.title = element_text(hjust = .5, vjust = .5,
        #                           size = legend_tite_text_size, 
        #                           family = "Arial", 
        #                           face = "bold"),
        #legend.justification = c(.5,1)
        #,legend.key.height = unit(.5, "cm")
        # legend.box.margin = margin(-20, 0, 0, 0),
        text = element_text(size = text_size, family = "Arial", face = "bold"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = text_size, 
                                 family = "Arial", 
                                 face = "bold",
                                 color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank()) +
  guides(shape = guide_legend(direction = "vertical", 
                              title.position = "top"))




# layout <- "
# AA
# BC
# BC
# BC
# BC
# BC
# BC
# BC
# BC
# BC
# BC
# BC
# BC
# BC
# BC
# BC
# BC
# BC
# BC
# BC
# BC
# BC
# "

# layout <- "
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# AB
# CC
# "

layout <- c("
AB
")

tiff(filename = paste0(parent_directory,
                       "/misc/002figure_aro_val_cate_dot.tiff"),
     width = 7, height = 8, units = "in", res = 300)
  arousal_plot + valence_plot + 
    plot_annotation(title = "Emotional Ratings of Stimuli",
                    theme = theme(
                      plot.title = element_text(face = "bold",hjust = .65, 
                                                size = legend_tite_text_size, 
                                                family = "Arial")
                    )) +
    plot_layout(design = layout)
dev.off()

# tiff(filename = paste0(parent_directory, 
#                        "/misc/002figure_aro_val_cate_dot.tiff"),
#      width = 6.5, height = 8, units = "in", res = 300)
#   arousal_plot + valence_plot + guide_area() +
#     plot_layout(design = layout,guides = "collect")
# dev.off()

svg(filename = paste0(parent_directory, 
                       "/misc/002figure_aro_val_cate_dot.svg"),
     width = 6.5, height = 8)#, units = "in", res = 300)
  guide_area() + arousal_plot + valence_plot +
    plot_layout(design = layout,guides = "collect")
dev.off()



# Figure 3 Video x picture ratings scatter####
library(grid)
library(jpeg)
# text_size <- 30
# fig7_colors <- c("darkblue",
#                  "blue1", 
#                  "black", 
#                  "red1", 
#                  "darkred")
# valence_colors <- c("blue1","black", "red1", "white")
rec_x <- .21
rec_y <- .135

inner_raster_text_size <- 7
scene_dot_size <- .20
# text_size <- 20
text_size <- 15

arousal_by_modality <- by_scene_ratings_with_path %>% 
  pivot_wider(id_cols = c("Stim_name", "path", "Stim_cat"),
              names_from = Stim_type, 
              values_from = mean_aro)

valence_by_modality <- by_scene_ratings_with_path %>% 
  pivot_wider(id_cols = c("Stim_name", "path", "Stim_cat"),
              names_from = Stim_type, 
              values_from = mean_val)

# arousal_scatter <- 
arousal_by_modality %>% 
  ggplot(aes(x = Pics, 
             y = Video,
             color = Stim_cat)) +
  geom_point() +
  scale_x_continuous(breaks = seq(2, 8, by = 1),
                     limits = c(2.5,8.5),
                     expand = c(0,0),
                     name = "Scene Arousal Rating") +
  scale_y_continuous(breaks = seq(2, 8, by = 1),
                     limits = c(2.5,8.5),
                     expand = c(0,0),
                     name = "Video Arousal Rating") +
  theme_classic() +
  theme(text = element_text(size = text_size, 
                            family = "Arial", 
                            face = "bold"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "none")



## raster below #### 

arousal_raster <- arousal_by_modality %>%
  ggplot(aes(x = Pics, y = Video)) +
  geom_blank() +
  scale_x_continuous(breaks = seq(2, 8, by = 1),
                     limits = c(2.5,8.5),
                     expand = c(0,0),
                     name = "Scene Arousal Rating") +
  scale_y_continuous(breaks = seq(2, 8, by = 1),
                     limits = c(2.5,8.5),
                     expand = c(0,0),
                     name = "Video Arousal Rating") +
  theme_classic() +
  theme(text = element_text(size = text_size, 
                            family = "Arial", 
                            face = "bold"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "none")


for (i in 1:90) {
  img <- arousal_by_modality$path[i] %>% 
    readJPEG() %>% 
    rasterGrob(interpolate=TRUE)
  if (arousal_by_modality$Stim_cat[i] == "Pleasant"){
    current_val_color = "blue1"
  } else if (arousal_by_modality$Stim_cat[i] == "Neutral"){
    current_val_color = "black"
  } else {
    current_val_color = "red1"
  }
  
  arousal_raster <- arousal_raster + 
    geom_rect(fill = current_val_color,
              xmin = arousal_by_modality$Pics[i] - rec_x, 
              xmax = arousal_by_modality$Pics[i] + rec_x, 
              ymin = arousal_by_modality$Video[i] - rec_y, 
              ymax = arousal_by_modality$Video[i] +rec_y) +
    annotation_custom(img, 
                      xmin = arousal_by_modality$Pics[i] -scene_dot_size, 
                      xmax = arousal_by_modality$Pics[i] +scene_dot_size, 
                      ymin = arousal_by_modality$Video[i] -scene_dot_size, 
                      ymax = arousal_by_modality$Video[i] +scene_dot_size)
  print(i)
}

# jpeg(filename = paste0(parent_directory, "/misc/arousal_raster.jpg"),
#      width = 8, height = 8, units = "in", res = 300)
arousal_raster <- arousal_raster +
  geom_line(data = data.frame(x = seq(1,9,by =.1),
                              y = seq(1,9,by =.1)),
            aes(x, y), linetype = "dashed", alpha = .5,
            linewidth = axis_line_thickness)
  # annotate("text", x = 4.25, y = 8,
  #          label = "Video More Arousing", 
  #          size = inner_raster_text_size) +
  # annotate("text", x = 4, y = 7,
  #          label = "Paired T-Test\nt(89) = 5.8, p < .001\nD = .19", size = 8) +
  # annotate("text", x = 6.75, y = 3,
  #          label = "Scene More Arousing", 
  #          size = inner_raster_text_size)
# dev.off()

valence_raster <- valence_by_modality %>% 
  ggplot(aes(Pics, Video)) +
  geom_blank() +
  scale_x_continuous(breaks = seq(2, 8, by = 1),
                     limits = c(2.5,8.5),
                     expand = c(0,0),
                     name = "Scene Valence Rating") +
  scale_y_continuous(breaks = seq(2, 8, by = 1),
                     limits = c(2.5,8.5),
                     expand = c(0,0),
                     name = "Video Valence Rating") +
  theme_classic() +
  theme(text = element_text(size = text_size, family = "Arial", face = "bold"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "none")


for (i in 1:90) {
  img <- valence_by_modality$path[i] %>% 
    readJPEG() %>% 
    rasterGrob(interpolate=TRUE)
  
  if (valence_by_modality$Stim_cat[i] == "Pleasant"){
    current_val_color = "blue1"
  } else if (valence_by_modality$Stim_cat[i] == "Neutral"){
    current_val_color = "black"
  } else {
    current_val_color = "red1"
  }
  
  valence_raster <- valence_raster + 
    geom_rect(fill = current_val_color,
              xmin = valence_by_modality$Pics[i] - rec_x, 
              xmax = valence_by_modality$Pics[i] + rec_x, 
              ymin = valence_by_modality$Video[i] - rec_y, 
              ymax = valence_by_modality$Video[i] +rec_y) +
    annotation_custom(img, 
                      xmin = valence_by_modality$Pics[i] -scene_dot_size, 
                      xmax = valence_by_modality$Pics[i] +scene_dot_size, 
                      ymin = valence_by_modality$Video[i] -scene_dot_size, 
                      ymax = valence_by_modality$Video[i] +scene_dot_size)
  print(i)
}

# jpeg(filename = paste0(parent_directory, "/misc/valence_raster.jpg"),
#      width = 8, height = 8, units = "in", res = 300)
valence_raster <- valence_raster +
  geom_line(data = data.frame(x = seq(1,9,by =.1),
                              y = seq(1,9,by =.1)),
            aes(x, y), linetype = "dashed", alpha = .5,
            linewidth = axis_line_thickness)
  # annotate("text", x = 4.25, y = 8,
  #          label = "Video More Pleasant", 
  #          size = inner_raster_text_size) +
  # annotate("text", x = 4, y = 7,
  #          label = "Paired T-Test\nt(89) = 2.1, p = .04\nD = .04", size = 8) +
  # annotate("text", x = 6.75, y = 3,
  #          label = "Scene More Pleasant", 
  #          size = inner_raster_text_size,)
# dev.off()


# layout <- "
# AB
# "
# 
# guide_area() +
#   arousal_plot + valence_plot + 
#   arousal_raster + valence_raster +
#   plot_layout(design = layout,guides = "collect")




tiff(filename = paste0(parent_directory, 
                       "/misc/003figure_by_scene_ratings.tiff"),
     width = 8.5, height = 4, units = "in", res = 300)
arousal_raster + valence_raster
dev.off()

svg(filename = paste0(parent_directory, 
                       "/misc/003figure_by_scene_ratings.svg"),
     width = 8.5, height = 4)#, units = "in", res = 300)
arousal_raster + valence_raster
dev.off()

# Figure 4 top:waveform category dot plot, bottom: erp X arousal cor####

### ERP dot plot ####
y_axis_breaks <- seq(-.9, .9, by = .2)
y_axis_limits <- c(-1,1)
text_size = 20
axis_line_thickness = 1
valence_colors <- c("blue1","black", "red1", "white")
# color_blind_valence_colors <- c("#0072B2","#009E73","#D55E00","ivory4")


gm_amp_dot_plot <- ggplot(gm_amp_long) +
  geom_line(aes(x = category,
                y = mean_amp, 
                group = erp_type),
            linetype = "dotted",
            position = position_dodge(width = 0.4)) + 
  geom_pointrange(aes(x = category, shape = erp_type,
                      y = mean_amp, ymax = mean_amp + se_amp,
                      ymin = mean_amp - se_amp),
                  color = "black",
                  position = position_dodge(width = 0.4),
                  size = 2.5, linewidth = 4) + 
  geom_pointrange(aes(x = category, shape = erp_type,
                      y = mean_amp, ymax = mean_amp + se_amp,
                      ymin = mean_amp - se_amp,
                      color = category),
                  position = position_dodge(width = 0.4),
                  size = 2, linewidth = 2) + 
  # annotate("text", x = 1.95, y = 0.45, 
  #          label = "Stimulus X Valence\nF(2,94) = 2.23, p = .12;\nηg2 = .023", 
  #          size = 8) +
  scale_y_continuous(breaks = y_axis_breaks,
                     name = "Scene Z-score",
                     limits = y_axis_limits,
                     expand = c(0,0),
                     sec.axis = sec_axis(trans = ~ . * -1, 
                                         name = "Video Z-score",
                                         breaks = -y_axis_breaks)) +
  scale_shape_manual(values = c(15,19), labels = c("Scene LPP", "Video ssVEP")) +
  scale_color_manual(values = c(valence_colors),) +
  scale_x_discrete(name = "Category", 
                   labels = c("Pleasant", "Neutral", "Unpleasant")#,guide = guide_axis(n.dodge = 2)
                   ) +
  guides(color = "none") +
  theme_classic() +
  theme(legend.position = c(.5,.97), 
        legend.title = element_blank(),
        legend.justification = c(.5,1)
        ,legend.key.height = unit(.9, "cm")
        , legend.box.margin = margin(-15, 0, 0, 0),
        text = element_text(size = text_size, family = "Arial", face = "bold"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(color = "black")) +
  guides(shape = guide_legend(direction = "vertical", title.position = "top"))

gm_amp_dot_plot

### ERP waveforms ####
line_width <- 2
line_outline <- .5

lpp_cat_wave_plot <- lpp_cat_wave %>% 
  ggplot() +
  geom_line(data = data.frame(x = c(0,0), 
                              y = c(-Inf, Inf)), 
            aes(x = x, y = y), linetype = "dashed") +
  geom_rect(aes(xmin = 400, xmax = 900, ymin = -Inf, ymax = Inf),
            fill = "lightgray")+
  geom_line(aes(x = time_ms, y = amp, group = category),
            color = "black",
            linewidth = line_width + line_outline) +
  # annotate("text", x = 140, y = 1.15, face = "bold",
  #          label = "Scene Evoked LPP\nF(2,94) = 49.03, p < .001;\n ηg2 = .511", size = 8) +
  geom_line(aes(x = time_ms, y = amp, color = category),
            linewidth = line_width) +
  geom_text(aes(x = 200, y = 1.75), 
            label = "Pleasant",
            fontface = "bold", 
            family = "Arial",
            color = "blue1",
            size = 10) +
  geom_text(aes(x = 200, y = 1.25), 
            label = "Neutral",
            fontface = "bold", 
            family = "Arial",
            color = "black",
            size = 10) +
  geom_text(aes(x = 200, y = .75), 
            label = "Unpleasant",
            fontface = "bold", 
            family = "Arial",
            color = "red1",
            size = 10) +
  scale_y_continuous(limits = c(-2.3, 2),
                     expand = c(0,0), name = "LPP (μV)") +
  scale_x_continuous(limits = c(-125,1000), expand = c(0,0),
                     breaks = seq(-100, 900, by = 100), name = "Time (msec)") +
  scale_color_manual(values = c(valence_colors)) +
  theme_classic() +
  # ggtitle("Scene Evoked LPP \n F(2,94) = 49.03, p < .001") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5, vjust = 1),
        text = element_text(size = text_size, family = "Arial", face = "bold"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"))


ssvep_cat_wave_plot <- ssvep_cat_wave %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_rect(aes(xmin = 1000, xmax = 9000, ymin = -Inf, ymax = Inf),
            fill = "lightgray")+
  geom_line(aes(x = time_ms, y = amp, group = category),
            color = "black",
            linewidth = line_width + line_outline) +
  geom_line(aes(x = time_ms, y = amp, color = category),
            linewidth = line_width) +
  # annotate("text", x = 5000, y = 1.25, label = "Video Reduction in ssVEP\nF(2,94) = 49.07, p < .001; ηg2 = .511", size = 8) +
  scale_y_continuous(limits = c(.9, 1.25),
                     expand = c(0,0), name = "ssVEP (μV)") +
  scale_x_continuous(limits = c(-2000,10000),expand = c(0,0),
                     breaks = seq(-1000, 9000, by = 1000),
                     labels = c(-1:9),
                     name = "Time (sec)"#,guide = guide_axis(n.dodge = 2)
                     ) +
  scale_color_manual(values = c("blue1","black", "red1")) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = text_size, family = "Arial", face = "bold"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"))


layout <- "
AAAAAAAAABBB
CCCCCCCCCBBB
"

tiff(filename = paste0(parent_directory,
                       "/misc/004figure_cate_wave_dot.tiff"),
     width = 11, height = 7, units = "in", res = 300)
lpp_cat_wave_plot + gm_amp_dot_plot + ssvep_cat_wave_plot +
  plot_layout(design = layout)
dev.off()

svg(filename = paste0(parent_directory,
                       "/misc/004figure_cate_wave_dot.svg"),
     width = 11, height = 7)#, units = "in", res = 300)
lpp_cat_wave_plot + gm_amp_dot_plot + ssvep_cat_wave_plot +
  plot_layout(design = layout)
dev.off()


# Figure 5 z-scored amps X arousal####
ratings_erps_path_by_scene %>% 
  filter(Stim_type =="Pics") %>% 
  summarise(correlation_test = list(cor.test(.$zscore_amp, .$mean_aro))) %>% 
  pull(correlation_test)

ratings_erps_path_by_scene %>% 
  filter(Stim_type =="Video") %>% 
  summarise(correlation_test = list(cor.test(.$zscore_amp, .$mean_aro))) %>% 
  pull(correlation_test)

dot_size <- 7
dodge_size <- .7
text_size = 15
axis_line_thickness <- 1

inner_raster_text_size <- 6
scene_dot_size <- .20
rec_x <- .21
rec_y <- .047

arousal_lpp_raster <- ratings_erps_path_by_scene %>% 
  filter(Stim_type == "Pics") %>% 
  ggplot(aes(mean_aro, zscore_amp)) +
  geom_blank() +
  scale_x_continuous(breaks = seq(3, 8, by = 1),
                     limits = c(3,8),
                     expand = c(0,0),
                     name = "Scene Arousal Rating") +
  scale_y_continuous(limits = c(-.85,1.05),
                     breaks = seq(-.75,1, by = .25),
                     expand = c(0,0),
                     name = "Z-scored LPP") +
  theme_classic() +
  theme(text = element_text(size = text_size, family = "Arial", face = "bold"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "none")

picture_modality <- ratings_erps_path_by_scene %>% 
  filter(Stim_type == "Pics")

for (i in 1:90) {
  img <- picture_modality$path[i] %>% 
    readJPEG() %>% 
    rasterGrob(interpolate=TRUE)
  
  if (picture_modality$Stim_cat[i] == "Pleasant"){
    current_val_color = "blue1"
  } else if (picture_modality$Stim_cat[i] == "Neutral"){
    current_val_color = "black"
  } else {
    current_val_color = "red1"
  }

  arousal_lpp_raster <- arousal_lpp_raster + 
    geom_rect(fill = current_val_color,
              xmin = picture_modality$mean_aro[i] - rec_x, 
              xmax = picture_modality$mean_aro[i] + rec_x, 
              ymin = picture_modality$zscore_amp[i] - rec_y, 
              ymax = picture_modality$zscore_amp[i] +rec_y) +
    annotation_custom(img, 
                      xmin = picture_modality$mean_aro[i] -scene_dot_size, 
                      xmax = picture_modality$mean_aro[i] + scene_dot_size, 
                      ymin = picture_modality$zscore_amp[i] - scene_dot_size, 
                      ymax = picture_modality$zscore_amp[i] + scene_dot_size)
  print(i)
}

# jpeg(filename = paste0(parent_directory, "/misc/zarousal_lpp_raster.jpg"),
#      width = 8, height = 8, units = "in", res = 300)
arousal_lpp_raster <- arousal_lpp_raster + 
  geom_line(stat = "smooth", method = "lm", aes(group = 1), 
            alpha = .5, se = F, color = "black", linewidth = axis_line_thickness) +
  geom_ribbon(stat = "smooth", method = "lm",
              aes(ymin = ..y.. - (1.96 * ..se..), # make 95% CI
                  ymax = ..y.. + (1.96 * ..se..)), 
              alpha = .2, color = "black", linetype = "blank") +
  annotate("text", x = 4.77, y = .87,
           label = "paste(italic('r'),'(88) = .52, ', italic('p'), ' < .001')",
           parse = TRUE, 
           size = inner_raster_text_size)
# dev.off()

arousal_ssvep_raster <- ratings_erps_path_by_scene %>% 
  filter(Stim_type == "Video") %>% 
  ggplot(aes(mean_aro, -1*zscore_amp)) +
  geom_blank() +
  scale_x_continuous(breaks = seq(3, 8, by = 1),
                     limits = c(3,8),
                     expand = c(0,0),
                     name = "Video Arousal Rating") +
  scale_y_continuous(limits = c(-.85,1.05),
                     breaks = seq(-.75,1, by = .25),
                     expand = c(0,0),
                     name = "Reversed Z-scored ssVEP") +
  theme_classic() +
  theme(text = element_text(size = text_size, family = "Arial", face = "bold"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "none")

video_modality <- ratings_erps_path_by_scene %>% 
  filter(Stim_type == "Video")

for (i in 1:90) {
  img <- video_modality$path[i] %>% 
    readJPEG() %>% 
    rasterGrob(interpolate=TRUE)
  
  if (picture_modality$Stim_cat[i] == "Pleasant"){
    current_val_color = "blue1"
  } else if (picture_modality$Stim_cat[i] == "Neutral"){
    current_val_color = "black"
  } else {
    current_val_color = "red1"
  }
  
  arousal_ssvep_raster <- arousal_ssvep_raster + 
    geom_rect(fill = current_val_color,
              xmin = video_modality$mean_aro[i] - rec_x, 
              xmax = video_modality$mean_aro[i] + rec_x, 
              ymin = (-1*video_modality$zscore_amp[i]) - rec_y, 
              ymax = (-1*video_modality$zscore_amp[i]) +rec_y) +
    annotation_custom(img, 
                      xmin = video_modality$mean_aro[i] - scene_dot_size, 
                      xmax = video_modality$mean_aro[i] + scene_dot_size, 
                      ymin = (-1*video_modality$zscore_amp[i]) - scene_dot_size, 
                      ymax = (-1*video_modality$zscore_amp[i]) + scene_dot_size)
  print(i)
}

# jpeg(filename = paste0(parent_directory, "/misc/zarousal_ssvep_raster.jpg"),
#      width = 8, height = 8, units = "in", res = 300)
arousal_ssvep_raster <- arousal_ssvep_raster + 
  geom_line(stat = "smooth", method = "lm", aes(group = 1), 
            alpha = .5, se = F, color = "black", linewidth = axis_line_thickness) +
  geom_ribbon(stat = "smooth", method = "lm",
              aes(ymin = ..y.. - (1.96 * ..se..), # make 95% CI
                  ymax = ..y.. + (1.96 * ..se..)), 
              alpha = .2, color = "black", linetype = "blank") +
  annotate("text", x = 4.77, y = .87,
           label = "paste(italic('r'),'(88) = .63',', ', italic('p'), ' < .001')",
           parse = T,
           size = inner_raster_text_size)
# dev.off()

tiff(filename = paste0(parent_directory, 
                       "/misc/005figure_arousalXz-score_amp.tiff"),
     width = 8.5, height = 4, units = "in", res = 300)
arousal_lpp_raster + arousal_ssvep_raster
dev.off()

svg(filename = paste0(parent_directory, 
                       "/misc/005figure_arousalXz-score_amp.svg"),
     width = 8.5, height = 4)
arousal_lpp_raster + arousal_ssvep_raster
dev.off()


#Figure 6 model1 valence####

pleasant_stimuli_ids <- data_for_stan_df %>% 
  filter(cate == 1) %>% 
  pull(stim) %>% 
  unique() %>% 
  paste0("bstim[",.,"]")
  
neutral_stimuli_ids <- data_for_stan_df %>% 
  filter(cate == 2) %>% 
  pull(stim) %>% 
  unique() %>% 
  paste0("bstim[",.,"]")

unpleasant_stimuli_ids <- data_for_stan_df %>% 
  filter(cate == 3) %>% 
  pull(stim) %>% 
  unique() %>% 
  paste0("bstim[",.,"]")

lpp_valence_posteriors <- 
  #model003_lpp_fit_draws %>% 
  model012_lpp_fit_draws %>% 
  mutate(., sample = 1:nrow(.)) %>% 
  select(sample, starts_with("bstim")) %>% 
  pivot_longer(cols = starts_with("bstim")) %>% 
  mutate(name = case_when(
    name %in% pleasant_stimuli_ids ~ "pleasant",
    name %in% neutral_stimuli_ids ~ "neutral",
    name %in% unpleasant_stimuli_ids ~ "unpleasant")) %>% 
  group_by(sample, name) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() 

lpp_valence_posteriors <- 
  lpp_valence_posteriors %>% 
  group_by(sample) %>% 
  summarise(value = 
              mean(value[name == "pleasant"], 
                   value[name == "unpleasant"]) -
              value[name == "neutral"]) %>% 
  ungroup() %>% 
  mutate(name = "emotional_difference",.after = sample) %>% 
  rbind.data.frame(lpp_valence_posteriors) %>% 
  mutate(name = factor(name, 
                       levels = c("pleasant",
                                  "neutral",
                                  "unpleasant",
                                  "emotional_difference"))) 

ssvep_valence_posteriors <- 
  #model003_ssvep_fit_draws %>% 
  model012_ssvep_fit_draws %>%  
  mutate(., sample = 1:nrow(.)) %>% 
  select(sample, starts_with("bstim")) %>% 
  pivot_longer(cols = starts_with("bstim")) %>% 
  mutate(name = case_when(
    name %in% pleasant_stimuli_ids ~ "pleasant",
    name %in% neutral_stimuli_ids ~ "neutral",
    name %in% unpleasant_stimuli_ids ~ "unpleasant")) %>% 
  group_by(sample, name) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() 

ssvep_valence_posteriors <- 
  ssvep_valence_posteriors %>% 
  group_by(sample) %>% 
  summarise(value = 
              mean(value[name == "pleasant"], 
                   value[name == "unpleasant"]) -
              value[name == "neutral"]) %>% 
  ungroup() %>% 
  mutate(name = "emotional_difference",.after = sample) %>% 
  rbind.data.frame(ssvep_valence_posteriors) %>% 
  mutate(name = factor(name, 
                       levels = c("pleasant",
                                  "neutral",
                                  "unpleasant",
                                  "emotional_difference"))) 


#plot elements
par_fill <- "gold1"
# valence_colors <- c("#0072B2","#009E73","#D55E00","ivory4")

line_thickness <- 2
density_line_thickness <- 1
panel.grid.major.x_line_thickness <- .5
par_pos_scale <- 2
par_pos_y_limits <- c(.75, 47.5)
ssvep_par_breaks_labels <- seq(.4, 1.6, by = .2)
par_alpha <- .8
font_size <- 22
font_font <- "Arial"

# layout_grid <- c("
# ef
# ef
# ef
# ef
# ef
# ef
# ef
# ef
# ef
# ef
# ef
# gg
# gg
# ac
# ac
# ac
# ac
# ac
# ac
# ac
# ac
# ac
# ac
# ac
# ac
# ac
# ac
# ac
# ac
# ac
# bd
# bd
# bd
# ")

# layout_grid <- c("
# ab
# ab
# ab
# ab
# ab
# ab
# ab
# ab
# ab
# ab
# ab
# ab
# cc
# df
# df
# df
# df
# df
# df
# df
# df
# df
# df
# df
# df
# df
# df
# eg
# eg
# eg
# ")

layout_grid <- c("
aa
bc
bc
bc
bc
bc
bc
bc
bc
bc
bc
bc
bc
df
df
df
df
df
df
df
df
df
df
df
df
df
df
eg
eg
eg
")

set.seed(0)
guide_area() +
(lpp_valence_posteriors %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0),
             linewidth = line_thickness) +
  geom_density(aes(x = value, 
                   fill = name),
               alpha = par_alpha,
               linewidth = density_line_thickness) +
  scale_fill_manual(values = valence_colors,
                    name = "Categories",
                    labels = c("Pleasant", 
                               "Neutral", 
                               "Unpleasant",
                               "Emotional vs Neutral")) +
  scale_x_continuous(name = "Δ LPP Microvoltage",
                     breaks = seq(-2, 2, by = 1),
                     labels = seq(-2, 2, by = 1),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.02)) +
  coord_cartesian(xlim = c(-2.5,2.5)) +
  ggtitle("LPP Valence Posteriors") +
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black",
                               margin = margin(t = 5, 
                                               unit = "pt")),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    line = element_line(linewidth = line_thickness,
                        lineend = "square"),
    text = element_text(size = font_size,
                        color = "black"),
    plot.title = element_text(hjust = 0.5,
                              face = "bold"),
    # legend.title = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom", 
    legend.text = element_text(angle = 0, size = font_size),
    # legend.title.align = 0,
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid.major.x = element_line(color = "black",
                                      linetype = "dotted",
                                      linewidth = 
                                        panel.grid.major.x_line_thickness))+ 
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))) +
  
  ssvep_valence_posteriors %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0),
             linewidth = line_thickness) +
  geom_density(aes(x = value, 
                   fill = name),
               alpha = par_alpha,
               linewidth = density_line_thickness) +
  scale_fill_manual(values = valence_colors,
                    name = "Categories",
                    labels = c("Pleasant", 
                               "Neutral", 
                               "Unpleasant",
                               "Emotional vs Neutral")) +
  scale_x_continuous(name = "Δ ssVEP Microvoltage",
                     breaks = seq(-.04, .04, by = .02),
                     labels = seq(-.04, .04, by = .02),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.1)) +
  coord_cartesian(xlim = c(-.0575, .0575)) +
  ggtitle("ssVEP Valence Posteriors") +
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black",
                               margin = margin(t = 5, 
                                               unit = "pt")),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    line = element_line(linewidth = line_thickness,
                        lineend = "square"),
    text = element_text(size = font_size,
                        color = "black"),
    plot.title = element_text(hjust = 0.5,
                              face = "bold"),
    # legend.title = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "top", 
    legend.text = element_text(angle = 0, size = font_size),
    # legend.title.align = 0,
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid.major.x = element_line(color = "black",
                                      linetype = "dotted",
                                      linewidth = 
                                        panel.grid.major.x_line_thickness))+ 
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  
  #model003_lpp_fit_draws %>% 
  model012_lpp_fit_draws %>% 
  select(starts_with("bpar")) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(name = factor(name,
                       levels = unique(name))) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 
                   # mean(model003_lpp_fit_draws$par_mean)),
                   mean(model012_lpp_fit_draws$par_mean)),
             linewidth = line_thickness) +
  geom_density_ridges(aes(x = value, 
                          y = name),
                      alpha = par_alpha,
                      fill = par_fill,
                      rel_min_height = 0.01,
                      size = density_line_thickness,
                      color = "black",
                      scale = par_pos_scale) +
  scale_x_continuous(name = "Microvoltage",
                     breaks = seq(-5, 5, by = 1),
                     labels = seq(-5, 5, by = 1),
                     expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  coord_cartesian(xlim = c(-5.5, 5.5),
                  ylim = par_pos_y_limits) +
  ggtitle("Mean LPP per Participant") +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        line = element_line(linewidth = line_thickness,
                            lineend = "square"),
        text = element_text(size = font_size,
                            family = font_font,
                            color = "black"),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "black",
                                          linetype = "dotted",
                                          linewidth = 
                                            panel.grid.major.x_line_thickness),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold")) +
  
  
# data.frame(name = "par_predictive",
#                value =rnorm(80000,
#                             model003_lpp_fit_draws$par_mean,
#                             model003_lpp_fit_draws$par_sd)) %>% 
data.frame(name = "par_predictive",
               value =rnorm(80000,
                            model012_lpp_fit_draws$par_mean,
                            model012_lpp_fit_draws$par_sd)) %>%
  ggplot() +
  geom_vline(aes(xintercept = 
                   # mean(model003_lpp_fit_draws$par_mean)),
                   mean(model012_lpp_fit_draws$par_mean)),
             linewidth = line_thickness) +
  geom_density(aes(x = value),
               alpha = par_alpha,
               fill = par_fill,
               linewidth = density_line_thickness) +
  scale_x_continuous(name = "Microvoltage",
                     breaks = seq(-5, 5, by = 1),
                     labels = seq(-5, 5, by = 1),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(-5.5, 5.5),
                  ylim = c(0,.27)) +
  ggtitle("LPP Participant Distribution") +
  theme_classic() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black",
                               margin = margin(t = 5, 
                                               unit = "pt")),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    line = element_line(linewidth = line_thickness,
                        lineend = "square"),
    text = element_text(size = font_size,
                        family = font_font,
                        color = "black"),
    panel.grid.major.x = element_line(color = "black",
                                      linetype = "dotted",
                                      linewidth = 
                                        panel.grid.major.x_line_thickness),
    plot.title = element_text(hjust = 0.5,
                              face = "bold")) +

  
# model003_ssvep_fit_draws %>% 
model012_ssvep_fit_draws %>%
  select(starts_with("bpar")) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(name = factor(name,
                       levels = unique(name))) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 
                   # mean(model003_ssvep_fit_draws$par_mean)),
                   mean(model012_ssvep_fit_draws$par_mean)),
             linewidth = line_thickness) +
  geom_density_ridges(aes(x = value, 
                          y = name),
                      alpha = par_alpha,
                      fill = par_fill,
                      rel_min_height = 0.01,
                      size = density_line_thickness,
                      color = "black",
                      scale = par_pos_scale) +
  scale_x_continuous(name = "Microvoltage",
                     breaks = ssvep_par_breaks_labels,
                     labels = ssvep_par_breaks_labels,
                     expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  coord_cartesian(xlim = c(.35,1.7),
                  ylim = par_pos_y_limits) +
  ggtitle("Mean ssVEP per Participant") +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        line = element_line(linewidth = line_thickness,
                            lineend = "square"),
        text = element_text(size = font_size,
                            family = font_font,
                            color = "black"),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "black",
                                          linetype = "dotted",
                                          linewidth = 
                                            panel.grid.major.x_line_thickness),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold"))  +
  
  
  data.frame(name = "par_predictive",
             value =rnorm(80000,
                          # model003_ssvep_fit_draws$par_mean,
                          # model003_ssvep_fit_draws$par_sd)) %>% 
                          model012_ssvep_fit_draws$par_mean,
                          model012_ssvep_fit_draws$par_sd)) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 
                   # mean(model003_ssvep_fit_draws$par_mean)),
                   mean(model012_ssvep_fit_draws$par_mean)),
             linewidth = line_thickness) +
  geom_density(aes(x = value),
               alpha = par_alpha,
               fill = par_fill,
               linewidth = density_line_thickness) +
  scale_x_continuous(name = "Microvoltage",
                     breaks = ssvep_par_breaks_labels,
                     labels = sprintf("%.1f", ssvep_par_breaks_labels),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(.35,1.7),
                  ylim = c(0, 1.8)) +
  ggtitle("ssVEP Participant Distribution") +
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black",
                               margin = margin(t = 5, 
                                               unit = "pt")),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    line = element_line(linewidth = line_thickness,
                        lineend = "square"),
    text = element_text(size = font_size,
                        family = font_font,
                        color = "black"),
    panel.grid.major.x = element_line(color = "black",
                                      linetype = "dotted",
                                      linewidth = 
                                        panel.grid.major.x_line_thickness),
    plot.title = element_text(hjust = 0.5,
                              face = "bold")) +
  plot_annotation(title = "Model 1 Posteriors\nSimilarly Strong Emotion Effects on Different Scales",
                  theme = theme(
                    plot.title = element_text(family = font_font,
                                              size = font_size + 7,
                                              color = "black",
                                              hjust = 0.5,
                                              face = "bold")
                  )) +
  plot_layout(design = layout_grid ,guides = "collect")




ggsave(filename = paste0(parent_directory, 
                         "/misc/006figure_model1_valence_pos_model12.tiff"),
       device = "tiff",dpi = 300,
       units = "in",height = 11, width = 8.5,
       scale = 1.275)

# ggsave(filename = paste0(parent_directory, 
#                          "/misc/006figure_model1_valence_pos.svg"),
#        device = "svg",height = 11, width = 8.5,
#        scale = 1.275)


# Figure 7 post hoc erotica gore posteriors ####

couple_indexes <- grepl(pattern = "ouple", x = data_for_stan_df$stim_name)
surgery_indexes <- grepl(pattern = "urgery", x = data_for_stan_df$stim_name)

couple_ids <- data_for_stan_df[couple_indexes,]$stim %>% unique()
surgery_ids <- data_for_stan_df[surgery_indexes,]$stim %>% unique()

posterior_erotica_ids <- data_for_stan_df %>% 
  filter(stim %in% couple_ids) %>% 
  pull(stim) %>% 
  unique() %>% 
  paste0("bstim[",.,"]")

posterior_pleasant_stimuli_ids_no_erotica <- data_for_stan_df %>% 
  filter(cate == 1,
         !stim %in% couple_ids) %>% 
  pull(stim) %>% 
  unique() %>% 
  paste0("bstim[",.,"]")

neutral_stimuli_ids <- data_for_stan_df %>% 
  filter(cate == 2) %>% 
  pull(stim) %>% 
  unique() %>% 
  paste0("bstim[",.,"]")

posterior_unpleasant_stimuli_ids_no_surgery <- data_for_stan_df %>% 
  filter(cate == 3,
         !stim %in% surgery_ids) %>% 
  pull(stim) %>% 
  unique() %>% 
  paste0("bstim[",.,"]")

posterior_surgery_ids <- data_for_stan_df %>% 
  filter(stim %in% surgery_ids) %>% 
  pull(stim) %>% 
  unique() %>% 
  paste0("bstim[",.,"]")

lpp_erot_surg_val_posteriors <- 
  # model003_lpp_fit_draws %>% 
  model012_lpp_fit_draws %>%
  mutate(., sample = 1:nrow(.)) %>% 
  select(sample, starts_with("bstim")) %>% 
  pivot_longer(cols = starts_with("bstim")) %>% 
  mutate(name = case_when(
    name %in% posterior_erotica_ids ~ "erotica",
    name %in% posterior_pleasant_stimuli_ids_no_erotica ~ "pleasant",
    name %in% neutral_stimuli_ids ~ "neutral",
    name %in% posterior_unpleasant_stimuli_ids_no_surgery ~ "unpleasant", 
    name %in% posterior_surgery_ids ~ "surgery")) %>% 
  group_by(sample, name) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  mutate(name = factor(name,
                       levels = c("erotica",
                                  "pleasant",
                                  "neutral",
                                  "unpleasant",
                                  "surgery")))



ssvep_erot_surg_val_posteriors <- 
  # model003_ssvep_fit_draws %>% 
  model012_ssvep_fit_draws %>%
  mutate(., sample = 1:nrow(.)) %>% 
  select(sample, starts_with("bstim")) %>% 
  pivot_longer(cols = starts_with("bstim")) %>% 
  mutate(name = case_when(
    name %in% posterior_erotica_ids ~ "erotica",
    name %in% posterior_pleasant_stimuli_ids_no_erotica ~ "pleasant",
    name %in% neutral_stimuli_ids ~ "neutral",
    name %in% posterior_unpleasant_stimuli_ids_no_surgery ~ "unpleasant", 
    name %in% posterior_surgery_ids ~ "surgery")) %>% 
  group_by(sample, name) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  mutate(name = factor(name,
                       levels = c("erotica",
                                  "pleasant",
                                  "neutral",
                                  "unpleasant",
                                  "surgery")))

# layout_grid = c("
# A
# A
# B
# B
# B
# B
# B
# B
# C
# C
# C
# C
# C
# C
# ")

layout_grid <- c("
A
B
")

fig7_colors <- c("darkblue",
                 "blue1", 
                 "black", 
                 "red1", 
                 "darkred")

# fig7_colors <- c("blue1",
#                  "blue1", 
#                  "black", 
#                  "red1", 
#                  "red1")

fig7_linetypes <- c(5,
                    1,
                    1,
                    1,
                    5)

fig7_alpha <- .75
fig7_density_line_thickness <- 2
line_thickness <- 1.5
text_size <- 20

# c(col2rgb("darkblue")/255)
# c(col2rgb("blue1")/255)
# c(col2rgb("black")/255)
# c(col2rgb("red1")/255)
# c(col2rgb("darkred")/255)
# 
# fig7_colors_alpha <- c(rgb(0, 0, .545098, .5),
#                           rgb(0, 0, 1,       .5), 
#                           rgb(0, 0, 0,       .5), 
#                           rgb(1, 0, 0,       .5), 
#                           rgb(.545098, 0, 0, .5))

# guide_area() +
fig7_lpp_top <- lpp_erot_surg_val_posteriors %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0),
             linewidth = line_thickness) +
  geom_density(aes(x = value, 
                   fill = name,
                   linetype = name),
               alpha = fig7_alpha,
               linewidth = fig7_density_line_thickness) +
  scale_fill_manual(values = fig7_colors,
                    name = "Categories",
                    labels = c("Erotica", 
                               "Pleasant without erotica", 
                               "Neutral",
                               "Unpleasant without surgeries",
                               "Surgeries")) +
  scale_linetype_manual(values = fig7_linetypes,
                        guide = "none") +
  scale_x_continuous(name = "Δ LPP Microvoltage",
                     breaks = seq(-1.5, 3.5, by = .5),
                     labels = seq(-1.5, 3.5, by = .5),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(-1.9, 3.8),
                  # ylim = c(0,1.93)) +
                  ylim = c(0, 2.01)) +
  # ggtitle("LPP Valence Posteriors") +
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black",
                               margin = margin(t = 5, 
                                               unit = "pt")),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    line = element_line(linewidth = line_thickness,
                        lineend = "square"),
    text = element_text(size = font_size,
                        color = "black"),
    # plot.title = element_text(hjust = 0.5,
    #                           face = "bold"),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "black",
                                      linetype = "dotted",
                                      linewidth = 
                                        panel.grid.major.x_line_thickness)) 
  
fig7_ssvep_bottom <- ssvep_erot_surg_val_posteriors %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0),
             linewidth = line_thickness) +
  geom_density(aes(x = value, 
                   fill = name,
                   linetype = name),
               alpha = fig7_alpha,
               linewidth = fig7_density_line_thickness) +
  geom_rect(aes(xmax = -.039, xmin = -.119,
                ymin = 30, ymax = 76),fill = "white") +
                # ymin = 24, ymax = 60),fill = "white") +
  scale_fill_manual(values = fig7_colors#,
                    # name = "Video ssVEP Does Not Have Typical Erotic and Gore Bias",
                    # labels = c("Erotica", 
                    #            "Pleasant without erotica", 
                    #            "Neutral",
                    #            "Unpleasant without surgeries",
                    #            "Surgeries")
                    ) +
  scale_linetype_manual(values = fig7_linetypes,
                        guide = "none") +
  scale_x_reverse(name = "Δ ssVEP Microvoltage (Axis Reversed)",
                  limits = c(.07,-.1206),
                  breaks = seq(.04, -.08, by = -.02),
                  expand = c(0,0),
                  labels = sprintf("%.2f", seq(.04, -.08, by = -.02))) +
  scale_y_continuous(expand = c(0,0)
                     # ,limits = c(0, 61)
                     ,limits = c(0, 77)
                     ) +
  coord_cartesian(xlim = c(.047, -.095),
                  ylim = c(0, 76)) +
  # ggtitle("ssVEP Valence Posteriors") +
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black",
                               margin = margin(t = 5, 
                                               unit = "pt")),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    axis.ticks.x = element_blank(),
    line = element_line(linewidth = line_thickness,
                        lineend = "square"),
    text = element_text(size = font_size,
                        color = "black"),
    plot.title = element_text(hjust = 0.5,
                              face = "bold"),
    panel.grid.major.x = element_line(color = "black",
                                      linetype = "dotted",
                                      linewidth = 
                                        panel.grid.major.x_line_thickness),
    legend.title = element_text(family = font_font,
                                size = font_size +2,
                                face = "bold"),
    legend.text = element_text(family = text_size)) + 
  # guides(fill = guide_legend(title.position = "top",
  #                            nrow = 2,
  #                            title.hjust = 0.5)) +
  geom_text(aes(x = -.06, y = 71),
            label = "Sexual Couples",
            fontface = "bold",
            family = "Arial",
            color = "darkblue",
            size = 10) +
  geom_text(aes(x = -.06, y = 62),
            label = "Pleasant",
            fontface = "bold",
            family = "Arial",
            color = "blue1",
            size = 10) +
  geom_text(aes(x = -.06, y = 53),
            label = "Neutral",
            fontface = "bold",
            family = "Arial",
            color = "black",
            size = 10) +
  geom_text(aes(x = -.06, y = 44),
            label = "Unpleasant",
            fontface = "bold",
            family = "Arial",
            color = "red1",
            size = 10) +
  geom_text(aes(x = -.06, y = 35),
            label = "Surgeries",
            fontface = "bold",
            family = "Arial",
            color = "darkred",
            size = 9)
  # geom_text(aes(x = -.081, y = 57),
  #           label = "Sexual Couples",
  #           fontface = "bold",
  #           family = "Arial",
  #           color = "darkblue",
  #           size = 10) +
  # geom_text(aes(x = -.081, y = 50),
  #           label = "Pleasant",
  #           fontface = "bold",
  #           family = "Arial",
  #           color = "blue1",
  #           size = 10) +
  # geom_text(aes(x = -.081, y = 43),
  #           label = "Neutral",
  #           fontface = "bold",
  #           family = "Arial",
  #           color = "black",
  #           size = 10) +
  # geom_text(aes(x = -.081, y = 36),
  #           label = "Unpleasant",
  #           fontface = "bold",
  #           family = "Arial",
  #           color = "red1",
  #           size = 10) +
  # geom_text(aes(x = -.081, y = 29),
  #           label = "Surgeries",
  #           fontface = "bold",
  #           family = "Arial",
  #           color = "darkred",
  #           size = 9)

fig7_lpp_top + fig7_ssvep_bottom +
  plot_layout(design = layout_grid, guides = "collect")  +
  plot_annotation(title = "Video ssVEP Did Not Feature Typical Erotic and Gore Bias",
                  theme = theme(
                    plot.title = element_text(family = font_font,
                                              size = font_size + 2.65,
                                              color = "black",
                                              hjust = 0.5,
                                              face = "bold")))

  

ggsave(filename = paste0(parent_directory,
                         "/misc/007figure_ero_surg_posteriors_model12.tiff"),
       device = "tiff",dpi = 300,
       units = "in",height = 8, width = 8.6,
       scale = 1.1)

# ggsave(filename = paste0(parent_directory,
#                          "/misc/007figure_ero_surg_posteriors.svg"),
#        device = "svg",
#        units = "in",height = 8, width = 7,
#        scale = 1.37)


  # scale_x_continuous(name = "Microvoltage",
  #                    breaks = seq(-2, 2, by = 1),
  #                    labels = seq(-2, 2, by = 1),
  #                    expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0.025)) +
  # coord_cartesian(xlim = c(-2.5,2.5)) +
  # ggtitle("LPP Valence Posteriors") +
  # theme_classic()+
  # theme(
  #   axis.title.y = element_blank(),
  #   axis.text.y = element_blank(),
  #   axis.text.x = element_text(color = "black",
  #                              margin = margin(t = 5, 
  #                                              unit = "pt")),
  #   axis.ticks.y = element_blank(),
  #   axis.ticks.x = element_blank(),
  #   line = element_line(linewidth = line_thickness,
  #                       lineend = "square"),
  #   text = element_text(size = font_size,
  #                       color = "black"),
  #   plot.title = element_text(hjust = 0.5,
  #                             face = "bold"),
  #   # legend.title = element_text(face = "bold"),
  #   legend.title = element_blank(),
  #   legend.position = "bottom", 
  #   legend.text = element_text(angle = 0),
  #   legend.title.align = 0,
  #   legend.background = element_blank(),
  #   legend.key = element_blank(),
  #   panel.grid.major.x = element_line(color = "black",
  #                                     linetype = "dotted",
  #                                     linewidth = 
  #                                       panel.grid.major.x_line_thickness))+ 
  # guides(fill = guide_legend(title.position = "top",
  #                            title.hjust = 0.5))

layout_grid = c("
A
B
B
B
B
B
B
C
C
C
C
C
C
")

#This is fun, but going to make them overlapping like the one above
fig7_par_pos_scale <- 9
fig7_par_pos_y_limits <- c(.9,12.9)
fig7_rel_min_height <- .001

fig7_par_pos_scale <- 1
fig7_par_pos_y_limits <- c(.96,5.75)
fig7_rel_min_height <- .005


guide_area() +
(lpp_erot_surg_val_posteriors %>% 
  mutate(name = fct_rev(name)) %>% 
 ggplot() +
  geom_vline(aes(xintercept = 0),
             linewidth = line_thickness) +
  geom_density_ridges(aes(x = value, 
                          y = name,
                          fill = name),
                      alpha = par_alpha,
                      rel_min_height = fig7_rel_min_height,
                      size = density_line_thickness,
                      color = "black",
                      scale = fig7_par_pos_scale) +
    scale_fill_manual(values = rev(fig7_colors),
                      name = "Categories",
                      labels = rev(c("Erotica", 
                                 "Pleasant without erotica", 
                                 "Neutral",
                                 "Unpleasant without surgeries",
                                 "Surgeries"))) +
    scale_x_continuous(name = "Microvoltage",
                       breaks = seq(-2, 4, by = .5),
                       labels = seq(-2, 4, by = .5),
                       expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    coord_cartesian(xlim = c(-2,4),
                    ylim = fig7_par_pos_y_limits) +
    ggtitle("LPP Valence Posteriors") +
    theme_classic()+
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(color = "black",
                                 margin = margin(t = 5, 
                                                 unit = "pt")),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      line = element_line(linewidth = line_thickness,
                          lineend = "square"),
      text = element_text(size = font_size,
                          color = "black"),
      plot.title = element_text(hjust = 0.5,
                                face = "bold"),
      # legend.title = element_text(face = "bold"),
      legend.title = element_text(size = font_size,
                                  color = "black",face = "bold"),
      legend.position = "bottom", 
      legend.text = element_text(angle = 0),
      legend.title.align = 0,
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.grid.major.x = element_line(color = "black",
                                        linetype = "dotted",
                                        linewidth = 
                                          panel.grid.major.x_line_thickness))+ 
    guides(fill = guide_legend(title.position = "top",
                               title.hjust = 0.5,
                               reverse = T))) +

(ssvep_erot_surg_val_posteriors %>% 
  mutate(name = fct_rev(name)) %>% 
 ggplot() +
  geom_vline(aes(xintercept = 0),
             linewidth = line_thickness) +
  geom_density_ridges(aes(x = value, 
                          y = name,
                          fill = name),
                      alpha = par_alpha,
                      rel_min_height = fig7_rel_min_height,
                      size = density_line_thickness,
                      color = "black",
                      scale = fig7_par_pos_scale) +
   scale_fill_manual(values = rev(fig7_colors),
                     name = "Categories",
                     labels = rev(c("Erotica", 
                                "Pleasant without erotica", 
                                "Neutral",
                                "Unpleasant without surgeries",
                                "Surgeries"))) +
   scale_x_reverse(name = "Microvoltage",
                      breaks = seq(.06, -.12, by = -.02),
                      labels = sprintf("%.2f", seq(.06, -.12, by = -.02)),
                      expand = c(0,0)) +
   scale_y_discrete(expand = c(0,0)) +
   coord_cartesian(xlim = c(.06, -.125),
                   ylim = fig7_par_pos_y_limits) +
   ggtitle("ssVEP Valence Posteriors") +
   theme_classic()+
   theme(axis.title.y = element_blank(),
     axis.text.y = element_blank(),
     axis.text.x = element_text(color = "black",
                                margin = margin(t = 5, 
                                                unit = "pt")),
     axis.ticks.y = element_blank(),
     axis.ticks.x = element_blank(),
     line = element_line(linewidth = line_thickness,
                         lineend = "square"),
     text = element_text(size = font_size,
                         color = "black"),
     plot.title = element_text(hjust = 0.5,
                               face = "bold"),
     # legend.title = element_text(face = "bold"),
     legend.title = element_text(size = font_size,
                                 color = "black",face = "bold"),
     legend.position = "top", 
     legend.text = element_text(angle = 0),
     legend.title.align = 0,
     legend.background = element_blank(),
     legend.key = element_blank(),
     panel.grid.major.x = element_line(color = "black",
                                       linetype = "dotted",
                                       linewidth = 
                                         panel.grid.major.x_line_thickness))+ 
   guides(fill = guide_legend(title.position = "top",
                              title.hjust = 0.5,
                              reverse = T))) +
  plot_layout(design = layout_grid ,guides = "collect")


ggsave(filename = paste0(parent_directory,
                         "/misc/007figure_ero_surg_posteriors.tiff"),
       device = "tiff",dpi = 300,
       units = "in",height = 11, width = 8.5,
       scale = 1.5)



# Figure 8 model 3 results ####

cor_fill <- "slategray"

lpp_ssvep_cor_fill <- c("#E69F00","#CC79A7")

cor_certainty_above99 <- c("#009E73", "#D55E00")

line_thickness <- 1
density_line_thickness <- 1
par_pos_scale <- 2
par_pos_y_limits <- c(1,47.5)
ssvep_par_breaks_labels <- seq(.4, 1.6, by = .2)
par_alpha <- .9
font_size <- 20
font_font <- "Arial"
cor_pos_y_limits <- c(.6,48.5)
bar_width <- 35
panel.grid.major.x_line_thickness <- .2

corr_layout <- c("
ab
ab
ab
ab
ab
ab
ab
ab
cc
                 ")

model011_lpp_fit_draws %>% 
  select(starts_with("par_amp_aro_cor[")) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(name = factor(name,
                       levels = unique(name))) %>% 
  group_by(name) %>% 
  mutate(
    percent_above_zero = (sum(value > 0) / n())*100,
    above_99_percent = factor(percent_above_zero >= 95,
                              levels = c("TRUE","FALSE"))) %>% 
  ungroup() %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0),
             linewidth = line_thickness)+
  geom_density_ridges(aes(x = value, 
                          y = name,
                          fill = percent_above_zero),
                      alpha = 0.8,
                      rel_min_height = 0.01,
                      size = density_line_thickness,
                      scale = par_pos_scale) +
scale_x_continuous(name = "Correlation",
                   breaks = seq(-.2, .6, by = .2),
                   labels = gsub("([-+])?0\\.", "\\1.", sprintf("%.1f", seq(-0.2, 0.6, by = 0.2)))) +
  scale_y_discrete(expand = c(0,0), name = "Participants") +
  scale_fill_gradient2(low = "#D55E00",mid = "#F0E442", high = "#009E73", 
                       midpoint = 97.5, limit = c(95, 100),
                       space = "Lab", name= "Probabilty Association is Greater Than Zero") +
  coord_cartesian(xlim = c(-.3, .6),
                  ylim = par_pos_y_limits) +
  ggtitle("Scene LPP")+
  theme_classic() +
  theme(#axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black",
                               margin = margin(t = 5, 
                                               unit = "pt")),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    line = element_line(linewidth = line_thickness,
                        lineend = "square"),
    text = element_text(size = font_size,
                        color = "black"),
    plot.title = element_text(hjust = 0.5,
                              face = "bold",
                              size = font_size + 2),
    legend.title = element_text(face = "bold"),
    legend.title.align = .5,
    legend.position = "bottom", 
    legend.justification = "center",
    legend.text = element_text(angle = 0),
    legend.text.align = 0.5,
    legend.background = element_blank(),
    legend.box.just = "center",
    legend.key = element_blank(),
    panel.grid.major.x = element_line(color = "black",
                                      linetype = "dotted",
                                      linewidth = 
                                        panel.grid.major.x_line_thickness)
  ) + 
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = bar_width, # Adjust the width as needed
    barheight = 1.5 # Adjust the height as needed
  )) +

model011_ssvep_fit_draws %>% 
  select(starts_with("par_amp_aro_cor[")) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(name = factor(name,
                       levels = unique(name))) %>% 
  group_by(name) %>% 
  mutate(
    percent_above_zero = (sum(value < 0) / n())*100,
    above_99_percent = factor(percent_above_zero <= 0.99,
                              levels = c("TRUE","FALSE"))) %>%
  ungroup() %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0),
             linewidth = line_thickness)+
  geom_density_ridges(aes(x = value, 
                          y = name,
                          fill = percent_above_zero),
                      alpha = 0.8,
                      rel_min_height = 0.01,
                      size = density_line_thickness,
                      scale = par_pos_scale) +
  scale_x_continuous(name = "Correlation",
                     breaks = seq(-.6, .2, by = .2),
                     labels = gsub("([-+])?0\\.", "\\1.", sprintf("%.1f", seq(-0.6, 0.3, by = 0.2)))) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient2(low = "#D55E00",mid = "#F0E442", high = "#009E73", 
                       midpoint = 97.5, limit = c(95, 100),
                       space = "Lab", name= "Probabilty Association is Greater Than Zero") +
  coord_cartesian(xlim = c(-.6, .3),
                  ylim = par_pos_y_limits) +
  ggtitle("Video ssVEP")+
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "black",
                                   margin = margin(t = 5, 
                                                   unit = "pt")),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        line = element_line(linewidth = line_thickness,
                            lineend = "square"),
        text = element_text(size = font_size,
                            color = "black"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = font_size + 2),
        legend.title = element_text(face = "bold"),
        legend.title.align = .5,
        legend.position = "bottom", 
        legend.justification = "center",
        legend.text.align = .5,
        legend.text = element_text(angle = 0),
        legend.background = element_blank(),
        legend.box.just = "center",
        legend.key = element_blank(),
        panel.grid.major.x = element_line(color = "black",
                                          linetype = "dotted",
                                          linewidth = 
                                            panel.grid.major.x_line_thickness)
  ) + 
  guides(    fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = bar_width, # Adjust the width as needed
    barheight = 1.5 # Adjust the height as needed
  )) +
  guide_area() +
  plot_annotation(title = "Model 3\nVideo ssVEP is Better Predicted by Rated Arousal",
                  theme = theme(
                    plot.title = element_text(size = font_size + 2,
                                              color = "black",
                                              hjust = 0.5,
                                              face = "bold")
                  )) +
  plot_layout(guides = "collect", design = corr_layout)

ggsave(filename = paste0(parent_directory,
                         "/misc/008figure_Model3_results.tiff"),
       device = "tiff",dpi = 300,
       units = "in",height = 8, width = 6.5,
       scale = 1.2)

# Figure 9 by participant clusters ####

# Processing for par_mu_amp[
ssvep_amp_data <- model011_ssvep_fit_draws %>%
  select(starts_with("par_mu_amp[")) %>%
  mutate(sample = 1:nrow(.), .before = 1) %>%
  pivot_longer(cols = starts_with("par_mu_amp["), names_to = "name", values_to = "value") %>%
  mutate(par_id = sub(".*\\[(.*)\\]", "\\1", name),
         amp = value) %>%
  select(sample, par_id, amp)

# Processing for par_mu_aro[
ssvep_aro_data <- model011_ssvep_fit_draws %>%
  select(starts_with("par_mu_aro[")) %>%
  mutate(sample = 1:nrow(.), .before = 1) %>%
  pivot_longer(cols = starts_with("par_mu_aro["), names_to = "name", values_to = "value") %>%
  mutate(par_id = sub(".*\\[(.*)\\]", "\\1", name),
         aro = value) %>%
  select(sample, par_id, aro)

# Combining amp_data and aro_data
ssvep_combined_data <- left_join(ssvep_amp_data, ssvep_aro_data, by = c("sample", "par_id"))

set.seed(0)
used_posterior_samples <- sample(x = 1:80000, size = 5000)

ssvep_combined_data %>% 
  filter(sample %in% used_posterior_samples) %>% 
  ggplot(aes( x = aro, y = amp, color = par_id)) +
  geom_point(size = .001, alpha = .05) +
  stat_ellipse(type = "norm",level = .95) +
  theme_classic()

# Processing for par_mu_amp[
lpp_amp_data <- model011_lpp_fit_draws %>%
  select(starts_with("par_mu_amp[")) %>%
  mutate(sample = 1:nrow(.), .before = 1) %>%
  pivot_longer(cols = starts_with("par_mu_amp["), names_to = "name", values_to = "value") %>%
  mutate(par_id = sub(".*\\[(.*)\\]", "\\1", name),
         amp = value) %>%
  select(sample, par_id, amp)

# Processing for par_mu_aro[
lpp_aro_data <- model011_lpp_fit_draws %>%
  select(starts_with("par_mu_aro[")) %>%
  mutate(sample = 1:nrow(.), .before = 1) %>%
  pivot_longer(cols = starts_with("par_mu_aro["), names_to = "name", values_to = "value") %>%
  mutate(par_id = sub(".*\\[(.*)\\]", "\\1", name),
         aro = value) %>%
  select(sample, par_id, aro)

# Combining amp_data and aro_data
lpp_combined_data <- left_join(lpp_amp_data, lpp_aro_data, by = c("sample", "par_id"))

set.seed(0)
used_posterior_samples <- sample(x = 1:80000, size = 5000)

lpp_combined_data %>% 
  filter(sample %in% used_posterior_samples) %>% 
  ggplot(aes( x = aro, y = amp, color = par_id)) +
  geom_point(size = .001, alpha = .05) +
  stat_ellipse(type = "norm",level = .95)

# Now we generate posterior predictive data



set.seed(0)
number_of_used_posterior_samples <- 500
used_posterior_samples <- sample(x = 1:80000, 
                                 size = number_of_used_posterior_samples)

posterior_pred_mat <- matrix(nrow = 0, ncol = 3)
  
for(j in 1:46){
  current_par_mu_amp <- model011_ssvep_fit_draws[used_posterior_samples,] %>%
    pull(paste0("par_mu_amp[",j,"]"))
  
  current_par_mu_aro <- model011_ssvep_fit_draws[used_posterior_samples,] %>%
    pull(paste0("par_mu_aro[",j,"]"))
  
  current_par_sd_amp <- model011_ssvep_fit_draws[used_posterior_samples,] %>%
    pull(paste0("par_sd_amp[",j,"]"))
  
  current_par_sd_aro <- model011_ssvep_fit_draws[used_posterior_samples,] %>%
    pull(paste0("par_sd_aro[",j,"]"))
  
  current_par_cov_amp_aro <- model011_ssvep_fit_draws[used_posterior_samples,] %>%
    pull(paste0("par_cov_amp_aro[",j,"]"))
  
  for(i in 1:number_of_used_posterior_samples){
    
    # Generate one sample from the multivariate normal distribution
    posterior_pred_mat <- rbind(
      posterior_pred_mat,
      c(j, 
        MASS::mvrnorm(n = 1, 
                      mu = c(current_par_mu_amp[i], 
                             current_par_mu_aro[i]), 
                      Sigma = matrix(c(current_par_sd_amp[i]^2,
                                       current_par_cov_amp_aro[i],
                                       current_par_cov_amp_aro[i],
                                       current_par_sd_aro[i]^2), 
                                     ncol = 2))))
  }
}

posterior_pred <- as_tibble(posterior_pred_mat) %>% 
  reframe(par_id = factor(V1,
                          levels = 1:46) , amp_pred = V2, aro_pred = V3)

posterior_pred %>% 
  ggplot(aes( x = aro_pred, 
              y = amp_pred, 
              color = par_id)) +
  geom_point(size = .001, alpha = .05) +
  stat_ellipse(type = "norm",level = .20) +
  coord_cartesian(xlim = c(1,9),
                  ylim = c(0.3,2)) +
  theme_classic()


# Visualization of cross-validation accuracy between 2 best models ####
valence_colors <- c("blue1","black", "red1", "white")

model012_lpp_fit_loo
model011_lpp_fit_loo


lpp_stim_predictor_cv_accuracy <- model012_lpp_fit_loo$pointwise[,1]
lpp_arousal_predictor_cv_accuracy <- model011_lpp_fit_loo$pointwise[,1]

loo::loo_compare(model012_lpp_fit_loo,
                 model011_lpp_fit_loo)

loo::loo_model_weights(list(model012_lpp_fit_loo,
                            model011_lpp_fit_loo))



lpp_cv_models <- tibble(par_id = data_list_for_stan_lpp$par,
                        stim_id = data_list_for_stan_lpp$stim,
                        cate_id = factor(data_list_for_stan_lpp$cate, levels = c(1:3)),
                        stim_cv_acc = lpp_stim_predictor_cv_accuracy,
                        arousal_cv_acc = lpp_arousal_predictor_cv_accuracy)

lpp_cv_models %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  geom_point(aes(x = stim_cv_acc, 
                 y = arousal_cv_acc, color = cate_id
                 ),
             size = .1, alpha = .2) +
  scale_color_manual(values = valence_colors)+
  coord_cartesian(xlim = c(-12,-2),
                  ylim = c(-12,-2)) +
  theme_classic()

lpp_cv_models %>% 
  group_by(par_id) %>%
  select(-cate_id) %>% 
  summarise_all(sum) %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  geom_point(aes(x = stim_cv_acc, 
                 y = arousal_cv_acc)) +
  coord_equal()+
  # coord_cartesian(xlim = c(-4.5,-3),
  #                 ylim = c(-4.5,-3)) +
  theme_classic()

lpp_cv_models %>% 
  group_by(stim_id, cate_id) %>% 
  summarise_all(sum) %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  geom_point(aes(x = stim_cv_acc, 
                 y = arousal_cv_acc, color = cate_id
                 )) +
  scale_color_manual(values = valence_colors)+
  coord_equal() +
  # coord_cartesian(xlim = c(-4,-2.5),
  #                 ylim = c(-4,-2.5)) +
  theme_classic()

model012_ssvep_fit_loo
model011_ssvep_fit_loo

bad_obs <- which(model012_ssvep_fit_loo$diagnostics$pareto_k > .7)

ssvep_stim_predictor_cv_accuracy <- model012_ssvep_fit_loo$pointwise[,1]
ssvep_arousal_predictor_cv_accuracy <- model011_ssvep_fit_loo$pointwise[,1]


diff_elpd_vec <- ssvep_stim_predictor_cv_accuracy[-bad_obs] - 
  ssvep_arousal_predictor_cv_accuracy[-bad_obs]

sum(diff_elpd_vec)

sqrt(length(diff_elpd_vec)) * sd(diff_elpd_vec)



loo::loo_compare(model012_ssvep_fit_loo,
                 model011_ssvep_fit_loo)

loo::loo_model_weights(list(model012_ssvep_fit_loo,
                            model011_ssvep_fit_loo))


ssvep_cv_models <- tibble(par_id = data_list_for_stan_ssvep$par[-bad_obs],
                          stim_id = data_list_for_stan_ssvep$stim[-bad_obs],
                          cate_id = factor(data_list_for_stan_ssvep$cate[-bad_obs],levels = c(1:3)),
                          stim_cv_acc = ssvep_stim_predictor_cv_accuracy[-bad_obs],
                          arousal_cv_acc = ssvep_arousal_predictor_cv_accuracy[-bad_obs])

ssvep_cv_models %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  geom_point(aes(x = stim_cv_acc, 
                 y = arousal_cv_acc, color = cate_id
  ),
  size = .1, alpha = .9) +
  scale_color_manual(values = valence_colors)+
  coord_equal() +
  # coord_cartesian(xlim = c(-30,2),
  #                 ylim = c(-30,2)) +
  theme_classic()

ssvep_cv_models %>% 
  group_by(par_id) %>% 
  select(-cate_id) %>% 
  summarise_all(sum) %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  geom_point(aes(x = stim_cv_acc, 
                 y = arousal_cv_acc)) +
  coord_equal() +
  # coord_cartesian(xlim = c(-2,2),
  #                 ylim = c(-2,2)) +
  theme_classic()

ssvep_cv_models %>% 
  # filter(!(par_id %in% c(6,20))) %>%
  group_by(stim_id, cate_id) %>% 
  summarise_all(sum) %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  geom_point(aes(x = stim_cv_acc, 
                 y = arousal_cv_acc, color = cate_id
  )) +
  scale_color_manual(values = valence_colors)+
  coord_equal() +
  # coord_cartesian(xlim = c(-.4,.8),
  #                 ylim = c(-.4,.8)) +
  theme_classic()

# how these loo_compare values are found
loo::loo_compare(model003_lpp_fit_loo, model011_lpp_fit_loo)

diff_elpd_vec <- lpp_stim_predictor_cv_accuracy - 
  lpp_arousal_predictor_cv_accuracy

sum(diff_elpd_vec)

sqrt(length(diff_elpd_vec)) * sd(diff_elpd_vec)

# Get model fit diff per participant

lpp_model_diff_per_par <- matrix(ncol = 2, nrow = 0)
ssvep_model_diff_per_par <- matrix(ncol = 2, nrow = 0)

for (i in 1:data_list_for_stan_lpp$npar) {
  
  current_lpp_diff_vec <- lpp_stim_predictor_cv_accuracy[
    data_list_for_stan_lpp$par == i] -
    lpp_arousal_predictor_cv_accuracy[
      data_list_for_stan_lpp$par == i]
  
  current_ssvep_diff_vec <- ssvep_stim_predictor_cv_accuracy[
    data_list_for_stan_ssvep$par == i] -
    ssvep_arousal_predictor_cv_accuracy[
      data_list_for_stan_ssvep$par == i]
  
  current_lpp_diff <- sum(current_lpp_diff_vec)
  
  current_lpp_diff_se <- sqrt(length(current_lpp_diff_vec)) * 
    sd(current_lpp_diff_vec)
  
  current_ssvep_diff <- sum(current_ssvep_diff_vec)
  
  current_ssvep_diff_se <- sqrt(length(current_ssvep_diff_vec)) * 
    sd(current_ssvep_diff_vec)
  
  lpp_model_diff_per_par <- rbind(lpp_model_diff_per_par,
                                  c(current_lpp_diff, current_lpp_diff_se))
  
  ssvep_model_diff_per_par <- rbind(ssvep_model_diff_per_par,
                                  c(current_ssvep_diff, current_ssvep_diff_se))
  
}

data_for_stan_df %>% 
  group_by(type, cate, vids_first_is_one) %>% 
  summarise(mean_amp = mean(amp),
            se_amp = plotrix::std.error(amp),
            mean_aro = mean(arousal),
            se_aro = plotrix::std.error(arousal))

afex::aov_ez(id = "par",
             dv = "amp", 
             data = data_for_stan_df[data_for_stan_df$type == 1,],
             within = "cate",
             between = "vids_first_is_one") 

afex::aov_ez(id = "par",
             dv = "amp", 
             data = data_for_stan_df[data_for_stan_df$type == 2,],
             within = "cate",
             between = "vids_first_is_one") %>% summary()

# old Figure 8 model 2 results ####

cor_fill <- "slategray"

lpp_ssvep_cor_fill <- c("#E69F00","#CC79A7")

cor_certainty_above99 <- c("#009E73", "#D55E00")

line_thickness <- 4
density_line_thickness <- 1
par_pos_scale <- 2
par_pos_y_limits <- c(0.1,48.5)
ssvep_par_breaks_labels <- seq(.4, 1.6, by = .2)
par_alpha <- .9
font_size <- 22
font_font <- "Arial"
cor_pos_y_limits <- c(.6,48.5)

corr_layout <- c("
ab
ab
ab
ab
ab
ab
ab
ab
cc
                 ")

bar_width <- 35


# model007_lpp_fit_draws %>% 
model010_lpp_fit_draws %>% 
  select(starts_with("baro_r[")) %>%
  # select(starts_with("baro[")) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(name = factor(name,
                       levels = unique(name))) %>% 
  group_by(name) %>% 
  mutate(
    percent_above_zero = (sum(value > 0) / n())*100,
    above_99_percent = factor(percent_above_zero >= 0.99,
                              levels = c("TRUE","FALSE"))) %>%
  ungroup() %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0),
             linewidth = line_thickness)+
  geom_density_ridges(aes(x = value, 
                          y = name,
                          fill = percent_above_zero),
                      alpha = 0.8,
                      rel_min_height = 0.01,
                      size = density_line_thickness,
                      scale = par_pos_scale) +
  scale_x_continuous(name = "Standardized Beta",
                     breaks = seq(-.1, .3, by = .1),
                     labels = seq(-.1, .3, by = .1),
                     sec.axis = sec_axis(trans = ~ . * 5.39325683#,breaks = -y_axis_breaks
                                         )) +
  scale_y_discrete(expand = c(0,0), name = "Participants") +
  scale_fill_gradient2(low = "#D55E00",mid = "#F0E442", high = "#009E73", 
                       midpoint = 97.5, limit = c(95, 100),
                       space = "Lab", name= "Probabilty Association is Greater Than Zero") +
  coord_cartesian(xlim = c(-.1, .3),
                  ylim = cor_pos_y_limits) +
  ggtitle("Δ LPP (μV)") +
  theme_classic() +
  theme(#axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "black",
                                   margin = margin(t = 5, 
                                                   unit = "pt")),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        line = element_line(linewidth = line_thickness,
                            lineend = "square"),
        text = element_text(size = font_size,
                            color = "black"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = font_size + 2),
        legend.title = element_text(face = "bold"),
        legend.title.align = .5,
        legend.position = "bottom", 
        legend.justification = "center",
        legend.text = element_text(angle = 0),
        legend.text.align = 0.5,
        legend.background = element_blank(),
        legend.box.just = "center",
        legend.key = element_blank()
        ) + 
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = bar_width, # Adjust the width as needed
    barheight = 1.5 # Adjust the height as needed
  )) +


model010_ssvep_fit_draws %>% 
  # select(starts_with("baro[")) %>%
# model007_ssvep_fit_draws %>% 
  select(starts_with("baro_r[")) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(name = factor(name,
                       levels = unique(name))) %>% 
  group_by(name) %>% 
  mutate(
    percent_above_zero = (sum(value < 0) / n())*100,
    above_99_percent = factor(percent_above_zero <= 0.99,
                              levels = c("TRUE","FALSE"))) %>%
  ungroup() %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0),
             linewidth = line_thickness)+
  geom_density_ridges(aes(x = value, 
                          y = name,
                          fill = percent_above_zero),
                      alpha = 0.8,
                      rel_min_height = 0.01,
                      size = density_line_thickness,
                      scale = par_pos_scale) +
  scale_x_continuous(name = "Standardized Beta",
                     breaks = seq(-.3, .1, by = .1),
                     labels = round(seq(-.3, .1, by = .1),1),
                     sec.axis = sec_axis(trans = ~ . * 0.156875062#,breaks = -y_axis_breaks
                     )) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient2(low = "#D55E00",mid = "#F0E442", high = "#009E73", 
                       midpoint = 97.5, limit = c(95, 100),
                       space = "Lab", name= "Probabilty Association is Greater Than Zero") +
  coord_cartesian(xlim = c(-.3, .1),
                  ylim = cor_pos_y_limits) +
  ggtitle("Δ ssVEP (μV)") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "black",
                                   margin = margin(t = 5, 
                                                   unit = "pt")),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        line = element_line(linewidth = line_thickness,
                            lineend = "square"),
        text = element_text(size = font_size,
                            color = "black"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = font_size + 2),
        legend.title = element_text(face = "bold"),
        legend.title.align = .5,
        legend.position = "bottom", 
        legend.justification = "center",
        legend.text.align = .5,
        legend.text = element_text(angle = 0),
        legend.background = element_blank(),
        legend.box.just = "center",
        legend.key = element_blank()
  ) + 
  guides(    fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = bar_width, # Adjust the width as needed
    barheight = 1.5 # Adjust the height as needed
  )) +
  guide_area() +
  plot_annotation(title = "Model 2\nAmplitude per Arousal Increase",
                  theme = theme(
                    plot.title = element_text(size = font_size + 2,
                                        color = "black",
                                        hjust = 0.5,
                                        face = "bold")
                  )) +
  plot_layout(guides = "collect", design = corr_layout)

ggsave(filename = paste0(parent_directory,
                         "/misc/008figure_Model2_results.tiff"),
       device = "tiff",dpi = 300,
       units = "in",height = 8, width = 6.5,
       scale = 1.2)


# by scene posteriors

labeling_df <- data_for_stan_df %>% 
  group_by(stim, stim_name, cate) %>% 
  summarise(n()) %>% 
  ungroup() %>% 
  mutate(erotica = if_else(str_starts(stim_name, "Coup"), T, F)) %>% 
  print(n = 90)

model003_lpp_fit_draws %>% 
  select(starts_with("bstim")) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(name = factor(name,
                       levels = unique(name))) %>% 
  group_by(name) %>% 
  # mutate(
  #   percent_above_zero = (sum(value > 0) / n())*100,
  #   above_99_percent = factor(percent_above_zero >= 0.99,
  #                             levels = c("TRUE","FALSE"))) %>%
  ungroup() %>% 
  ggplot() +
  # geom_vline(aes(xintercept = 0),
  #            linewidth = line_thickness)+
  geom_density_ridges(aes(x = value, 
                          y = name#,
                          # fill = percent_above_zero
                          ),
                      alpha = 0.8,
                      rel_min_height = 0.01,
                      size = density_line_thickness,
                      scale = par_pos_scale) +
  # scale_x_continuous(name = "Standardized Beta",
  #                    breaks = seq(-.1, .3, by = .1),
  #                    labels = seq(-.1, .3, by = .1)) +
  scale_y_discrete(expand = c(0,0), name = "Stimuli") +
  # scale_fill_gradient2(low = "#D55E00",mid = "#F0E442", high = "#009E73", 
  #                      midpoint = 97.5, limit = c(95, 100),
  #                      space = "Lab", name= "Probabilty greater than zero") +
  # coord_cartesian(xlim = c(-.1, .3),
  #                 ylim = cor_pos_y_limits) +
  ggtitle("LPP") +
  theme_classic() +
  theme(#axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black",
                               margin = margin(t = 5, 
                                               unit = "pt")),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    line = element_line(linewidth = line_thickness,
                        lineend = "square"),
    text = element_text(size = font_size,
                        color = "black"),
    plot.title = element_text(hjust = 0.5,
                              face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom", 
    legend.text = element_text(angle = 0),
    legend.title.align = 0,
    legend.background = element_blank(),
    legend.key = element_blank()
  ) + 
  guides(    fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 25, # Adjust the width as needed
    barheight = 1.5 # Adjust the height as needed
  )) +
  
  
  model003_ssvep_fit_draws %>% 
  select(starts_with("bstim")) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(name = factor(name,
                       levels = unique(name))) %>% 
  group_by(name) %>% 
  # mutate(
  #   percent_above_zero = (sum(value > 0) / n())*100,
  #   above_99_percent = factor(percent_above_zero >= 0.99,
  #                             levels = c("TRUE","FALSE"))) %>%
  ungroup() %>% 
  ggplot() +
  # geom_vline(aes(xintercept = 0),
  #            linewidth = line_thickness)+
  geom_density_ridges(aes(x = value, 
                          y = name#,
                          # fill = percent_above_zero
  ),
  alpha = 0.8,
  rel_min_height = 0.01,
  size = density_line_thickness,
  scale = par_pos_scale) +
  # scale_x_continuous(name = "Standardized Beta",
  #                    breaks = seq(-.1, .3, by = .1),
  #                    labels = seq(-.1, .3, by = .1)) +
  scale_y_discrete(expand = c(0,0), name = "Stimuli") +
  # scale_fill_gradient2(low = "#D55E00",mid = "#F0E442", high = "#009E73", 
  #                      midpoint = 97.5, limit = c(95, 100),
  #                      space = "Lab", name= "Probabilty greater than zero") +
  # coord_cartesian(xlim = c(-.1, .3),
  #                 ylim = cor_pos_y_limits) +
  ggtitle("ssVEP") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "black",
                                   margin = margin(t = 5, 
                                                   unit = "pt")),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        line = element_line(linewidth = line_thickness,
                            lineend = "square"),
        text = element_text(size = font_size,
                            color = "black"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom", 
        legend.text = element_text(angle = 0),
        legend.title.align = 0,
        legend.background = element_blank(),
        legend.key = element_blank()
  ) + 
  guides(    fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 25, # Adjust the width as needed
    barheight = 1.5 # Adjust the height as needed
  )) +
  guide_area() +
  plot_annotation(title = "Amplitude predicted by arousal per trial",
                  theme = theme(
                    plot.title = element_text(size = font_size + 5,
                                              color = "black",
                                              hjust = 0.5,
                                              face = "bold")
                  )) +
  plot_layout(guides = "collect", design = corr_layout)



#by cate cor

model007_lpp_fit_draws %>% 
  select(starts_with("baro_r")) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(name = factor(name,
                       levels = unique(name))) %>% 
  group_by(name) %>% 
  mutate(
    percent_above_zero = sum(value > 0) / n(),
    above_99_percent = factor(percent_above_zero >= 0.99,
                              levels = c("TRUE","FALSE"))) %>%
  ungroup() %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0))+
  geom_density_ridges(aes(x = value, 
                          y = name,
                          fill = percent_above_zero),
                      alpha = 0.8,
                      rel_min_height = 0.01,
                      size = density_line_thickness,
                      scale = 2) +
  scale_x_continuous(name = "Correlation",
                     breaks = seq(-.1, .3, by = .05),
                     labels = seq(-.1, .3, by = .05),
                     expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient2(low = "#D55E00",mid = "#F0E442", high = "#009E73", 
                       midpoint = 0.975, limit = c(.95, 1),
                       space = "Lab", name="Percentage\nAbove Zero") +
  coord_cartesian(xlim = c(-.1, .3)) +
  theme_classic() +
  theme(axis.title.y = element_blank()) 














model007_lpp_fit_draws %>% 
  select(starts_with("baro_r")) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(name = factor(name,
                       levels = unique(name))) %>% 
  group_by(name) %>% 
  mutate(
    percent_above_zero = sum(value > 0) / n(),
    above_99_percent = factor(percent_above_zero >= 0.99,
                              levels = c("TRUE","FALSE"))) %>%
  ungroup() %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0))+
  geom_density_ridges(aes(x = value, 
                          y = name,
                          fill = above_99_percent),
                      alpha = 0.8,
                      rel_min_height = 0.01,
                      size = density_line_thickness,
                        scale = 2) +
  scale_x_continuous(name = "Correlation",
                     breaks = seq(-.1, .3, by = .05),
                     labels = seq(-.1, .3, by = .05),
                     expand = c(0,0)) +
  scale_y_discrete(name = "Participants",
                   labels = c(1:46)) +
  scale_fill_manual(values = cor_certainty_above99)+
  coord_cartesian(xlim = c(-.1, .3)) +
  theme_classic() +
  theme(axis.title.y = element_blank()) 

model007_ssvep_fit_draws %>% 
  select(starts_with("baro_r")) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(name = factor(name,
                       levels = unique(name)),
         value = - value) %>% 
  group_by(name) %>% 
  mutate(
    percent_above_zero = sum(value > 0) / n(),
    above_99_percent = factor(percent_above_zero >= 0.99,
                              levels = c("TRUE","FALSE"))) %>%
  ungroup() %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0))+
  geom_density_ridges(aes(x = value, 
                          y = name,
                          fill = above_99_percent),
                      alpha = 0.8,
                      rel_min_height = 0.01,
                      size = density_line_thickness,
                        scale = 2) +
  scale_x_continuous(name = "Correlation",
                     breaks = seq(-.1, .3, by = .05),
                     labels = seq(-.1, .3, by = .05),
                     expand = c(0,0)) +
  scale_y_discrete(name = "Participants",
                   labels = c(1:46)) +
  scale_fill_manual(values = cor_certainty_above99)+
  coord_cartesian(xlim = c(-.1, .3)) +
  theme_classic() +
  theme(axis.title.y = element_blank()) 








model007_lpp_fit_draws %>% 
  select(aro_r) %>%
  pivot_longer(cols = everything()) %>% 
  ggplot() +
  geom_density(aes(x = value),
               fill = cor_fill)+
  scale_x_continuous(name = "Correlation",
                     breaks = seq(-.05, .3, by = .05),
                     labels = seq(-.05, .3, by = .05),
                     expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  coord_cartesian(xlim = c(-.1, .3)) +
  theme_classic() +
  theme(axis.title.y = element_blank())



model007_ssvep_fit_draws %>% 
  select(aro_r) %>%
  pivot_longer(cols = everything()) %>% 
  ggplot() +
  geom_density(aes(x = value),
               fill = cor_fill)+
  scale_x_continuous(name = "Correlation",
                     breaks = seq(.05, -.25, by = -.05),
                     labels = seq(.05, -.25, by = -.05),
                     expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  coord_cartesian(xlim = c(.1, -.3)) +
  theme_classic() +
  theme(axis.title.y = element_blank())


model007_lpp_fit_draws %>% 
  select(starts_with("baro_r")) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(name = factor(name,
                       levels = unique(name))) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0))+
  geom_density_ridges(aes(x = value, 
                          y = name),
                      alpha = 0.8,
                      rel_min_height = 0.01,
                      fill = cor_fill,
                      size = density_line_thickness,
                        scale = 2) +
  scale_x_continuous(name = "Correlation",
                     breaks = seq(-.1, .3, by = .05),
                     labels = seq(-.1, .3, by = .05),
                     expand = c(0,0)) +
  scale_y_discrete(name = "Participants",
                   labels = c(1:46)) +
  coord_cartesian(xlim = c(-.1, .3)) +
  theme_classic() +
  theme(axis.title.y = element_blank()) 


# Figure 7? residual error 

loo::loo_compare(model003_lpp_fit_loo,
                 model007_lpp_fit_loo)

loo::loo_model_weights(list(model003_lpp_fit_loo,
                            model007_lpp_fit_loo))

loo::loo_compare(model003_ssvep_fit_loo,
                 model007_ssvep_fit_loo)

loo::loo_model_weights(list(model003_ssvep_fit_loo,
                            model007_ssvep_fit_loo))




model003_lpp_fit_loo_pointwise <- model003_lpp_fit_loo$pointwise[, "elpd_loo"]
model007_lpp_fit_loo_pointwise <- model007_lpp_fit_loo$pointwise[, "elpd_loo"]
model009_lpp_fit_loo_pointwise <- model009_lpp_fit_loo$pointwise[, "elpd_loo"]

model003_ssvep_fit_loo_pointwise <- model003_ssvep_fit_loo$pointwise[, "elpd_loo"]
model007_ssvep_fit_loo_pointwise <- model007_ssvep_fit_loo$pointwise[, "elpd_loo"]
model009_ssvep_fit_loo_pointwise <- model009_ssvep_fit_loo$pointwise[, "elpd_loo"]

# Creating a function to sort observations based on elpd_loo
sort_observations <- function(elpd_loo) {
  sorted <- order(elpd_loo)
  data.frame(observation = 1:length(elpd_loo), elpd_loo = elpd_loo[sorted])
}

# Applying the function to each model
lpp_model003 <- sort_observations(model003_lpp_fit_loo_pointwise)
lpp_model007 <- sort_observations(model007_lpp_fit_loo_pointwise)
lpp_model009 <- sort_observations(model009_lpp_fit_loo_pointwise)

ssvep_model003 <- sort_observations(model003_ssvep_fit_loo_pointwise)
ssvep_model007 <- sort_observations(model007_ssvep_fit_loo_pointwise)
ssvep_model009 <- sort_observations(model009_ssvep_fit_loo_pointwise)



ggplot() +
  geom_line(data = lpp_model003, 
             aes(x = observation, 
                 y = elpd_loo),
             alpha = .3,
             color = "blue") +
  geom_line(data = lpp_model007, 
             aes(x = observation, 
                 y = elpd_loo),
             alpha = .3,
             color = "red") +
  geom_line(data = lpp_model009, 
             aes(x = observation, 
                 y = elpd_loo),
             alpha = .3,
             color = "green") +
  # coord_cartesian(ylim = c(-3, -2.5)) +
  theme_minimal() +
  labs(title = "LOO Cross-Validation Scores per Observation", 
       x = "Observation", 
       y = "ELPD LOO")

ggplot() +
  geom_line(data = ssvep_model003, 
             aes(x = observation, 
                 y = elpd_loo),
             alpha = .3,
             color = "blue") +
  geom_line(data = ssvep_model007, 
             aes(x = observation, 
                 y = elpd_loo),
             alpha = .3,
             color = "red") +
  geom_line(data = ssvep_model009, 
             aes(x = observation, 
                 y = elpd_loo),
             alpha = .3,
             color = "green") +
  # coord_cartesian(ylim = c(-3, -2.5)) +
  theme_minimal() +
  labs(title = "LOO Cross-Validation Scores per Observation", 
       x = "Observation", 
       y = "ELPD LOO")

ggplot() +
  geom_density(data = lpp_model003, 
             aes(x = elpd_loo),
             alpha = .3,
             color = "blue") +
  geom_vline(aes(xintercept = mean(model003_lpp_fit_loo_pointwise)),
             color = "blue") +
  geom_vline(aes(xintercept = median(model003_lpp_fit_loo_pointwise)),
             color = "blue",linetype = "dashed") +
  geom_density(data = lpp_model007, 
             aes(x = elpd_loo),
             alpha = .3,
             color = "red") +
  geom_vline(aes(xintercept = mean(model007_lpp_fit_loo_pointwise)),
             color = "red") +
  geom_vline(aes(xintercept = median(model007_lpp_fit_loo_pointwise)),
             color = "red",linetype = "dashed") +
  geom_density(data = lpp_model009, 
             aes(x = elpd_loo),
             alpha = .3,
             color = "green") +
  geom_vline(aes(xintercept = mean(model009_lpp_fit_loo_pointwise)),
             color = "green") +
  geom_vline(aes(xintercept = median(model009_lpp_fit_loo_pointwise)),
             color = "green",linetype = "dashed") +
  coord_cartesian(xlim = c(-2.5, -4)) +
  theme_minimal() +
  labs(title = "LOO Cross-Validation Scores per Observation", x = "Observation", y = "ELPD LOO")

ggplot() +
  geom_density(data = ssvep_model003, 
               aes(x = elpd_loo),
               alpha = .3,
               color = "blue") +
  geom_vline(aes(xintercept = mean(model003_ssvep_fit_loo_pointwise)),
             color = "blue") +
  geom_vline(aes(xintercept = median(model003_ssvep_fit_loo_pointwise)),
             color = "blue",linetype = "dashed") +
  geom_density(data = ssvep_model007, 
               aes(x = elpd_loo),
               alpha = .3,
               color = "red") +
  geom_vline(aes(xintercept = mean(model007_ssvep_fit_loo_pointwise)),
             color = "red") +
  geom_vline(aes(xintercept = median(model007_ssvep_fit_loo_pointwise)),
             color = "red",linetype = "dashed") +
  geom_density(data = ssvep_model009, 
               aes(x = elpd_loo),
               alpha = .3,
               color = "green") +
  geom_vline(aes(xintercept = mean(model009_ssvep_fit_loo_pointwise)),
             color = "green") +
  geom_vline(aes(xintercept = median(model009_ssvep_fit_loo_pointwise)),
             color = "green",linetype = "dashed") +
  coord_cartesian(xlim = c(1, -1)) +
  theme_minimal() +
  labs(title = "LOO Cross-Validation Scores per Observation", x = "Observation", y = "ELPD LOO")





loo::loo_compare(model003_ssvep_fit_loo,
                 model007_ssvep_fit_loo,
                 model009_ssvep_fit_loo)

loo::loo_model_weights(list(model003_ssvep_fit_loo,
                            model007_ssvep_fit_loo,
                            model009_ssvep_fit_loo))



loo::loo_compare(model003_lpp_fit_loo,
                 model007_lpp_fit_loo,
                 model009_lpp_fit_loo)

loo::loo_model_weights(list(model003_lpp_fit_loo,
                            model007_lpp_fit_loo,
                            model009_lpp_fit_loo))

loo::loo_compare(model003_ssvep_fit_loo,
                 model007_ssvep_fit_loo,
                 model009_ssvep_fit_loo)

loo::loo_model_weights(list(model003_ssvep_fit_loo,
                            model007_ssvep_fit_loo,
                            model009_ssvep_fit_loo))







loo::loo_compare(model001_lpp_fit_loo, 
                 model002_lpp_fit_loo,
                 model003_lpp_fit_loo,
                 model004_lpp_fit_loo,
                 model005_lpp_fit_loo,
                 model006_lpp_fit_loo,
                 model007_lpp_fit_loo)

loo::loo_compare(model001_ssvep_fit_loo, 
                 model002_ssvep_fit_loo,
                 model003_ssvep_fit_loo,
                 model004_ssvep_fit_loo,
                 model005_ssvep_fit_loo,
                 model006_ssvep_fit_loo,
                 model007_ssvep_fit_loo)


loo::loo_model_weights(list(model001_lpp_fit_loo, 
                            model002_lpp_fit_loo,
                            model003_lpp_fit_loo,
                            model004_lpp_fit_loo,
                            model005_lpp_fit_loo,
                            model006_lpp_fit_loo,
                            model007_lpp_fit_loo))

loo::loo_model_weights(list(model001_ssvep_fit_loo, 
                            model002_ssvep_fit_loo,
                            model003_ssvep_fit_loo,
                            model004_ssvep_fit_loo,
                            model005_ssvep_fit_loo,
                            model006_ssvep_fit_loo,
                            model007_ssvep_fit_loo))





ssvep_posterior_summary_df001 <- model001_summary %>% 
  filter(str_detect(variable,"^bpar_ssvep")) %>% 
  mutate(variable = factor(variable, levels = variable))


model001_lpp_fit_summary %>% 
  filter(str_detect(variable,"^bpar")) %>% 
  mutate(variable = factor(variable, levels = variable)) %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Participant") +
  scale_y_continuous(name = "Microvoltage") +
  ggtitle("LPP participant mean") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5)) +
  
  
    model001_ssvep_fit_summary %>% 
    filter(str_detect(variable,"^bpar")) %>% 
    mutate(variable = factor(variable, levels = variable)) %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Participant") +
  scale_y_continuous(name = "Microvoltage") +
  ggtitle("ssVEP participant mean") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5))



model002_lpp_fit_summary %>% 
  filter(str_detect(variable,"^bpar")) %>% 
  mutate(variable = factor(variable, levels = variable)) %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Participant") +
  scale_y_continuous(name = "Microvoltage") +
  ggtitle("LPP participant mean") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5)) +

    model002_ssvep_fit_summary %>% 
    filter(str_detect(variable,"^bpar")) %>% 
    mutate(variable = factor(variable, levels = variable)) %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Participant") +
  scale_y_continuous(name = "Microvoltage") +
  ggtitle("ssVEP participant mean") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5))


model003_lpp_fit_summary %>% 
  filter(str_detect(variable,"^bpar")) %>% 
  mutate(variable = factor(variable, levels = variable)) %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Participant") +
  scale_y_continuous(name = "Microvoltage") +
  ggtitle("LPP participant mean") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5)) +

    model003_ssvep_fit_summary %>% 
    filter(str_detect(variable,"^bpar")) %>% 
    mutate(variable = factor(variable, levels = variable)) %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Participant") +
  scale_y_continuous(name = "Microvoltage") +
  ggtitle("ssVEP participant mean") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5))

#stim
model003_lpp_fit_summary %>% 
  filter(str_detect(variable,"^bstim")) %>% 
  mutate(variable = factor(variable, levels = variable)) %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Participant") +
  scale_y_continuous(name = "Microvoltage") +
  ggtitle("LPP participant mean") +
  theme_bw() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5)) +

    model003_ssvep_fit_summary %>% 
    filter(str_detect(variable,"^bstim")) %>% 
    mutate(variable = factor(variable, levels = variable)) %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Participant") +
  scale_y_continuous(name = "Microvoltage") +
  ggtitle("ssVEP participant mean") +
  theme_bw() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5))
#not done
(model002__draws_df %>% 
    mutate(lpp_emotional_difference = ((`bcate_lpp[1]` + `bcate_lpp[3]`)/2) - `bcate_lpp[2]`) %>% 
    select(lpp_emotional_difference,
           starts_with("bcate")) %>% 
    pivot_longer(everything()) %>%
    ggplot() +
    geom_density(aes(x = value, fill = name), alpha = .5) +
    coord_cartesian(xlim = c(-10, 10)) +
    scale_fill_manual(values = c("blue","green","red","black"),
                      labels = c("Pleasant",
                                 "Neutral",
                                 "Unpleasant",
                                 "Emotional - Neutral"),
                      name = "Categories")+
    scale_x_continuous(breaks = seq(-10,10,by=1),
                       name = "Microvoltage") +
    ggtitle("LPP Category Means") +
    theme_bw()) /
  
  (model007_draws_df %>% 
     mutate(ssvep_emotional_difference = ((`bcate_ssvep[1]` + `bcate_ssvep[3]`)/2) - `bcate_ssvep[2]`) %>% 
     select(ssvep_emotional_difference,
            starts_with("bcate_ssvep")) %>% 
     pivot_longer(everything()) %>%
     ggplot() +
     geom_density(aes(x = value, fill = name), alpha = .5) +
     coord_cartesian(xlim = c(.275, -.275)) +
     scale_fill_manual(values = c("blue","green","red","black"),
                       labels = c("Pleasant",
                                  "Neutral",
                                  "Unpleasant",
                                  "Emotional - Neutral"),
                       name = "Categories")+
     scale_x_continuous(breaks = seq(.25,-.25, by = -.05),
                        name = "Microvoltage") +
     ggtitle("ssVEP Category Means") +
     theme_bw())



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
      select(par_id, stim_int_alphabetic, cate_int, lpp_amp, valence, arousal) %>% 
      reframe(par = par_id,
              type = as.integer(1),
              cate = cate_int,
              stim = stim_int_alphabetic,
              amp = lpp_amp,
              valence = valence,
              arousal = arousal),
    by_video_ssvep_amp_ratings %>% 
      merge(x = ., y = integer_key_for_stan, 
            by.x = "scene", by.y = "stim_name_alphabetic") %>% 
      mutate(cate_int = case_when(
        Stim_cat == "Pleasant" ~ 1,
        Stim_cat == "Neutral" ~ 2,
        Stim_cat == "Unpleasant" ~ 3)) %>% 
      select(par_id, stim_int_alphabetic, cate_int, ssvep_amp, valence, arousal) %>% 
      reframe(par = par_id,
              type = as.integer(2),
              cate = cate_int,
              stim = stim_int_alphabetic,
              amp = ssvep_amp,
              valence = valence,
              arousal = arousal))) %>% 
  mutate(.before = 2,
         vids_first_is_one = case_when(par %in% saw_vids_first ~ 1,
                                       par %in% saw_pics_first ~ 2))



# need to find proper priors for each ERP
data_list_for_stan <- list()

## uninformative priors for par higher-level distribution
data_list_for_stan$picture_par_bar_mean_prior <-
  data_for_stan_df %>%
  filter(type == 1) %>% 
  summarise("mean_amp" = mean(amp)) %>% 
  pull(mean_amp)

data_list_for_stan$picture_par_bar_sd_prior <-
  data_for_stan_df %>% 
  filter(type == 1) %>% 
  group_by(cate, type) %>%
  mutate(mean_amp = mean(amp),
         cate_adjusted_amp = amp - mean_amp) %>% 
  ungroup() %>% 
  summarise(cate_adjusted_amp_sd_prior = sd(cate_adjusted_amp) * 2) %>% 
  pull(cate_adjusted_amp_sd_prior)

data_list_for_stan$video_par_bar_mean_prior <-
  data_for_stan_df %>%
  filter(type == 2) %>% 
  summarise("mean_amp" = mean(amp)) %>% 
  pull(mean_amp)

data_list_for_stan$video_par_bar_sd_prior <-
  data_for_stan_df %>%
  filter(type == 2) %>% 
  group_by(cate, type) %>%
  mutate(mean_amp = mean(amp),
         cate_adjusted_amp = amp - mean_amp) %>% 
  ungroup() %>% 
  summarise(cate_adjusted_amp_sd_prior = sd(cate_adjusted_amp) * 2) %>% 
  pull(cate_adjusted_amp_sd_prior)

## uninformative priors for cate higher-level distribution

data_list_for_stan$picture_cate_bar_sd_prior <-
  data_for_stan_df %>% 
  filter(type == 1) %>% 
  group_by(par, type) %>%
  mutate(mean_amp = mean(amp),
         par_adjusted_amp = amp - mean_amp) %>% 
  ungroup() %>% 
  summarise(par_adjusted_amp_sd_prior = sd(par_adjusted_amp) * 2) %>% 
  pull(par_adjusted_amp_sd_prior)

data_list_for_stan$video_cate_bar_sd_prior <-
  data_for_stan_df %>%
  filter(type == 2) %>% 
  group_by(par, type) %>%
  mutate(mean_amp = mean(amp),
         par_adjusted_amp = amp - mean_amp) %>% 
  ungroup() %>% 
  summarise(par_adjusted_amp_sd_prior = sd(par_adjusted_amp) * 2) %>% 
  pull(par_adjusted_amp_sd_prior)

## uninformative priors for parXcate higher-level distribution
data_list_for_stan$picture_parxcate_sd_prior <-
  data_for_stan_df %>% 
  filter(type == 1) %>% 
  group_by(par, cate, type) %>%
  mutate(mean_amp = mean(amp),
         par_adjusted_amp = amp - mean_amp) %>% 
  ungroup() %>% 
  summarise(par_adjusted_amp_sd_prior = sd(par_adjusted_amp) * 2) %>% 
  pull(par_adjusted_amp_sd_prior)

data_list_for_stan$video_parxcate_sd_prior <-
  data_for_stan_df %>%
  filter(type == 2) %>% 
  group_by(par, cate, type) %>%
  mutate(mean_amp = mean(amp),
         par_adjusted_amp = amp - mean_amp) %>% 
  ungroup() %>% 
  summarise(par_adjusted_amp_sd_prior = sd(par_adjusted_amp) * 2) %>% 
  pull(par_adjusted_amp_sd_prior)


data_list_for_stan$nobs <- nrow(data_for_stan_df)
data_list_for_stan$nlpp <- nrow(data_for_stan_df[data_for_stan_df$type == 1,])
data_list_for_stan$nssvep <- nrow(data_for_stan_df[data_for_stan_df$type == 2,])
data_list_for_stan$npar <- data_for_stan_df$par %>% unique() %>% length()
data_list_for_stan$ncate <- as.integer(3)
data_list_for_stan$ntype <- as.integer(2)

data_list_for_stan$par = data_for_stan_df$par %>% as.factor() %>% as.integer()
data_list_for_stan$type = data_for_stan_df$type
data_list_for_stan$cate = data_for_stan_df$cate
data_list_for_stan$amp = data_for_stan_df$amp
data_list_for_stan$arousal = data_for_stan_df$arousal

#  ####

data_for_stan_df %>% 
  mutate(cate = factor(cate)) %>% 
  ggplot(aes(y = amp, 
             x = arousal)) +
  geom_quasirandom(aes(color = cate)) +
  geom_line(stat = "smooth", method = "lm", aes(group = 1), 
            alpha = .5, se = F, color = "black", linewidth = 2) +
  geom_ribbon(stat = "smooth", method = "lm",
              aes(ymin = ..y.. - (1.96 * ..se..), # make 95% CI
                  ymax = ..y.. + (1.96 * ..se..)), 
              alpha = .2, color = "black", linetype = "blank") +
  scale_color_manual(values = c("blue", "green", "red")) +
  # scale_color_colorblind()+
  scale_x_continuous(breaks = seq(1, 9, by = 1),
                     limits = c(0.5, 9.5))+
  facet_wrap(.~ type,scales = "free_y") +
  theme_classic()


data_for_stan_df %>% 
  group_by(cate, type, vids_first_is_one) %>%
  mutate(cate = factor(cate)) %>% 
  summarise(mean_amp = mean(amp),
            se_amp = std.error(amp)) %>% 
  ggplot() +
  geom_pointrange(aes(x = cate, y = mean_amp,
                      ymax = mean_amp + se_amp,
                      ymin = mean_amp - se_amp)) +
  facet_wrap(. ~ type + vids_first_is_one,scales = "free_y")


data_for_stan_df %>% 
  group_by(par, cate, type, vids_first_is_one) %>%
  mutate(cate = factor(cate)) %>% 
  summarise(mean_amp = mean(amp),
            se_amp = std.error(amp)) %>% 
  ggplot() +
  geom_line(aes(x = cate, y = mean_amp, group = par)) +
  geom_beeswarm(aes(x = cate, y = mean_amp,
                    color = cate)) +
  facet_wrap(. ~ type + vids_first_is_one, scales = "free_y")

data_for_stan_df %>% 
  group_by(par, type) %>%
  mutate(mean_amp = mean(amp),
         adjusted_amp = amp - mean_amp) %>% 
  group_by(par, cate, type, vids_first_is_one) %>%
  mutate(cate = factor(cate)) %>% 
  summarise(mean_amp = mean(adjusted_amp),
            se_amp = std.error(adjusted_amp)) %>% 
  ggplot() +
  geom_line(aes(x = cate, y = mean_amp, group = par)) +
  geom_beeswarm(aes(x = cate, y = mean_amp,
                    color = cate)) +
  facet_wrap(. ~ type + vids_first_is_one, scales = "free_y")


data_for_stan_df %>% 
  group_by(par, type) %>%
  mutate(mean_amp = mean(amp),
         adjusted_amp = amp - mean_amp) %>% 
  group_by(type) %>% 
  summarise(sd(adjusted_amp))

data_for_stan_df %>% 
  group_by(cate, type) %>%
  mutate(mean_amp = mean(amp),
         adjusted_amp = amp - mean_amp) %>% 
  group_by(type) %>% 
  summarise(sd(adjusted_amp))

data_for_stan_df %>% 
  group_by(par, cate, type) %>%
  mutate(mean_amp = mean(amp),
         adjusted_amp = amp - mean_amp) %>% 
  group_by(type) %>% 
  summarise(sd(adjusted_amp))

data_for_stan_df %>% 
  filter(type == 1) %>% 
  mutate(emotional = case_when(cate == 1 ~ "emotional",
                               cate == 2 ~ "neutral",
                               cate == 3 ~ "emotional")) %>% 
  group_by(par,emotional) %>%
  summarise(mean_amp = mean(amp)) %>% 
  group_by(par) %>% 
  summarise(diff_amp = sum(mean_amp[emotional == "emotional"]) - sum(mean_amp[emotional == "neutral"])) %>%
  filter(diff_amp > 0) %>%
  summarise(count = n()/46) %>% 
  print(n =50)



data_for_stan_df %>% 
  filter(type == 2) %>% 
  mutate(emotional = case_when(cate == 1 ~ "emotional",
                               cate == 2 ~ "neutral",
                               cate == 3 ~ "emotional")) %>% 
  group_by(par,emotional) %>%
  summarise(mean_amp = mean(amp)) %>% 
  group_by(par) %>% 
  summarise(diff_amp = sum(mean_amp[emotional == "emotional"]) - sum(mean_amp[emotional == "neutral"])) %>%
  filter(diff_amp < 0) %>%
  summarise(count = n()/46) %>% 
  print(n =50)

# Category models, probably would keep model1,2,7 with 7 being the final####
# Fit model 1: random effect participant ####

model001_path <- paste0(pic_vid_repository,"/pic_vid001_par.stan")

model001 <- cmdstan_model(model001_path)

model001_fit <- model001$sample(
  data = data_list_for_stan,
  refresh = 200,
  seed = 2, 
  iter_warmup = 5000,
  iter_sampling = 8000,
  save_warmup = F,
  show_messages = T,
  chains = 4, 
  parallel_chains = 4)

model001_meta_data <- model001_fit$metadata()

relevant_model001_parameters <- model001_meta_data$model_params[
  !str_detect(model001_meta_data$model_params, "log_lik")]

model001_summary <- model001_fit$summary(
  variables = relevant_model001_parameters)

model001_loo <- model001_fit$loo(save_psis = TRUE)

# I should look at prior compared to posterior distributions here


# model001_plot_df <- 
lpp_posterior_summary_df001 <- model001_summary %>% 
  filter(str_detect(variable,"^bpar_lpp")) %>% 
  mutate(variable = factor(variable, levels = variable))

ssvep_posterior_summary_df001 <- model001_summary %>% 
  filter(str_detect(variable,"^bpar_ssvep")) %>% 
  mutate(variable = factor(variable, levels = variable))

lpp_posterior_summary_df001 %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Participant") +
  scale_y_continuous(name = "Microvoltage") +
  ggtitle("LPP participant mean") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5)) +
  
  ssvep_posterior_summary_df001 %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Participant") +
  scale_y_continuous(name = "Microvoltage") +
  ggtitle("ssVEP participant mean") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5))

# Fit model 2: random effect participant, category effect####

model002_path <-model001_path <- paste0(pic_vid_repository,
                                        "/pic_vid002_par_cate.stan")

model002 <- cmdstan_model(model002_path)

model002_fit <- model002$sample(
  data = data_list_for_stan,
  refresh = 200,
  seed = 2, 
  iter_warmup = 10000,
  iter_sampling = 8000,
  save_warmup = F,
  show_messages = T,
  chains = 4, 
  parallel_chains = 4)

model002_meta_data <- model002_fit$metadata()

relevant_model002_parameters <- model002_meta_data$model_params[
  !str_detect(model002_meta_data$model_params, "log_lik")]

model002_summary <- model002_fit$summary(
  variables = relevant_model002_parameters)


# model001_loo <- model001_fit$loo(save_psis = TRUE)
model002_loo <- model002_fit$loo(save_psis = TRUE)

model001_loo
model002_loo

loo::loo_compare(model001_loo, model002_loo)
loo::loo_model_weights(list(model001_loo, model002_loo))

model002_draws <- model002_fit$draws(inc_warmup = F)
bayesplot::mcmc_trace(model002_draws[,,c(1:5)])

lpp_posterior_summary_df <- model002_summary %>% 
  filter(str_detect(variable,"^bpar_lpp")) %>% 
  mutate(variable = factor(variable, levels = variable))

ssvep_posterior_summary_df <- model002_summary %>% 
  filter(str_detect(variable,"^bpar_ssvep")) %>% 
  mutate(variable = factor(variable, levels = variable))

lpp_posterior_summary_df %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Parameter") +
  # scale_y_continuous(breaks = seq(-2,16, by = 2), name = "Microvoltage") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5)) +
  
  ssvep_posterior_summary_df %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Parameter") +
  # scale_y_continuous(breaks = seq(-2,16, by = 2), name = "Microvoltage") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5))


model002_draws_df <- model002_fit$draws(format = "df")


(model002_draws_df %>% 
    select(starts_with("bcate_lpp")) %>% 
    pivot_longer(cols = everything()) %>% 
    ggplot() +
    geom_density(aes(x = value, fill = name), alpha = .5) +
    scale_x_continuous(limits = c(-10,10)) +
    theme_bw()) /
  
  (model002_draws_df %>% 
     select(starts_with("bcate_ssvep")) %>% 
     pivot_longer(cols = everything()) %>% 
     ggplot() +
     geom_density(aes(x = -value, fill = name), alpha = .5) +
     scale_x_continuous(limits = c(-.275,.275)) +
     theme_bw())

model002_draws_df %>% 
  mutate(lpp_emotional_difference = ((`bcate_lpp[1]` + `bcate_lpp[3]`)/2) - `bcate_lpp[2]`) %>% 
  select(lpp_emotional_difference,
         starts_with("bcate_lpp")) %>% 
  pivot_longer(everything()) %>%
  ggplot() +
  geom_density(aes(x = value, fill = name), alpha = .5) +
  scale_x_continuous(limits = c(-10,10)) +
  theme_bw()

model002_draws_df %>% 
  mutate(ssvep_emotional_difference = ((`bcate_ssvep[1]` + `bcate_ssvep[3]`)/2) - `bcate_ssvep[2]`) %>% 
  select(ssvep_emotional_difference,
         starts_with("bcate_ssvep")) %>% 
  pivot_longer(everything()) %>%
  ggplot() +
  geom_density(aes(x = value, fill = name), alpha = .5) +
  scale_x_continuous(limits = c(-.275,.275)) +
  theme_bw()

# make posterior predictive
lpp_pos_pred_df <- model002_draws_df %>% 
  mutate(lpp_emotional_difference = ((`bcate_lpp[1]` + `bcate_lpp[3]`)/2) - `bcate_lpp[2]`) %>% 
  select(lpp_emotional_difference, lpp_sd)

(sum(rnorm(12000, 
           mean = lpp_pos_pred_df$lpp_emotional_difference, 
           sd = lpp_pos_pred_df$lpp_sd) > 0) / 12000) *100

rnorm(12000, mean = lpp_pos_pred_df$lpp_emotional_difference, sd = lpp_pos_pred_df$lpp_sd) %>% 
  density() %>% 
  plot()

ssvep_pos_pred_df <- model002_draws_df %>% 
  mutate(ssvep_emotional_difference = ((`bcate_ssvep[1]` + `bcate_ssvep[3]`)/2) - `bcate_ssvep[2]`)  %>% 
  select(ssvep_emotional_difference, ssvep_sd)

(sum(rnorm(12000, 
           mean = ssvep_pos_pred_df$ssvep_emotional_difference, 
           sd = ssvep_pos_pred_df$ssvep_sd) < 0) / 12000) *100

# Fit model 3: random effect participant, category effect, participant x category interaction ####

model003_path <-"/home/andrewf/Repositories/Pic_Vid/pic_vid003_par_cate_int.stan"

model003 <- cmdstan_model(model003_path)

model003_fit <- model003$sample(
  data = data_list_for_stan,
  refresh = 100,
  seed = 2, 
  iter_warmup = 2000,
  iter_sampling = 2000,
  save_warmup = F,
  show_messages = T,
  chains = 6, 
  parallel_chains = 6)

model003_summary <- model003_fit$summary()


model003_draws <- model003_fit$draws(inc_warmup = F)
bayesplot::mcmc_trace(model003_draws[,,c(1:5)])

lpp_posterior_summary_df <- model003_summary %>% 
  filter(str_detect(variable,"^bpar_lpp")) %>% 
  mutate(variable = factor(variable, levels = variable))

ssvep_posterior_summary_df <- model003_summary %>% 
  filter(str_detect(variable,"^bpar_ssvep")) %>% 
  mutate(variable = factor(variable, levels = variable))

lpp_posterior_summary_df %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Parameter") +
  # scale_y_continuous(breaks = seq(-2,16, by = 2), name = "Microvoltage") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5)) +
  
  ssvep_posterior_summary_df %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Parameter") +
  # scale_y_continuous(breaks = seq(-2,16, by = 2), name = "Microvoltage") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5))

model003_draws_df <- model003_fit$draws(format = "df")

(model002_draws_df %>% 
    mutate(lpp_emotional_difference = ((`bcate_lpp[1]` + `bcate_lpp[3]`)/2) - `bcate_lpp[2]`) %>% 
    select(lpp_emotional_difference,
           starts_with("bcate_lpp")) %>% 
    pivot_longer(everything()) %>%
    ggplot() +
    geom_density(aes(x = value, fill = name), alpha = .5) +
    scale_x_continuous(limits = c(-10,10)) +
    theme_bw()) /
  (model003_draws_df %>% 
     mutate(lpp_emotional_difference = ((`bcate_lpp[1]` + `bcate_lpp[3]`)/2) - `bcate_lpp[2]`) %>% 
     select(lpp_emotional_difference,
            starts_with("bcate_lpp")) %>% 
     pivot_longer(everything()) %>%
     ggplot() +
     geom_density(aes(x = value, fill = name), alpha = .5) +
     scale_x_continuous(limits = c(-10,10)) +
     theme_bw())

(model002_draws_df %>% 
    mutate(ssvep_emotional_difference = ((`bcate_ssvep[1]` + `bcate_ssvep[3]`)/2) - `bcate_ssvep[2]`) %>% 
    select(ssvep_emotional_difference,
           starts_with("bcate_ssvep")) %>% 
    pivot_longer(everything()) %>%
    ggplot() +
    geom_density(aes(x = value, fill = name), alpha = .5) +
    scale_x_continuous(limits = c(-.275,.275)) +
    theme_bw()) /
  (model003_draws_df %>% 
     mutate(ssvep_emotional_difference = ((`bcate_ssvep[1]` + `bcate_ssvep[3]`)/2) - `bcate_ssvep[2]`) %>% 
     select(ssvep_emotional_difference,
            starts_with("bcate_ssvep")) %>% 
     pivot_longer(everything()) %>%
     ggplot() +
     geom_density(aes(x = value, fill = name), alpha = .5) +
     scale_x_continuous(limits = c(-.275,.275)) +
     theme_bw())

# make posterior predictive
lpp_pos_pred_df <- model003_draws_df %>% 
  mutate(lpp_emotional_difference = ((`bcate_lpp[1]` + `bcate_lpp[3]`)/2) - `bcate_lpp[2]`) %>% 
  select(lpp_emotional_difference, lpp_sd)

(sum(rnorm(12000, 
           mean = lpp_pos_pred_df$lpp_emotional_difference, 
           sd = lpp_pos_pred_df$lpp_sd) > 0) / 12000) *100

rnorm(12000, mean = lpp_pos_pred_df$lpp_emotional_difference, sd = lpp_pos_pred_df$lpp_sd) %>% 
  density() %>% 
  plot()

ssvep_pos_pred_df <- model003_draws_df %>% 
  mutate(ssvep_emotional_difference = ((`bcate_ssvep[1]` + `bcate_ssvep[3]`)/2) - `bcate_ssvep[2]`)  %>% 
  select(ssvep_emotional_difference, ssvep_sd)

(sum(rnorm(12000, 
           mean = ssvep_pos_pred_df$ssvep_emotional_difference, 
           sd = ssvep_pos_pred_df$ssvep_sd) < 0) / 12000) *100

# Fit model 4: random effect participant, category effect skew normal ####

model004_path <-"/home/andrewf/Repositories/Pic_Vid/pic_vid004_par_cate_skew.stan"

model004 <- cmdstan_model(model004_path)

model004_fit <- model004$sample(
  data = data_list_for_stan,
  refresh = 100,
  seed = 2, 
  iter_warmup = 10000,
  iter_sampling = 5000,
  save_warmup = F,
  show_messages = T,
  chains = 6, 
  parallel_chains = 6)

model004_summary <- model004_fit$summary()


model004_draws <- model004_fit$draws(inc_warmup = F)
bayesplot::mcmc_trace(model004_draws[,,c(1:5)])

lpp_posterior_summary_df004 <- model004_summary %>% 
  filter(str_detect(variable,"^bpar_lpp")) %>% 
  mutate(variable = factor(variable, levels = variable))

ssvep_posterior_summary_df004 <- model004_summary %>% 
  filter(str_detect(variable,"^bpar_ssvep")) %>% 
  mutate(variable = factor(variable, levels = variable))

lpp_posterior_summary_df004 %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Parameter") +
  # scale_y_continuous(breaks = seq(-2,16, by = 2), name = "Microvoltage") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5)) +
  
  ssvep_posterior_summary_df004 %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Parameter") +
  # scale_y_continuous(breaks = seq(-2,16, by = 2), name = "Microvoltage") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5))

# Fit model 5: random effect participant, category effect t-distribution ####

model005_path <-"/home/andrewf/Repositories/Pic_Vid/pic_vid005_par_cate_tdis.stan"

model005 <- cmdstan_model(model005_path)

model005_fit <- model005$sample(
  data = data_list_for_stan,
  refresh = 100,
  seed = 2, 
  iter_warmup = 10000,
  iter_sampling = 5000,
  save_warmup = F,
  show_messages = T,
  chains = 6, 
  parallel_chains = 6)

model005_summary <- model005_fit$summary()


model005_draws <- model005_fit$draws(inc_warmup = F)
bayesplot::mcmc_trace(model005_draws[,,c(1:5)])

lpp_posterior_summary_df005 <- model005_summary %>% 
  filter(str_detect(variable,"^bpar_lpp")) %>% 
  mutate(variable = factor(variable, levels = variable))

ssvep_posterior_summary_df005 <- model005_summary %>% 
  filter(str_detect(variable,"^bpar_ssvep")) %>% 
  mutate(variable = factor(variable, levels = variable))

lpp_posterior_summary_df005 %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Parameter") +
  # scale_y_continuous(breaks = seq(-2,16, by = 2), name = "Microvoltage") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5)) +
  
  ssvep_posterior_summary_df005 %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Parameter") +
  # scale_y_continuous(breaks = seq(-2,16, by = 2), name = "Microvoltage") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5))

model005_draws_df <- model005_fit$draws(format = "df")


(model005_draws_df %>% 
    select(starts_with("bcate_lpp")) %>% 
    pivot_longer(cols = everything()) %>% 
    ggplot() +
    geom_density(aes(x = value, fill = name), alpha = .5) +
    scale_x_continuous(limits = c(-10,10)) +
    theme_bw()) /
  
  (model005_draws_df %>% 
     select(starts_with("bcate_ssvep")) %>% 
     pivot_longer(cols = everything()) %>% 
     ggplot() +
     geom_density(aes(x = -value, fill = name), alpha = .5) +
     scale_x_continuous(limits = c(-.275,.275)) +
     theme_bw())

model005_draws_df %>% 
  mutate(lpp_emotional_difference = ((`bcate_lpp[1]` + `bcate_lpp[3]`)/2) - `bcate_lpp[2]`) %>% 
  select(lpp_emotional_difference,
         starts_with("bcate_lpp")) %>% 
  pivot_longer(everything()) %>%
  ggplot() +
  geom_density(aes(x = value, fill = name), alpha = .5) +
  scale_x_continuous(limits = c(-10,10)) +
  theme_bw()

model005_draws_df %>% 
  mutate(ssvep_emotional_difference = ((`bcate_ssvep[1]` + `bcate_ssvep[3]`)/2) - `bcate_ssvep[2]`) %>% 
  select(ssvep_emotional_difference,
         starts_with("bcate_ssvep")) %>% 
  pivot_longer(everything()) %>%
  ggplot() +
  geom_density(aes(x = value, fill = name), alpha = .5) +
  scale_x_continuous(limits = c(-.275,.275)) +
  theme_bw()

# Fit model 6: random effect participant, category effect t-distribution ####

model006_path <-"/home/andrewf/Repositories/Pic_Vid/pic_vid006_par_cate_tdis.stan"

model006 <- cmdstan_model(model006_path)

model006_fit <- model006$sample(
  data = data_list_for_stan,
  refresh = 100,
  seed = 2, 
  iter_warmup = 2000,
  iter_sampling = 2000,
  save_warmup = F,
  show_messages = T,
  chains = 6, 
  parallel_chains = 6)

model006_summary <- model006_fit$summary()


model006_draws <- model006_fit$draws(inc_warmup = F)
bayesplot::mcmc_trace(model006_draws[,,c(1:5)])

lpp_posterior_summary_df006 <- model006_summary %>% 
  filter(str_detect(variable,"^bpar_lpp")) %>% 
  mutate(variable = factor(variable, levels = variable))

ssvep_posterior_summary_df006 <- model006_summary %>% 
  filter(str_detect(variable,"^bpar_ssvep")) %>% 
  mutate(variable = factor(variable, levels = variable))

lpp_posterior_summary_df006 %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Parameter") +
  # scale_y_continuous(breaks = seq(-2,16, by = 2), name = "Microvoltage") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5)) +
  
  ssvep_posterior_summary_df006 %>% 
  ggplot() +
  geom_pointrange(aes(x = variable, y = mean,
                      ymin = q5, ymax = q95), 
                  color = "red") +
  # geom_point(aes(x = variable, y = mean_epn)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3), name = "Parameter") +
  # scale_y_continuous(breaks = seq(-2,16, by = 2), name = "Microvoltage") +
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 5))

model006_draws_df <- model006_fit$draws(format = "df")


(model006_draws_df %>% 
    select(starts_with("bcate_lpp")) %>% 
    pivot_longer(cols = everything()) %>% 
    ggplot() +
    geom_density(aes(x = value, fill = name), alpha = .5) +
    scale_x_continuous(limits = c(-10,10)) +
    theme_bw()) /
  
  (model006_draws_df %>% 
     select(starts_with("bcate_ssvep")) %>% 
     pivot_longer(cols = everything()) %>% 
     ggplot() +
     geom_density(aes(x = -value, fill = name), alpha = .5) +
     scale_x_continuous(limits = c(-.275,.275)) +
     theme_bw())

model006_draws_df %>% 
  mutate(lpp_emotional_difference = ((`bcate_lpp[1]` + `bcate_lpp[3]`)/2) - `bcate_lpp[2]`) %>% 
  select(lpp_emotional_difference,
         starts_with("bcate_lpp")) %>% 
  pivot_longer(everything()) %>%
  ggplot() +
  geom_density(aes(x = value, fill = name), alpha = .5) +
  scale_x_continuous(limits = c(-10,10)) +
  theme_bw()

model006_draws_df %>% 
  mutate(ssvep_emotional_difference = ((`bcate_ssvep[1]` + `bcate_ssvep[3]`)/2) - `bcate_ssvep[2]`) %>% 
  select(ssvep_emotional_difference,
         starts_with("bcate_ssvep")) %>% 
  pivot_longer(everything()) %>%
  ggplot() +
  geom_density(aes(x = value, fill = name), alpha = .5) +
  scale_x_continuous(limits = c(-.275,.275)) +
  theme_bw()


# Fit model 7: random effect participant, category effect t-distribution ####


model007_path <-model001_path <- paste0(pic_vid_repository,
                                        "/pic_vid007_par_cate_tdis.stan")

model007 <- cmdstan_model(model007_path)

model007_fit <- model007$sample(
  data = data_list_for_stan,
  refresh = 200,
  seed = 2, 
  iter_warmup = 10000,
  iter_sampling = 8000,
  save_warmup = F,
  show_messages = T,
  chains = 4, 
  parallel_chains = 4)

model007_meta_data <- model007_fit$metadata()

relevant_model007_parameters <- model007_meta_data$model_params[
  !str_detect(model007_meta_data$model_params, "log_lik")]

model007_summary <- model007_fit$summary(
  variables = relevant_model007_parameters)

# model001_loo <- model001_fit$loo(save_psis = TRUE)
# model002_loo <- model002_fit$loo(save_psis = TRUE)
model007_loo <- model007_fit$loo(save_psis = TRUE)

model001_loo
model002_loo
model007_loo

loo::loo_compare(model001_loo, model002_loo, model007_loo)
loo::loo_model_weights(list(model001_loo, model002_loo, model007_loo))
loo::loo_model_weights(list(model001_loo, model002_loo))
loo::loo_model_weights(list(model007_loo, model002_loo))


# make posterior predictive
model007_draws_df <- model007_fit$draws(format = "df")

# n number of trials
lpp_predict_emotion_trial_contrast <- function(model_draws_df){
  
  pleasant_trial = 
    rnorm(nrow(model_draws_df),
          mean = model_draws_df$`bcate_lpp[1]`,
          sd = model007_draws_df$lpp_sd)
  
  neutral_trial =
    rnorm(nrow(model_draws_df),
          mean = model_draws_df$`bcate_lpp[2]`,
          sd = model007_draws_df$lpp_sd)
  
  unpleasant_trial =
    rnorm(nrow(model_draws_df),
          mean = model_draws_df$`bcate_lpp[3]`,
          sd = model007_draws_df$lpp_sd)
  
  emotional_difference_prediction <- ((pleasant_trial + unpleasant_trial)/2) -
    neutral_trial
  
  emotional_difference_prediction
}

ssvep_predict_emotion_trial_contrast <- function(model_draws_df){
  
  pleasant_trial = 
    rnorm(nrow(model_draws_df),
          mean = model_draws_df$`bcate_ssvep[1]`,
          sd = model007_draws_df$ssvep_sd)
  
  neutral_trial =
    rnorm(nrow(model_draws_df),
          mean = model_draws_df$`bcate_ssvep[2]`,
          sd = model007_draws_df$ssvep_sd)
  
  unpleasant_trial =
    rnorm(nrow(model_draws_df),
          mean = model_draws_df$`bcate_ssvep[3]`,
          sd = model007_draws_df$ssvep_sd)
  
  emotional_difference_prediction <- ((pleasant_trial + unpleasant_trial)/2) -
    neutral_trial
  
  emotional_difference_prediction
}

mean_list_vectors <- function(list_of_vectors){
  list_of_vectors %>% list2DF() %>% rowMeans()
}

for (trials in c(1,5,15,30,50)) {
  
  lpp_list_of_vectors <- purrr::map(rep(list(model007_draws_df), times = trials),
                                    lpp_predict_emotion_trial_contrast) 
  ssvep_list_of_vectors <- purrr::map(rep(list(model007_draws_df), times = trials),
                                      ssvep_predict_emotion_trial_contrast) 
  
  if (trials == 1) {
    lpp_trials_plot_df <- data.frame(trials = trials,
                                     samples = mean_list_vectors(lpp_list_of_vectors))
    ssvep_trials_plot_df <- data.frame(trials = trials,
                                       samples = mean_list_vectors(ssvep_list_of_vectors))
  } else {
    lpp_trials_plot_df <- rbind.data.frame(lpp_trials_plot_df,
                                           data.frame(
                                             trials = trials,
                                             samples = mean_list_vectors(
                                               lpp_list_of_vectors)))
    ssvep_trials_plot_df <- rbind.data.frame(ssvep_trials_plot_df,
                                             data.frame(
                                               trials = trials,
                                               samples = mean_list_vectors(
                                                 ssvep_list_of_vectors)))
  }
}

lpp_trials_plot_df <- lpp_trials_plot_df %>% 
  mutate(trials = factor(trials))
ssvep_trials_plot_df <- ssvep_trials_plot_df %>% 
  mutate(trials = factor(trials))

line_thickness = 1.5
line_alpha = 1


(lpp_trials_plot_df %>%  
    ggplot() +
    geom_vline(aes(xintercept = 0),
               linewidth = line_thickness,
               linetype = "dashed") +
    geom_line(aes(x = samples, group = trials),
              color = "black",
              linewidth = line_thickness +.5, 
              stat="density",
              alpha=line_alpha) +
    geom_line(aes(x = samples, 
                  color = trials),
              linewidth = line_thickness, 
              stat="density",
              alpha=line_alpha) +
    scale_color_colorblind(name = "Number of trials per valence") +
    coord_cartesian(xlim = c(-10, 10)) +
    scale_x_continuous(breaks = seq(-10, 10, by = 2),
                       name = "Microvoltage") +
    ggtitle("LPP posterior predictive") +
    theme_classic()) /
  
  (ssvep_trials_plot_df %>% 
     ggplot() +
     geom_vline(aes(xintercept = 0),
                linewidth = line_thickness,
                linetype = "dashed") +
     geom_line(aes(x = samples, group = trials),
               color = "black",
               linewidth = line_thickness +.5, 
               stat="density",
               alpha=line_alpha) +
     geom_line(aes(x = samples, 
                   color = trials),
               linewidth = line_thickness, 
               stat="density",
               alpha=line_alpha) +
     scale_color_colorblind(name = "Number of trials per valence") +
     coord_cartesian(xlim = c(.27, -.27)) +
     scale_x_continuous(breaks = seq(.25, -.25, by = -.05),
                        labels = round(seq(.25, -.25, by = -.05),2),
                        name = "Microvoltage") +
     ggtitle("ssVEP posterior predictive") +
     # scale_x_reverse() +
     theme_classic())

lpp_trials_plot_df %>% 
  group_by(trials) %>% 
  summarise(percentage = mean(samples > 0) * 100)

ssvep_trials_plot_df %>% 
  group_by(trials) %>% 
  summarise(percentage = mean(samples < 0) * 100)



(model007_draws_df %>% 
    mutate(lpp_emotional_difference = ((`bcate_lpp[1]` + `bcate_lpp[3]`)/2) - `bcate_lpp[2]`) %>% 
    select(lpp_emotional_difference,
           starts_with("bcate_lpp")) %>% 
    pivot_longer(everything()) %>%
    ggplot() +
    geom_density(aes(x = value, fill = name), alpha = .5) +
    coord_cartesian(xlim = c(-10, 10)) +
    scale_fill_manual(values = c("blue","green","red","black"),
                      labels = c("Pleasant",
                                 "Neutral",
                                 "Unpleasant",
                                 "Emotional - Neutral"),
                      name = "Categories")+
    scale_x_continuous(breaks = seq(-10,10,by=1),
                       name = "Microvoltage") +
    ggtitle("LPP Category Means") +
    theme_bw()) /
  
  (model007_draws_df %>% 
     mutate(ssvep_emotional_difference = ((`bcate_ssvep[1]` + `bcate_ssvep[3]`)/2) - `bcate_ssvep[2]`) %>% 
     select(ssvep_emotional_difference,
            starts_with("bcate_ssvep")) %>% 
     pivot_longer(everything()) %>%
     ggplot() +
     geom_density(aes(x = value, fill = name), alpha = .5) +
     coord_cartesian(xlim = c(.275, -.275)) +
     scale_fill_manual(values = c("blue","green","red","black"),
                       labels = c("Pleasant",
                                  "Neutral",
                                  "Unpleasant",
                                  "Emotional - Neutral"),
                       name = "Categories")+
     scale_x_continuous(breaks = seq(.25,-.25, by = -.05),
                        name = "Microvoltage") +
     ggtitle("ssVEP Category Means") +
     theme_bw())





# Arousal models ####

# Fit model 8: just arousal erp covariance ####

model008_path <-"/home/andrewf/Repositories/Pic_Vid/pic_vid008_par_arousal.stan"

model008 <- cmdstan_model(model008_path)

model008_fit <- model008$sample(
  data = data_list_for_stan,
  refresh = 200,
  seed = 2, 
  iter_warmup = 10000,
  iter_sampling = 8000,
  save_warmup = F,
  show_messages = T,
  chains = 4, 
  parallel_chains = 4)

model008_meta_data <- model008_fit$metadata()

relevant_model008_parameters <- model008_meta_data$model_params[
  !str_detect(model008_meta_data$model_params, "log_lik")]

model008_summary <- model008_fit$summary(
  variables = relevant_model008_parameters)

# model001_loo <- model001_fit$loo(save_psis = TRUE)
# model002_loo <- model002_fit$loo(save_psis = TRUE)
model008_loo <- model008_fit$loo(save_psis = TRUE)

# Fit model 9: erp X arousal participant intercepts ####

model009_path <-"/home/andrewf/Repositories/Pic_Vid/pic_vid009_par_arousal.stan"

model009 <- cmdstan_model(model009_path)

model009_fit <- model009$sample(
  data = data_list_for_stan,
  refresh = 200,
  seed = 2, 
  iter_warmup = 10000,
  iter_sampling = 8000,
  save_warmup = F,
  show_messages = T,
  chains = 4, 
  parallel_chains = 4)

model009_meta_data <- model009_fit$metadata()

relevant_model009_parameters <- model009_meta_data$model_params[
  !str_detect(model009_meta_data$model_params, "log_lik")]

model009_summary <- model009_fit$summary(
  variables = relevant_model009_parameters)

# model001_loo <- model001_fit$loo(save_psis = TRUE)
# model002_loo <- model002_fit$loo(save_psis = TRUE)
model009_loo <- model009_fit$loo(save_psis = TRUE)

model001_loo
model002_loo
model007_loo
model008_loo
model009_loo

loo::loo_compare(model001_loo, model002_loo, model007_loo,model008_loo, model009_loo)
loo::loo_model_weights(list(model001_loo, model002_loo, model007_loo,model008_loo, model009_loo))
loo::loo_model_weights(list(model001_loo, model002_loo))
loo::loo_model_weights(list(model007_loo, model002_loo))
loo::loo_model_weights(list(model007_loo, model008_loo))
loo::loo_model_weights(list(model008_loo, model009_loo))
loo::loo_model_weights(list(model007_loo, model009_loo))

data_for_stan_df %>% 
  filter(type == 1) %>% 
  lm(data = ., formula = amp ~ arousal) %>% 
  lm.beta::lm.beta()
  summary()

data_for_stan_df %>% 
  filter(type == 2) %>% 
  lm(data = ., formula = amp ~ arousal) %>% 
  lm.beta::lm.beta() %>% 
  summary()

## Residuals / predictions for each observation ####
  
  
