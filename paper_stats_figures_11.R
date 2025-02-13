# packages ####
library(tidyverse)
library(patchwork)
library(cmdstanr)
library(plotrix)
library(ggbeeswarm)
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
no_pictures <- c(9) # corrupted file
no_videos <- c(38) # pulled out of study, felt ill
missing_over_50perc_from_valence_cat <- c(22,39,49)
bad_participants <- c(no_pictures, no_videos, missing_over_50perc_from_valence_cat) 

## no picture participants were missing more than 50% for any valence

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

## Load in ERPs ####
# lpp 400ms 279pt - 900ms 526pt
start_time_ms <- -125
number_of_time_points <- 1089
sample_rate_hz <- 512

lpp_time_key <- data.frame(time_pt = 1:number_of_time_points) %>% 
  mutate(time_ms = rep(start_time_ms,number_of_time_points) + 
           ((((1:number_of_time_points)-1) * (1000/sample_rate_hz))),
         V_time_pt = paste0("V", time_pt))

## load by trial data ####
load(paste0(parent_directory,
            "/paper_data_models/data/pic_vid_paper.RData"))

## Get by category / grand means ####
lpp_cat_dat <- data_for_stan_df %>%
  filter(type == 1) %>% 
  select(par, cate, amp) %>% 
  rename("lpp_amp" = amp) %>% 
  group_by(par,cate) %>% 
  summarise_all(mean) %>% 
  mutate("category_name" = case_when(
    cate == 1 ~ "pleasant",
    cate == 2 ~ "neutral",
    cate == 3 ~ "unpleasant"),
    .after = cate) %>% 
  mutate("zscore_lpp_amp" = as.numeric(scale(lpp_amp)))

ssvep_cat_dat <- data_for_stan_df %>%
  filter(type == 2) %>% 
  select(par, cate, amp) %>% 
  rename("ssvep_amp" = amp) %>% 
  group_by(par,cate) %>% 
  summarise_all(mean) %>% 
  mutate("category_name" = case_when(
    cate == 1 ~ "pleasant",
    cate == 2 ~ "neutral",
    cate == 3 ~ "unpleasant"),
    .after = cate) %>% 
  mutate("zscore_ssvep_amp" = as.numeric(scale(ssvep_amp)))


ssvep_lpp_dat_by_participant <- lpp_cat_dat %>% 
  full_join(ssvep_cat_dat,  by = c("par","cate", "category_name")) %>% 
  mutate(cate = factor(cate,
                       levels = c(1,2,3)),
         category_name = factor(category_name,
                                levels = c("pleasant", "neutral", "unpleasant")))

gm_ssvep_lpp <- ssvep_lpp_dat_by_participant %>% 
  group_by(category_name) %>% 
  summarise(mean_zscore_lpp   = mean(zscore_lpp_amp, na.rm = T),
            se_zscore_lpp     = plotrix::std.error(zscore_lpp_amp),
            mean_zscore_ssvep = mean(-zscore_ssvep_amp, na.rm = T),
            se_zscore_ssvep   = plotrix::std.error(zscore_ssvep_amp))

gm_amp_long <- gm_ssvep_lpp %>% 
  pivot_longer(
    cols = starts_with("mean_") | starts_with("se_"),
    names_to = c(".value", "erp_type"),
    names_pattern = "(mean_|se_)(.*)"
  ) %>% 
  rename(mean_amp = mean_,
         se_amp = se_) 

gm_by_scene_lpp_amp <-  data_for_stan_df %>%
  filter(type == 1) %>% 
  select(par, stim, stim_name, amp, valence, arousal) %>% 
  rename("lpp_amp" = amp) %>% 
  group_by(par) %>% 
  mutate("zscore_lpp_amp" = as.numeric(scale(lpp_amp))) %>%
  ungroup() %>% 
  select(-par) %>% 
  group_by(stim, stim_name) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  mutate(Stim_type = factor("Pics",
                            levels = c("Pics",
                                       "Video")),
         .before = 1)

gm_by_video_ssvep_amp <-  data_for_stan_df %>%
  filter(type == 2) %>% 
  select(par, stim, stim_name, amp, valence, arousal) %>% 
  rename("ssvep_amp" = amp) %>% 
  group_by(par) %>% 
  mutate("zscore_ssvep_amp" = as.numeric(scale(ssvep_amp))) %>%
  ungroup() %>% 
  select(-par) %>% 
  group_by(stim, stim_name) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  mutate(Stim_type = factor("Video",
                            levels = c("Pics",
                                       "Video")),
         .before = 1)



gm_erp_by_scene <- rbind.data.frame(rename(gm_by_scene_lpp_amp,
                                           "amp" = lpp_amp,
                                           "zscore_amp" = zscore_lpp_amp),
                                    rename(gm_by_video_ssvep_amp,
                                           "amp" = ssvep_amp,
                                           "zscore_amp" = zscore_ssvep_amp))

by_scene_ratings_with_path <- ratings_data %>% 
  group_by(Stim_name, Stim_type, Stim_cat) %>% 
  summarise(mean_aro = mean(arousal),
            mean_val = mean(valence)) %>% 
  mutate(path = paste0("/home/andrewf/Research_data/EEG/Pic_Vid/Stimuli/matt_diss/Pics/", 
                       Stim_name, ".jpg"))  

ratings_erps_path_by_scene <- merge(gm_erp_by_scene, y = by_scene_ratings_with_path,
                                    by.x = c("Stim_type", "stim_name"), by.y = c("Stim_type", "Stim_name"))




## Get category waveforms ####
# note that matched pairs of videos and scenes did not have the same id
# number in the original study so different keys have to be used
pic_id_key <- read.csv(paste0(parent_directory, "/misc/Picture_id_number.csv"))

stim_name_cate <- data_for_stan_df %>% 
  select(stim_name, cate) %>% 
  group_by(stim_name, cate) %>% 
  summarise_all(mean)


lpp_trial_wave <- EMEGShelper::read_ar_files(data_folders = picture_by_scene_ar_directory,
                                             patterns = ".ar$",
                                             baseline_pts = c(14:65),
                                             average_channels = T,
                                             include_file_name = T,
                                             extract_channels = lpp_chans)

lpp_trial_wave <- lpp_trial_wave %>% 
  mutate(par = stringr::str_extract(file_name, "\\d+") %>% as.numeric(),
         stim = stringr::str_extract(file_name, "(\\d+)(?!.*\\d)") %>% as.numeric(),
         .before = 1) 

lpp_trial_wave <- lpp_trial_wave %>% 
  filter(!par %in% bad_participants)


lpp_trial_wave <- merge(x = pic_id_key, 
                        y = lpp_trial_wave, 
                        by.x = "con_id", 
                        by.y = "stim", 
                        all.y = T)

lpp_trial_wave <- merge(x = stim_name_cate, 
                        y = lpp_trial_wave, 
                        by.x = "stim_name", 
                        by.y = "picture", 
                        all.y = T)


lpp_cat_wave <- lpp_trial_wave %>% 
  select(-c(stim_name, par, file_name)) %>% 
  mutate(category_name = case_when(
    cate == 1 ~ "pleasant",
    cate == 2 ~ "neutral",
    cate == 3 ~ "unpleasant"), .before = 1) %>% 
  mutate(category_name = factor(category_name,
                                levels = c("pleasant", "neutral", "unpleasant"))) %>% 
  group_by(category_name) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  pivot_longer(cols = starts_with("V"), 
               values_to = "amp",
               names_to = "time_point") %>% 
  merge(y = lpp_time_key, by.x = "time_point", by.y = "V_time_pt", all.x = T) %>% 
  select(time_ms, category_name, amp) %>% 
  arrange(category_name, time_ms)

## Load by video/scene ERPs####
# ssVEP by video
load(paste0(parent_directory,"/misc/by_video.RData"))

# ssVEP 1000ms 1537pt - 9000ms 5633pt
start_time_ms <- -2000
number_of_time_points <- 6146
sample_rate_hz <- 512

ssvep_time_key <- data.frame(time_pt = 1:number_of_time_points) %>% 
  mutate(time_ms = rep(start_time_ms,number_of_time_points) + 
           ((((1:number_of_time_points)-1) * (1000/sample_rate_hz))),
         V_time_pt = paste0("V", time_pt))

ssvep_cat_wave <- by_scene_info %>% 
  mutate(par = stringr::str_extract(file_name, "\\d+") %>% 
           as.numeric(),.before = "par_id") %>% 
  filter(!par %in% bad_participants) %>% 
  filter(channel_names %in% occipital_chans) %>% 
  select(-c(scene_id_con_num,scene,par,par_id,file_name,channel_names)) %>% 
  group_by(category) %>% 
  summarise_all(mean) %>% 
  mutate(category = case_when(
    category == 1 ~ "pleasant",
    category == 2 ~ "neutral",
    category == 3 ~ "unpleasant"), .before = V1) %>%
  mutate(category = factor(category,
                           levels = c("pleasant", "neutral", "unpleasant"))) %>% 
  pivot_longer(cols = starts_with("V"), 
               values_to = "amp",
               names_to = "time_point") %>% 
  merge(y = ssvep_time_key, by.x = "time_point", by.y = "V_time_pt", all.x = T) %>% 
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


# Models and draws ####
load(paste0(parent_directory,"/paper_data_models/models/models.RData"))
load(paste0(parent_directory,"/paper_data_models/models/paper_models.RData"))

# model011_lpp_fit <- model011_lpp_fit$output_files() %>% 
#   basename() %>% 
#   paste0(parent_directory,
#          "/paper_data_models/models/chains/",
#          .) %>% 
#   as_cmdstan_fit()
# model011_ssvep_fit <- model011_ssvep_fit$output_files() %>% 
#   basename() %>% 
#   paste0(parent_directory,
#          "/paper_data_models/models/chains/",
#          .) %>% 
#   as_cmdstan_fit()

model011_lpp_fit_draws <- model011_lpp_fit$draws(format = "df")
model011_ssvep_fit_draws <- model011_ssvep_fit$draws(format = "df")
model012_lpp_fit_draws <- model012_lpp_fit$draws(format = "df")
model012_ssvep_fit_draws <- model012_ssvep_fit$draws(format = "df")


model011_lpp_fit_loo <- model011_lpp_fit$loo()
model011_ssvep_fit_loo <- model011_ssvep_fit$loo()
model012_lpp_fit_loo <- model012_lpp_fit$loo()
model012_ssvep_fit_loo <- model012_ssvep_fit$loo()

# Loo ####

loo::loo_compare(model011_lpp_fit_loo,
                 model012_lpp_fit_loo)

loo::loo_model_weights(list(model011_lpp_fit_loo,
                            model012_lpp_fit_loo))

loo::loo_compare(model011_ssvep_fit_loo,
                 model012_ssvep_fit_loo)

loo::loo_model_weights(list(model011_ssvep_fit_loo,
                            model012_ssvep_fit_loo))

loo::loo_compare(model011_fft_fit_loo,
                 model012_fft_fit_loo)

loo::loo_model_weights(list(model011_fft_fit_loo,
                            model012_fft_fit_loo))

# Loo without problematic observations, doesn't make a difference
lpp_good_indice <- which(model012_lpp_fit_loo$pointwise[,5] < .5)

ssvep_good_indice <- which(model012_ssvep_fit_loo$pointwise[,5] < .5)

lpp_good_indice_names <- paste0("log_lik[",lpp_good_indice,"]")

ssvep_good_indice_names <- paste0("log_lik[",ssvep_good_indice,"]")

model011_lpp_fit_loo_no_outliers <- model011_lpp_fit$loo(variables = lpp_good_indice_names)

model012_lpp_fit_loo_no_outliers <- model012_lpp_fit$loo(variables = lpp_good_indice_names)

model011_ssvep_fit_loo_no_outliers <- model011_ssvep_fit$loo(variables = ssvep_good_indice_names)

model012_ssvep_fit_loo_no_outliers <- model012_ssvep_fit$loo(variables = ssvep_good_indice_names)

loo::loo_compare(model011_lpp_fit_loo,
                 model012_lpp_fit_loo)

loo::loo_compare(model011_lpp_fit_loo_no_outliers,
                 model012_lpp_fit_loo_no_outliers)

loo::loo_model_weights(list(model011_lpp_fit_loo,
                            model012_lpp_fit_loo))

loo::loo_model_weights(list(model011_lpp_fit_loo_no_outliers,
                            model012_lpp_fit_loo_no_outliers))

loo::loo_compare(model011_ssvep_fit_loo,
                 model012_ssvep_fit_loo)

loo::loo_compare(model011_ssvep_fit_loo_no_outliers,
                 model012_ssvep_fit_loo_no_outliers)

loo::loo_model_weights(list(model011_ssvep_fit_loo,
                            model012_ssvep_fit_loo))

loo::loo_model_weights(list(model011_ssvep_fit_loo_no_outliers,
                            model012_ssvep_fit_loo_no_outliers))



median(model011_lpp_fit_draws$mu_cov_amp_aro/
         (model011_lpp_fit_draws$mu_par_sd_amp * model011_lpp_fit_draws$mu_par_sd_aro))

quantile(model011_lpp_fit_draws$mu_cov_amp_aro/
           (model011_lpp_fit_draws$mu_par_sd_amp * model011_lpp_fit_draws$mu_par_sd_aro),
         probs = c(.025, .975))

median(model011_ssvep_fit_draws$mu_cov_amp_aro/
         (model011_ssvep_fit_draws$mu_par_sd_amp * model011_ssvep_fit_draws$mu_par_sd_aro))

quantile(model011_ssvep_fit_draws$mu_cov_amp_aro/
           (model011_ssvep_fit_draws$mu_par_sd_amp * model011_ssvep_fit_draws$mu_par_sd_aro),
         probs = c(.025, .975))

median(model011_fft_fit_draws$mu_cov_amp_aro/
         (model011_fft_fit_draws$mu_par_sd_amp * model011_fft_fit_draws$mu_par_sd_aro))

quantile(model011_fft_fit_draws$mu_cov_amp_aro/
           (model011_fft_fit_draws$mu_par_sd_amp * model011_fft_fit_draws$mu_par_sd_aro),
         probs = c(.025, .975))

median((model011_lpp_fit_draws$mu_cov_amp_aro/
          (model011_lpp_fit_draws$mu_par_sd_amp * model011_lpp_fit_draws$mu_par_sd_aro)) +
         (model011_ssvep_fit_draws$mu_cov_amp_aro/
            (model011_ssvep_fit_draws$mu_par_sd_amp * model011_ssvep_fit_draws$mu_par_sd_aro)) )

sum((model011_lpp_fit_draws$mu_cov_amp_aro/
       (model011_lpp_fit_draws$mu_par_sd_amp * model011_lpp_fit_draws$mu_par_sd_aro)) +
      (model011_ssvep_fit_draws$mu_cov_amp_aro/
         (model011_ssvep_fit_draws$mu_par_sd_amp * model011_ssvep_fit_draws$mu_par_sd_aro)) < 0) /80000


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
summary(amp_cate_type_aov)
effectsize::eta_squared(amp_cate_type_aov)

amp_cate_scene_aov <- data_for_stan_df %>%
  select(par,type,cate,amp) %>% 
  group_by(par,type,cate) %>%
  summarise_all(mean) %>% 
  mutate(zamp = as.vector(scale(amp))) %>% 
  ungroup() %>% 
  mutate(zamp = if_else(type == 2, -1*zamp, zamp)) %>% 
  filter(type == 1) %>% 
  afex::aov_ez(data = ., dv = "zamp", 
               within = c( "cate"),
               id = "par")

amp_cate_scene_aov
summary(amp_cate_scene_aov)
effectsize::eta_squared(amp_cate_scene_aov)

amp_cate_video_aov <- data_for_stan_df %>%
  select(par,type,cate,amp) %>% 
  group_by(par,type,cate) %>%
  summarise_all(mean) %>% 
  mutate(zamp = as.vector(scale(amp))) %>% 
  ungroup() %>% 
  mutate(zamp = if_else(type == 2, -1*zamp, zamp)) %>% 
  filter(type == 2) %>% 
  afex::aov_ez(data = ., dv = "zamp", 
               within = c( "cate"),
               id = "par")

amp_cate_video_aov
summary(amp_cate_video_aov)
effectsize::eta_squared(amp_cate_video_aov)


data_for_stan_df %>%
  select(par,type,cate,amp) %>% 
  group_by(par,type,cate) %>%
  summarise_all(mean) %>% 
  mutate(zamp = as.vector(scale(amp))) %>% 
  ungroup() %>% 
  mutate(zamp = if_else(type == 2, -1*zamp, zamp)) %>% 
  group_by(type, cate) %>% 
  summarise(mean_zamp = mean(zamp),
            se_zamp = plotrix::std.error(zamp))



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
  summarise(mean_aro = mean(valence),
            se_aro = plotrix::std.error(valence))

video_SAM_cate_avg %>% 
  group_by(cate) %>% 
  summarise(mean_aro = mean(valence),
            se_aro = plotrix::std.error(valence))

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

val_cate_scene_aov <- data_for_stan_df %>%
  select(par,type,cate,valence) %>% 
  filter(type == 1) %>% 
  group_by(par,type,cate) %>%
  summarise_all(mean) %>% 
  ungroup() %>% 
  afex::aov_ez(data = ., dv = "valence", 
               within = c("cate"),
               id = "par")

val_cate_scene_aov
summary(val_cate_scene_aov)
effectsize::eta_squared(val_cate_scene_aov)

val_cate_video_aov <- data_for_stan_df %>%
  select(par,type,cate,valence) %>% 
  filter(type == 2) %>% 
  group_by(par,type,cate) %>%
  summarise_all(mean) %>% 
  ungroup() %>% 
  afex::aov_ez(data = ., dv = "valence", 
               within = c("cate"),
               id = "par")

val_cate_video_aov
summary(val_cate_video_aov)
effectsize::eta_squared(val_cate_video_aov)

t.test(scene_SAM_cate_avg$valence[scene_SAM_cate_avg$cate == 1],
       video_SAM_cate_avg$valence[video_SAM_cate_avg$cate == 1],
       paired = T)  

effectsize::cohens_d(scene_SAM_cate_avg$valence[scene_SAM_cate_avg$cate == 1],
                     video_SAM_cate_avg$valence[video_SAM_cate_avg$cate == 1],
                     paired = T)  

t.test(scene_SAM_cate_avg$valence[scene_SAM_cate_avg$cate == 2],
       video_SAM_cate_avg$valence[video_SAM_cate_avg$cate == 2],
       paired = T)  

effectsize::cohens_d(scene_SAM_cate_avg$valence[scene_SAM_cate_avg$cate == 2],
                     video_SAM_cate_avg$valence[video_SAM_cate_avg$cate == 2],
                     paired = T)  

t.test(scene_SAM_cate_avg$valence[scene_SAM_cate_avg$cate == 3],
       video_SAM_cate_avg$valence[video_SAM_cate_avg$cate == 3],
       paired = T)  

effectsize::cohens_d(scene_SAM_cate_avg$valence[scene_SAM_cate_avg$cate == 3],
                     video_SAM_cate_avg$valence[video_SAM_cate_avg$cate == 3],
                     paired = T)

## R^2 ####

### R^2 using residuals for full model

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

# Full model predicted means
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

model012_ssvep_predicted_par_means_per_obs <- as.matrix(
  model012_ssvep_fit_draws[
    paste0("bpar[",
           data_list_for_stan_ssvep$par, "]")])

model012_lpp_predicted_stim_means_per_obs <- as.matrix(
  model012_lpp_fit_draws[
    paste0("bstim[",
           data_list_for_stan_lpp$par, "]")])

model012_ssvep_predicted_stim_means_per_obs <- as.matrix(
  model012_ssvep_fit_draws[
    paste0("bstim[",
           data_list_for_stan_ssvep$par, "]")])


model_R2_posteriors <- data.frame(
                                  model011_lpp_R2 =
                                    bayes_R2_residuals(
                                      data_list_for_stan_lpp$amp,
                                      model011_lpp_predicted_means),
                                  model011_ssvep_R2 =
                                    bayes_R2_residuals(
                                      data_list_for_stan_ssvep$amp,
                                      model011_ssvep_predicted_means),
                                  # model011_lpp_R2_arousal =
                                  #   bayes_R2_residuals(
                                  #     data_list_for_stan_lpp$amp,
                                  #     model011_lpp_predicted_stim_means_per_obs,
                                  #     model011_lpp_predicted_par_means_per_obs),
                                  # model011_ssvep_R2_arousal =
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

median(model_R2_posteriors$model011_lpp_R2)

quantile(model_R2_posteriors$model011_lpp_R2, 
         probs = c(.025, .975))

median(model_R2_posteriors$model012_lpp_R2)

quantile(model_R2_posteriors$model012_lpp_R2, 
         probs = c(.025, .975))

median(model_R2_posteriors$model011_ssvep_R2)

quantile(model_R2_posteriors$model011_ssvep_R2, 
         probs = c(.025, .975))

median(model_R2_posteriors$model012_ssvep_R2)

quantile(model_R2_posteriors$model012_ssvep_R2, 
         probs = c(.025, .975))

# Model 12 stim, minus effect of participant
median(model_R2_posteriors$model012_lpp_R2_stim)

quantile(model_R2_posteriors$model012_lpp_R2_stim, 
         probs = c(.025, .975))

median(model_R2_posteriors$model012_ssvep_R2_stim)

quantile(model_R2_posteriors$model012_ssvep_R2_stim, 
         probs = c(.025, .975))


# model 11 arousal, minus effect of participant
model011_lpp_predicted_means_per_obs_minus_par_effect <- 
  model011_lpp_predicted_means - model011_lpp_predicted_par_means_per_obs

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

median(hold_lpp_bayesian_R_squared)
quantile(hold_lpp_bayesian_R_squared, 
         probs = c(.025, .975))

model011_ssvep_predicted_means_per_obs_minus_par_effect <- 
  model011_ssvep_predicted_means - model011_ssvep_predicted_par_means_per_obs


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


median(hold_ssvep_bayesian_R_squared)

quantile(hold_ssvep_bayesian_R_squared, 
         probs = c(.025, .975))


## contrast
median(model_R2_posteriors$model012_lpp_R2_stim - hold_lpp_bayesian_R_squared)

quantile(model_R2_posteriors$model012_lpp_R2_stim - hold_lpp_bayesian_R_squared, 
         probs = c(.025, .975))


median(model_R2_posteriors$model012_ssvep_R2_stim - hold_ssvep_bayesian_R_squared)

quantile(model_R2_posteriors$model012_ssvep_R2_stim - hold_ssvep_bayesian_R_squared, 
         probs = c(.025, .975))


# Figure 1 is the paradigm made in Inkscape ####



# Figure 2 Category rating dot plot####
dot_size <- 7
dodge_size <- .7
text_size <- 25
legend_tite_text_size <- 30
axis_line_thickness <- 1
valence_colors <- c("blue1","black", "red1", "white")
# valence_colors <- c("blue1","gray", "red1", "white")
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
            color = valence_colors[3],
            size = 10) +
  geom_text(aes(x = 2, y = 1.5), 
            label = "Neutral",
            fontface = "bold", 
            family = "Arial",
            color = valence_colors[2],
            size = 10) +
  geom_text(aes(x = 2, y = 2), 
            label = "Pleasant",
            fontface = "bold", 
            family = "Arial",
            color = valence_colors[1],
            size = 10) +
  scale_y_continuous(breaks = ratings_cat_breaks,
                     limits = ratings_cat_limits,
                     name = "SAM Rating",) +
  scale_shape_manual(name = "Stimulus Modality", values = c(15,19), 
                     labels = c("Scenes", "Videos")) +
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
    axis.title.y = element_text(size = legend_tite_text_size, 
                                family = "Arial", face = "bold")) +
  guides(shape = guide_legend(direction = "horizontal", 
                              title.position = "top"))

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
        legend.justification = c(0.5, 3.6),
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

## RR Figure 2####
ratings_cat_by_par <- ratings_data %>% 
  group_by(par_id, Stim_cat, Stim_type) %>% 
  summarise(mean_val = mean(valence),
            # se_val = plotrix::std.error(valence),
            mean_aro = mean(arousal)#,se_aro = plotrix::std.error(arousal)
  ) %>% 
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = c(".value", "rating"),
    names_pattern = "(mean_)(.*)"
  ) %>% 
  rename(mean_rating = mean_) 

ratings_cat_breaks <- seq(1, 9, by = 1)
ratings_cat_limits <- c(1,9)
dodge_amount <- .2
dot_size <- 3
valence_colors <- c("blue1","gray", "red1", "white")
stroke_size <- 1

arousal_plot <- ratings_cat_by_par %>%
  filter(rating == "aro") %>% 
  mutate(Stim_cat_dodged = as.numeric(as.factor(Stim_cat)) + 
           ifelse(Stim_type == "Pics", -dodge_amount, dodge_amount)) %>% 
  ggplot() +
  geom_quasirandom(aes(x = Stim_cat_dodged,
                       shape = Stim_type,
                       y = mean_rating,
                       fill = Stim_cat),
                   size = dot_size,
                   stroke = stroke_size) +
  # geom_line(aes(x = Stim_cat,
  #               y = mean_rating, 
  #               group = Stim_type),
  #           linetype = "dotted",
  #           position = position_dodge(width = dodge_size)) + 
  # geom_point(aes(x = Stim_cat, shape = Stim_type,
  #                y = mean_rating),
  #            color = "black",
  #            position = position_dodge(width = dodge_size),
  #            size = dot_size + 1) +
  # geom_point(aes(x = Stim_cat, shape = Stim_type,
  #                y = mean_rating,
  #                color = Stim_cat),
  #            position = position_dodge(width = dodge_size),
  #            size = dot_size) +
  # geom_text(aes(x =2, y = 9),
  #           label = "Arousal",
  #           fontface = "bold",
  #           family = "Arial",
  #           size = 10) +
  # geom_text(aes(x = 1.62, y = 8),
  #           label = "Unpleasant",
  #           fontface = "bold",
  #           family = "Arial",
  #           color = valence_colors[3],
  #           size = 10) +
  # geom_text(aes(x = 1.23, y = 8.5),
  #           label = "Neutral",
  #           fontface = "bold",
  #           family = "Arial",
  #           color = valence_colors[2],
  #           size = 10) +
  # geom_text(aes(x = 1.36, y = 9),
  #           label = "Pleasant",
  #           fontface = "bold",
  #           family = "Arial",
  #           color = valence_colors[1],
  #           size = 10) +
  annotate("text",
           x = 2, 
           y = 9,
           label = "Arousal",
           family = "Arial",
           color = "black",
           size = 10) +
  scale_y_continuous(breaks = ratings_cat_breaks,
                     limits = c(0,9),
                     name = "SAM Rating",) +
  scale_shape_manual(name = "Stimulus Modality", values = c(22,21), 
                     labels = c("Scenes", "Videos")) +
  scale_fill_manual(values = c(valence_colors),) +
  # scale_x_discrete(name = "Category", 
  #                  labels = c("Pleasant", "Neutral", "Unpleasant    "),
  #                  guide = guide_axis(n.dodge = 2)) +
  coord_cartesian(ylim = c(1,9)) +
  guides(color = "none") +
  theme_classic() +
  # ggtitle("Arousal") +
  theme(#legend.position = c(.5,.97), 
    legend.title.align = .5,
    plot.title = element_text(hjust = .5, vjust = .5,
                              size = legend_tite_text_size,
                              family = "Arial",
                              face = "bold"),
    legend.text = element_text(size = legend_tite_text_size, 
                               family = "Arial", 
                               face = "bold"),
    #legend.justification = c(.5,1)
    #,legend.key.height = unit(.5, "cm")
    legend.box.margin = margin(-20, 0, 0, 0),
    text = element_text(size = text_size, family = "Arial"),
    axis.line = element_line(size = axis_line_thickness,
                             lineend = "square"),
    axis.ticks = element_blank(),
    axis.text = element_text(size = text_size, 
                             family = "Arial", 
                             color = "black"),
    axis.text.x = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = legend_tite_text_size, 
                                family = "Arial")) +
  guides(shape = guide_legend(direction = "horizontal", 
                              title.position = "top"))

valence_plot <- ratings_cat_by_par %>%
  filter(rating == "val") %>% 
  mutate(Stim_cat_dodged = as.numeric(as.factor(Stim_cat)) + 
           ifelse(Stim_type == "Pics", -dodge_amount, dodge_amount)) %>% 
  ggplot() +
  geom_quasirandom(aes(x = Stim_cat_dodged,
                       shape = Stim_type,
                       y = mean_rating,
                       fill = Stim_cat),
                   size = dot_size,
                   stroke = stroke_size) +
  # geom_line(aes(x = Stim_cat,
  #               y = mean_rating, 
  #               group = Stim_type),
  #           linetype = "dotted",
  #           position = position_dodge(width = 0.4)) + 
  # geom_point(aes(x = Stim_cat, shape = Stim_type,
  #                y = mean_rating),
  #            color = "black",
  #            position = position_dodge(width = dodge_size),
  #            size = dot_size + 1) + 
  # geom_point(aes(x = Stim_cat, shape = Stim_type,
  #                y = mean_rating,
  #                color = Stim_cat),
  #            position = position_dodge(width = dodge_size),
  #            size = dot_size) + 
  annotate("text",
           x = 1.31, 
           y = 1.8,
           label = "Pleasant",
           family = "Arial",
           color = valence_colors[1],
           size = 10) +
  annotate("text",
           x = 1.18, 
           y = 1.35,
           label = "Neutral",
           family = "Arial",
           color = "gray52",
           size = 10) +
  annotate("text",
           x = 1.55, 
           y = .9,
           label = "Unpleasant",
           family = "Arial",
           color = valence_colors[3],
           size = 10) +
  annotate("text",
           x = 2, 
           y = 9,
           label = "Valence",
           family = "Arial",
           color = "black",
           size = 10) +
  # annotate("text", x = 2, y = 0.5, label = "Stimulus X Valence\nF(2,94) = 2.23, p = .12;\nηg2 = .023", size = 8) +
  scale_y_continuous(breaks = ratings_cat_breaks,
                     limits = c(0,9)) +
  scale_shape_manual(name = "Ratings by Stimulus Modality", values = c(22,21), labels = c("Scenes", "Videos")) +
  scale_fill_manual(values = c(valence_colors),) +
  # scale_x_discrete(name = "Category", 
  #                  labels = c("Pleasant", "Neutral", "Unpleasant    "),
  #                  guide = guide_axis(n.dodge = 2)) +
  coord_cartesian(ylim = c(1,9)) +
  guides(fill = "none") +
  theme_classic() +
  # ggtitle("Valence") +
  theme(legend.position = c(.5,.5), 
        legend.justification = c(0.825, 2.9),
        legend.box.just = "center",
        # legend.title.align = .5,
        legend.text = element_text(size = 27, 
                                   family = "Arial"),
        legend.text.align = .5,
        legend.title = element_blank(),
        plot.title = element_text(hjust = .5, vjust = .5,
                                  size = legend_tite_text_size,
                                  family = "Arial",
                                  face = "bold"),
        #legend.justification = c(.5,1)
        #,legend.key.height = unit(.5, "cm")
        # legend.box.margin = margin(-20, 0, 0, 0),
        text = element_text(size = text_size, family = "Arial"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = text_size, 
                                 family = "Arial", 
                                 color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank()) +
  guides(shape = guide_legend(direction = "vertical", 
                              title.position = "top",
                              override.aes = list(size = 7, stroke = 1.75)))


layout <- c("
AB
")

tiff(filename = paste0(parent_directory,
                       "/misc/002figure_aro_val_cate_dot_RR.tiff"),
     width = 7, height = 8, units = "in", res = 300)
arousal_plot + valence_plot + 
  plot_annotation(title = "Emotional Ratings by Participant",
                  theme = theme(
                    plot.title = element_text(face = "bold",hjust = .65,
                                              vjust = .6,
                                              size = legend_tite_text_size, 
                                              family = "Arial")
                  )) +
  plot_layout(design = layout)
dev.off()

# Figure 3 Video x picture ratings scatter####
library(grid)
library(jpeg)

dot_size <- 7
dodge_size <- .7
legend_tite_text_size <- 30
axis_line_thickness <- 1
valence_colors <- c("blue1","gray", "red1", "white")
# valence_colors <- c("blue1","gray", "red1", "white")
color_blind_valence_colors <- c("#0072B2","#009E73","#D55E00","ivory4")
rec_x <- .21
rec_y <- .135

inner_raster_text_size <- 7
scene_dot_size <- .20
text_size <- 15

arousal_by_modality <- by_scene_ratings_with_path %>% 
  pivot_wider(id_cols = c("Stim_name", "path", "Stim_cat"),
              names_from = Stim_type, 
              values_from = mean_aro)

valence_by_modality <- by_scene_ratings_with_path %>% 
  pivot_wider(id_cols = c("Stim_name", "path", "Stim_cat"),
              names_from = Stim_type, 
              values_from = mean_val)


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
                            family = "Arial"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title = element_text(face = "bold"),
        legend.position = "none")


for (i in 1:90) {
  img <- arousal_by_modality$path[i] %>% 
    readJPEG() %>% 
    rasterGrob(interpolate=TRUE)
  if (arousal_by_modality$Stim_cat[i] == "Pleasant"){
    current_val_color = valence_colors[1]
  } else if (arousal_by_modality$Stim_cat[i] == "Neutral"){
    current_val_color = "black"
  } else {
    current_val_color = valence_colors[3]
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

arousal_raster <- arousal_raster +
  geom_line(data = data.frame(x = seq(1,9,by =.1),
                              y = seq(1,9,by =.1)),
            aes(x, y), linetype = "dashed", alpha = .5,
            linewidth = axis_line_thickness)


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
  theme(text = element_text(size = text_size, family = "Arial"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title = element_text(face = "bold"),
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

valence_raster <- valence_raster +
  geom_line(data = data.frame(x = seq(1,9,by =.1),
                              y = seq(1,9,by =.1)),
            aes(x, y), linetype = "dashed", alpha = .5,
            linewidth = axis_line_thickness)


tiff(filename = paste0(parent_directory, 
                       "/misc/003figure_by_scene_ratings.tiff"),
     width = 8.5, height = 4, units = "in", res = 300)
arousal_raster + valence_raster
dev.off()

# Figure 4 top:waveform category dot plot, bottom: erp X arousal cor####

### ERP dot plot ####
y_axis_breaks <- seq(-.9, .9, by = .2)
y_axis_limits <- c(-1,1)
text_size = 20
axis_line_thickness = 1
valence_colors <- c("blue1","black", "red1", "white")


gm_amp_dot_plot <- ggplot(gm_amp_long) +
  # geom_pointrange(aes(x = category_name, shape = erp_type,
  #                     y = mean_amp, ymax = mean_amp + se_amp,
  #                     ymin = mean_amp - se_amp),
  #                 color = "black",
  #                 position = position_dodge(width = 0.4),
  #                 size = 2.5, linewidth = 4) + 
  geom_pointrange(aes(x = category_name, shape = erp_type,
                      y = mean_amp, ymax = mean_amp + se_amp,
                      ymin = mean_amp - se_amp,
                      fill = category_name),
                  position = position_dodge(width = 0.4),
                  size = 2, stroke = 2, linewidth = 2) + 
  scale_y_continuous(breaks = y_axis_breaks,
                     name = "Scene Z-score",
                     limits = y_axis_limits,
                     expand = c(0,0),
                     sec.axis = sec_axis(trans = ~ . * -1, 
                                         name = "Video Z-score",
                                         breaks = -y_axis_breaks)) +
  scale_shape_manual(values = c(22,21), labels = c("Scene-LPP", "Video-Power")) +
  scale_fill_manual(values = c(valence_colors),) +
  scale_x_discrete(name = "Category", 
                   labels = c("Pleasant", "Neutral", "Unpleasant")
  ) +
  theme_classic() +
  theme(legend.position = c(.5,.97), 
        legend.title = element_blank(),
        legend.justification = c(.5,1),
        legend.text = element_text(face = "bold"),
        legend.key.height = unit(.9, "cm"),
        legend.box.margin = margin(-15, 0, 0, 0),
        text = element_text(size = text_size, family = "Arial"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title = element_text(face = "bold")) +
  guides(shape = guide_legend(direction = "vertical", title.position = "top",override.aes = list(linetype = 0)),fill = "none")

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
  geom_line(aes(x = time_ms, y = amp, group = category_name),
            color = "black",
            linewidth = line_width + line_outline) +
  geom_line(aes(x = time_ms, y = amp, color = category_name),
            linewidth = line_width) +
  geom_text(aes(x = 200, y = 1.75), 
            label = "Pleasant",
            # fontface = "bold", 
            family = "Arial",
            color = "blue1",
            size = 10) +
  geom_text(aes(x = 200, y = 1.25), 
            label = "Neutral",
            # fontface = "bold", 
            family = "Arial",
            color = "black",
            size = 10) +
  geom_text(aes(x = 200, y = .75), 
            label = "Unpleasant",
            # fontface = "bold", 
            family = "Arial",
            color = "red1",
            size = 10) +
  scale_y_continuous(limits = c(-2.3, 2),
                     expand = c(0,0), name = "LPP (μV)") +
  scale_x_continuous(limits = c(-125,1000), expand = c(0,0),
                     breaks = seq(-100, 900, by = 100), name = "Time (milliseconds)") +
  scale_color_manual(values = c(valence_colors)) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5, vjust = 1),
        text = element_text(size = text_size, family = "Arial"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title = element_text(face = "bold"))


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
  scale_y_continuous(limits = c(.93, 1.28),
                     breaks = seq(.95,1.25,by = .05),
                     expand = c(0,0), name = "Video-Power (μV)") +
  scale_x_continuous(limits = c(-2000,10000),expand = c(0,0),
                     breaks = seq(-1000, 9000, by = 1000),
                     labels = c(-1:9),
                     name = "Time (seconds)"
  ) +
  scale_color_manual(values = valence_colors) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = text_size, family = "Arial"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title = element_text(face = "bold"))


# layout <- "
# AAAAAAAAABBB
# CCCCCCCCCBBB
# "
layout <- "
AAAAAAAAABBBBB
CCCCCCCCCBBBBB
"

tiff(filename = paste0(parent_directory,
                       "/misc/004figure_cate_wave_dot.tiff"),
     width = 11, height = 7, units = "in", res = 300)
lpp_cat_wave_plot + free(gm_amp_dot_plot) + ssvep_cat_wave_plot +
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

lm_arousal_amp_type_interaction <- ratings_erps_path_by_scene %>% 
  mutate(zscore_amp = case_when(
    Stim_type == "Video" ~ -1*zscore_amp,
    Stim_type == "Pics" ~ zscore_amp
  )) %>% 
  lm(zscore_amp ~ mean_aro + Stim_type + mean_aro*Stim_type, 
     data = .)

summary(lm_arousal_amp_type_interaction)
anova(lm_arousal_amp_type_interaction)
effectsize::eta_squared(lm_arousal_amp_type_interaction)


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
                     name = "Z-scored Scene-LPP") +
  theme_classic() +
  theme(text = element_text(size = text_size, family = "Arial"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title = element_text(face = "bold"),
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

arousal_ssvep_raster <- ratings_erps_path_by_scene %>% 
  filter(Stim_type == "Video") %>% 
  ggplot(aes(mean_aro, zscore_amp)) +
  geom_blank() +
  scale_x_continuous(breaks = seq(3, 8, by = 1),
                     limits = c(3,8),
                     expand = c(0,0),
                     name = "Video Arousal Rating") +
  scale_y_reverse(limits = c(.85,-1.05),
                  breaks = seq(75,-1, by = -.25),
                  expand = c(0,0),
                  name = "Z-scored Video-Power") +
  theme_classic() +
  theme(text = element_text(size = text_size, family = "Arial"),
        axis.line = element_line(size = axis_line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title = element_text(face = "bold"),
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

arousal_ssvep_raster <- arousal_ssvep_raster + 
  geom_line(stat = "smooth", method = "lm", aes(group = 1), 
            alpha = .5, se = F, color = "black", linewidth = axis_line_thickness) +
  geom_ribbon(stat = "smooth", method = "lm",
              aes(ymin = ..y.. - (1.96 * ..se..), # make 95% CI
                  ymax = ..y.. + (1.96 * ..se..)), 
              alpha = .2, color = "black", linetype = "blank") +
  annotate("text", x = 4.77, y = -.87,
           label = "paste(italic('r'),'(88) = .63',', ', italic('p'), ' < .001')",
           parse = T,
           size = inner_raster_text_size)

tiff(filename = paste0(parent_directory, 
                       "/misc/005figure_arousalXz-score_amp.tiff"),
     width = 8.5, height = 4, units = "in", res = 300)
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

lpp_valence_posteriors %>% 
  group_by(name) %>% 
  summarise(median_amp = median(value),
            q2_5 = quantile(value, probs = .025),
            q97_5 = quantile(value, probs = .975))

ssvep_valence_posteriors <- 
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

ssvep_valence_posteriors %>% 
  group_by(name) %>% 
  summarise(median_amp = median(value),
            q2_5 = quantile(value, probs = .025),
            q97_5 = quantile(value, probs = .975))

#plot elements
par_fill <- "gold1"
valence_colors <- c("blue1","gray", "red1", "white")

line_thickness <- 1
density_line_thickness <- 1
panel.grid.major.x_line_thickness <- .5
par_pos_scale <- 2
par_pos_y_limits <- c(.75, 47.5)
ssvep_par_breaks_labels <- seq(.4, 1.6, by = .2)
par_alpha <- .8
font_size <- 20
font_font <- "Arial"
SNR_annotation_size <- 4.4

layout_grid <- c("
aa
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
     annotate("text", 
              x = 1.65, 
              y = .34,
              label = "SNR",
              # fontface = "bold",
              family = "Arial",
              color = "black",
              size = SNR_annotation_size) +
     annotate("text", 
              x = 1.65, 
              y = .22,
              label = "0.33",
              # fontface = "bold",
              family = "Arial",
              color = "black",
              size = SNR_annotation_size) +
     annotate("text", 
              x = 1.65, 
              y = .1,
              label = "[0.24, 0.40]",
              # fontface = "bold",
              family = "Arial",
              color = "black",
              size = SNR_annotation_size) +
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
     scale_y_continuous(expand = c(0,0.007)) +
     coord_cartesian(xlim = c(-2.5,2.5)) +
     ggtitle("Scene Valence Posteriors") +
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
       legend.title = element_blank(),
       legend.position = "bottom", 
       legend.text = element_text(angle = 0, size = font_size),
       legend.background = element_blank(),
       legend.key = element_blank(),
       panel.grid.major.x = element_line(color = "black",
                                         linetype = "dotted",
                                         linewidth = 
                                           panel.grid.major.x_line_thickness)))+ 
  
  ssvep_valence_posteriors %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0),
             linewidth = line_thickness) +
  geom_density(aes(x = value, 
                   fill = name),
               alpha = par_alpha,
               linewidth = density_line_thickness) +
  annotate("text", 
           x = -0.034, 
           y = 13,
           label = "SNR",
           # fontface = "bold",
           family = "Arial",
           color = "black",
           size = SNR_annotation_size) +
  annotate("text", 
           x = -0.034, 
           y = 8.5,
           label = "0.24",
           # fontface = "bold",
           family = "Arial",
           color = "black",
           size = SNR_annotation_size) +
  annotate("text", 
           x = -0.034, 
           y = 4,
           label = "[0.16, 0.31]",
           # fontface = "bold",
           family = "Arial",
           color = "black",
           size = SNR_annotation_size) +
  scale_fill_manual(values = valence_colors,
                    name = "Categories",
                    labels = c("Pleasant", 
                               "Neutral", 
                               "Unpleasant",
                               "Emotional vs Neutral")) +
  scale_x_continuous(name = "Δ Power Microvoltage",
                     breaks = seq(-.04, .04, by = .02),
                     labels = seq(-.04, .04, by = .02),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.1)) +
  coord_cartesian(xlim = c(-.0575, .0575)) +
  ggtitle("Video Valence Posteriors") +
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
    legend.title = element_blank(),
    legend.position = "top", 
    legend.text = element_text(angle = 0, size = font_size),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid.major.x = element_line(color = "black",
                                      linetype = "dotted",
                                      linewidth = 
                                        panel.grid.major.x_line_thickness))+ 
  model012_lpp_fit_draws %>% 
  select(starts_with("bpar")) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(name = factor(name,
                       levels = unique(name))) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 
                   mean(model012_lpp_fit_draws$par_mean)),
             linewidth = density_line_thickness) +
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
  
  
  data.frame(name = "par_predictive",
             value =rnorm(80000,
                          model012_lpp_fit_draws$par_mean,
                          model012_lpp_fit_draws$par_sd)) %>%
  ggplot() +
  geom_vline(aes(xintercept = 
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
  
  
  model012_ssvep_fit_draws %>%
  select(starts_with("bpar")) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(name = factor(name,
                       levels = unique(name))) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 
                   mean(model012_ssvep_fit_draws$par_mean)),
             linewidth = density_line_thickness) +
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
  ggtitle("Mean Power per Participant") +
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
                          model012_ssvep_fit_draws$par_mean,
                          model012_ssvep_fit_draws$par_sd)) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 
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
  ggtitle("Power Participant Distribution") +
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
       units = "in",height = 8, width = 8,
       scale = 1.275)

## RR SNR emotion effect ####

snr_posteriors <- data.frame(lpp_emot_diff_snr_post_samp = numeric(),
                             ssvep_emot_diff_snr_post_samp = numeric())

pls_lpp_mu <- lpp_valence_posteriors %>% 
  filter(name == "pleasant") %>% 
  pull(value)

pls_lpp_obs_sim <- rnorm(n = 80000, 
                         mean = pls_lpp_mu,
                         sd = model012_lpp_fit_draws$amp_sd)

unpls_lpp_mu <- lpp_valence_posteriors %>% 
  filter(name == "unpleasant") %>% 
  pull(value)

unpls_lpp_obs_sim <- rnorm(n = 80000, 
                           mean = unpls_lpp_mu,
                           sd = model012_lpp_fit_draws$amp_sd)

neutral_lpp_mu <- lpp_valence_posteriors %>% 
  filter(name == "neutral") %>% 
  pull(value)

neutral_lpp_obs_sim <- rnorm(n = 80000, 
                             mean = neutral_lpp_mu,
                             sd = model012_lpp_fit_draws$amp_sd)

emot_diff_obs_sim <- ((pls_lpp_obs_sim + unpls_lpp_obs_sim) / 2) -
  neutral_lpp_obs_sim

emot_diff_obs_sim %>% density() %>% plot()

emotion_diff_mu <- lpp_valence_posteriors %>% 
  filter(name == "emotional_difference") %>% 
  pull(value)

emot_diff_obs_sim_2 <- rnorm(n = 80000, mean = emotion_diff_mu, sd = model012_lpp_fit_draws$amp_sd)

emot_diff_obs_sim %>% density() %>% plot()
emot_diff_obs_sim_2 %>% density() %>% plot()

sd(emot_diff_obs_sim)
sd(emot_diff_obs_sim_2)

for (post_sim_i in 1:80000) {

    
}

lpp_valence_posteriors %>% 
  group_by(name) %>% 
  summarise(median_amp = median(value),
            q2_5 = quantile(value, probs = .025),
            q97_5 = quantile(value, probs = .975))

model012_lpp_fit_draws %>% 
  summarise(median_sd_per_trial = median(amp_sd),
            q2_5 = quantile(amp_sd, probs = .025),
            q97_5 = quantile(amp_sd, probs = .975))
  

ssvep_valence_posteriors %>% 
  group_by(name) %>% 
  summarise(median_amp = median(value),
            q2_5 = quantile(value, probs = .025),
            q97_5 = quantile(value, probs = .975))

model012_ssvep_fit_draws %>% 
  summarise(median_sd_per_trial = median(amp_sd),
            q2_5 = quantile(amp_sd, probs = .025),
            q97_5 = quantile(amp_sd, probs = .975))

fft_valence_posteriors %>% 
  group_by(name) %>% 
  summarise(median_amp = median(value),
            q2_5 = quantile(value, probs = .025),
            q97_5 = quantile(value, probs = .975))

model012_fft_fit_draws %>% 
  summarise(median_sd_per_trial = median(amp_sd),
            q2_5 = quantile(amp_sd, probs = .025),
            q97_5 = quantile(amp_sd, probs = .975))


((lpp_valence_posteriors %>% 
  filter(name == "emotional_difference") %>% 
  pull(value)) / model012_lpp_fit_draws$amp_sd) %>% 
  data.frame("SNR_posterior_samples" = .) %>% 
  summarise(median_SNR = median(SNR_posterior_samples),
            CI2_5 = quantile(SNR_posterior_samples, .025),
            CI97_5 = quantile(SNR_posterior_samples, .975))


((ssvep_valence_posteriors %>% 
  filter(name == "emotional_difference") %>% 
  pull(value) *-1) / model012_ssvep_fit_draws$amp_sd) %>% 
  data.frame("SNR_posterior_samples" = .) %>% 
  summarise(median_SNR = median(SNR_posterior_samples),
            CI2_5 = quantile(SNR_posterior_samples, .025),
            CI97_5 = quantile(SNR_posterior_samples, .975))

((fft_valence_posteriors %>% 
  filter(name == "emotional_difference") %>% 
  pull(value) *-1) / model012_fft_fit_draws$amp_sd) %>% 
  data.frame("SNR_posterior_samples" = .) %>% 
  summarise(median_SNR = median(SNR_posterior_samples),
            CI2_5 = quantile(SNR_posterior_samples, .025),
            CI97_5 = quantile(SNR_posterior_samples, .975))

(((lpp_valence_posteriors %>% 
    filter(name == "emotional_difference") %>% 
    pull(value)) / model012_lpp_fit_draws$amp_sd) -
((ssvep_valence_posteriors %>% 
    filter(name == "emotional_difference") %>% 
    pull(value) *-1) / model012_ssvep_fit_draws$amp_sd) %>% 
  data.frame("SNR_posterior_contrast_samples" = .)) %>% 
  summarise(median_SNR = median(SNR_posterior_contrast_samples),
            CI2_5 = quantile(SNR_posterior_contrast_samples, .025),
            CI97_5 = quantile(SNR_posterior_contrast_samples, .975))
  

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

lpp_erot_surg_val_posteriors %>% 
  group_by(name) %>% 
  summarise(median_amp = median(value),
            q2_5 = quantile(value, probs = .025),
            q97_5 = quantile(value, probs = .975))

ssvep_erot_surg_val_posteriors <- 
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

ssvep_erot_surg_val_posteriors %>% 
  group_by(name) %>% 
  summarise(median_amp = median(value),
            q2_5 = quantile(value, probs = .025),
            q97_5 = quantile(value, probs = .975))


layout_grid <- c("
A
B
")

fig7_colors <- c("darkblue",
                 "blue1", 
                 "gray", 
                 "red1", 
                 "darkred")


fig7_linetypes <- c(5,
                    1,
                    1,
                    1,
                    5)

fig7_alpha <- .75
fig7_density_line_thickness <- 2
line_thickness <- 1.5
text_size <- 20

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
                  ylim = c(0, 2.01)) +
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
  scale_fill_manual(values = fig7_colors
  ) +
  scale_linetype_manual(values = fig7_linetypes,
                        guide = "none") +
  scale_x_reverse(name = "Δ Video-Power Microvoltage (Axis Reversed)",
                  limits = c(.07,-.1206),
                  breaks = seq(.04, -.08, by = -.02),
                  expand = c(0,0),
                  labels = sprintf("%.2f", seq(.04, -.08, by = -.02))) +
  scale_y_continuous(expand = c(0,0)
                     ,limits = c(0, 77)
  ) +
  coord_cartesian(xlim = c(.047, -.095),
                  ylim = c(0, 76)) +
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
  geom_text(aes(x = -.06, y = 71),
            label = "Erotica",
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
            color = "gray",
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
            size = 10)


layout_grid <- c("
A
B
")

fig7_lpp_top + fig7_ssvep_bottom +
  plot_layout(design = layout_grid, guides = "collect")  +
  plot_annotation(title = "Video-power Lacks Typical Scene-LPP Erotica and Gore Sensitivity",
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

## RR Figure 7 ####
text_size <- 22
label_size <- 10
line_width <- 3
dodge_amount <- .6
dot_size <- 1.5
stroke_size <- 2

### Arousal dot####
fig7_RR_aro_dot <- data_for_stan_df %>% 
  mutate(cate_f7 = case_when(
    stim %in% couple_ids ~ "couple",
    stim %in% surgery_ids ~ "surgery",
    cate == 1 ~ "pleasant",
    cate == 2 ~ "neutral",
    cate == 3 ~ "unpleasant"
  )) %>% 
  mutate(cate_f7 = factor(cate_f7,levels = c("couple",
                                             "pleasant",
                                             "neutral",
                                             "unpleasant",
                                             "surgery"))) %>% 
  select(par, type, cate_f7, valence, arousal) %>% 
  group_by(par, type, cate_f7) %>% 
  summarise_all(mean) %>% 
  group_by(type, cate_f7) %>% 
  summarise(mean_aro = mean(arousal),
            se_aro = plotrix::std.error(arousal)) %>%
  ungroup() %>% 
  mutate(type = as.factor(type)) %>% 
  ggplot() +
  geom_pointrange(aes(x = cate_f7,
                      y = mean_aro,
                      ymin = mean_aro - se_aro,
                      ymax = mean_aro + se_aro,
                      fill = cate_f7,
                      shape = type),
                  position = position_dodge(width = dodge_amount),
                  size = dot_size, stroke = stroke_size, linewidth = line_width) + 
  scale_y_continuous(expand = c(0,0), 
                     breaks = seq(3, 7, by = 1), 
                     limits = c(2.75, 7.25),
                     name = "SAM Ratings") +
  scale_shape_manual(values = c(22,21), labels = c("Scenes", "Videos")) +
  scale_fill_manual(values = c(fig7_colors)) +
  ggtitle("Arousal") +
  guides(fill = "none",shape = guide_legend(override.aes = list(linetype = 0, stroke = 1.5, size = 2))) +
  theme_classic() +
  theme(legend.position = c(.5,.15),
        legend.title = element_blank(),
        legend.justification = c(.5,1),
        legend.key.height = unit(.9, "cm"),
        legend.box.margin = margin(-15, 0, 0, 0),
        legend.text = element_text(family = font_font, size = 27),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold"),
        text = element_text(size = text_size, 
                            family = "Arial"),
        axis.line = element_line(size = line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black"))


### Valence Dot####
fig7_RR_val_dot <- data_for_stan_df %>% 
  mutate(cate_f7 = case_when(
    stim %in% couple_ids ~ "couple",
    stim %in% surgery_ids ~ "surgery",
    cate == 1 ~ "pleasant",
    cate == 2 ~ "neutral",
    cate == 3 ~ "unpleasant"
  )) %>% 
  mutate(cate_f7 = factor(cate_f7,levels = c("couple",
                                             "pleasant",
                                             "neutral",
                                             "unpleasant",
                                             "surgery"))) %>% 
  select(par, type, cate_f7, valence, arousal) %>% 
  group_by(par, type, cate_f7) %>% 
  summarise_all(mean) %>% 
  group_by(type, cate_f7) %>% 
  summarise(mean_val = mean(valence),
            se_val = plotrix::std.error(valence)) %>%
  ungroup() %>% 
  mutate(type = as.factor(type)) %>% 
  ggplot() +
  geom_pointrange(aes(x = cate_f7,
                      y = mean_val,
                      ymin = mean_val - se_val,
                      ymax = mean_val + se_val,
                      fill = cate_f7,
                      shape = type),
                  position = position_dodge(width = dodge_amount),
                  size = dot_size, stroke = stroke_size, linewidth = line_width) + 
  scale_y_continuous(expand = c(0,0), 
                     limits = c(2.75, 7.25)) +
  scale_shape_manual(values = c(22,21), labels = c("Scenes", "Videos")) +
  scale_fill_manual(values = c(fig7_colors)) +
  ggtitle("Valence") +
  guides(fill = "none") +
  theme_classic() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5,
                                  face = "bold"),
        text = element_text(size = text_size, family = "Arial"),
        axis.line = element_line(size = line_thickness,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) 

# legend.position = c(.5,.97)
# legend.title = element_blank(),
# legend.justification = c(.5,1)
# ,legend.key.height = unit(.9, "cm")
# , legend.box.margin = margin(-15, 0, 0, 0),
#   guides(shape = guide_legend(direction = "vertical", title.position = "top"))

### lpp posterior plots #####
RR_fig7_lpp <- lpp_erot_surg_val_posteriors %>% 
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
                  ylim = c(0, 2.01)) +
  ggtitle("Posterior of Mean per Category") +
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black",
                               margin = margin(t = 5, 
                                               unit = "pt")),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5,
                              face = "bold"),
    line = element_line(linewidth = line_thickness,
                        lineend = "square"),
    text = element_text(size = font_size,
                        color = "black"),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "black",
                                      linetype = "dotted",
                                      linewidth = 
                                        panel.grid.major.x_line_thickness)) 

### ssvep posterior plot
RR_fig7_ssvep <- ssvep_erot_surg_val_posteriors %>%
  ggplot() +
  geom_vline(aes(xintercept = 0),
             linewidth = line_thickness) +
  annotate("rect", xmax = -.05, xmin = -.119,
           ymin = -Inf, ymax = Inf,fill = "white") +
  geom_density(aes(x = value, 
                   fill = name,
                   linetype = name),
               alpha = fig7_alpha,
               linewidth = fig7_density_line_thickness) +
  scale_fill_manual(values = fig7_colors
  ) +
  scale_linetype_manual(values = fig7_linetypes,
                        guide = "none") +
  scale_x_reverse(name = "Δ Video-Power Microvoltage (Axis Reversed)",
                  limits = c(.07,-.1206),
                  breaks = seq(.04, -.08, by = -.02),
                  expand = c(0,0),
                  labels = sprintf("%.2f", seq(.04, -.08, by = -.02))) +
  scale_y_continuous(expand = c(0,0)
                     ,limits = c(0, 77)
  ) +
  coord_cartesian(xlim = c(.047, -.095),
                  ylim = c(0, 76)) +
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
  annotate("text", 
           x = -.081, 
           y = 53.5,
           label = "Erotica",
           # fontface = "bold",
           family = "Arial",
           color = "darkblue",
           size = label_size) +
  annotate("text",
           x = -.0775, 
           y = 42,
           label = "Pleasant",
           # fontface = "bold",
           family = "Arial",
           color = "blue1",
           size = label_size) +
  annotate("text",
           x = -.08, 
           y = 30.5,
           label = "Neutral",
           # fontface = "bold",
           family = "Arial",
           color = "gray52",
           size = label_size) +
  annotate("text",
           x = -.0735, 
           y = 19,
           label = "Unpleasant",
           # fontface = "bold",
           family = "Arial",
           color = "red1",
           size = label_size) +
  annotate("text",
           x = -.077, 
           y = 7.5,
           label = "Surgeries",
           # fontface = "bold",
           family = "Arial",
           color = "darkred",
           size = label_size)

### Save plot####

layout_grid <- c("
AAAAAAAAAAAAAAACCCCCDDDD
BBBBBBBBBBBBBBBCCCCCDDDD
")


free(RR_fig7_lpp) + free(RR_fig7_ssvep) + free(fig7_RR_aro_dot) + free(fig7_RR_val_dot) +
  plot_layout(design = layout_grid)  +
  plot_annotation(title = "Video-Power Lacks Typical Scene-LPP Erotica and Gore Sensitivity",
                  theme = theme(
                    plot.title = element_text(family = font_font,
                                              size = font_size + 8,
                                              color = "black",
                                              hjust = 0.5,
                                              face = "bold")))



ggsave(filename = paste0(parent_directory,
                         "/misc/007figure_ero_surg_posteriors_model12_RR.tiff"),
       device = "tiff",dpi = 300,
       units = "in",height = 5, width = 8.6,
       scale = 1.5)

# ggsave(filename = paste0(parent_directory,
#                          "/misc/007figure_ero_surg_posteriors_model12_RR.svg"),
#        device = "svg",
#        units = "in",height = 5, width = 8.6,
#        scale = 1.5)

# Figure 8 FFT power ####
library(tidyverse)
library(R.matlab)
## Load fft results ####
fft_structs <- readMat('/home/andrewf/Research_data/EEG/Pic_Vid/misc/high_pass_3_1hz_video_single_trial_fft.mat')

ssvepOccipitalChannels = c(56, 61, 59, 62, 63, 64, 125, 127, 128)

# looking at sliding window 7.5 hz cate averages

start_index <- 1
SW_fft_df <- data.frame(par = numeric(), 
                        stim = numeric(),
                        amp = numeric())

while (start_index < length(fft_structs$SWPerTrialResultStruc)) {
  
  current_filepath <- fft_structs$SWPerTrialResultStruc[start_index] %>% unlist()
  
  current_par <- sub(".*_(\\d+)_.*", "\\1", current_filepath) %>% as.numeric()
  current_stim <- sub(".*\\.at(\\d+).ar$", "\\1", current_filepath) %>% as.numeric()
  
  amp_per_freq <- fft_structs$SWPerTrialResultStruc[start_index + 1] %>% 
    unlist() 
  
  occipital_amp_per_freq <- amp_per_freq[ssvepOccipitalChannels] %>% mean()
  
  current_fft_df_entry <- data.frame(par = current_par, 
                                     stim = current_stim,
                                     amp = occipital_amp_per_freq)
  
  SW_fft_df <- rbind(SW_fft_df, current_fft_df_entry)
  
  start_index <- start_index + 5
  
}

video_key <- read.csv(file = '/home/andrewf/Research_data/EEG/Pic_Vid/misc/video_key.csv')

SW_fft_df <- merge(x = SW_fft_df, y = video_key, 
                   by.x = "stim", by.y = "video_id",
                   all.x = T)

par_id_ordered <- unique(SW_fft_df$par) %>% 
  sort() %>% 
  as.character()

stim_id_ordered <- unique(SW_fft_df$stim) %>% 
  sort() %>% 
  as.character()

SW_fft_df <- SW_fft_df %>% 
  reframe(par = factor(as.character(par),
                       levels = par_id_ordered),
          stim = factor(as.character(stim),
                        levels = stim_id_ordered),
          amp = amp,
          video = video,
          cat_id = factor(cat_id,
                          levels = c(1:3)))

SW_fft_df %>% 
  select(-c("stim","video")) %>% 
  group_by(par, cat_id) %>% 
  summarise_all(mean) %>% 
  group_by(par) %>% 
  mutate(zamp = as.vector(scale(amp))) %>% 
  group_by(cat_id) %>% 
  summarise(mean_amp = mean(amp),
            se_amp = plotrix::std.error(amp),
            mean_zamp = mean(zamp),
            se_zamp = plotrix::std.error(zamp))


# look at 7.5 hz noremal fft cate averages
all_frequencies <- fft_structs$PerTrialResultStruc[3] %>% unlist()
frequencies <- all_frequencies[2:241] # .125 to 30 hz
# frequencies <- all_frequencies[57:65] # 7 to 8 hz
# frequencies <- all_frequencies[25:89] # 3 to 11hz

start_index <- 1
fft_df <- data.frame(par = numeric(), 
                     stim = numeric(),
                     amp = numeric(),
                     freqs = numeric())


while (start_index < length(fft_structs$PerTrialResultStruc)) {
  
  current_filepath <- fft_structs$PerTrialResultStruc[start_index] %>% unlist()
  
  current_par <- sub(".*_(\\d+)_.*", "\\1", current_filepath) %>% as.numeric()
  current_stim <- sub(".*\\.at(\\d+).ar$", "\\1", current_filepath) %>% as.numeric()
  
  amp_per_all_freqs <- fft_structs$PerTrialResultStruc[start_index + 1] %>% 
    unlist() %>% 
    matrix(nrow = 9) 
  
  amp_per_freq <- amp_per_all_freqs[,2:241] %>% colMeans() # change indices if changing frequencies
  
  current_fft_df_entry <- data.frame(par = current_par, 
                                     stim = current_stim,
                                     amp = amp_per_freq,
                                     freqs = frequencies)
  
  fft_df <- rbind(fft_df, current_fft_df_entry)
  
  start_index <- start_index + 3
  
}


video_key <- read.csv(file = '/home/andrewf/Research_data/EEG/Pic_Vid/misc/video_key.csv')

fft_df <- merge(x = fft_df, y = video_key, 
                by.x = "stim", by.y = "video_id",
                all.x = T)

# par_id_ordered <- unique(fft_df$par) %>% 
#   sort() %>% 
#   as.character()
# 
# stim_id_ordered <- unique(fft_df$stim) %>% 
#   sort() %>% 
#   as.character()

fft_df <- fft_df %>% 
  reframe(par = par,
          stim = stim,
          amp = amp,
          video = video,
          # freqs = factor(as.character(freqs),
          #                levels = frequencies),
          freqs = freqs,
          cat_id = factor(cat_id,
                          levels = c(1:3)))


fft_df %>% 
  filter(freqs >= 7, freqs <= 8) %>% 
  select(-c("stim","video","freqs")) %>% 
  group_by(par, cat_id) %>% 
  summarise_all(mean) %>% 
  group_by(par) %>% 
  mutate(zamp = as.vector(scale(amp))) %>% 
  mutate(amp_diff = amp - mean(amp)) %>% 
  group_by(cat_id) %>% 
  summarise(mean_amp = mean(amp),
            se_amp = plotrix::std.error(amp),
            mean_zamp = mean(zamp),
            se_zamp = plotrix::std.error(zamp),
            mean_amp_diff = mean(amp_diff),
            se_amp_diff = plotrix::std.error(amp_diff))

fft_df %>%
  filter(freqs == 7.5) %>% 
  select(-c("stim","video","freqs")) %>% 
  group_by(par, cat_id) %>% 
  summarise_all(mean) %>% 
  group_by(par) %>% 
  mutate(zamp = as.vector(scale(amp))) %>% 
  mutate(amp_diff = amp - mean(amp)) %>% 
  group_by(cat_id) %>% 
  summarise(mean_amp = mean(amp),
            se_amp = plotrix::std.error(amp),
            mean_zamp = mean(zamp),
            se_zamp = plotrix::std.error(zamp),
            mean_amp_diff = mean(amp_diff),
            se_amp_diff = plotrix::std.error(amp_diff))

fft_df %>%
  filter(freqs != 7.5, freqs >= 7, freqs <= 8) %>% 
  select(-c("stim","video","freqs")) %>% 
  group_by(par, cat_id) %>% 
  summarise_all(mean) %>% 
  group_by(par) %>% 
  mutate(zamp = as.vector(scale(amp))) %>% 
  mutate(amp_diff = amp - mean(amp)) %>% 
  group_by(cat_id) %>% 
  summarise(mean_amp = mean(amp),
            se_amp = plotrix::std.error(amp),
            mean_zamp = mean(zamp),
            se_zamp = plotrix::std.error(zamp),
            mean_amp_diff = mean(amp_diff),
            se_amp_diff = plotrix::std.error(amp_diff))

cat_df <- fft_df %>%
  filter(freqs >= 7, freqs <= 8) %>% 
  select(-c("stim","video","freqs")) %>% 
  group_by(par, cat_id) %>% 
  summarise_all(mean) %>% 
  group_by(par) %>% 
  mutate(zamp = as.vector(scale(amp))) %>% 
  mutate(amp_diff = amp - mean(amp)) %>% 
  ungroup()

cat_df_ssvep <- fft_df %>%
  filter(freqs == 7.5) %>% 
  select(-c("stim","video","freqs")) %>% 
  group_by(par, cat_id) %>% 
  summarise_all(mean) %>% 
  group_by(par) %>% 
  mutate(zamp = as.vector(scale(amp))) %>% 
  mutate(amp_diff = amp - mean(amp)) %>% 
  ungroup()

cat_df_exclude_ssvep <- fft_df %>%
  filter(freqs != 7.5, freqs >= 7, freqs <= 8) %>% 
  select(-c("stim","video","freqs")) %>% 
  group_by(par, cat_id) %>% 
  summarise_all(mean) %>% 
  group_by(par) %>% 
  mutate(zamp = as.vector(scale(amp))) %>% 
  mutate(amp_diff = amp - mean(amp)) %>% 
  ungroup()

cat_df_wide <- fft_df %>%
  filter(freqs != 7.5, freqs >= 3, freqs <= 10) %>% 
  select(-c("stim","video","freqs")) %>% 
  group_by(par, cat_id) %>% 
  summarise_all(mean) %>% 
  group_by(par) %>% 
  mutate(zamp = as.vector(scale(amp))) %>% 
  mutate(amp_diff = amp - mean(amp)) %>% 
  ungroup()


afex::aov_ez(id = "par", 
             dv = "zamp", 
             within = "cat_id", 
             data =cat_df)

afex::aov_ez(id = "par", 
             dv = "zamp", 
             within = "cat_id", 
             data =cat_df_ssvep)

afex::aov_ez(id = "par", 
             dv = "zamp", 
             within = "cat_id", 
             data =cat_df_exclude_ssvep)

afex::aov_ez(id = "par", 
             dv = "zamp", 
             within = "cat_id", 
             data =cat_df_wide)


gm_cat_df_ssvep <- cat_df_ssvep %>% 
  group_by(cat_id) %>% 
  summarise(mean_zamp = -mean(zamp),
            se_zamp = plotrix::std.error(zamp),
            mean_amp_diff = mean(amp_diff),
            se_amp_diff = plotrix::std.error(amp_diff)) %>% 
  mutate(type = factor(1,levels = c(1:3)))

gm_cat_df <- cat_df %>% 
  group_by(cat_id) %>% 
  summarise(mean_zamp = -mean(zamp),
            se_zamp = plotrix::std.error(zamp),
            mean_amp_diff = mean(amp_diff),
            se_amp_diff = plotrix::std.error(amp_diff)) %>% 
  mutate(type = factor(2,levels = c(1:3)))

gm_cat_df_wide <- cat_df_wide %>% 
  group_by(cat_id) %>% 
  summarise(mean_zamp = -mean(zamp),
            se_zamp = plotrix::std.error(zamp),
            mean_amp_diff = mean(amp_diff),
            se_amp_diff = plotrix::std.error(amp_diff)) %>% 
  mutate(type = factor(3,levels = c(1:3)))

rbind.data.frame(gm_cat_df,gm_cat_df_ssvep,gm_cat_df_wide) %>% 
  ggplot() +
  geom_pointrange(aes(x = cat_id, 
                      y = mean_zamp,
                      ymin = mean_zamp - se_zamp,
                      ymax = mean_zamp + se_zamp,
                      color = cat_id,
                      pch = type),
                  position = position_dodge(.1)) +
  scale_color_manual(values = fig8_val_colors,
                     labels = c("Pleasant",
                                "Neutral",
                                "Unpleasant")) +
  scale_shape_discrete(name = "Measure",
                       labels = c("7.5Hz", 
                                  "7 - 8Hz",
                                  "3 - 10HZ")) +
  ggtitle("Z-scored amplitudes") +
  theme_classic()

rbind.data.frame(gm_cat_df, gm_cat_df_ssvep, gm_cat_df_wide) %>% 
  ggplot() +
  geom_pointrange(aes(x = cat_id, 
                      y = mean_amp_diff,
                      ymin = mean_amp_diff - se_amp_diff,
                      ymax = mean_amp_diff + se_amp_diff,
                      color = cat_id,
                      pch = type),
                  position = position_dodge(.1)) +
  scale_color_manual(values = fig8_val_colors,
                     labels = c("Pleasant",
                                "Neutral",
                                "Unpleasant")) +
  scale_shape_discrete(name = "Measure",
                   labels = c("7.5Hz", 
                              "7 - 8Hz",
                              "3 - 10HZ")) +
  ggtitle("difference from participant mean") +
  theme_classic()



## Plot ####
fig8_val_colors <- c("blue1", "black", "red1")
plot_font_size <- 15
dot_y_axis_breaks <- seq(-.9, .9, by = .2)

layout_grid <- c("
AAAAAAAAAAAAAAACCCCC
BBBBBBBBBBBBBBBCCCCC
")

fft_gm_plot <- fft_df %>% 
  filter(freqs >= 1, freqs < 30) %>%
  select(-c("stim","video")) %>%
  group_by(par, cat_id, freqs) %>%
  summarise_all(mean) %>% 
  group_by(par, freqs) %>% 
  mutate(zamp = as.vector(scale(amp))) %>% 
  mutate(amp_diff = amp - mean(amp)) %>% 
  group_by(freqs, cat_id) %>%
  summarise(mean_amp = mean(amp),
            se_amp = plotrix::std.error(amp),
            mean_zamp = mean(zamp),
            se_zamp = plotrix::std.error(zamp),
            mean_amp_diff = mean(amp_diff),
            se_amp_diff = plotrix::std.error(amp_diff)) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 7), linetype = "dashed") +
  geom_vline(aes(xintercept = 8), linetype = "dashed") +
  geom_line(aes(x = freqs, y = mean_amp, color = cat_id), linewidth = 1)  +
  scale_color_manual(values = fig8_val_colors, name = "Category", labels = c("Pleasant", "Neutral", "Unpleasant")) +
  scale_x_continuous(breaks = seq(2.5,30,by = 2.5),name = "Frequency (Hz)", expand = c(0,0)) +
  scale_y_continuous(name = "Power (Aribitary Units)",expand = c(0,0)) +
  theme_bw() +
  theme(text = element_text(size = plot_font_size),
        legend.position = "none",
        axis.line = element_line(size = 1,
                                 lineend = "square"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  annotate("text",
           x = 25, 
           y = .47,
           label = "Pleasant",
           family = "Arial",
           color = fig8_val_colors[1],
           size = 11) +
  annotate("text",
           x = 25, 
           y = .39,
           label = "Neutral",
           family = "Arial",
           color = fig8_val_colors[2],
           size = 11) +
  annotate("text",
           x = 25, 
           y = .31,
           label = "Unpleasant",
           family = "Arial",
           color = fig8_val_colors[3],
           size = 11) 


fft_diff_plot <- fft_df %>% 
  filter(freqs >= 1, freqs < 30) %>%
  select(-c("stim","video")) %>%
  group_by(par, cat_id, freqs) %>%
  summarise_all(mean) %>% 
  group_by(par, freqs) %>% 
  mutate(zamp = as.vector(scale(amp))) %>% 
  mutate(amp_diff = amp - mean(amp)) %>% 
  group_by(freqs, cat_id) %>%
  summarise(mean_amp = mean(amp),
            se_amp = plotrix::std.error(amp),
            mean_zamp = mean(zamp),
            se_zamp = plotrix::std.error(zamp),
            mean_amp_diff = mean(amp_diff),
            se_amp_diff = plotrix::std.error(amp_diff)) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 7), linetype = "dashed") +
  geom_vline(aes(xintercept = 8), linetype = "dashed") +
  geom_errorbar(aes(x = freqs,
                    ymin = mean_amp_diff - se_amp_diff, ymax = mean_amp_diff + se_amp_diff,
                    color = cat_id), linewidth = .5, width = 0)  +
  geom_line(aes(x = freqs, y = mean_amp_diff, color = cat_id), linewidth = 1)  +
  scale_color_manual(values = fig8_val_colors, name = "Category", labels = c("Pleasant", "Neutral", "Unpleasant")) +
  scale_fill_manual(values = fig8_val_colors, name = "Category", labels = c("Pleasant", "Neutral", "Unpleasant")) +
  scale_x_continuous(breaks = seq(2.5,30,by = 2.5), labels = seq(2.5,30,by = 2.5),name = "Frequency (Hz)", expand = c(0,0)) +
  scale_y_continuous(name = "Δ Power",expand = c(0,0)) +
  theme_bw() +
  theme(text = element_text(size = plot_font_size),
        axis.line = element_line(size = 1,
                                 lineend = "square"),
        axis.ticks = element_blank(),
        legend.position = "none")


fig8_dot_plot <- rbind(gm_cat_df,gm_cat_df_ssvep) %>% 
     ggplot() +
     # geom_pointrange(aes(x = category_name, shape = erp_type,
     #                     y = mean_amp, ymax = mean_amp + se_amp,
     #                     ymin = mean_amp - se_amp),
     #                 color = "black",
     #                 position = position_dodge(width = 0.4),
     #                 size = 2.5, linewidth = 4) + 
     geom_pointrange(aes(x = cat_id,shape = type,
                         y = mean_zamp, ymax = mean_zamp + se_zamp,
                         ymin = mean_zamp - se_zamp,
                         fill = cat_id),
                     position = position_dodge(width = 0.4),
                     size = 1.5, stroke = 2, linewidth = 2) + 
     scale_y_continuous(breaks = dot_y_axis_breaks,
                        name = "Reversed Z-score",
                        limits = c(-1,1),
                        expand = c(0,0)) +
     scale_shape_manual(values = c(22,21),labels = c("7 - 8Hz Power", "7.5Hz ssVEP Power")) +
     scale_fill_manual(values = c(fig8_val_colors)) +
     scale_x_discrete(name = "Category", 
                      labels = c("Pleasant", "Neutral", "Unpleasant")) +
     theme_classic() +
     theme(legend.position = c(.5,.98), 
           legend.title = element_blank(),
           legend.justification = c(.5,1),
           legend.text = element_text(face = "bold"),
           legend.key.height = unit(.9, "cm"),
           legend.box.margin = margin(-15, 0, 0, 0),
           text = element_text(size = 15, family = "Arial"),
           axis.line = element_line(size = 1,
                                    lineend = "square"),
           axis.ticks = element_blank(),
           axis.text.x = element_blank(),
           axis.text = element_text(color = "black")) +
     guides(shape = guide_legend(direction = "vertical", title.position = "top",override.aes = list(linetype = 0)),fill = "none")

(fft_gm_plot) + (fft_diff_plot) + free(fig8_dot_plot) +
  plot_layout(design = layout_grid)  +
  plot_annotation(title = "Power Between 7-8Hz Mostly Likely Cause of Video Effects",
                  theme = theme(
                    plot.title = element_text(family = "Arial",
                                              size = 25,
                                              color = "black",
                                              hjust = 0.5,
                                              face = "bold")))


ggsave(filename = paste0(parent_directory,
                         "/misc/008FFT_results.tiff"),
       device = "tiff",dpi = 300,
       units = "in",height = 3, width = 6,
       scale = 2)

ggsave(filename = paste0(parent_directory,
                         "/misc/008FFT_results.svg"),
       device = "svg",
       units = "in",height = 3, width = 6,
       scale = 2)






# Supplemental Figure 1 ####
library(tidyverse)
library(patchwork)
library(ggbeeswarm) # needs to be 0.6.0, version 0.7.0 currently doesn't position lines correctly
amp_per_cat_per_par_df <- data_for_stan_df %>%
  select(par,type,cate,amp) %>% 
  group_by(par,type,cate) %>%
  summarise_all(mean) %>% 
  mutate(zamp = as.vector(scale(amp)),
         demean_amp = amp - mean(amp)) %>% 
  ungroup()

lpp_amp_cat_aov <- amp_per_cat_per_par_df %>% 
  filter(type == 1) %>% 
  afex::aov_ez(id = "par", within = "cate", data = ., dv = "amp")
lpp_zamp_cat_aov <- amp_per_cat_per_par_df %>% 
  filter(type == 1) %>% 
  afex::aov_ez(id = "par", within = "cate", data = ., dv = "zamp")
ssvep_amp_cat_aov <- amp_per_cat_per_par_df %>% 
  filter(type == 2) %>% 
  afex::aov_ez(id = "par", within = "cate", data = ., dv = "amp")
ssvep_zamp_cat_aov <- amp_per_cat_per_par_df %>% 
  filter(type == 2) %>% 
  afex::aov_ez(id = "par", within = "cate", data = ., dv = "zamp")

effectsize::eta_squared(lpp_amp_cat_aov)
effectsize::eta_squared(lpp_zamp_cat_aov)
effectsize::eta_squared(ssvep_amp_cat_aov)
effectsize::eta_squared(ssvep_zamp_cat_aov)

raw_lpp_cat_par_bee_line <- amp_per_cat_per_par_df %>% 
  filter(type == 1) %>% 
  ggplot(aes(x = cate, 
             y = amp, 
             group = par)) +
  geom_beeswarm(size = 1) +
  geom_line(position = ggbeeswarm::position_beeswarm(),
            linewidth = .1) +
  scale_y_continuous(name = "Microvoltage",
                     breaks = c(-5:6),
                     labels = c(-5:6)) +
  scale_x_continuous(name = "Category",
                     limits = c(0.75,3.25),
                     breaks = c(1:3),
                     labels = c("Pleasant", "Neutral", "Unpleasant")) +
  ggtitle("Raw") +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5)) 

demeaned_lpp_cat_par_bee_line <- amp_per_cat_per_par_df %>% 
  filter(type == 1) %>% 
  ggplot(aes(x = cate, 
             y = demean_amp, 
             group = par)) +
  geom_beeswarm(size = 1) +
  geom_line(position = ggbeeswarm::position_beeswarm(),
            linewidth = .1) +
  scale_y_continuous(name = "Microvoltage",
                     breaks = c(-5:6),
                     labels = c(-5:6)) +
  scale_x_continuous(name = "Category",
                     limits = c(0.75,3.25),
                     breaks = c(1:3),
                     labels = c("Pleasant", "Neutral", "Unpleasant")) +
  ggtitle("Change from participant mean") +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5)) 

zscore_lpp_cat_par_bee_line <- amp_per_cat_per_par_df %>% 
  filter(type == 1) %>% 
  ggplot(aes(x = cate, 
             y = zamp, 
             group = par)) +
  geom_beeswarm(size = 1) +
  geom_line(position = ggbeeswarm::position_beeswarm(),
            linewidth = .1) +
  scale_y_continuous(name = "Z-score",
                     breaks = c(seq(-2,2,by = .2)),
                     labels = c(seq(-2,2,by = .2))) +
  scale_x_continuous(name = "Category",
                     limits = c(0.75,3.25),
                     breaks = c(1:3),
                     labels = c("Pleasant", "Neutral", "Unpleasant")) +
  ggtitle("Z-scored within-participant") +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5)) 

raw_ssvep_cat_par_bee_line <- amp_per_cat_per_par_df %>% 
  filter(type == 2) %>% 
  ggplot(aes(x = cate, 
             y = amp, 
             group = par)) +
  geom_beeswarm(size = 1) +
  geom_line(position = ggbeeswarm::position_beeswarm(),
            linewidth = .1) +
  scale_y_reverse(name = "Reversed Microvoltage",
                  breaks = c(seq(-.5,2,by = .1)),
                  labels = c(seq(-.5,2,by = .1))) +
  scale_x_continuous(name = "Category",
                     limits = c(0.75,3.25),
                     breaks = c(1:3),
                     labels = c("Pleasant", "Neutral", "Unpleasant")) +
  ggtitle("Raw") +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5)) 

demeaned_ssvep_cat_par_bee_line <- amp_per_cat_per_par_df %>% 
  filter(type == 2) %>% 
  ggplot(aes(x = cate, 
             y = demean_amp, 
             group = par)) +
  geom_beeswarm(size = 1) +
  geom_line(position = ggbeeswarm::position_beeswarm(),
            linewidth = .1) +
  scale_y_reverse(name = "Reversed Microvoltage",
                  breaks = c(seq(-.4,.2,by = .05)),
                  labels = c(seq(-.4,.2,by = .05))) +
  scale_x_continuous(name = "Category",
                     limits = c(0.75,3.25),
                     breaks = c(1:3),
                     labels = c("Pleasant", "Neutral", "Unpleasant")) +
  ggtitle("Change from participant mean") +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5)) 

zscore_ssvep_cat_par_bee_line <- amp_per_cat_per_par_df %>% 
  filter(type == 2) %>% 
  ggplot(aes(x = cate, 
             y = zamp, 
             group = par)) +
  geom_beeswarm(size = 1) +
  geom_line(position = ggbeeswarm::position_beeswarm(),
            linewidth = .1) +
  scale_y_reverse(name = "Reversed Z-score",
                  breaks = c(seq(-2,2,by = .2)),
                  labels = c(seq(-2,2,by = .2))) +
  scale_x_continuous(name = "Category",
                     limits = c(0.75,3.25),
                     breaks = c(1:3),
                     labels = c("Pleasant", "Neutral", "Unpleasant")) +
  ggtitle("Z-scored within-participant") +
  theme_classic() +
  theme(plot.title = element_text(hjust = .5)) 

layout_grid <- c("
AB
CD
EF
")

raw_lpp_cat_par_bee_line + raw_ssvep_cat_par_bee_line +
  demeaned_lpp_cat_par_bee_line + demeaned_ssvep_cat_par_bee_line +
  zscore_lpp_cat_par_bee_line + zscore_ssvep_cat_par_bee_line +
  plot_layout(design = layout_grid)  +
  plot_annotation(title = "            Scene-LPP                                                   Video-ssVEP",
                  theme = theme(
                    plot.title = element_text(size = 15,
                                              color = "black",
                                              hjust = 0.5,
                                              face = "bold"))) 


ggsave(filename = paste0(parent_directory,
                         "/misc/S1Figure.tiff"),
       device = "tiff",dpi = 300,
       units = "in",height = 6, width = 6,
       scale = 1.3)


# Supplemental Figure 2 ####
lpp_correlation_summary <- model011_lpp_fit_draws %>% 
  select(starts_with("par_amp_aro_cor")) %>% 
  pivot_longer(
    cols = starts_with("par_amp_aro_cor")) %>% 
  group_by(name) %>% 
  summarise(probability_above_zero = sum(value > 0)/n() * 100,
            difference_from_97_5 = if_else(probability_above_zero > 97.5,
                                           probability_above_zero - 97.5,
                                           0),
            significant_T_F = if_else(probability_above_zero > 97.5,
                                      T,
                                      F))
  
merged_lpp_correlation_draws <- (model011_lpp_fit_draws %>% 
                                  select(starts_with("par_amp_aro_cor")) %>% 
                                  pivot_longer(cols = starts_with("par_amp_aro_cor"))) %>%
  merge(x = ., y = lpp_correlation_summary,
        by.x = "name", by.y = "name", all.x = T)

ssvep_correlation_summary <- model011_ssvep_fit_draws %>% 
  select(starts_with("par_amp_aro_cor")) %>% 
  pivot_longer(
    cols = starts_with("par_amp_aro_cor")) %>% 
  group_by(name) %>% 
  summarise(probability_above_zero = sum(value < 0)/n() * 100,
            difference_from_97_5 = if_else(probability_above_zero > 97.5,
                                           probability_above_zero - 97.5,
                                           0),
            significant_T_F = if_else(probability_above_zero > 97.5,
                                      T,
                                      F))
  
merged_ssvep_correlation_draws <- (model011_ssvep_fit_draws %>% 
                                  select(starts_with("par_amp_aro_cor")) %>% 
                                  pivot_longer(cols = starts_with("par_amp_aro_cor"))) %>%
  merge(x = ., y = ssvep_correlation_summary,
        by.x = "name", by.y = "name", all.x = T)
  


x_axis <- c(-.15,.6)
y_axis <- c(0, 1.05)
line_thickness_range <- c(1,2.5)
alpha_range <- c(.1, .7)
axis_line_thickness <- 1
text_size <- 15

(merged_lpp_correlation_draws %>% 
  ggplot() +
    geom_vline(aes(xintercept = 0),
               size = 2) +
    geom_line(aes(x = value,
                  group = name,
                  y = after_stat(scaled),
                  linewidth = difference_from_97_5,
                  alpha = difference_from_97_5,
                  color = significant_T_F),
              stat = "density") +
    scale_color_manual(values = c("red1", "blue1")) +
    scale_linewidth_continuous(range = line_thickness_range) +
    scale_alpha_continuous(range = alpha_range) +
    scale_y_continuous(name = "Normalized Density") +
    scale_x_continuous(name = "Scene-LPP Arousal Correlation",
                       breaks = seq(-.1, .5, by = .1)) +
    coord_cartesian(xlim = x_axis, ylim = y_axis, expand = c(0)) +
    theme_classic() +
    theme(text = element_text(size = text_size, 
                              family = "Arial"),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line = element_line(size = axis_line_thickness,
                                   lineend = "square"),
          axis.text = element_text(color = "black"))) /
(merged_ssvep_correlation_draws %>% 
  ggplot() +
    geom_vline(aes(xintercept = 0),
               size = 2) +
    geom_line(aes(x = value,
                  group = name,
                  y = after_stat(scaled),
                  linewidth = difference_from_97_5,
                  alpha = difference_from_97_5,
                  color = significant_T_F),
                  stat = "density") +
    scale_color_manual(values = c("red1", "blue1")) +
    scale_linewidth_continuous(range = line_thickness_range) +
    scale_alpha_continuous(range = alpha_range) +
    scale_x_reverse(name = "Video-ssVEP Arousal Correlation",
                       breaks = seq(.1, -.5, by = -.1)) +
    scale_y_continuous(name = "Normalized Density") +
    coord_cartesian(xlim = x_axis*-1, ylim = y_axis, expand = c(0))+
    theme_classic() +
    theme(text = element_text(size = text_size, 
                              family = "Arial"),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line = element_line(size = axis_line_thickness,
                                   lineend = "square"),
          axis.text = element_text(color = "black"))) + 
  plot_annotation(title = "Model 2: EEG by Arousal Correlation per Participant",
          theme = theme(
            plot.title = element_text(family = "Arial",
                                      size = 21,
                                      color = "black",
                                      hjust = 0.5,
                                      face = "bold")
          ))


ggsave(filename = paste0(parent_directory,
                         "/misc/S2Figure.tiff"),
device = "tiff",dpi = 300,
units = "in", height = 4, width = 6,
scale = 1.25)
