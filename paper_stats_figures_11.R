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
# Note that par id in data for stan is not the original parids, bad participants already excluded
load(paste0(parent_directory,
            "/paper_data_models/data/pic_vid_paper.RData"))

## Get by category / grand means ####
data_for_stan_df %>%
  filter(type == 1) %>% 
  select(par, cate, amp) %>% 
  group_by(par,cate) %>% 
  summarise_all(mean) %>% 
  mutate("category_name" = case_when(
    cate == 1 ~ "pleasant",
    cate == 2 ~ "neutral",
    cate == 3 ~ "unpleasant"),
    .after = cate) %>% 
  mutate("zscore_amp" = as.numeric(scale(amp)))


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


## Get category waveforms ####

# Delete later
# lpp_cat_dat <- EMEGShelper::read_ar_files(data_folders = picture_by_cat_ar_directory,
#                                           patterns = ".ar$",
#                                           baseline_pts = c(14:65),
#                                           select_time_points = c(279:526),
#                                           average_channels = T,
#                                           average_timepoints = T,
#                                           include_file_name = T,
#                                           extract_channels = lpp_chans) %>% 
#   rename(amp = V1)
# 
# lpp_cat_wave <- EMEGShelper::read_ar_files(data_folders = picture_by_cat_wave_directory,
#                                            patterns = ".ar$",
#                                            baseline_pts = c(14:65),
#                                            average_channels = T,
#                                            include_file_name = T,
#                                            extract_channels = lpp_chans)

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

# Loo ####

loo::loo_compare(model011_lpp_fit_loo,
                 model012_lpp_fit_loo)

loo::loo_model_weights(list(model011_lpp_fit_loo,
                            model012_lpp_fit_loo))

loo::loo_compare(model011_ssvep_fit_loo,
                 model012_ssvep_fit_loo)

loo::loo_model_weights(list(model011_ssvep_fit_loo,
                            model012_ssvep_fit_loo))

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
# model 11 arousal, minus effect of participant
median(hold_lpp_bayesian_R_squared)
quantile(hold_lpp_bayesian_R_squared, 
         probs = c(.025, .975))
median(hold_ssvep_bayesian_R_squared)
quantile(hold_ssvep_bayesian_R_squared, 
         probs = c(.025, .975))

# Model 12 stim, minus effect of participant

median(model_R2_posteriors$model012_lpp_R2_stim)

quantile(model_R2_posteriors$model012_lpp_R2_stim, 
         probs = c(.025, .975))

median(model_R2_posteriors$model012_ssvep_R2_stim)

quantile(model_R2_posteriors$model012_ssvep_R2_stim, 
         probs = c(.025, .975))

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

# Figure 3 Video x picture ratings scatter####
library(grid)
library(jpeg)

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
                   labels = c("Pleasant", "Neutral", "Unpleasant")
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
  scale_y_continuous(limits = c(.9, 1.25),
                     expand = c(0,0), name = "ssVEP (μV)") +
  scale_x_continuous(limits = c(-2000,10000),expand = c(0,0),
                     breaks = seq(-1000, 9000, by = 1000),
                     labels = c(-1:9),
                     name = "Time (sec)"
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
       units = "in",height = 8, width = 8,
       scale = 1.275)


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
  scale_x_reverse(name = "Δ ssVEP Microvoltage (Axis Reversed)",
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


fig7_lpp_top + fig7_ssvep_bottom +
  plot_layout(design = layout_grid, guides = "collect")  +
  plot_annotation(title = "Video-ssVEP Lacks Typical Scene-LPP Erotica and Gore Sensitivity",
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

# RR Figure 7 ####

data_for_stan_df %>% 
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
  ggplot() +
  geom_pointrange(aes(x = cate_f7,
                      y = mean_aro,
                      ymin = mean_aro - se_aro,
                      ymax = mean_aro + se_aro,
                      color = cate_f7)) +
  scale_color_manual(values = fig7_colors) +
  facet_grid(~type) +
  theme_classic()
  

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
