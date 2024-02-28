library(tidyverse)
library(patchwork)
library(cmdstanr)
library(plotrix)
library(ggbeeswarm)
library(gganimate)
library(ggthemes)


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


save(ssvep_lpp_dat_by_participant,
     ratings_erps_path_by_scene,
     gm_
     data_for_stan_df, 
     file = paste0(parent_directory,
                   "/paper_data_models/data/pic_vid_paper.RData"))

# Fit models ####
# fit ssvep and lpp separately, but use same model for each

posterior_samples_per_chain <- 10000
number_of_chains <- 8 # 8 performance cores on mac
number_of_parallel_chains <- ifelse(parallel::detectCores() > 8, 8, 4)


## par intercept ####
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


## par and cate intercepts ####

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


## par and stim intercepts ####

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





## par intercept, arousal slope ####

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


# par and cate intercepts, arousal slope

model005_path <- paste0(pic_vid_repository,
                        "/stan_models/model005_par_cate_intercepts_arousal_slope.stan")

model005 <- cmdstan_model(model005_path, force_recompile = T)

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


# par and stim intercepts, arousal slope

model006_path <- paste0(pic_vid_repository,
                        "/stan_models/model006_par_stim_intercepts_arousal_slope.stan")

model006 <- cmdstan_model(model006_path, force_recompile = T)

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

# multi-level correlation

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

#
model008_path <- paste0(pic_vid_repository,
                        "/stan_models/model008_par_cate_intercepts_arousal_slope_ML.stan")

model008 <- cmdstan_model(model008_path, force_recompile = T)

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

# 9
model009_path <- paste0(pic_vid_repository,
                        "/stan_models/model009_par_stim_intercepts_arousal_slope_ML.stan")

model009 <- cmdstan_model(model009_path, force_recompile = T)

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


# save models

save(
  model003_lpp_fit,
  model003_lpp_fit_summary,
  model003_ssvep_fit,
  model003_ssvep_fit_summary,
  model007_lpp_fit,
  model007_lpp_fit_summary,
  model007_ssvep_fit,
  model007_ssvep_fit_summary,
  model009_lpp_fit,
  model009_lpp_fit_summary,
  model009_ssvep_fit,
  model009_ssvep_fit_summary,
  file = paste0(parent_directory,"/paper_data_models/models/paper_models.RData"))

save(
  model001_lpp_fit,
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
  file = paste0(parent_directory,"/paper_data_models/models/models.RData"))


# Old below this####

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

model001_path <- paste0(pic_vid_repository,
                        "/stan_models/pic_vid001_par.stan")

model001 <- cmdstan_model(model001_path, force_recompile = T)

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
                                        "/stan_models/pic_vid002_par_cate.stan")

model002 <- cmdstan_model(model002_path, force_recompile = T)

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
                                        "/stan_models/pic_vid007_par_cate_tdis.stan")

model007 <- cmdstan_model(model007_path,force_recompile = T)

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
  lm.beta::lm.beta()
  summary()

