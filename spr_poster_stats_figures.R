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
  picture_by_cat_wave_directory <- paste0(sabat_data_folder, 
                                        "/stats_folder/picture_by_cat_49pars/CAVG")
  picture_by_scene_ar_directory <- paste0(sabat_data_folder, 
                                        "/avg_files/pics/by_scene")
  video_by_cat_directory <- paste0(sabat_data_folder, 
                                "/hamp_files/videos/category")
  video_by_cat_wave_directory <- paste0(sabat_data_folder, 
                                "/stats_folder/video_by_cat_49pars_not_same_as_picture/CAVG")
  
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
} else{
  stop("Something is wrong, are you working on the right computer with access to the data")
}


pic_vid_trial_data <- data.frame("par_id" = 1:50)

# Bad participants
no_pictures <- c(9)
no_videos <- c(38)
bad_participants <- c(9,38)

## no picture participants were missing more than 50% for any valence
# more_than_50_percent_of_video_trials_missing <- c(4, 22, 24, 25, 39, 45, 49, 50) # this is total trials, not per valence

pic_vid_trial_data <- pic_vid_trial_data %>% 
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

pic_vid_trial_data <- merge(x = pic_vid_trial_data, y = number_of_good_trials,
                      by.x = "par_id", by.y = "par_id", all.x = T)

pic_vid_trial_data %>% 
  summarise(mean_pic = mean(number_of_good_trials_pictures),
            mean_vid = mean(number_of_good_trials_video))

# write.csv(pic_vid_trial_data,
#           file = paste0(parent_directory, "/misc/pic_vid_good_trials_out_of_90.csv"),
#           quote = F,row.names = F)


# Demographics
demographic_information <- read.csv(paste0(parent_directory, "/misc/Participant_data.csv")) %>% 
  mutate(par_id = 1:50, .before = 1) %>% 
  select(-Par_id) %>% 
  filter(!par_id %in% bad_participants)

demographic_information %>% pull(age) %>% summary()

demographic_information %>% pull(sex) %>% table()

demographic_information %>% pull(race.ethnicity) %>% table()

# Valence Arousal
ratings_data <- read.csv(paste0(parent_directory, "/misc/ratings_by_par_by_scene.csv")) %>% 
  mutate(par_id = stringr::str_extract(Par_id, "\\d+") %>% as.numeric(),
         .before = 1) %>% 
  select(-Par_id) %>% 
  filter(!par_id %in% bad_participants) %>% 
  mutate(Stim_cat = factor(Stim_cat,
                           levels = c("Pleasant",
                                      "Neutral",
                                      "Unpleasant")))


# Load in video hamp and scene LPP data

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


#lpp 400ms 279pt - 900ms 526pt
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


  

#ssvep 1000ms 1537pt - 9000ms 5633pt
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

# ssvep by video
# 
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

lpp_scene_dat <- by_scene_info %>% 
  select(scene_id_con_num, scene) %>% 
  merge(y = lpp_scene_dat, by.x = "scene_id_con_num", by.y = "scene_id", all.y = T) %>% 
  rename(scene_id = scene_id_con_num)

# Add necessary information and merge
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
  summarise_all(mean)

gm_lpp_scene_dat <- lpp_scene_dat %>% 
  group_by(scene) %>% 
  select(-par_id) %>% 
  summarise_all(mean)
  


# Stats
afex::aov_ez(id = "par_id", 
             dv = "zscore_ssvep_amp", 
             within = "category", 
             data = ssvep_lpp_dat_by_participant)

afex::aov_ez(id = "par_id", 
             dv = "zscore_lpp_amp", 
             within = "category", 
             data = ssvep_lpp_dat_by_participant)


ssvep_lpp_dat_by_participant %>% 
  select(-c("lpp_amp", "ssvep_amp")) %>% 
  pivot_longer(
  cols = starts_with("zscore_")) %>% 
  afex::aov_ez(id = "par_id", dv = "value", within = c("category", "name")) %>% 
  summary()

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



# Figures

## ratings
dot_size <- 7
dodge_size <- .7

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
  geom_line(aes(x = Stim_cat,
                y = mean_rating, 
                group = Stim_type),
            linetype = "dotted",
            position = position_dodge(width = dodge_size)) + 
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
  # annotate("text", x = 2, y = 0.5, label = "Stimulus X Valence\nF(2,94) = 2.23, p = .12;\nηg2 = .023", size = 8) +
  scale_y_continuous(breaks = ratings_cat_breaks,
                     limits = ratings_cat_limits,
                     name = "Unarousing / Unpleasant         Arousing / Pleasant") +
  scale_shape_manual(name = "Stimulus Modality", values = c(15,19), labels = c("Scenes", "Videos")) +
  scale_color_manual(values = c("blue", "black", "red"),) +
  scale_x_discrete(name = "Category", labels = c("Pleasant", "Neutral", "Unpleasant")) +
  guides(color = "none") +
  theme_classic() +
  ggtitle("Arousal") +
  theme(#legend.position = c(.5,.97), 
        legend.title.align = .5,
        plot.title = element_text(hjust = .5, vjust = .5),
        #legend.justification = c(.5,1)
        #,legend.key.height = unit(.5, "cm")
        legend.box.margin = margin(-20, 0, 0, 0),
        text = element_text(size = text_size, family = "Arial", face = "bold"),
        axis.line = element_line(size = axis_line_thickness),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "none",
        axis.title.x = element_blank()) +
  guides(shape = guide_legend(direction = "horizontal", title.position = "top"))

valence_plot <- gm_ratings_long %>% 
  filter(rating == "val") %>% 
  ggplot() +
  geom_line(aes(x = Stim_cat,
                y = mean_rating, 
                group = Stim_type),
            linetype = "dotted",
            position = position_dodge(width = 0.4)) + 
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
  # annotate("text", x = 2, y = 0.5, label = "Stimulus X Valence\nF(2,94) = 2.23, p = .12;\nηg2 = .023", size = 8) +
  scale_y_continuous(breaks = ratings_cat_breaks,
                     limits = ratings_cat_limits) +
  scale_shape_manual(name = "Ratings by Stimulus Modality", values = c(15,19), labels = c("Scenes", "Videos")) +
  scale_color_manual(values = c("blue", "black", "red"),) +
  scale_x_discrete(name = "Category", labels = c("Pleasant", "Neutral", "Unpleasant")) +
  guides(color = "none") +
  theme_classic() +
  ggtitle("Valence") +
  theme(legend.position = c(.5,.5), 
        legend.justification = c(0.5, 0.5),
        legend.box.just = "center",
        # legend.title.align = .5,
        legend.text = element_text(size = text_size + 2),
        legend.text.align = .5,
        legend.title = element_blank(),
        plot.title = element_text(hjust = .5, vjust = .5),
        #legend.justification = c(.5,1)
        #,legend.key.height = unit(.5, "cm")
        # legend.box.margin = margin(-20, 0, 0, 0),
        text = element_text(size = text_size, family = "Arial", face = "bold"),
        axis.line = element_line(size = axis_line_thickness),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  guides(shape = guide_legend(direction = "horizontal", title.position = "top"))

layout <- "
AA
BC
BC
BC
BC
BC
BC
BC
BC
BC
BC
BC
BC
BC
BC
BC
BC
BC
BC
"


jpeg(filename = paste0(parent_directory, "/misc/cat_ratings.jpg"),
     width = 8, height = 8, units = "in", res = 300)
guide_area() + arousal_plot + valence_plot +
  plot_layout(design = layout,guides = "collect")
dev.off()

## By scene ratings (trying to have each point be a picture)
library(grid)
library(jpeg)

by_scene_ratings_with_path <- ratings_data %>% 
  group_by(Stim_name, Stim_type, Stim_cat) %>% 
  summarise(mean_aro = mean(arousal),
            mean_val = mean(valence)) %>% 
  mutate(path = paste0("/home/andrewf/Research_data/EEG/Pic_Vid/Stimuli/matt_diss/Pics/", 
                       Stim_name, ".jpg"))

# video by picture figures

arousal_by_modality <- by_scene_ratings_with_path %>% 
  pivot_wider(id_cols = c("Stim_name", "path"),
              names_from = Stim_type, 
              values_from = mean_aro)

valence_by_modality <- by_scene_ratings_with_path %>% 
  pivot_wider(id_cols = c("Stim_name", "path"),
              names_from = Stim_type, 
              values_from = mean_val)
  
arousal_raster <- arousal_by_modality %>% 
  ggplot(aes(Pics, Video)) +
  geom_blank() +
  geom_line(data = data.frame(x = seq(1,9,by =.1), y = seq(1,9,by =.1)),
            aes(x, y), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(2, 8, by = 1),
                     limits = c(2.5,8.5),
                     expand = c(0,0),
                     name = "Scene Arousal Rating") +
  scale_y_continuous(breaks = seq(2, 8, by = 1),
                     limits = c(2.5,8.5),
                     expand = c(0,0),
                     name = "Video Arousal Rating") +
  theme_classic()


for (i in 1:90) {
  img <- arousal_by_modality$path[i] %>% 
    readJPEG() %>% 
    rasterGrob(interpolate=TRUE)
  
  arousal_raster <- arousal_raster + 
    annotation_custom(img, 
                      xmin = arousal_by_modality$Pics[i] -.15, 
                      xmax = arousal_by_modality$Pics[i] +.15, 
                      ymin = arousal_by_modality$Video[i] -.15, 
                      ymax = arousal_by_modality$Video[i] +.15)
  print(i)
}

jpeg(filename = paste0(parent_directory, "/misc/arousal_raster.jpg"),
     width = 8, height = 8, units = "in", res = 300)
arousal_raster
dev.off()

valence_raster <- valence_by_modality %>% 
  ggplot(aes(Pics, Video)) +
  geom_blank() +
  geom_line(data = data.frame(x = seq(1,9,by =.1), y = seq(1,9,by =.1)),
            aes(x, y), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(2, 8, by = 1),
                     limits = c(2.5,8.5),
                     expand = c(0,0),
                     name = "Scene Valence Rating") +
  scale_y_continuous(breaks = seq(2, 8, by = 1),
                     limits = c(2.5,8.5),
                     expand = c(0,0),
                     name = "Video Valence Rating") +
  theme_classic()


for (i in 1:90) {
  img <- valence_by_modality$path[i] %>% 
    readJPEG() %>% 
    rasterGrob(interpolate=TRUE)
  
  valence_raster <- valence_raster + 
    annotation_custom(img, 
                      xmin = valence_by_modality$Pics[i] -.15, 
                      xmax = valence_by_modality$Pics[i] +.15, 
                      ymin = valence_by_modality$Video[i] -.15, 
                      ymax = valence_by_modality$Video[i] +.15)
  print(i)
}

jpeg(filename = paste0(parent_directory, "/misc/valence_raster.jpg"),
     width = 8, height = 8, units = "in", res = 300)
valence_raster
dev.off()

# arousal predict lpp or ssvep stats and plots ####



#arousal by valence figures


by_picture_raster <- by_scene_ratings_with_path %>% 
  filter(Stim_type == "Pics") %>% 
  ggplot(aes(mean_aro, mean_val)) +
  geom_blank() +
  scale_x_continuous(breaks = seq(2, 9, by = 1),
                     limits = c(2,9),
                     name = "Arousal") +
  scale_y_continuous(breaks = seq(2, 9, by = 1),
                     limits = c(2,9),
                     name = "Valence") +
  theme_classic()

img_paths <- by_scene_ratings_with_path %>% 
  filter(Stim_type == "Pics") %>% 
  pull(path)

just_picture_ratings <- by_scene_ratings_with_path %>% 
  filter(Stim_type == "Pics")

for (i in 1:90) {
img <- img_paths[i] %>% 
  readJPEG() %>% 
  rasterGrob(interpolate=TRUE)

by_picture_raster <- by_picture_raster + 
  annotation_custom(img, 
                    xmin = just_picture_ratings$mean_aro[i] -.15, 
                    xmax = just_picture_ratings$mean_aro[i] +.15, 
                    ymin = just_picture_ratings$mean_val[i] -.15, 
                    ymax = just_picture_ratings$mean_val[i] +.15)
print(i)
}

jpeg(filename = paste0(parent_directory, "/misc/pic_raster_scatter.jpg"),
     width = 8, height = 8, units = "in", res = 300)
by_picture_raster
dev.off()

by_video_raster <- by_scene_ratings_with_path %>% 
  filter(Stim_type == "Video") %>% 
  ggplot(aes(mean_aro, mean_val)) +
  geom_blank() +
  scale_x_continuous(breaks = seq(2, 9, by = 1),
                     limits = c(2,9),
                     name = "Arousal") +
  scale_y_continuous(breaks = seq(2, 9, by = 1),
                     limits = c(2,9),
                     name = "Valence") +
  theme_classic()

img_paths <- by_scene_ratings_with_path %>% 
  filter(Stim_type == "Video") %>% 
  pull(path)

just_video_ratings <- by_scene_ratings_with_path %>% 
  filter(Stim_type == "Video")

for (i in 1:90) {
  img <- img_paths[i] %>% 
    readJPEG() %>% 
    rasterGrob(interpolate=TRUE)
  
  by_video_raster <- by_video_raster + 
    annotation_custom(img, 
                      xmin = just_video_ratings$mean_aro[i] -.15, 
                      xmax = just_video_ratings$mean_aro[i] +.15, 
                      ymin = just_video_ratings$mean_val[i] -.15, 
                      ymax = just_video_ratings$mean_val[i] +.15)
  print(i)
}

jpeg(filename = paste0(parent_directory, "/misc/video_raster_scatter.jpg"),
     width = 8, height = 8, units = "in", res = 300)
by_video_raster
dev.off()


## ERP category
y_axis_breaks <- seq(-.8, .6, by = .2)
y_axis_limits <- c(-1,.8)
text_size = 20
axis_line_thickness = 2


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
  annotate("text", x = 2, y = 0.5, label = "Stimulus X Valence\nF(2,94) = 2.23, p = .12;\nηg2 = .023", size = 8) +
  scale_y_continuous(breaks = y_axis_breaks,
                     name = "Scene Z-score",
                     limits = y_axis_limits,
                     expand = c(0,0),
                     sec.axis = sec_axis(trans = ~ . * -1, 
                                         name = "Video Z-score",
                                         breaks = -y_axis_breaks)) +
  scale_shape_manual(name = "Measure", values = c(15,19), labels = c("Scene LPP", "Video ssVEP")) +
  scale_color_manual(values = c("blue", "black", "red"),) +
  scale_x_discrete(name = "Category", labels = c("Pleasant", "Neutral", "Unpleasant"), expand = c(0,.25)) +
  guides(color = "none") +
  theme_classic() +
  theme(legend.position = c(.5,.97), 
        legend.title.align = .5,
        legend.justification = c(.5,1)
        #,legend.key.height = unit(.5, "cm")
        , legend.box.margin = margin(-20, 0, 0, 0),
        text = element_text(size = text_size, family = "Arial", face = "bold"),
        axis.line = element_line(size = axis_line_thickness),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black")) +
  guides(shape = guide_legend(direction = "horizontal", title.position = "top"))

gm_amp_dot_plot


# write.csv(ssvep_lpp_dat_by_participant,
#           file = paste0(parent_directory, 
#                         "/misc/ssvep_lpp_dat_by_participant.csv"),
#           quote = F, 
#           row.names = F)

# ERP waveforms
line_width <- 2
line_outline <- .5

lpp_cat_wave_plot <- lpp_cat_wave %>% 
  ggplot() +
  geom_line(data = data.frame(x = c(0,0), y = c(-Inf, .9)), aes(x = x, y = y), linetype = "dashed") +
  geom_rect(aes(xmin = 400, xmax = 900, ymin = -Inf, ymax = Inf),
            fill = "lightgray")+
  geom_line(aes(x = time_ms, y = amp, group = category),
            color = "black",
            linewidth = line_width + line_outline) +
  annotate("text", x = 140, y = 1.3, face = "bold",
           label = "Scene Evoked LPP\nF(2,94) = 49.03, p < .001;\n ηg2 = .511", size = 8) +
  geom_line(aes(x = time_ms, y = amp, color = category),
            linewidth = line_width) +
  scale_y_continuous(limits = c(-2.3, 2),
                     expand = c(0,0), name = "MicroVoltage") +
  scale_x_continuous(limits = c(-125,1000), expand = c(0,0),
                     breaks = seq(-100, 900, by = 100), name = "Time (msec)") +
  scale_color_manual(values = c("blue1","black", "red1")) +
  theme_classic() +
  # ggtitle("Scene Evoked LPP \n F(2,94) = 49.03, p < .001") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5, vjust = 1),
        text = element_text(size = text_size, family = "Arial", face = "bold"),
        axis.line = element_line(size = axis_line_thickness),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"))

# lpp_cat_wave_plot

ssvep_cat_wave_plot <- ssvep_cat_wave %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_rect(aes(xmin = 1000, xmax = 9000, ymin = -Inf, ymax = 1.2),
            fill = "lightgray")+
  geom_line(aes(x = time_ms, y = amp, group = category),
            color = "black",
            linewidth = line_width + line_outline) +
  geom_line(aes(x = time_ms, y = amp, color = category),
            linewidth = line_width) +
  annotate("text", x = 5000, y = 1.25, label = "Video Reduction in ssVEP\nF(2,94) = 49.07, p < .001; ηg2 = .511", size = 8) +
  scale_y_continuous(limits = c(.9, 1.3),
                     expand = c(0,0), name = "Hilbert Amplitude") +
  scale_x_continuous(limits = c(-1250,10000),expand = c(0,0),
                     breaks = seq(-1000, 9000, by = 1000),
                     name = "Time (msec)") +
  scale_color_manual(values = c("blue1","black", "red1")) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = text_size, family = "Arial", face = "bold"),
        axis.line = element_line(size = axis_line_thickness),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"))

# ssvep_cat_wave_plot



layout <- "
AAAAABBB
CCCCCBBB
"

jpeg(filename = paste0(parent_directory, "/misc/cat_GM_erp_results.jpg"),
     width = 15, height = 10, units = "in", res = 300)
lpp_cat_wave_plot + gm_amp_dot_plot + ssvep_cat_wave_plot +
  plot_layout(design = layout)
dev.off()






# jpeg(filename = "/home/andrewf/Research_data/EEG/EEGface_128_channel_UGA/misc/RQ3_figure1.jpg",
#      width = 15, height = 10, units = "in", res = 300)
# uga_wave_n170_epn + um_wave_n170_epn + n170_uga_dot_plots + epn_uga_dot_plots + n170_um_dot_plots + epn_um_dot_plots +
#   uga_wave_lpp + um_wave_lpp + lpp_uga_dot_plots + lpp_um_dot_plots + guide_area() +
#   plot_layout(design = layout, guides = "collect")
# dev.off()


# Old below

# 10 channels more anterior
# lpp_chans <- c(26, #FCC1H top left
#                30, #C1
#                34, #CCP1H
#                39, #CP1 bottom left
#                95, #CZ
#                40, #CPZ
#                90, #FCC2H top right
#                96, #C2
#                100, #CCP2H
#                104) #CP2 bottom right

# occipital_chans_many <- c(48, # P3
#                           52, # PPO5h
#                           54, # PO7
#                           58, # POO9h
#                           61, # O9/I1
#                           49, # P1
#                           53, # PPO1h
#                           55, # PO3
#                           60, # POO1
#                           59, # O1
#                           62, # OI1h
#                           50, # Pz
#                           56, # POZ
#                           63, # OZ
#                           64, # IZ
#                           113,# P2
#                           118,# PPO2h
#                           121,# PO4
#                           124,# POO2
#                           125,# O2
#                           127,# OI2h
#                           114,# P4
#                           119,# PPO6h
#                           122,# PO8
#                           126,# POO10h,
#                           128)# O10/I2

# ten channels
# occipital_chans_few <- c(58, # POO9h
#                          61, # O9/I1
#                          59, # O1
#                          62, # OI1h
#                          63, # OZ
#                          64, # IZ
#                          125,# O2
#                          127,# OI2h
#                          126,# POO10h
#                          128)# O10/I2

# ssvep_cat_many_sensors_dat <- EMEGShelper::read_ar_files(data_folders = cat_hamps_directory,
#                                                          patterns = ".hamp8$",
#                                                          select_time_points = c(1537:5633),
#                                                          average_channels = T,
#                                                          average_timepoints = T,
#                                                          extract_channels = occipital_chans_many) %>% 
#   reframe(file_name = list.files(path = cat_hamps_directory, 
#                                  pattern = ".hamp8$"),
#           amp = V1)

# ssvep_cat_few_sensors_dat <- EMEGShelper::read_ar_files(data_folders = cat_hamps_directory,
#                                                          patterns = ".hamp8$",
#                                                          select_time_points = c(1537:5633),
#                                                          average_channels = T,
#                                                          average_timepoints = T,
#                                                          extract_channels = occipital_chans_few) %>% 
#   reframe(file_name = list.files(path = cat_hamps_directory, 
#                                  pattern = ".hamp8$"),
#           amp = V1)

# ssvep_cat_many_sensors_dat <- ssvep_cat_many_sensors_dat %>% 
#   reframe(par_id = stringr::str_extract(ssvep_cat_many_sensors_dat$file_name, "\\d+") %>% as.numeric(),
#           cat_id = stringr::str_extract(ssvep_cat_many_sensors_dat$file_name, pattern = "at[1-3].hamp8$") %>% 
#             stringr::str_extract("\\d+") %>% as.numeric(),
#           ssvep_many_sensors_amp = amp) %>% 
#   mutate("category" = case_when(
#     cat_id == 1 ~ "pleasant",
#     cat_id == 2 ~ "neutral",
#     cat_id == 3 ~ "unpleasant"),
#     .before = ssvep_many_sensors_amp) %>% 
#   group_by(par_id) %>% 
#   mutate("zscore_ssvep_many_sensors_amp" = as.numeric(scale(ssvep_many_sensors_amp)))

# ssvep_cat_dat <- ssvep_cat_few_sensors_dat %>% 
#   reframe(par_id = stringr::str_extract(ssvep_cat_few_sensors_dat$file_name, "\\d+") %>% as.numeric(),
#           cat_id = stringr::str_extract(ssvep_cat_few_sensors_dat$file_name, pattern = "at[1-3].hamp8$") %>% 
#             stringr::str_extract("\\d+") %>% as.numeric(),
#           ssvep_amp = -amp) %>% 
#   mutate("category" = case_when(
#     cat_id == 1 ~ "pleasant",
#     cat_id == 2 ~ "neutral",
#     cat_id == 3 ~ "unpleasant"),
#     .before = ssvep_amp) %>% 
#   group_by(par_id) %>% 
#   mutate("zscore_ssvep_amp" = as.numeric(scale(ssvep_amp)))