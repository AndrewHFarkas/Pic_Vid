#Moving the hamp files from the ar average folder to the hamp folder 
library(fs)
ar_averaged_folder<- "/Volumes/startech3TB_bkup/research_data/Pic_Vid/averaged_files/videos/by_scene"
hamp_folder<- "/Volumes/startech3TB_bkup/research_data/Pic_Vid/hamp_files/videos/by_scene"

file_pattern<- "\\.f\\.at\\d+\\.ar\\.hamp8$"

files_to_move<- list.files(ar_averaged_folder, pattern = file_pattern, full.names = T)

for(file in files_to_move){
  file_copy(file, file.path(hamp_folder, basename(file)))
  file_delete(file)
}

#The rest of this code averages all the hamp files (by scene hamp files) from the hamp folder by category and averages them by their category. This can be done for vids and pics but we usually don't hilbert transformation on the pics
#Averaging the hamp files that were by scene back into category averages 
library(tidyverse)
library(reticulate)
library(EMEGShelper)


#These lines of code take the hamp files that were converted into MATLAB and puts them into R objects 
by_scene_hamps_directory <- "/Volumes/startech3TB_bkup/research_data/Pic_Vid/hamp_files/videos/by_scene"

start_time <- Sys.time()

#Function to read in the hamp files (despite the function being called read_ar_files)
by_scene <- read_ar_files(data_folders = by_scene_hamps_directory,
                          patterns = "hamp8$",
                          include_channel_name = T,
                          include_file_name = T
                          # ,
                          # select_time_points = c(sec_1_tmpt:sec_9_tmpt),
                          # average_timepoints = T
)

end_time <- Sys.time()
end_time - start_time

#Saves the hamp files as a .RData object so that it is easier to pull later 
save(by_scene,
     file = "/Volumes/startech3TB_bkup/research_data/Pic_Vid/misc/by_scene_hamp.RData")

#Loads in the .Rdata object that was saved in the lines above 
load("/Volumes/startech3TB_bkup/research_data/Pic_Vid/misc/by_scene_hamp.RData")

#This is created manually and serves as a object that shows the video's category number and the unique video ID number 
vid_name_key <- read.csv("/Volumes/startech3TB_bkup/research_data/Pic_Vid/misc/video_key.csv")

by_scene_info <- data.frame("category" = NA,
                                     "scene_id_con_num" = NA,
                                     "scene" = NA,
                                     "par_id" = NA,
                                     #"block" = NA,
                            by_scene)



par_id <- stringi::stri_extract_first_words(by_scene_info$file_name)

par_id %>% unique()

by_scene_info$par_id <- par_id


#Can be adjusted for videos only or both pics and vids. This pairs the hamp data with the vid_name, unique id number, and category number
vid_name_key <- data.frame("ats" = paste0("at",c(1:90),".ar"),
                           vid_name_key)

videokey_row_index = 1
for (videokey_row_index in 1:nrow(vid_name_key)) {
  
  logical_current_video_match <- grepl(pattern = vid_name_key$ats[videokey_row_index], 
                                       x = by_scene_info$file_name)
  
  by_scene_info$scene_id_con_num[logical_current_video_match] <- 
    vid_name_key$video_id[videokey_row_index]
  
  by_scene_info$category[logical_current_video_match] <- 
    vid_name_key$cat_id[videokey_row_index]
  
  by_scene_info$scene[logical_current_video_match] <- 
    vid_name_key$video[videokey_row_index]
  
}



num_samples <- 0:5171
time_ms <- rep(-100, 5172)
time_btw_smpls <- 1000/512
time_to_add <- time_btw_smpls*num_samples
time_ms <- time_ms + time_to_add

video_column_key <- data.frame(time_ms,
                               column_label = paste0("V", 1:length(time_ms)))


save(by_scene_info, video_column_key,
     file = "/Volumes/startech3TB_bkup/research_data/Pic_Vid/misc/by_video.RData")

load("/Volumes/startech3TB_bkup/research_data/Pic_Vid/misc/by_video.RData")

by_scene_info$category %>% table()
by_scene_info$channel_names %>% table()
by_scene_info$scene_id_con_num %>% table()
by_scene_info$scene %>% table()
by_scene_info$par_id %>% table() 

by_category_info <- by_scene_info %>% 
  select(-c(scene_id_con_num, scene, file_name)) %>%
  group_by(par_id, category, channel_names) %>% 
  summarise_all(mean)

by_scene_info[1:7,1:7]


write.csv(by_category_info, 
          file = "/Volumes/startech3TB_bkup/research_data/Pic_Vid/misc/by_categorey_3_from_by_scene.csv")


save(by_category_info, video_column_key,
     file = "/Volumes/startech3TB_bkup/research_data/Pic_Vid/misc/by_categorey_3_from_by_scene.RData")
