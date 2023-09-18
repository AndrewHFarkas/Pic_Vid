library(tidyverse)


# WARNING this will move old ar files, so if you are not careful when moving files, you could combine ar files from different analyses.
# So check the data created information on the files you move.

# Andrew's last know location of data in sabat lab
sabat_data_folder <- "/Volumes/startech3TB_bkup/research_data/Pic_Vid"
andrew_data_folder <- "/home/andrewf/Research_data/EEG/Pic_Vid"

if (dir.exists(sabat_data_folder)) {
  parent_directory <- sabat_data_folder
  path_to_raw_data <- paste0(sabat_data_folder, "/raw_data")
  pic_by_scene_destination <- paste0(sabat_data_folder, "/average_files/pics/by_scene/")
  pic_by_category_destination <- paste0(sabat_data_folder, "/average_files/pics/by_category/")
  vid_by_scene_destination <- paste0(sabat_data_folder, "/average_files/videos/by_scene/")
} else if(dir.exists(andrew_data_folder)) {
  parent_directory <- andrew_data_folder
  path_to_raw_data <- paste0(andrew_data_folder, "/raw_data")
  pic_by_scene_destination <- paste0(andrew_data_folder, "/average_files/pics/by_scene/")
  pic_by_category_destination <- paste0(andrew_data_folder, "/average_files/pics/by_category/")
  vid_by_scene_destination <- paste0(andrew_data_folder, "/average_files/videos/by_scene/")
} else{
  stop("Something is wrong, are you working on the right computer with access to the data")
}

video_conditions <- c("A_1", "B_2", "C_1", "D_2")
video_conditions_regex  <- video_conditions %>% paste(collapse = "|")
picture_conditions  <- c("A_2", "B_1", "C_2", "D_1")
picture_conditions_regex  <- picture_conditions %>% paste(collapse = "|")

grab_files_newer_than <- ymd_h("2023-09-18 2", tz = "EDT")

picture_files_to_move <- list.files(path = path_to_raw_data,
           pattern = ".ar$",
           recursive = T,
           full.names = T) %>% 
  file.info() %>%
  add_column(., 
             file_name = rownames(.),
             .before = 1) %>% 
  filter(str_detect(file_name, picture_conditions_regex),
         ctime > grab_files_newer_than) %>% 
  pull(file_name)


# file.copy(from = picture_files_to_move,
#           to = pic_by_scene_destination, 
#           overwrite = T)

file.copy(from = picture_files_to_move,
          to = pic_by_category_destination,
          overwrite = T)


video_files_to_move <- list.files(path = path_to_raw_data,
                                  pattern = ".ar$",
                                  recursive = T,
                                  full.names = T) %>% 
  file.info() %>%
  add_column(., 
             file_name = rownames(.),
             .before = 1) %>% 
  filter(str_detect(file_name, video_conditions_regex),
         ctime > grab_files_newer_than) %>% 
  pull(file_name)





# video_files_to_move <- 
#   paste0(video_conditions, "\\.f\\.at[1-9][0-9]?\\.ar$") %>% 
#   paste(collapse = "|") %>% 
#   list.files(path = path_to_raw_data,
#              pattern = .,
#              recursive = T,
#              full.names = T) 

file.copy(from = video_files_to_move,
          to = vid_by_scene_destination, 
          overwrite = T,
          copy.date = T)


hold <- list.files(path = path_to_raw_data,
           pattern = ".ar$",
           recursive = T,
           full.names = T)
hold2 <- file.info(hold)

(hold2$ctime > ymd("2023/9/11"))



files_to_move <- list.files(path = path_to_raw_data,
                            pattern = "at[1-3].ar$",
                            recursive = T,
                            full.names = T)

file.copy(from = files_to_move,
          to = path_to_destination_folder, 
          overwrite = T)

# Sablab Mac
path_to_raw_data<- "/Volumes/startech3TB_bkup/research_data/Pic_Vid/raw_data"

path_to_destination_folder<- "/Volumes/startech3TB_bkup/research_data/Pic_Vid/averaged_files/pics/by_category"

files_to_move <- list.files(path = path_to_raw_data,
                            pattern = "at[1-3].ar$",
                            recursive = T,
                            full.names = T)

file.copy(from = files_to_move,
          to = path_to_destination_folder, 
          overwrite = T)

library(fs)
path_to_raw_data<- "/Volumes/startech3TB_bkup/research_data/Pic_Vid/raw_data"

path_to_destination_folder<- "/Volumes/startech3TB_bkup/research_data/Pic_Vid/averaged_files/pics/by_category"

first_level_subfolders<- dir_ls(path_to_raw_data, regexp = "Pic_Vid_\\d+_\\w")

for(subfolder in first_level_subfolders){
  
  second_level_subfolder<- dir_ls(subfolder)
  
  for(run_folder in second_level_subfolder){
    
    files_to_move<- dir_ls(run_folder, regexp = "\\.f\\.at[1-9][0-9]?\\.ar$")
    
    print(files_to_move)
    file_move(files_to_move, path_to_destination_folder)
    
  }
}
