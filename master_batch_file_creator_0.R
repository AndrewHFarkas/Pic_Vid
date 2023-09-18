library(tidyverse)

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
                                        "/avgerage_files/pics/by_category")
  cat_hamps_directory <- paste0(andrew_data_folder, 
                                "/hamp_files/videos/by_category")
} else{
  stop("Something is wrong, are you working on the right computer with access to the data")
}

video_conditions <- c("A_1", "B_2", "C_1", "D_2")

picture_conditions  <- c("A_2", "B_1", "C_2", "D_1")

# bdfs 

video_bdf_paths <- paste0(video_conditions, ".bdf$") %>% 
  paste(collapse = "|") %>% 
  list.files(path = parent_directory, 
             pattern = ., 
             recursive = T,
             full.names = T)

video_bdf_paths[38] <- paste0("%", video_bdf_paths[38], " corrupted file")

scene_bdf_paths <- paste0(picture_conditions, ".bdf$") %>% 
  paste(collapse = "|") %>% 
  list.files(path = parent_directory, 
             pattern = ., 
             recursive = T,
             full.names = T)

scene_bdf_paths[9] <- paste0("%", scene_bdf_paths[9], " corrupted file")

write.table(x = video_bdf_paths,
            file = paste0(batch_file_directory, "/video_bdf_paths.rep"),
            quote = F,
            row.names = F,
            col.names = F)

write.table(x = scene_bdf_paths,
            file = paste0(batch_file_directory, "/scene_bdf_paths.rep"),
            quote = F,
            row.names = F,
            col.names = F)

# AEM files

video_aem_paths <- paste0(video_conditions, ".f.ses.AEM.AR$") %>% 
  paste(collapse = "|") %>% 
  list.files(path = parent_directory, 
             pattern = ., 
             recursive = T,
             full.names = T)

write.table(x = video_aem_paths,
            file = paste0(batch_file_directory, "/video_aem_paths.rep"),
            quote = F,
            row.names = F,
            col.names = F)

# 128est files (necessary when doing EditAEM via batchfiles)
video_est_paths <- paste0(video_conditions, ".f.ses.128est$") %>% 
  paste(collapse = "|") %>% 
  list.files(path = parent_directory, 
             pattern = ., 
             recursive = T,
             full.names = T)

write.table(x = video_est_paths,
            file = paste0(batch_file_directory, "/video_est_paths.rep"),
            quote = F,
            row.names = F,
            col.names = F)

# Picture by category ar files
picture_by_cat_ar_paths <- list.files(path = picture_by_cat_ar_directory, 
                                      pattern = "ar$", 
                                      recursive = T,
                                      full.names = T)

write.table(x = picture_by_cat_ar_paths,
            file = paste0(batch_file_directory, "/picture_by_cat_ar_paths.rep"),
            quote = F,
            row.names = F,
            col.names = F)

#hamps in order by category vids 

pleasant_hamps <- list.files(cat_hamps_directory, 
                             pattern = "at1.hamp8$",
                             full.names = T)

neutral_hamps <- list.files(cat_hamps_directory, 
                            pattern = "at2.hamp8$",
                            full.names = T)

unpleasant_hamps <- list.files(cat_hamps_directory, 
                               pattern = "at3.hamp8$",
                               full.names = T)

hamps <- c(pleasant_hamps,
           neutral_hamps,
           unpleasant_hamps)

write.table(x = hamps,
            file = paste0(batch_file_directory, 
                          "/vid_hamps_by_category_by_participant.rep"),
            quote = F,
            row.names = F,
            col.names = F)

#old below
z



#hamps in order by category pics
batch_files_dir <- "/Volumes/startech3TB_bkup/research_data/Pic_Vid/batch_files/"
cat_ar_dir <- '/Volumes/startech3TB_bkup/research_data/Pic_Vid/averaged_files/pics/by_category'

pleasant_ar <-   list.files(cat_ar_dir, 
                               pattern = "at1.ar$",
                               full.names = T)

neutral_ar <-    list.files(cat_ar_dir, 
                               pattern = "at2.ar$",
                               full.names = T)

unpleasant_ar <-                 list.files(cat_ar_dir, 
                                               pattern = "at3.ar$",
                                               full.names = T)

ars <- c(pleasant_ar,
           neutral_ar,
           unpleasant_ar)

write.table(x = ars,
            file = paste0(batch_files_dir, "pics_ar_by_category_by_par.rep"),
            quote = F,
            row.names = F,
            col.names = F)


