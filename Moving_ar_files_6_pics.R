##For pics 

# WARNING this will move old ar files, so if you are not careful when moving files, you could combine ar files from different analyses.
# So check the data created information on the files you move.

# Dean's computer
path_to_raw_data<- "/Volumes/DATA/PicVid/raw_data/"

path_to_destination_folder<- "/Volumes/DATA/PicVid/avg_files/pics/by_category/"

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
