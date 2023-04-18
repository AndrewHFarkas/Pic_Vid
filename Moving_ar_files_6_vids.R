library(fs)

##For Vids
path_to_raw_data<- "/Volumes/startech3TB_bkup/research_data/Pic_Vid/raw_data"

path_to_destination_folder<- "/Volumes/startech3TB_bkup/research_data/Pic_Vid/averaged_files/videos/by_scene"

first_level_subfolders<- dir_ls(path_to_raw_data, regexp = "Pic_Vid_\\d+_\\w")

for(subfolder in first_level_subfolders){
  
  second_level_subfolder<- dir_ls(subfolder)
  
  for(run_folder in second_level_subfolder){
    
    files_to_move<- dir_ls(run_folder, regexp = "\\.f\\.at[1-9][0-9]?\\.ar$")
    
    print(files_to_move)
    file_move(files_to_move, path_to_destination_folder)
    
  }
}
