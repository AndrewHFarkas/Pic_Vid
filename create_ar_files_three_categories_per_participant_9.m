
%% 

% three_cats_per_par = readtable("/Volumes/startech3TB_bkup/research_data/Pic_Vid/misc/by_categorey_3_from_by_scene.csv");

three_cats_per_par = readtable("/home/andrewf/Research_data/EEG/Pic_Vid/misc/by_categorey_3_from_by_scene.csv");

%% 
out_file_path_parent = char("/home/andrewf/Research_data/EEG/Pic_Vid/hamp_files/videos/by_category/");

number_of_sensors = 128;
trigger_point = 1025;

start_index = 1;
end_index = number_of_sensors;

ssvep_mat = table2array(three_cats_per_par( : , 5:end));

number_of_participants = 49;
number_of_categories = 3;
number_of_files_to_generate = number_of_participants*number_of_categories;

for file_index = 1:number_of_files_to_generate
    current_par = table2cell(three_cats_per_par(start_index, 2));
    current_cat = table2cell(three_cats_per_par(start_index, 3));
    out_file_path = char(strcat(out_file_path_parent, current_par{1}, '.at', num2str(current_cat{1}), '.hamp8'));

    current_mat = ssvep_mat(start_index:end_index, :);

    WriteMat2At(current_mat, out_file_path, [], 512, [], [], [], trigger_point)

    start_index = start_index + number_of_sensors;
    end_index = end_index + number_of_sensors;
end
 
%% 











%% 
