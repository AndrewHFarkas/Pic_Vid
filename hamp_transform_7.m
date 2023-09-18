

% ar_directory = '/Volumes/startech3TB_bkup/research_data/Pic_Vid/averaged_files/videos/by_scene';

ar_directory = '/home/andrewf/Research_data/EEG/Pic_Vid/average_files/videos/by_scene';

cd(ar_directory)

% not looking for .DS_store, so might be a problem
filemat = getfilesindir(ar_directory);
% if .DS store is problem
filemat = filemat(2:end,:);

[demodmat, phasemat] = steadyHilbert(filemat, 7.5, [10:50], 8 , 0);