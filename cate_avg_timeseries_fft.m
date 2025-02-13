by_scene_at_path = '/home/andrewf/Research_data/EEG/Pic_Vid/average_files/videos/by_category';
 
fileInfo = dir(fullfile(by_scene_at_path, '*.ar'));  % Get all .app* files

arFilePaths = fullfile(by_scene_at_path, {fileInfo.name});

ssvepOccipitalChannels = [56 61 59 62 63 64 125 127 128];

ssVEPWindow1To9sec = [1538:5633];

cate_avg_fft_results = struct();

for i = 1:length(arFilePaths)

    current_timeseries = ReadAvgFile(arFilePaths{i});

    current_timeseries = current_timeseries(ssvepOccipitalChannels, ssVEPWindow1To9sec);

    [rawAmp, rawFreqs, rawFFTcomp] = ...
        freqtag_FFT3D(current_timeseries, 512);


    cate_avg_fft_results(i).filename = arFilePaths{i};

    cate_avg_fft_results(i).rawAmp = rawAmp;

    cate_avg_fft_results(i).rawFreqs = rawFreqs;

end

save('/home/andrewf/Repositories/Pic_Vid/cate_timeseries_avg_fft.mat', 'cate_avg_fft_results')

