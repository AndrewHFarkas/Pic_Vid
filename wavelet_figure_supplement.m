% Set up
%run_emegs_28
clear
sampleRateHz = 512;

timeAxisMs =  -2000:(1000/sampleRateHz):10000;

totalTimeSeconds = 12;

frequencyResolution = 1/totalTimeSeconds;

nyquist = sampleRateHz / 2;

frequencyAxisTotal = 0:frequencyResolution:nyquist;


frquencyRangeOfInterestHz = [3 35];

[differenceFromTargetStartFrequencyHz , frequencyAxisIndexStart] = ...
    min(abs(frequencyAxisTotal - frquencyRangeOfInterestHz(1)));

[differenceFromTargetStopFrequencyHz , frequencyAxisIndexStop] = ...
    min(abs(frequencyAxisTotal - frquencyRangeOfInterestHz(2)));

% numberOfFrequenciesToSkip = 10;
numberOfFrequenciesToSkip = 6;

finalFrequencyIndices = ...
    frequencyAxisIndexStart : numberOfFrequenciesToSkip : frequencyAxisIndexStop;

condensedFrequencyAxis = frequencyAxisTotal(finalFrequencyIndices);

baselineSamplePoints = [200:1000];

bySceneAppPath = '/home/andrewf/Research_data/EEG/Pic_Vid/average_files/videos/by_category_app_cat3_3_1Hz_high_pass';
cd(fullfile(bySceneAppPath))
 
pleasantApp = dir(fullfile(bySceneAppPath, '*.app1'));  
neutralApp = dir(fullfile(bySceneAppPath, '*.app2'));  
unpleasantApp = dir(fullfile(bySceneAppPath, '*.app3'));  

pleasantAppFilePaths = fullfile(bySceneAppPath, {pleasantApp.name});
neutralAppFilePaths = fullfile(bySceneAppPath, {neutralApp.name});
unpleasantAppFilePaths = fullfile(bySceneAppPath, {unpleasantApp.name});

pleasantMat = [];
finalSize = 0;
waveletPowerStructPleasant = struct;

for i = 1:length(pleasantAppFilePaths)

    currentPleasantMat = app2mat(pleasantAppFilePaths{i}, 0);

    [currentPleasantMatWavletpower] = wavelet_app_mat(currentPleasantMat, ...
    sampleRateHz, finalFrequencyIndices(1), finalFrequencyIndices(end), numberOfFrequenciesToSkip, [],...
    'currentPleasantMatWavletpower');

    [currentPleasantMatWavletpowerBaselined] =...
    bslcorrWAMat_percent(currentPleasantMatWavletpower, baselineSamplePoints);

    waveletPowerStructPleasant(i).waveletPower = currentPleasantMatWavletpower;

    waveletPowerStructPleasant(i).baselinedWaveletPower = currentPleasantMatWavletpowerBaselined;

    pleasantMat = cat(3, pleasantMat, currentPleasantMat);
    i
    finalSize = finalSize + size(currentPleasantMat,3)

end


neutralMat = [];
finalSize = 0;
waveletPowerStructNeutral = struct;

for i = 1:length(neutralAppFilePaths)

    currentNeutralMat = app2mat(neutralAppFilePaths{i}, 0);

    [currentNeutralMatWavletpower] = wavelet_app_mat(currentNeutralMat, ...
    sampleRateHz, finalFrequencyIndices(1), finalFrequencyIndices(end), numberOfFrequenciesToSkip, [],...
    'currentNeutralMatWavletpower');

    [currentNeutralMatWavletpowerBaselined] =...
    bslcorrWAMat_percent(currentNeutralMatWavletpower, baselineSamplePoints);

    waveletPowerStructNeutral(i).waveletPower = currentNeutralMatWavletpower;

    waveletPowerStructNeutral(i).baselinedWaveletPower = currentNeutralMatWavletpowerBaselined;

    neutralMat = cat(3, pleasantMat, currentNeutralMat);
    i
    finalSize = finalSize + size(currentNeutralMat,3)

end

unpleasantMat = [];
finalSize = 0;
waveletPowerStructUnpleasant = struct;

for i = 1:length(unpleasantAppFilePaths)

    currentUnpleasantMat = app2mat(unpleasantAppFilePaths{i}, 0);

    [currentUnpleasantMatWavletpower] = wavelet_app_mat(currentUnpleasantMat, ...
    sampleRateHz, finalFrequencyIndices(1), finalFrequencyIndices(end), numberOfFrequenciesToSkip, [],...
    'currentUnpleasantMatWavletpower');

    [currentUnpleasantMatWavletpowerBaselined] =...
    bslcorrWAMat_percent(currentUnpleasantMatWavletpower, baselineSamplePoints);

    waveletPowerStructUnpleasant(i).waveletPower = currentUnpleasantMatWavletpower;

    waveletPowerStructUnpleasant(i).baselinedWaveletPower = currentUnpleasantMatWavletpowerBaselined;

    unpleasantMat = cat(3, unpleasantMat, currentUnpleasantMat);
    i
    finalSize = finalSize + size(currentUnpleasantMat,3)

end

occipital_chans = [56, % POz
                     61,  % O9/I1
                     59,  % O1
                     62,  % OI1h
                     63,  % OZ
                     64,  % IZ
                     125, % O2
                     127, % OI2h
                     128]; % O10/I2

% avgWaveletStruct = struct;


% avgWaveletStruct.pleasant = wavelet_app_mat(pleasantMat, ...
%     sampleRateHz, finalFrequencyIndices(1), finalFrequencyIndices(end), numberOfFrequenciesToSkip, [],...
%     'pleasantMatWavletpower');
% 
% avgWaveletStruct.neutral = wavelet_app_mat(neutralMat, ...
%     sampleRateHz, finalFrequencyIndices(1), finalFrequencyIndices(end), numberOfFrequenciesToSkip, [],...
%     'neutralMatWavletpower');
% 
% avgWaveletStruct.unpleasant = wavelet_app_mat(unpleasantMat, ...
%     sampleRateHz, finalFrequencyIndices(1), finalFrequencyIndices(end), numberOfFrequenciesToSkip, [],...
%     'unpleasantMatWavletpower');

sumMatrixPleasant = zeros(128,6145,65);
sumMatrixPleasantBaseline = zeros(128,6145,65);

sumMatrixNeutral = zeros(128,6145,65);
sumMatrixNeutralBaseline = zeros(128,6145,65);

sumMatrixUnpleasant = zeros(128,6145,65);
sumMatrixUnpleasantBaseline = zeros(128,6145,65);

for i = 1:45

    sumMatrixPleasant = sumMatrixPleasant + waveletPowerStructPleasant(i).waveletPower;
    sumMatrixPleasantBaseline = sumMatrixPleasantBaseline + waveletPowerStructPleasant(i).baselinedWaveletPower;

    sumMatrixNeutral = sumMatrixNeutral + waveletPowerStructNeutral(i).waveletPower;
    sumMatrixNeutralBaseline = sumMatrixNeutralBaseline + waveletPowerStructNeutral(i).baselinedWaveletPower;

    sumMatrixUnpleasant = sumMatrixUnpleasant + waveletPowerStructUnpleasant(i).waveletPower;
    sumMatrixUnpleasantBaseline = sumMatrixUnpleasantBaseline + waveletPowerStructUnpleasant(i).baselinedWaveletPower;

end

avgSumMatrixPleasant = sumMatrixPleasant / 45;
avgSumMatrixPleasantBaseline = sumMatrixPleasantBaseline / 45;

avgSumMatrixNeutral = sumMatrixNeutral/ 45;
avgSumMatrixNeutralBaseline = sumMatrixNeutralBaseline/ 45;

avgSumMatrixUnpleasant = sumMatrixUnpleasant / 45;
avgSumMatrixUnpleasantBaseline = sumMatrixUnpleasantBaseline / 45;

figure(1)
contourf(timeAxisMs, condensedFrequencyAxis, squeeze(mean(avgSumMatrixPleasant(occipital_chans,:,:), 1))'), colorbar
caxis([0 3.5])
set(gca , 'FontName', 'Arial',...
                'FontSize', 35,...
                'YTick', [2.5:2.5:40], ...
                'XTick', [-1000:1000:9000],...
                'XTickLabel', [-1:1:9])
figure(2)
contourf(timeAxisMs, condensedFrequencyAxis, squeeze(mean(avgSumMatrixPleasantBaseline(occipital_chans,:,:), 1))'), colorbar
caxis([-30 20])
set(gca , 'FontName', 'Arial',...
                'FontSize', 35,...
                'YTick', [2.5:2.5:40], ...
                'XTick', [-1000:1000:9000],...
                'XTickLabel', [-1:1:9])

figure(3)
contourf(timeAxisMs, condensedFrequencyAxis, squeeze(mean(avgSumMatrixNeutral(occipital_chans,:,:), 1))'), colorbar
caxis([0 3.5])
set(gca , 'FontName', 'Arial',...
                'FontSize', 35,...
                'YTick', [2.5:2.5:40], ...
                'XTick', [-1000:1000:9000],...
                'XTickLabel', [-1:1:9])
figure(4)
contourf(timeAxisMs, condensedFrequencyAxis, squeeze(mean(avgSumMatrixNeutralBaseline(occipital_chans,:,:), 1))'), colorbar
caxis([-30 20])
set(gca , 'FontName', 'Arial',...
                'FontSize', 35,...
                'YTick', [2.5:2.5:40], ...
                'XTick', [-1000:1000:9000],...
                'XTickLabel', [-1:1:9])

figure(5)
contourf(timeAxisMs, condensedFrequencyAxis, squeeze(mean(avgSumMatrixUnpleasant(occipital_chans,:,:), 1))'), colorbar
caxis([0 3.5])
set(gca , 'FontName', 'Arial',...
                'FontSize', 35,...
                'YTick', [2.5:2.5:40], ...
                'XTick', [-1000:1000:9000],...
                'XTickLabel', [-1:1:9])
figure(6)
contourf(timeAxisMs, condensedFrequencyAxis, squeeze(mean(avgSumMatrixUnpleasantBaseline(occipital_chans,:,:), 1))'), colorbar
caxis([-30 20])
set(gca , 'FontName', 'Arial',...
                'FontSize', 35,...
                'YTick', [2.5:2.5:40], ...
                'XTick', [-1000:1000:9000],...
                'XTickLabel', [-1:1:9])


figure(1)
contourf(timeAxisMs, condensedFrequencyAxis, squeeze(mean(currentPleasantMatWavletpower(occipital_chans,:,:), 1))'), colorbar



[currentPleasantMatWavletpowerBaselined] =...
    bslcorrWAMat_percent(currentPleasantMatWavletpower, baselineSamplePoints);

figure(2)
contourf(timeAxisMs, condensedFrequencyAxis, squeeze(mean(currentPleasantMatWavletpowerBaselined(occipital_chans,:,:), 1))'), colorbar



OzChannel = 137;
PzChannel = 126;

figure(1)
contourf(timeAxisMs, condensedFrequencyAxis, squeeze(emo002run1WavletpowerPleasant(PzChannel,:,:))'), colorbar




frquencyRangeOfInterestHz = [3 35];

[differenceFromTargetStartFrequencyHz , frequencyAxisIndexStart] = ...
    min(abs(frequencyAxisTotal - frquencyRangeOfInterestHz(1)));

[differenceFromTargetStopFrequencyHz , frequencyAxisIndexStop] = ...
    min(abs(frequencyAxisTotal - frquencyRangeOfInterestHz(2)));

numberOfFrequenciesToSkip = 10;

finalFrequencyIndices = ...
    frequencyAxisIndexStart : numberOfFrequenciesToSkip : frequencyAxisIndexStop;

condensedFrequencyAxis = frequencyAxisTotal(finalFrequencyIndices);

baselineSamplePoints = [500:1500];

% example wavelet
% emo002run1Pleasant = '/home/andrewf/Research_data/EEG/EmoClips/raw_data/emo002/EEG/run1/emo002_20240306_032232.fl35h0.E1.app1';
% emo002run1Neutral = '/home/andrewf/Research_data/EEG/EmoClips/raw_data/emo002/EEG/run1/emo002_20240306_032232.fl35h0.E1.app2';
% emo002run1Unpleasant = '/home/andrewf/Research_data/EEG/EmoClips/raw_data/emo002/EEG/run1/emo002_20240306_032232.fl35h0.E1.app3';
emo002run1Pleasant = '/Users/csea/Documents/emo002_20240306_032232.fl35h0.E1.app1';
emo002run1Neutral = '/Users/csea/Documents/emo002_20240306_032232.fl35h0.E1.app2';
emo002run1Unpleasant = '/Users/csea/Documents/emo002_20240306_032232.fl35h0.E1.app3';

emo002run1PleasantMat = app2mat(emo002run1Pleasant, 0);
emo002run1NeutralMat = app2mat(emo002run1Neutral, 0);
emo002run1UnpleasantMat = app2mat(emo002run1Unpleasant, 0);


[emo002run1WavletpowerPleasant] = wavelet_app_mat(emo002run1PleasantMat, ...
    sampleRateHz, finalFrequencyIndices(1), finalFrequencyIndices(end), numberOfFrequenciesToSkip, [],...
    'emo002run1Pleasant');
[emo002run1WavletpowerNeutral] = wavelet_app_mat(emo002run1NeutralMat, ...
    sampleRateHz, finalFrequencyIndices(1), finalFrequencyIndices(end), numberOfFrequenciesToSkip, [],...
    'emo002run1Neutral');
[emo002run1WavletpowerUnpleasant] = wavelet_app_mat(emo002run1UnpleasantMat, ...
    sampleRateHz, finalFrequencyIndices(1), finalFrequencyIndices(end), numberOfFrequenciesToSkip, [],...
    'emo002run1Unpleasant');


[emo002run1WavletpowerPleasantBaselined] =...
    bslcorrWAMat_percent(emo002run1WavletpowerPleasant, baselineSamplePoints);
[emo002run1WavletpowerNeutralBaselined] =...
    bslcorrWAMat_percent(emo002run1WavletpowerNeutral, baselineSamplePoints);
[emo002run1WavletpowerUnpleasantBaselined] =...
    bslcorrWAMat_percent(emo002run1WavletpowerUnpleasant, baselineSamplePoints);

OzChannel = 137;
PzChannel = 126;

figure(1)
contourf(timeAxisMs, condensedFrequencyAxis, squeeze(emo002run1WavletpowerPleasant(PzChannel,:,:))'), colorbar
caxis([.8 4])
figure(2)
contourf(timeAxisMs, condensedFrequencyAxis, squeeze(emo002run1WavletpowerNeutral(PzChannel,:,:))'), colorbar
caxis([.8 4])
figure(3)
contourf(timeAxisMs, condensedFrequencyAxis, squeeze(emo002run1WavletpowerUnpleasant(PzChannel,:,:))'), colorbar
caxis([.8 4])

figure(4)
contourf(timeAxisMs, condensedFrequencyAxis, squeeze(emo002run1WavletpowerPleasantBaselined(PzChannel,:,:))'), colorbar
caxis([-50 35])
figure(5)
contourf(timeAxisMs, condensedFrequencyAxis, squeeze(emo002run1WavletpowerNeutralBaselined(PzChannel,:,:))'), colorbar
caxis([-50 35])
figure(6)
contourf(timeAxisMs, condensedFrequencyAxis, squeeze(emo002run1WavletpowerUnpleasantBaselined(PzChannel,:,:))'), colorbar
caxis([-50 35])

% Save the topography, matrix is channelsXsamplePointsXfrequency with each
% cell as an amplitude.

startFrequencyToAverageHz = 6.5;
stopFrequencyToAverageHz = 9;

[differenceFromTargetStartAverageFrequencyHz , frequencyAxisAverageIndexStart] = ...
    min(abs(condensedFrequencyAxis - startFrequencyToAverageHz));

[differenceFromTargetStopAverageFrequencyHz , frequencyAxisAverageIndexStop] = ...
    min(abs(condensedFrequencyAxis - stopFrequencyToAverageHz));

%SaveAvgFile(FilePath,AvgMat,NTrialAvgVec,StdChanTimeMat, ...
%     SampRate,MedMedRawVec,MedMedAvgVec,EegMegStatus,NChanExtra,TrigPoint,HybridFactor,...
%     HybridDataCell,DataTypeVal,EffectDf,ErrorDf)
SaveAvgFile('/Users/csea/Documents/emo002run1WapowerPleasant.at', ...
    squeeze((mean(emo002run1WavletpowerPleasant(:,:,frequencyAxisAverageIndexStart:frequencyAxisAverageIndexStop),3))), ...
    [],[],1000,[],[],1,[],2001)

SaveAvgFile('/Users/csea/Documents/emo002run1WapowerNeutral.at', ...
    squeeze((mean(emo002run1WavletpowerNeutral(:,:,frequencyAxisAverageIndexStart:frequencyAxisAverageIndexStop),3))), ...
    [],[],1000,[],[],1,[],2001)

SaveAvgFile('/Users/csea/Documents/emo002run1WapowerUnpleasant.at', ...
    squeeze((mean(emo002run1WavletpowerUnpleasant(:,:,frequencyAxisAverageIndexStart:frequencyAxisAverageIndexStop),3))), ...
    [],[],1000,[],[],1,[],2001)




% example of using batchfile
% AppBatchFilePath = '/home/andrewf/Research_data/EEG/EmoClips/batch_files/valence_app_files.txt';
% 
% fileID = fopen(AppBatchFilePath, 'r');
% 
% % Check if the file was opened successfully
% if fileID == -1
%     error('Failed to open the file: %s', AppBatchFilePath);
% end
% 
% % Read all lines at once
% C = textscan(fileID, '%s');
% fclose(fileID);
% 
% % Extract the lines from the cell array C
% filePaths = C{1};
% 
% % Process each path
% dataStruct = struct();
% for i = 1:length(filePaths)
%     line = filePaths{i};
%     [~, name, ext] = fileparts(line);
% 
%     originalName = ['E' name ext]
% 
%     modifiedName = strrep(originalName, '.', '_');
% 
%     CurrentMat = app2mat(line, 0);
%     
%     dataStruct.(modifiedName) = wavelet_app_mat(CurrentMat, 1000, 40, 400, 20, [], modifiedName);
% end

