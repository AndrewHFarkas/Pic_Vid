% run_emegs_28
by_scene_at_path = '/home/andrewf/Research_data/EEG/Pic_Vid/average_files/videos/by_scene_3_1hz_high_pass';
 
fileInfo = dir(fullfile(by_scene_at_path, '*.ar'));  % Get all .app* files

arFilePaths = fullfile(by_scene_at_path, {fileInfo.name});


resampleRateHz = 525;

ssvepOccipitalChannels = [56 61 59 62 63 64 125 127 128];

ssVEPWindow1To9sec = [1538:5633];

PerTrialResultStruc = struct();

SWPerTrialResultStruc = struct();

for i = 1:length(arFilePaths)

    current_data = ReadAvgFile(arFilePaths{i});

    [rawAmp, rawFreqs, rawFFTcomp] = ...
        freqtag_FFT3D(current_data( ...
        ssvepOccipitalChannels,ssVEPWindow1To9sec,:), 512);


    PerTrialResultStruc(i).filename = arFilePaths{i};

    PerTrialResultStruc(i).rawAmp = rawAmp;

    PerTrialResultStruc(i).rawFreqs = rawFreqs;


    [trialamp7_5Hz, winmat3d7_5Hz, phasestabmat7_5Hz, trialSNR7_5Hz] = ...
        freqtag_slidewin(current_data(:,:,:), ...
        0, [], ssVEPWindow1To9sec, ...
        7.5, resampleRateHz, 512, 'whatever.txt');

    SWPerTrialResultStruc(i).filename = arFilePaths{i};

    SWPerTrialResultStruc(i).trialamp7_5Hz = trialamp7_5Hz;

    SWPerTrialResultStruc(i).winmat3d7_5Hz = winmat3d7_5Hz;

    SWPerTrialResultStruc(i).phasestabmat7_5Hz = phasestabmat7_5Hz;

    SWPerTrialResultStruc(i).trialSNR7_5Hz = trialSNR7_5Hz;

end

save('/home/andrewf/Research_data/EEG/Pic_Vid/misc/high_pass_3_1hz_video_single_trial_fft.mat', 'SWPerTrialResultStruc', 'PerTrialResultStruc')


% cat app below
app_path = '/home/andrewf/Research_data/EEG/Pic_Vid/video_by_cat_app';

fileInfo = dir(fullfile(app_path, '*app*'));  % Get all .app* files

% Apply regex filtering to only keep files that end with .app1, .app2, or .app3
pattern = '\.app[123]$';
fileInfo = fileInfo(~cellfun(@isempty, regexp({fileInfo.name}, pattern, 'once')));

resampleRateHz = 525;

% Get the full file paths
appFilePaths = fullfile(app_path, {fileInfo.name});

ssVEPPower = zeros(length(appFilePaths),1);


NearssVEPPower = zeros(length(appFilePaths),1);

SWssVEPPower = zeros(length(appFilePaths),1);


SWNearssVEPPower = zeros(length(appFilePaths),1);

full_fft_results = struct();


full_SW_fft_results = struct();

for i = 1:length(appFilePaths)

    clear currentParCateMat

%     [currentParCateMatResampledPath]=ResampleAppFiles(appFilePaths{i},1,resampleRateHz,0,[]);


    currentParCateMat = app2mat(appFilePaths{i},[]);

    ssvepOccipitalChannels = [56 61 59 62 63 64 125 127 128];

    ssVEPWindow1To9sec = [1538:5633]; % one point later to get perfect step at ssvep




    [rawAmp, rawFreqs, rawFFTcomp] = ...
        freqtag_FFT3D(currentParCateMat( ...
        ssvepOccipitalChannels,ssVEPWindow1To9sec,:), 512);

    full_fft_results(i).filename = appFilePaths{i};

    full_fft_results(i).rawAmp = rawAmp;

    full_fft_results(i).rawFreqs = rawFreqs;

    full_fft_results(i).rawFFTcomp = rawFFTcomp;


   [~, ssVEPFreqIndex] = min(abs(rawFreqs - 7.5));

    ssVEPPower(i) = mean(rawAmp(:, ssVEPFreqIndex));

    NearssVEPPower(i) = (mean(rawAmp(:, ssVEPFreqIndex-1)) +mean(rawAmp(:, ssVEPFreqIndex+1)) )/2;

    [trialamp7_5Hz, winmat3d7_5Hz, phasestabmat7_5Hz, trialSNR7_5Hz] = ...
            freqtag_slidewin(currentParCateMat(:,:,:), ...
            0, [], ssVEPWindow1To9sec, ...
            7.5, resampleRateHz, 512, 'whatever.txt');



    [SWrawAmp, SWrawFreqs, SWrawFFTcomp] = ...
        freqtag_FFT3D(winmat3d7_5Hz( ...
        ssvepOccipitalChannels,:,:), resampleRateHz);


    full_SW_fft_results(i).filename = appFilePaths{i};

    full_SW_fft_results(i).SWrawAmp = SWrawAmp;

    full_SW_fft_results(i).SWrawFreqs = SWrawFreqs;

    full_SW_fft_results(i).SWrawFFTcomp = SWrawFFTcomp;

    full_SW_fft_results(i).trialamp7_5Hz = trialamp7_5Hz;

    full_SW_fft_results(i).winmat3d7_5Hz = winmat3d7_5Hz;

    full_SW_fft_results(i).phasestabmat7_5Hz = phasestabmat7_5Hz;

    full_SW_fft_results(i).trialSNR7_5Hz = trialSNR7_5Hz;



% 
%    [~, SWssVEPFreqIndex] = min(abs(SWrawFreqs - 7.5));
% 
%     SWssVEPPower(i) = mean(SWrawAmp(:, SWssVEPFreqIndex));
% 
%     SWNearssVEPPower(i) = (mean(SWrawAmp(:, SWssVEPFreqIndex-1)) +mean(SWrawAmp(:, SWssVEPFreqIndex+1)) )/2;

end

% 
figure(1), plot(full_SW_fft_results(2).SWrawFreqs(1:20), mean(full_SW_fft_results(2).SWrawAmp(:, 1:20),1))   %plot the all sensors 

ax = gca;         %editing the plot
ax.FontSize = 18; %set the font size
ax.Box = 'off';   %remove the box around the plot
xlabel('Frequency (Hz)'), ylabel('Amplitude (μV)') %label the axis

figure(2), plot(full_SW_fft_results(1).trialamp7_5Hz(ssvepOccipitalChannels,:))   

