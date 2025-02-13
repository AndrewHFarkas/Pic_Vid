% steadyHilbert() is found the the EMEGS library emegs3.2/emegs2dLib
% This function is pasted below commented out

% ar_directory = '/Volumes/startech3TB_bkup/research_data/Pic_Vid/averaged_files/videos/by_scene';

ar_directory = '/home/andrewf/Research_data/EEG/Pic_Vid/average_files/videos/by_category';

cd(ar_directory)

% not looking for .DS_store, so might be a problem
filemat = getfilesindir(ar_directory);
% if .DS store is problem
% filemat = filemat(1:end,:);
% 
% [demodmat, phasemat] = steadyHilbert(filemat, 7.5, [10:50], 10 , 1);

for i = 1:135
    current_data = ReadAvgFile(filemat(i,:));

    current_hilb = freqtag_HILB(current_data, 7.5, 10, 63, 0, 512);
    SaveAvgFile([filemat(i,:) '.hamp.at'] ,current_hilb,[],[], 2001,[],[],[],[],512);
end


%% hilbert transform and display/saving of envelope 
%function[demodmat, phasemat]=steadyHilbert(filemat, targetfreq, bslvec, filterorder, plotflag);
%
%if nargin < 5, plotflag=0; end
%if nargin < 4, filterorder=8; end
%if nargin < 3, bslvec = 50:80; end
%
%for index = 1:size(filemat,1); 
%    
%    if ~isstruct(filemat); 
%        atgPath = deblank(filemat(index,:));
%    else
%        atgPath = filemat(index).name; 
%    end
%	
%    [AvgMat,File,Path,FilePath,NTrialAvgVec,StdChanTimeMat,SampRate] = ReadAvgFile(deblank(atgPath)); 
%    
%    if ~isempty(bslvec)
%    AvgMat = bslcorr(AvgMat, bslvec);
%    else 
%   AvgMat = AvgMat;
%    end
%    
%    % calculate time axis
%	
%	taxis = 0:1000/SampRate:size(AvgMat,2)*1000/SampRate - 1000/SampRate;
%	taxis = taxis/1000; 
%    
%	M2 = 100;
%    
%	squarecos1 = (cos(pi/2:(pi-pi/2)/M2:pi-(pi-pi/2)/M2)).^2;
%	
%	squarecosfunction = [squarecos1 ones(1,length(taxis)-length(squarecos1).*2) fliplr(squarecos1)];
%    
%    mat4mul = repmat(squarecosfunction, size(AvgMat,1), 1); 
%    
%    AvgMat = AvgMat.*mat4mul;
%	
%    %design two filters around targetfreq and filter accordingly
%    uppercutoffHz = targetfreq + .50; 
%    lowercutoffHz = targetfreq - .50; 
%    
%    [Blow,Alow] = butter(filterorder, uppercutoffHz/(SampRate/2)); 
%    
%     [Bhigh,Ahigh] = butter(filterorder, lowercutoffHz/(SampRate/2), 'high'); 
%	
%	% flip AvgMat and filter over all channels
%    AvgMat = AvgMat'; 
%    
%    % filter now
%     lowpasssig = filtfilt(Blow,Alow, AvgMat); 
%     lowhighpasssig = filtfilt(Bhigh, Ahigh, lowpasssig);
%     
%     % calculate hilbert transform
%     tempmat = hilbert(lowhighpasssig); 
%     
%    % flip back to avoid confusion; now channels are rows again
%    tempmat = tempmat'; 
%  
%     % plot if plotflag on 
%     if plotflag
%    figure(3)
%      plot(taxis, lowhighpasssig(:,29)), title(atgPath), hold on, plot(taxis, imag(tempmat(29,:)), 'r'), plot(taxis, abs(tempmat(29,:)), 'k')
%    pause(1)
%     end
%     
%     hold off
%    
% demodmat = (abs(tempmat)); 
% phasemat = atan(real(tempmat) ./ imag(tempmat)); 
%
%	
%	[File,Path,FilePath]=SaveAvgFile([atgPath '.hamp' num2str(round(targetfreq))],demodmat,NTrialAvgVec,StdChanTimeMat, SampRate);
%   % [File,Path,FilePath]=SaveAvgFile([atgPath '.hphas'],phasemat,NTrialAvgVec,StdChanTimeMat, SampRate);
%    %eval(['save ' deblank(atgPath) '.pha phasemat -ascii'])
%end