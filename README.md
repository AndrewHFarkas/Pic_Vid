# The Pic Vid Project

Here are the steps to recreate the analyses from the Pic_Vid study from raw bdf files to final statistics.

The files are numberered to indicate the order in which they would need to be run.

It is assumed you will be using the version of EMEGS set up in Andrew's private repository EMEGS_UGA because he made necessary changes to original EMEGS version 3.2.

# Step 0 - master_batch_file_creator

This is code we used to create batch files different parts of the processing procedure.

# Step 1 - creating_BDF_folders

Each bdf (raw eeg data file) needs to be in its own folder. It is easier if it is in a folder of the correct name.

The file here creates the folders for you.

# Step 2 - PrePro

PrePro is an function from the EMEGS matlab based software package. Typing PrePro into the matlab terminal opens the PrePro Gui in which bdf files can be loaded in. Press the open file button to load in a bdf. 

You can also use a batch file in load in multiple at once. That should be a text file in which each line is file path to a bdf file. This is also the structure for all batch files.

There are two pictures that show what the PrePro gui should look like. One for the pictures and one for the video condition. You'll need to run separate PrePro session for each condition. Match the gui to replicate the final results.

Video conditions were: A_1, B_2, C_1, D_2

Scene conditions were: A_2, B_1, C_2, D_1

# Step 3 - EditAEM 

EditAEM is a function from the EMEGS MATLAB based software package. Typing in EditAEM into the MATLAB terminal opens the EditAEM Gui where MATLAB will filter out trials that were not good. 

Two ways to go through this process. If you press the "EditAEM processed file" from the PrePro menu it will automatically bring up the EditAEM menu. I would advise doing it this way.

Once the EditAEM is up from either clicking "EditAEM processed file" or typing in "EditAEM" into the MATLAB terminal. Match the GUI from the picture "Edit_AEM_3.png". The parameters are the same for Videos and Pictures

Now you can press "Abs MaxStd Grad" button and if you pressed the "EditAEM processed file" from PrePro another window asking if you would like to use the files from the clipboard pop up, select yes. If you closed PrePro and typed EditAEM into the terminal then you will have to press the "User Defined" button and manually select all of the files that end with ".AEM.AR" files. 

Do not use the auto feature, it will not work, you need to drag the slider to .25 for every file.

# Step 4 - adjusting_condition_file

Each particiapnt needs to have a .ses.con file that shows the order of the stimuli. This file copies 'master' condition files and copies them into the correct participants folder to average correctly. 

# Step 5 - EmegsAvg 

EmegsAvg is a function from the EMEGS MATLAB based software package. Typing in "EmegsAvg" will bring up a GUI. From here the first thing that you need to do is to click the "Open file(s)" and manually load in each of the .ses files from each participants folder. Then match the GUI parameters to the "Emegs_Avg_5_Pics" or the "Emegs_Avg_5_Vids" depending on which medium is being analyzed. 

Videos should only averaged by trial and not by category. This is because averaging by trial can be influenced by deconstructive frequency interference as well as the number of trials per condition.

# Step 6 - Move averaged files

Once trials are averaged by scene or by category, they should be moved the appropriate folder. We have made R scripts that move either video average files or scene averaged files to the correct folders.

# Step 7 - Transform by video ssvep to hamp files

A Hilbert transformation, in a sense, works like a sliding window fourier transform. The MATLAB script transforms raw averaged files into hamp files that represent the amplitude of the driven 7.5 Hz frequency.

The resulting hamp files should be moved to the correct folder afterwards.

# Step 8 - Average by scene hamps to by category csv file

The R document for this step loads in the individual hamp files and then averages them down by category. The averages by category by participant are then saved as a csv.

# Step 9 - Create EMEGS compatible files from by participant by category hamp csv

This MATLAB code uses EMEGS functions to create the csv as a series of average files. It will create an average file for each category by each participant.

# Step 10 - Stan model fitting

In the second half of the manuscript, the data will be looked at by-trial via 2 Stan Bayesian multi-level models. Here the data is organized an the models are fit. Currently there is a lot in this script because I tried many different models. Final models will be model003 (multi-level category prediction) and model007 (multi-level arousal rating prediction)

# Step 11 - Manuscript statistics and figures

Here is were most of the final figures and analyses were done.


