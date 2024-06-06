# Here is data and modeling results from Farkas et al., 2024 
# To load the data, you have to load the pic_vid_stan_df.RData file.
# To see how the data was arrived at, you can look at the github repository & paper.
load("pic_vid_stan_df.RData")

# This is the full dataset used for all analyses.
# `par` is participant ID number
# `age` is the participant's age
# `sex` is the participant's sex, which could have been left blank or filled in
# `race.ethnicity` is the participant's self-reported race / ethnicity
# `vids_first_is_one` states if the participant saw the video or picture series first
# `type` is if the trial was a picture (1) or a video (2)
# `cate` is the predetermined valence category pleasant (1), neutral (2), unpleasant (3)
# `stim` is the stimuli id number (1 to 90)
# `amp` is the amplitude of the scene-LPP or video-ssVEP depending on `type`
# `stim_name` is the name of stimuli (out of 90)
# `valence` is the self-assessment manikin pleasantness ratings (1 completely unplesant to 9 completely pleasant)
# `arousal` is the self-assessment manikin arousal ratings (1 unarousing to 9 arousing)
pic_vid_stan_df

# Following best Bayesian modeling practices, Models were built up from the most
# simple to most complex. The final two models used for the paper were model 11 
# (called model 2 in the paper) and 12 (paper model 1). These models had the 
# highest cross-validation and interpretability. They can be loaded from from 
# this from paper_models.RData. Other models can loaded from all_models.RData. 
load("paper_models.RData")

# If you want to work with models, you can look into how cmdstanr fit models work.
# This is an example of a fit model (model011_lpp_fit).
# For quick look at the posteriors each fit model has a summary.
# this describes the distribution of posterior samples and how well they were
# sampled.
model012_lpp_fit_summary
