#Look at correlations for individual participants

data_for_stan_df$par %>% head()

data_list_for_stan_lpp$par %>% head()

data_for_stan_df$par <- data_for_stan_df$par %>% 
  as.factor() %>% 
  as.integer()

data_for_stan_df$cate <- data_for_stan_df$cate %>% 
  as.factor()

for(par_index in c(1:46)){
  
  number_of_trials <- data_for_stan_df %>% 
    filter(par == par_index,
           type == 1) %>% 
    nrow()
  
  p <- data_for_stan_df %>% 
    filter(par == par_index,
           type == 1) %>% 
    ggplot() +
    geom_line(aes(x = arousal, y = amp),
              stat = "smooth", method = "lm") +
    geom_text(aes(x = arousal,
                  y = amp,
                  label = stim_name,
                  color = cate)) +
    ggtitle(paste0("Participant ", par_index,
                   " number of good trials ", number_of_trials)) +
    scale_color_manual(values = c("blue", "green", "red")) +
    theme_classic()
  
  print(p)
  
  Sys.sleep(.25)
  
  readline(prompt="Press [Enter] to continue to the next iteration...")
  
}

