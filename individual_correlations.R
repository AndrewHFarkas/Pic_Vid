# Define mean and standard deviation
mean_value <- 0.29852
sd_value <- 0.04886


# Plot the normal distribution
curve(dnorm(x, mean = mean_value, sd = sd_value),
      from = 0, 
      to = mean_value + 5*sd_value,
      main = "Normal Distribution",
      xlab = "Value",
      ylab = "Density",
      col = "red",
      lwd = 2)


# Define mean and standard deviation
mean_value <- 0.29428083
sd_value <- 0.053144820


# Plot the normal distribution
curve(dnorm(x, mean = mean_value, sd = sd_value),
      from = 0, 
      to = mean_value + 5*sd_value,
      main = "Normal Distribution",
      xlab = "Value",
      ylab = "Density",
      col = "blue",
      lwd = 2, add = T)



# Define mean and standard deviation
mean_value <- 0.012489
sd_value <- 0.001378


# Plot the normal distribution
curve(dnorm(x, mean = mean_value, sd = sd_value),
      from = 0, 
      to = mean_value + 5*sd_value,
      main = "Normal Distribution",
      xlab = "Value",
      ylab = "Density",
      col = "red",
      lwd = 2)

# Define mean and standard deviation
mean_value <- 0.012964506
sd_value <- 0.001916270


# Plot the normal distribution
curve(dnorm(x, mean = mean_value, sd = sd_value),
      from = 0, 
      to = mean_value + 5*sd_value,
      main = "Normal Distribution",
      xlab = "Value",
      ylab = "Density",
      col = "blue",
      lwd = 2, add = T)


library(tidyverse)
lm_hold <- data_for_stan_df %>% 
  filter(#par == par_index,
    type == 1) %>%
  mutate(par = factor(par)) %>% 
  lm(data = .,formula = amp ~ par + arousal - 1)

lm_hold %>% summary()


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
           type == 2) %>% 
    nrow()
  
  lm_hold <- data_for_stan_df %>% 
    filter(#par == par_index,
           type == 2) %>%
    mutate(par = factor(par)) %>% 
    lm(data = .,formula = amp ~ par + arousal - 1)
  
  lm_hold %>% summary()

  p <- data_for_stan_df %>% 
    filter(par == par_index,
           type == 2) %>% 
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


