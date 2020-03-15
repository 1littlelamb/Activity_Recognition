# Initial Plotting of the Data to get a better understanding of what is happening
setwd('C:/Users/felix/Documents/UCI Bullshit Forms/CLASSES/MAE 195 (Machine Learning)/Actitivty_Recognition/code/Activity_Recognition/')

# Loading in custom font from local machine
extrafont::loadfonts()
extrafont::font_import(path = "fonts/", pattern = "lmroman*")
windowsFonts(`LM Roman 10` = windowsFont('LM Roman 8'))

# Loading in necessary libraries
pacman::p_load(rio, dplyr, tidyr, ggplot2, ggthemes, corrplot,
               corrgram, gridExtra, egg, Cairo, knitr, grid, lattice)

source('functions/complex_magnitude.R')
opts_chunk$set(dev = 'CairoPDF')

# Loading in the data, if necessary
df <- readRDS('./../../rds_data/wisdm_dataset_list.rds')

data <- readRDS('./../../rds_data/wisdm_dataset_df.rds')

# Creating the color palette for coloring the lines
master_color <- "#ff5828"

my_colors <- c('#942e2e','#94462e','#946b2e','#94932e','#7c942e','#34942e','#2e9450','#2e948a','#2e7094',
               '#2e4694','#442e94','#612e94','#852e94','#942e70','#942e46','#942e2e','#2c8130','#2c3081')

user_gradient <- c("#ff1919", "#ff682b", "#ff201a", "#ff702d", "#ff281c", "#ff782f", "#ff301e",
                   "#ff8031", "#ff3820", "#ff8833", "#ff4022", "#ff9035", "#ff4824", "#ff9837",
                   "#ff5026", "#ffa039", "#ff5828", "#ffa83b", "#ff602a", "#ffb03d")

activity_dict <- data.frame(description = c(
  "Walking",
  "Jogging",
  "Stairs",
  "Sitting",
  "Standing",
  "Typing",
  "Brushing Teeth",
  "Eating Soup",
  "Eating Chips",
  "Eating Pasta",
  "Drinking from Cup",
  "Eating Sandwich",
  "Kicking (Soccer Ball)",
  "Playing Catchw/Tennis Ball",
  "Dribblinlg (Basketball)",
  "Writing",
  "Clapping",
  "Folding Clothes"),
  alphabet = LETTERS[1:19][-14])


# Task Selection
selected_activity <- 'G'
activity_description <- as.character(filter(activity_dict, alphabet == selected_activity)$description)

# Test Subject
subject <- 'p43'

# Initial Conditions
sample_generator <- function(start_time, interval){
  freq <- 20 # Hz
  dt <- 1/freq
  n <- interval/dt 
  start_index <- start_time/dt
  end_index <- start_index + n
  indices <- start_index:end_index
  return(list(indices = indices,
              interval = interval,
              start_index = start_index,
              end_index = end_index,
              n = n,
              delT = dt))
}

sample <- sample_generator(10, 10)

### 1. How different users logged their data via sys_time #########################################

pl1 <- ggplot(df$phone_accel[[subject]], aes(x=1:length(sys_time), y=sys_time)) + 
  geom_line(color = '#183e2c') + 
  geom_line(data = df$phone_accel[['p32']], aes(x=1:length(sys_time), y=sys_time), color = my_colors[1]) + 
  geom_line(data = df$phone_accel[['p22']], aes(x=1:length(sys_time), y=sys_time), color = my_colors[2]) 
print(pl1)
# Are watch and phone systime synced?
pl1.1 <- df$phone_accel[[subject]] %>% filter(activity == selected_activity)
print(ggplot(pl1.1[sample$indices,]) + geom_line(aes(x = sys_time, y = x_axis)))


### 2. How long each activity is for a user #######################################################

# This has been made obsolete since everything has been trimmed to be the same length
# It was helpful at first in noticing the inconsistent activity duration
pl2 <- ggplot(df$phone_accel$p43, aes(x=factor(activity))) + 
  geom_bar(aes(fill = factor(activity)),
           show.legend = FALSE)

print(pl2)

### 3. The x accel density for all tasks and one user ############################################

pl3_data <- data %>% filter(User == 1626) %>% dplyr::select(Activity, PAX)
pl3 <- ggplot(pl3_data) + 
         geom_density(aes(PAX, fill = Activity, color = Activity),
                      alpha = 0.8) + 
         scale_x_continuous(limits = c(1,7)) +
         theme_fivethirtyeight(base_family = 'LM Roman 10') + 
         theme(axis.title = element_text(), axis.title.y = element_blank(),
               axis.text.y = element_blank(), legend.position = 'none') +
         labs(title = 'One User | All Activities',
              subtitle = 'Phone Acceleration in the x-Axis') +
         xlab('Acceleration') 
print(pl3)
ggsave('./../../images/all_actvts.pdf', pl3, device = cairo_pdf)

### 4. The xyz accel vs sys_time of a given task #################################################

pl4_data <- data %>% filter(Activity == 'E') %>% filter(User < 1620) %>% dplyr::select(User, PAX)
pl4 <- ggplot(pl4_data) + 
       geom_density(aes(PAX, fill = factor(User), color = factor(User)),
                    alpha = 0.8) +
       scale_x_continuous(limits = c(-10,6)) +
       scale_fill_manual(values = user_gradient) + 
       scale_color_manual(values = user_gradient) +
       theme_fivethirtyeight(base_family = 'LM Roman 10') + 
       theme(axis.title = element_text(), axis.title.y = element_blank(),
             axis.text.y = element_blank(), legend.position = 'none') +
       labs(title = 'Twenty Different Users | Activity: Standing',
            subtitle = 'Phone Acceleration in the x-Axis') +
       xlab('Acceleration') 
print(pl4)
ggsave('./../../images/user_standing.pdf', pl4, device = cairo_pdf)

### 5. The Time Domain for xyz accel of a given task #############################################
pl5_data <- data %>% filter(Activity == 'G') %>% filter(User == 1628) %>% 
  dplyr::select(PAX) %>% slice(1600:1800)

pl5 <- ggplot(pl5_data) + 
  geom_line(aes(seq(0,10, 1/20), PAX), color = master_color) +
  theme_fivethirtyeight(base_family = 'LM Roman 10') +
  theme(axis.title = element_text(), legend.position = 'none') +
  labs(title = 'Digital Signal') +
  xlab('Time (sec)') + ylab('Acceleration') 

print(pl5)


### 6. The Frequency Domain of all three axes ####################################################

pl6_fft <- data %>% filter(Activity == 'G') %>% filter(User == 1628) %>% 
           dplyr::select(PAX) %>% slice(1600:1800) %>%
           as.matrix() %>% fft() %>% sapply(function(x) complex_magnitude(m = x, n = sample$n)) %>% 
           as.data.frame() %>% slice(2:101) %>% 
           'names<-'('freq')

pl6 <- ggplot(pl6_fft) + 
  geom_bar(aes(seq(1,100), freq), fill = master_color, stat = 'identity', width = 1) +
  theme_fivethirtyeight(base_family = 'LM Roman 10') +
  theme(axis.title = element_text(), axis.text.y = element_blank(), legend.position = 'none') +
  labs(title = 'Fourier Transform') +
  xlab('Frequency (Hz)') + ylab('Intensity') 

print(pl6)
ggplot2::ggsave('./../../images/fft_toothbrush.pdf', pl6, device = cairo_pdf)

### 7. Combining the Frequency and Time Domain ###################################################

grid1 <- grid.arrange(pl5, pl6, nrow = 1)
ggsave('./../../images/fft_grid.pdf', grid1, device = cairo_pdf)

### 2D Density of Axis State Space ###############################################################

pl7 <- ggplot(subset(df$phone_accel$p1, activity == selected_activity),
             aes(x=x_axis, y=z_axis)) + 
  
  geom_density2d(color = my_colors[1]) + 
  
  geom_density2d(data = subset(df$phone_accel$p1, activity == selected_activity),
            aes(x=y_axis, y=x_axis),
            color = my_colors[1]) +
  
  geom_density2d(data = subset(df$phone_accel$p1, activity == selected_activity),
            aes(x=y_axis, y=z_axis),
            color = my_colors[17]) 

print(pl7)

# 3d Plot ###################### NOT WORKING
# task_A <- subset(df$phone_accel$p1, activity == selected_activity)
# plot_ly(x=task_A$x_axis, y=task_A$y_axis, z=task_A$z_axis,
#         type = 'scatter3d',
#         mode = 'markers',
#         color = task_A$z_axis) 
### A nice fourier transform ############
df2 <- import('loading_wisdm_df.R')

q <- data %>% filter(Activity == 'G') %>% slice(20000:20200)
f <- fft(as.matrix(dplyr::select(q, -Activity, -User, -Time, PAX)))
plot(seq(1,200,1),complex_magnitude(f[-1,1]),'h')

