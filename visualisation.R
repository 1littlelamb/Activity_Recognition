# Initial Plotting of the Data to get a better understanding of what is happening

# Loading in necessary libraries
pacman::p_load(rio, dplyr, tidyr, ggplot2, ggthemes, corrplot, corrgram, gridExtra, egg)
source('functions/magnitude.R')

# Loading in the data, if necessary
df <- readRDS('./../../wisdm_dataset_list.rds')

# Creating the color palette for coloring the lines
my_colors <- c('#942e2e','#94462e','#946b2e','#94932e','#7c942e','#34942e','#2e9450','#2e948a','#2e7094',
               '#2e4694','#442e94','#612e94','#852e94','#942e70','#942e46','#942e2e','#2c8130','#2c3081')

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
              n = n))
}


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

pl2 <- ggplot(df$phone_accel$p43, aes(x=factor(activity))) + 
  geom_bar(aes(fill = factor(activity)),
           show.legend = FALSE)

print(pl2)

### 3. The x accel density for all tasks ####################################################################

x.accel_tasks <- function(df, na.rm = TRUE, ...){
  
  # Make the initial plot
  pl3 <- ggplot(filter(df$phone_accel[[subject]], activity == 'A'), aes(x_axis)) + 
    geom_density(color = my_colors[18])
  
  # Add all the additional plots to pl3
  for (i in 2:18) {
    pl3 <- pl3 + geom_density(data = filter(df$phone_accel[[subject]], activity == LETTERS[i]),
                              color = my_colors[i])
  }
  pl3 <- pl3 + scale_x_continuous(limits = c(-8,6))
  
  # Printing the plot, couldn't figure out the return() method
  print(pl3)
}

x.accel_tasks(df)

### 4. The xyz accel vs sys_time of a given task ####################################################

xyz.accel_sys.time <- function(df, na.rm = TRUE, ...){
  
  # Making the graph
  pl4 <- ggplot(subset(df$phone_accel[[subject]], activity == selected_activity),
               aes(z_axis)) + 
    
    geom_density(color = my_colors[5]) + 
    
    geom_density(data = subset(df$phone_accel[[subject]], activity == selected_activity),
                aes(y_axis),
                color = my_colors[1]) + 
    
    geom_density(data = subset(df$phone_accel[[subject]], activity == selected_activity),
                aes(x_axis),
                color = my_colors[12]) +
    theme_fivethirtyeight()
  
  # Printing the graph
  print(pl4)
    
}

xyz.accel_sys.time(df)

### 5. The Time Domain for xyz accel of a given task #########################################################

plot_time_domain <- function(){
  
  plot5_df <- df$watch_gyro[[subject]] %>% 
    filter(activity == selected_activity) %>%
    dplyr::select(sys_time, x_axis, y_axis, z_axis) %>% 
    slice(sample$indices)
  
  colnames(plot5_df) <- c("Sys.Time","x-Axis", "y-Axis", "z-Axis")
  plot5_df <- pivot_longer(plot5_df, c("x-Axis", "y-Axis", "z-Axis"), names_to = "axes", values_to = "value")
  
  pl5 <- ggplot(plot5_df) + 
    geom_line(aes(x = Sys.Time,
                  y = value,
                  group = axes,
                  color = axes),
              size = 1) + 
    facet_wrap( ~ axes, nrow = 3) +
    labs(title = 'Time Domain') +
    theme_fivethirtyeight()
  
  return(pl5)
}

pl5 <- plot_time_domain()


### 6. The Frequency Domain of all three axes ################################################################

frequency_domain <- function(){
  
  # Creating the frequency array
  intervals <- seq(0, sample$interval, by = dt)
  freq_array <- 1:length(intervals)/interval
  
  plot6_df <- df$watch_gyro[[subject]] %>% 
              filter(activity == selected_activity) %>%
              dplyr::select(x_axis, y_axis, z_axis) %>% 
              sapply(fft) %>% 
              as.data.frame() %>% 
              slice(sample$indices)
  
  for (i in 1:dim(plot6_df)[2]){
    plot6_df[,i] <- as.vector(sapply(plot6_df[,i], function(x) magnitude(x, n = n)))
  }  
  
  plot6_df <- cbind.data.frame(plot6_df, freq_array)
  
  names(plot6_df) <- c('x-Axis','y-Axis','z-Axis', 'freq_array')
  plot6_df <- pivot_longer(plot6_df, c('x-Axis','y-Axis','z-Axis'),
                           names_to = 'axes',
                           values_to = 'value')
  
  
  pl6 <- ggplot(plot6_df[1:(length(plot6_df$freq_array)/2),]) + 
                geom_bar(aes(x = freq_array,
                             y = value,
                             group = axes,
                             fill = axes),
                         stat = 'identity',
                         width = 0.02) + 
                facet_wrap( ~ axes, nrow = 3) +
                labs(title = 'Frequency Domain') + 
                theme(legend.position = 'none') +
                theme_fivethirtyeight()

  return(pl6)
}

pl6 <- frequency_domain()

### 7. Combining the Frequency and Time Domain ###############################################################

grid.arrange(pl5, pl6,
             top = paste('Activity -',activity_description),
             nrow = 1)


### 8. Plotting the Fourier Transform of short time interval #########################################

# Still not entirely sure how this works
# I want this line to be smooth kinda like a density plot.

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2]
  plot.data <- as.data.frame(plot.data)
  
  pl <- ggplot(plot.data) + geom_line(aes(x = V1, y = V2), binwidth = 1) 
  print(pl)
  return(plot.data)
}

ft <- fft(df$watch_gyro[[subject]]$x_axis[1:100])
plot.frequency.spectrum(ft)


# Retrieving the desired data


### 2D Density of Axis State Space #########################

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

