# Load Packages ----
libs = c("tidyverse")
lapply(libs, require, character.only = TRUE)
rm(libs)

# Simulation of Landscape Data ----
xrange = 1:50
yrange = 1:50
# Data Structure 
# df = data.frame(x, y, Stimulus)
set.seed(20201021)
# Function to create the dataset
# There is an easier way to do this with the raster package, 
# but I didn't want to deal with the conversions.
step1 = function(x, yrange){
  step2 = function(y, x){
    stim = rnorm(1, mean = 0, sd = 0.5)
    stim = ifelse(stim > 1, 1, stim)
    stim = ifelse(stim < -1, -1, stim)
    output = data.frame(x = x, y = y, Stimulus = stim)
  }
  output = map(yrange, step2, x)
  output = do.call("rbind", output)
}
output = map(xrange, step1, yrange)
output = do.call("rbind", output)

# Simulated Map ----
landscape = ggplot(output) +
  geom_raster(mapping = aes(x = x, y = y, alpha = Stimulus), fill = "darkgreen") + 
  coord_equal() +
  theme_void()
landscape

# Simulated Animal ----
steps = data.frame(x1 = round(runif(n = 1, min = 1, max = 50), digits = 0), 
                   y1 = round(runif(n = 1, min = 1, max = 50), digits = 0))

landscape + geom_point(aes(x = x1, y = y1), color = "red", data = steps)

# Simulate Kernel Map ----
kernel = landscape + geom_point(aes(x = x1, y = y1), shape = "square", size = 29, color = "red", 
                       alpha = 0.10, data = steps) + 
  geom_point(aes(x = x1, y = y1), shape = "square", size = 20, color = "red", 
                       alpha = 0.20, data = steps) + 
  geom_point(aes(x = x1, y = y1), shape = "square", size = 11, color = "red", 
                       alpha = 0.30, data = steps) + 
  geom_point(aes(x = x1, y = y1), color = "black", data = steps) 
kernel

# Simulate Decision ----
decision = function(steps, output){
  x1 = steps$x1
  y1 = steps$y1
  output2 = output %>% filter(x > (x1 - 3)) %>% filter(x < (x1 + 3)) %>%
    filter(y > (y1 - 3)) %>% filter(y < (y1 + 3))
  output2$n = 1:length(output2$x)
  
  #Output looks right, but not sure if this is doing what I want.
  selection = function(N, output2, x1, y1){
    # declare variables
    o2 = output2 %>% filter(n == N)
    n = o2$n[1]
    x2 = o2$x[1]
    y2 = o2$y[1]
    stim = o2$Stimulus[1]
    # calculate simple selection based on stimulus magnitude
    # 1 is original, 2 is new 
    # Opposite Theta ~ y length
    opp1 = abs(y2 - y1)
    # Adjacent Theta ~ x length
    adj1 = abs(x2 - x1)
    # Hypotenuse2 = Hypotenuse 1 * Stimulus
    hyp1 = sqrt(adj1^2 + opp1^2)
    costheta = adj1 / hyp1
    sintheta = opp1 / hyp1
    hyp2 = hyp1 * stim
    x3 = (costheta * hyp2) + x1
    y3 = (sintheta * hyp2) + y1
    
    # output dataframe
    output = data.frame(group = rep(c("No Selection", "Selection"), 2),
                        step = c("Original", "Original", "No Selection", "Selection"),
                        x = c(x1, x1, x2, x3), y = c(y1, y1, y2, y3)) %>%
      mutate(line_group = paste(n, group))
    return(output)
  }
  
  df = map(output2$n, selection, output2, x1, y1)
  df = do.call("rbind", df)
  
  nosel = df %>% filter(group == "No Selection")
  sel = df %>% filter(group == "Selection") 
  
  mn = sel %>% filter(step == "Selection") 
  x2_sel = mean(unique(mn$x), na.rm = T)
  y2_sel = mean(unique(mn$y), na.rm = T)
  
  output2 %>% ggplot() + 
    geom_raster(mapping = aes(x = x, y = y, alpha = Stimulus), fill = "darkgreen") + 
    geom_path(aes(x = x, y = y, group = line_group), color = "black", data = nosel) +
    geom_point(aes(x = x1, y = y1), color = "red", data = steps) +
    coord_equal() +
    theme_void() +
    labs(title = "Possible Routes w/ No Selective Response")
    
  output2 %>% ggplot() + 
    geom_raster(mapping = aes(x = x, y = y, alpha = Stimulus), fill = "darkgreen") + 
    geom_path(aes(x = x, y = y, group = line_group), color = "black", data = sel) +
    geom_point(aes(x = x1, y = y1), color = "red", data = steps) +
    geom_point(aes(x = x2_sel, y = y2_sel), color = "blue", data = steps) +
    coord_equal() +
    theme_void() +
    labs(title = "Possible Routes w/ Selective Response")
  
  steps = data.frame(x = x2_sel, y = y2_sel)
  return(steps)
}
decision(steps, output)











