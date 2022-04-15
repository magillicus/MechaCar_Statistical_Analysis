library(dplyr)

# Read in the csv file
mechacar_df <- read.csv(file='Resources/MechaCar_mpg.csv', check.names=F, stringAsFactors=F)

# Linear regression
mechacar_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mechacar_df)

# Determine p-value and r-squared value for regression
summary(mechacar_lm)

# Read in the csv file
suspension_coil_df <- read.csv(file='Resources/Suspension_Coil.csv', check.names=F, stringsAsFactors=F)

# Create total summary
total_summary <- suspension_coil_df %>% 
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups='keep')

# Create lot summary
lot_summary <- suspension_coil_df %>% group_by(Manufacturing_Lot) %>% 
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups='keep')

# Perform t-test 
t.test(suspension_coil_df$PSI,mu = 1500)

# Test Lot 1
t.test(subset(suspension_coil_df,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)

# Test Lot 2
t.test(subset(suspension_coil_df,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)

# Test Lot 3
t.test(subset(suspension_coil_df,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)