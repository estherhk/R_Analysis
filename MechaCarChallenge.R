mecha_table <- read.csv(file='MechaCar_mpg.csv', sep=",", header = T) #import table

#MPG Regression
lm(mpg ~ vehicle.length + vehicle.weight+ spoiler.angle + ground.clearance + AWD, data = mecha_table) #create multiple linear regression statement
lm(mpg ~ vehicle.length, data=mecha_table) #generate single linear regression for vehicle length
lm(mpg ~ ground.clearance, data=mecha_table) #generate single linear regression for ground clearance

summary (lm(mpg ~ vehicle.length + vehicle.weight+ spoiler.angle + ground.clearance + AWD, data = mecha_table)) #generate a summary statement
summary (lm(mpg ~ vehicle.length, data=mecha_table)) #generate summary stats for vehicle length
summary (lm(mpg ~ ground.clearance, data=mecha_table)) #generate summary stats for ground clearance

#Suspension Coil Summary
suspension_table <-read.csv(file='Suspension_Coil.csv',sep=',', header = T)
suspension_table %>%
  group_by(Manufacturing_Lot)%>%
  summarise(PSI_mean=mean(PSI), PSI_sd = sd(PSI), PSI_median = median(PSI), PSI_variance = var(PSI))

sample_table <- suspension_table %>% sample_n(50) #generate 50 randomly sampled
sample_table2 <- suspension_table %>% sample_n(50) #generate another 50 randomly sampled data

t.test(sample_table$PSI, sample_table2$PSI) #compare means
       
#Suspension Coil T-Test
t.test(subset(suspension_table, Manufacturing_Lot=="Lot1")$PSI, mu = 1500)
t.test(subset(suspension_table, Manufacturing_Lot=="Lot2")$PSI, mu = 1500)
t.test(subset(suspension_table, Manufacturing_Lot=="Lot3")$PSI, mu = 1500)

#Own Study, Horse Power vs MPG
lm(mpg ~ hp + cyl + disp + drat+ wt + qsec + vs + am + gear + carb, data=mtcars) #generate multiple linear regression model
lm(mpg ~ hp,data=mtcars) #generate single linear regression model

summary_table_mtcars<-summary(lm(mpg ~ hp + cyl + disp + drat+ wt + qsec + vs + am + gear + carb, data=mtcars)) #generate multiple linear regression model
summary_table_mtcars
summary(lm(mpg ~ hp,data=mtcars)) #generate single linear regression model

summary(lm(mpg ~ hp + cyl + disp + drat+ wt + qsec + vs + am + gear + carb, data=mtcars)) #generate multiple linear regression model
mtcars %>%
  summarise(hp_mean= mean(hp), hp_sd = sd(hp), hp_median = median(hp), hp_variance = var(hp))
sample_table_mtcars <- mtcars %>% sample_n(16) #generate 16 randomly sampled data points
t.test(sample_table_mtcars$hp, mu = 146)
