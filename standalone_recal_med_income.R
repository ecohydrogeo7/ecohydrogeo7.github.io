# Re-calcuate medians and their margin of errors for aggredated ACS data
# recalculate median for income

# library
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)

# input data format
input_df <- read.csv("C:\\Users\\admin\\Documents\\github\\income_sample.csv",sep=",")


#### Median ####

# add income bin
bin_min <-c(2500,10000,15000,20000,25000,30000,35000,40000,45000,50000,60000,75000,100000,125000,150000,200000)
bin_max <-c(9999,14999,19999,24999,29999,34999,39999,44999,49999,59999,74999,99999,124999,149999,199999,1000000)


income <- input_df %>% mutate(cum_var= cumsum(counts),cum_var_pct =(cum_var/max(cum_var))*100)
# add cumulative
income_cum <- mutate(income, bin_min =bin_min,bin_max=bin_max)

# intermediate vars
v <- income_cum$cum_var

mid_pnt <- max(v)/2

after_mid <- if(any(v>mid_pnt)){
  after_mid <-min(v[v>mid_pnt])
  print(after_mid)
}

before_mid <- if(any(v<mid_pnt)){
  before_mid <-max(v[v<mid_pnt])
  print(before_mid)
} 

# final vars
mid_range_index <- which(v == after_mid,arr.ind  = T)
mid_range_lower <- bin_min[mid_range_index]
mid_range_hh <- income_cum$counts[mid_range_index]
mid_range_width <- income_cum$bin_max[mid_range_index] - income_cum$bin_min[mid_range_index]

# results
med_income <- mid_range_lower + ((mid_pnt-before_mid)/mid_range_hh)*mid_range_width

# print result
print(med_income)


#### MOE ####

# define functions

# define SE of a 50% proportion
SE_MOE_50 <- function(B,DF){# B= the total households # DF the design factor from PUMS accuracy statement
  SE_MOE <- DF*sqrt(99/B*50^2) 
  return(SE_MOE)
}

# upper and lower bound for 50%
p_lower_fun <- function(SE){
  p_lower<- 50-SE
  return(p_lower)
}

p_upper_fun <- function(SE){
  p_upper<- 50+SE
  return(p_upper)
}

# bound formula
lower_bound_fun <- function(a1,a2,c1,c2){
  lower_bound<- ((p_lower -c1)/(c2-c1))*(a2-a1) +a1
  return(lower_bound)
}

upper_bound_fun <- function(a1,a2,c1,c2){
  upper_bound<- ((p_upper -c1)/(c2-c1))*(a2-a1) +a1
  return(upper_bound)
}

SE_med_fun <- function(upper_bound,lower_bound){
  SE_med <- 0.5*(upper_bound-lower_bound)
  return(SE_med)
}

# approximation
income <- input_df %>% 
  mutate(cum_var= cumsum(counts),cum_var_pct =(cum_var/max(cum_var))*100)
# add cumulative
income_cum <- mutate(income, bin_min =bin_min,bin_max=bin_max)

# approximate SE of a 50 percent proportion
v <- income_cum$cum_var

SE_MOE_50(max(v),1.5) #  ACS design accuracy 1.5?

p_lower <- p_lower_fun(SE_MOE_50(max(v),1.5))
p_upper <- p_upper_fun(SE_MOE_50(max(v),1.5))

# decide p_lower and p_upper are in same range or not 
pct <- income_cum$cum_var_pct

# find nearest value
p_lower_index <- which(pct == min(pct[which(pct>p_lower)]))
p_upper_index <- which(pct == min(pct[which(pct>p_upper)]))

if(p_lower_index != p_upper_index){
  
  # lower bound
  c1_lo <- if(any(pct<p_lower)){
    c1_lo <-max(pct[pct<p_lower])
    print(c1_lo)
  } 
  
  c2_lo <- if(any(pct>p_lower)){
    c2_lo <-min(pct[pct>p_lower])
    print(c2_lo)
  } 
  
  #a1_lo <- bin_min[which(pct== c1_lo,arr.ind = T)]
  #a2_lo <- bin_min[which(pct== c2_lo,arr.ind = T)]
  a1_lo <- bin_min[p_lower_index]
  a2_lo <- bin_min[p_lower_index + 1]
  
  lower_bound <- lower_bound_fun(a1_lo,a2_lo,c1_lo,c2_lo)
  
  # upper bound
  c1_up <- if(any(pct<p_upper)){
    c1_up <-max(pct[pct<p_upper])
    print(c1_up)
  } 
  
  c2_up <- if(any(pct>p_upper)){
    c2_up <-min(pct[pct>p_upper])
    print(c2_up)
  } 
  
  #a1_up <- bin_min[which(pct== c1_up,arr.ind = T)]
  #a2_up <- bin_min[which(pct== c2_up,arr.ind = T)]
  a1_up <- bin_min[p_upper_index]
  a2_up <- bin_min[p_upper_index + 1]
  
  upper_bound <- upper_bound_fun(a1_up,a2_up,c1_up,c2_up)
  
} else {
  
  c1 <- if(any(pct<p_lower)){
    c1 <-max(pct[pct<p_lower])
    print(c1)
  } 
  
  c2 <- if(any(pct>p_upper)){
    c2 <- min(pct[pct>p_upper])
    print(c2)
  } 
  
  a1 <- bin_min[which(pct== c1,arr.ind = T)]
  a2 <- bin_min[which(pct== c2,arr.ind = T)]
  
  lower_bound <- lower_bound_fun(a1,a2,c1,c2)
  upper_bound <- upper_bound_fun(a1,a2,c1,c2)
  
}
# approximate SE
SE <- SE_med_fun(upper_bound,lower_bound)

# MOE at 90% confidence interval
MOE <- 1.645 * SE

# print result
print(SE)
print(MOE)























