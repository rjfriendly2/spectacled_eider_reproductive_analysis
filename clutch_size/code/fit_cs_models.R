# Fit mixed effects models for clutch size

# Load Packages
library(lme4) #used for modeling mixed effects models
library(data.table) #for easy data manipulation and visuals
library(optimx) #another method for optimzation in model fitting
library(plotrix) #can use to get standard error
library(merTools)
library(ggeffects)
library(ggplot2)
library(dplyr)

# Load data
cs_data <- read.csv("clutch_size/data/clutch_wx.csv", header = TRUE)
head(cs_data)
summary(cs_data)

# Remove nest with 34 eggs
cs_data <- cs_data[!(cs_data$maxclutch == "34"),]
summary(cs_data)

# Make sure site and year are factor variables
is.factor(cs_data$site)
cs_data$site <- as.factor(cs_data$site)
is.factor(cs_data$site)

is.factor(cs_data$year)
cs_data$year <- as.factor(cs_data$year)
is.factor(cs_data$year)

# Visualize data
hist(cs_data$maxclutch)
boxplot(cs_data$maxclutch)
hist(cs_data$win_hi)
hist(cs_data$wxt)
hist(cs_data$sxt)
hist(cs_data$wxw)
hist(cs_data$sxw)
hist(cs_data$wsi)
head(cs_data)

# Frequencies of each clutch size
clutch_freq <- table(cs_data$maxclutch)
print(clutch_freq)

# Standardize variables
#cs_data$cwin_hi <- cs_data$win_hi - mean(cs_data$win_hi)
cs_data$swin_hi <- scale(cs_data$win_hi, center = TRUE, scale = TRUE) #standardized, to center only set scale=FALSE
# 
#cs_data$cwsi <- cs_data$wsi - mean(cs_data$wsi)
cs_data$swsi <- scale(cs_data$wsi, center = TRUE, scale = TRUE) #standardized
#
cs_data$sInit <- scale(cs_data$Init, center = TRUE, scale = TRUE) #standardized

head(cs_data)

####################################### 
# Make a graph to visualize data to see if there are any outliers
### 
ggplot(cs_data, aes(x = maxclutch)) +
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue") +
  geom_text(stat = 'bin', aes(label = ..count..), vjust = -0.5, binwidth = 1) +
  theme_minimal()

### the nest with 10 eggs have 5 nopi eggs. Change the value to 5 from nest JKL036
cs_data <- cs_data %>%
  mutate(maxclutch = ifelse(Nest == "JKL036", 5, maxclutch))

# check data summary
summary(cs_data)

### TLM062 has 8 eggs, but notes say 4 spri and 4 coei. change maxclutch value to 4 from 8.
cs_data <- cs_data %>%
  mutate(maxclutch = ifelse(Nest == "TLM062", 4, maxclutch))

### 18-MWM037 has 8 eggs, and that one egg was dump egg. change maxclutch value to 7
cs_data <- cs_data %>%
  mutate(maxclutch = ifelse(Nest == "18-MWM037", 7, maxclutch))

# Frequencies of each clutch size
clutch_freq <- table(cs_data$maxclutch)
print(clutch_freq)


### Graph again
ggplot(cs_data, aes(x = maxclutch)) +
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue") +
  geom_text(stat = 'bin', aes(label = ..count..), vjust = -0.5, binwidth = 1) +
  theme_minimal()

##############################################################################


# Fit mixed effect models
# maxclutch ~ 1
c1 <- glmer(maxclutch ~ 1 + (1|year), data = cs_data,
           family = poisson(link = "log"))
summary(c1)
# get output
loglike <- logLik(c1)[1]
beta_c1 <- coef(summary(c1))[ ,"Estimate"]
k <- length(beta_c1)
model <- "null"

# summary of model fit
c1.sum <- summary(c1)
print(c1.sum, digits=3)


# maxclutch ~ sxt + sxw + Win_Hi + Init + site + (1|year)
c2 <- glmer(maxclutch ~ sxt + sxw + swin_hi + sInit + site + (1|year), 
            data = cs_data, family = poisson(link = "log")) #Peterson & Douglas
summary(c2)
loglike <- append(loglike, logLik(c2)[1])
beta_c2 <- coef(summary(c2))[ ,"Estimate"]
k <- append(k, length(beta_c2))
model <- append(model, "sxt+sxw+win_hi+init+site")

# summary of model fit
c2.sum <- summary(c2)
print(c2.sum, digits=3)
plot(ranef(c2)) #QQ plot
plot(c2)

logLik(c2)
AIC(c2)


# maxclutch ~ sxt + sxw + Win_Hi + I(Win_Hi^2) + Init + site + (1|year)
c3 <- glmer(maxclutch ~ sxt + sxw + swin_hi + I(swin_hi^2) + sInit + site + (1|year), 
            data = cs_data, family = poisson(link = "log")) #Peterson & Douglas
summary(c3)
loglike <- append(loglike, logLik(c3)[1])
beta_c3 <- coef(summary(c3))[ ,"Estimate"]
k <- append(k, length(beta_c3))
model <- append(model, "sxt+sxw+win_hi+win_hi2+init+site")

# summary of model fit
c3.sum <- summary(c3)
print(c3.sum, digits=3)
plot(ranef(c3))



# maxclutch ~ wsi + Init + site + (1|year)
c4 <- glmer(maxclutch ~ swsi + sInit + site + (1|year), 
            data = cs_data, family = poisson(link = "log")) #Paul Flint
summary(c4)
loglike <- append(loglike, logLik(c4)[1])
beta_c4 <- coef(summary(c4))[ ,"Estimate"]
k <- append(k, length(beta_c4))
model <- append(model, "wsi+init+site")

# summary of model fit
c4.sum <- summary(c4)
print(c4.sum, digits=3)
plot(ranef(c4))




# maxclutch ~ Win_Hi + I(Win_Hi^2) + Init + site + (1|year)
c5 <- glmer(maxclutch ~ swin_hi + I(swin_hi^2) + sInit[,1] + site + (1|year), 
            data = cs_data, family = poisson(link = "log")) #Katie Christie
summary(c5)
loglike <- append(loglike, logLik(c5)[1])
beta_c5 <- coef(summary(c5))[ ,"Estimate"]
k <- append(k, length(beta_c5))
model <- append(model, "win_hi+win_hi2+init+site")

# summary of model fit
c5.sum <- summary(c5)
print(c5.sum, digits=3)
plot(ranef(c5))



# maxclutch ~ Win_Hi + Init + site + (1|year)
c6 <- glmer(maxclutch ~ swin_hi + sInit + site + (1|year), 
            data = cs_data, family = poisson(link = "log")) #Paul Flint
summary(c6)
loglike <- append(loglike, logLik(c6)[1])
beta_c6 <- coef(summary(c6))[ ,"Estimate"]
k <- append(k, length(beta_c6))
model <- append(model, "win_hi+init+site")

# summary of model fit
c6.sum <- summary(c6)
print(c6.sum, digits=3)
plot(ranef(c6))



# AICc Model Selection

# Create AICc table
table <- data.frame(model)
table$LL <- loglike
table$neg2LL <- loglike*-2 #calculate negative 2 loglikelihood from log likelihood
table$n <- nrow(cs_data) #number of observations
table$k <- k+2 #add 2 parameters for the random year effect and the residual estimate
table$AIC <- table$neg2LL + (2*table$k) #calculate AIC (not corrected for small sample)
table$AICc <- table$AIC + ((2*table$k^2+2*table$k)/(table$n-table$k-1)) #calculate AICc
table$AICcmin <- min(table$AICc) #find smallest AIC value
table$deltaAICc <- table$AICc-table$AICcmin #calculate difference between smallest AIC and each AICc
table$odds <- exp(-0.5*table$deltaAICc) #calculate model odds, need for model weights
summodds <- sum(table$odds) #summary model odds
table$AICcwt <- table$odds/summodds #calculate AICc model weights=strength of support for each
print(table, digits=2) 




# Save model oupput to output folder
# Write the results output into excel (csv)
clutch_model_table_2 <- table
write.csv(clutch_model_table_2, "clutch_size/output/clutch_model_table_2.csv")



###Get mean clutch size and standard errors for both sites
kig_csdata <- cs_data[cs_data$site == "kig",]
utq_csdata <- cs_data[cs_data$site == "utq",]
mean(kig_csdata$maxclutch) #4.8
mean(utq_csdata$maxclutch) #4.2

# Activate package
library(plotrix) #used to get standard error
std.error(kig_csdata$maxclutch) #0.021
std.error(utq_csdata$maxclutch) #0.11
aggregate(maxclutch ~ site, data = cs_data, FUN = mean) # get the mean for two sites from main data
klci <- mean(kig_csdata$maxclutch) - 1.96*std.error(kig_csdata$maxclutch) #4.75
kuci <- mean(kig_csdata$maxclutch) + 1.96*std.error(kig_csdata$maxclutch) #4.83

ulci <- mean(utq_csdata$maxclutch) - 1.96*std.error(utq_csdata$maxclutch) #4.01
uuci <- mean(utq_csdata$maxclutch) + 1.96*std.error(utq_csdata$maxclutch) #4.46




mean(cs_data$maxclutch) #4.76
std.error(cs_data$maxclutch) #0.0208
lci <- mean(cs_data$maxclutch) - 1.96*std.error(cs_data$maxclutch) #4.72
uci <- mean(cs_data$maxclutch) + 1.96*std.error(cs_data$maxclutch) #4.80

# Get overall mean
m_clutch <- mean(cs_data$maxclutch) #4.76
# get sample size
n <- length(cs_data$maxclutch) #2861
# find standard deviation
st.dev <- sd(cs_data$maxclutch)
# get standard error
st.error <- st.dev/sqrt(n)
alpha = 0.05
deg_of_free <- n-1
t_score <- qt(p = alpha/2, df = deg_of_free, lower.tail = F)
mar_of_error <- t_score*st.error
lower_bound <- m_clutch - mar_of_error
upper_bound <- m_clutch + mar_of_error


# Average clutch sizes per year
aggregate(maxclutch ~ year, data = cs_data, FUN = mean) # get mean clutch size for each year both sites combined

# average clutch sizes per year at each site
kig_eggs <- aggregate(maxclutch ~ year, data = kig_csdata, FUN = mean)
utq_eggs <- aggregate(maxclutch ~ year, data = utq_csdata, FUN = mean)

plot(kig_eggs$year, kig_eggs$maxclutch)
library(ggplot2)
ggplot(kig_eggs, aes(x = year, y = maxclutch)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))


############# TOP MODEL #########################
# c6 <- glmer(maxclutch ~ swin_hi + sInit + site + (1|year)
################################################

# Get confidence intervals from the top model (c6)
# winter ice days (swin_hi)
# beta = 0.03517
# std. error = 0.01995
win_ice_upper <- 0.03517 + 1.96*0.01995 #0.074
win_ice_lower <- 0.03517 - 1.96*0.01995 #-0.003
# the CIs overlap zero, is this important? No?

# initiation date (sInit)
# beta = -0.07582
# std. error = 0.01237
init_upper <- (-0.07582) + (1.96*0.01237) #-0.0515
init_lower <- (-0.07582) - (1.96*0.01237) #0.-0.1000

summary(c6)

# exponentiate the fixed effects coefficients to get rates on natural scale
exp(fixef(c6)) # get the beta estimates on Probability scale
exp(-0.07582) # the above code worked.
# get the CI for the fixed effects (log scale)
confint(c6)
exp(confint(c6)) # get the CI for the fixed effects (probability scale) 

confint.merMod(c6)



###############################################################################

# Graphing preparation


##################### TOP MODEL ###############################

# c6 <- glmer(maxclutch ~ swin_hi + sInit + site + (1|year)

################################################################


# first let's extract beta coefficients along with their 95% CIs
beta_coeff1 <- fixef(c6) # extract fixed effects
print(beta_coeff1)

conf_int1 <- confint(c6, method = "profile")
print(conf_int1)
conf_int1 <- conf_int1[-1, ]
print(conf_int1)

# combine beta coeff with CIs
results1 <- data.frame(
  Estimate = beta_coeff1,
  low.95 = conf_int1[, 1],
  up.95 = conf_int1[, 2]
)
print(results1)

#save beta coeff and ci
write.csv(results1, "clutch_size/output/model1_fixedeffects.csv")



### work on getting predicted values

#c6 <- glmer(maxclutch ~ swin_hi + sInit + site + (1|year)

####################################
############## sInit ###############
#####################################

predicted_mmm <- ggpredict(c6, terms = "sInit [min, mean, max]") # predict for min mean and max
write.csv(predicted_mmm, "clutch_size/output/predicted_min_mean_max_init.csv")


predicted_sInit <- ggpredict(c6, terms = c("sInit", "site")) # predict for both sites


### Change names and values in the data
colnames(predicted_sInit)[colnames(predicted_sInit) == "group"] <- "Site" # change 'group' to 'Site'
predicted_sInit$Site <- as.character(predicted_sInit$Site) # so we can change the names below
predicted_sInit$Site[predicted_sInit$Site == "kig"] <- "Kigigak Island"
predicted_sInit$Site[predicted_sInit$Site == "utq"] <- "Utqiaġvik"
predicted_sInit$Site <- as.factor(predicted_sInit$Site) # make it a factor variable again
write.csv(predicted_sInit, "clutch_size/output/predicted_init_site.csv")

ggplot(predicted_sInit, aes(x = x, y = predicted, color = Site, fill = Site)) + 
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Site), 
              alpha = 0.2) +
  geom_line() +
  labs(
    x = "Standardized Nest Initiation Date",
    y = "Predicted Clutch Size",
    color = "Site"
  ) +
  scale_color_manual(values = c("red","blue")) +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(legend.position = "bottom")
ggsave("clutch_size/output/clutch_sInit.jpg", width = 7, height = 5, dpi = 600)



### work on getting predicted values

####################################
############## swin_hi ###############
#####################################

predicted_swin_hi_mmm <- ggpredict(c6, terms = "swin_hi [min, mean, max]") # predict for min mean and max
write.csv(predicted_swin_hi_mmm, "clutch_size/output/predicted_min_mean_max_swin_hi.csv")


predicted_swin_hi <- ggpredict(c6, terms = c("swin_hi", "site")) # predict for both sites


### Change names and values in the data
colnames(predicted_swin_hi)[colnames(predicted_swin_hi) == "group"] <- "Site" # change 'group' to 'Site'
predicted_swin_hi$Site <- as.character(predicted_swin_hi$Site) # so we can change the names below
predicted_swin_hi$Site[predicted_swin_hi$Site == "kig"] <- "Kigigak Island"
predicted_swin_hi$Site[predicted_swin_hi$Site == "utq"] <- "Utqiaġvik"
predicted_swin_hi$Site <- as.factor(predicted_swin_hi$Site) # make it a factor variable again
write.csv(predicted_swin_hi, "clutch_size/output/predicted_swin_hi_site.csv")

ggplot(predicted_swin_hi, aes(x = x, y = predicted, color = Site, fill = Site)) + 
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Site), 
              alpha = 0.2) +
  geom_line() +
  labs(
    x = "Standardized Winter Ice Days",
    y = "Predicted Clutch Size",
    color = "Site"
  ) +
  scale_color_manual(values = c("red","blue")) +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(legend.position = "bottom")
ggsave("clutch_size/output/clutch_swin_hi.jpg", width = 7, height = 5, dpi = 600)







##################### 2nd RANKED MODEL #########################

# c4 <- glmer(maxclutch ~ swsi + sInit + site + (1|year)

################################################################


####################################
############## swsi ###############
#####################################

# first let's extract beta coefficients along with their 95% CIs
beta_coeff2 <- fixef(c4) # extract fixed effects
print(beta_coeff2)

conf_int2 <- confint(c4, method = "profile")
print(conf_int2)
conf_int2 <- conf_int2[-1, ]
print(conf_int2)

# combine beta coeff with CIs
results2 <- data.frame(
  Estimate = beta_coeff2,
  low.95 = conf_int2[, 1],
  up.95 = conf_int2[, 2]
)
print(results1)

#save beta coeff and ci
write.csv(results2, "clutch_size/output/model2_fixedeffects.csv")


predicted_swsi_mmm <- ggpredict(c4, terms = "swsi [min, mean, max]") # predict for min mean and max
write.csv(predicted_swsi_mmm, "clutch_size/output/predicted_min_mean_max_swsi.csv")


predicted_swsi <- ggpredict(c4, terms = c("swsi", "site")) # predict for both sites


### Change names and values in the data
colnames(predicted_swsi)[colnames(predicted_swsi) == "group"] <- "Site" # change 'group' to 'Site'
predicted_swsi$Site <- as.character(predicted_swsi$Site) # so we can change the names below
predicted_swsi$Site[predicted_swsi$Site == "kig"] <- "Kigigak Island"
predicted_swsi$Site[predicted_swsi$Site == "utq"] <- "Utqiaġvik"
predicted_swsi$Site <- as.factor(predicted_swsi$Site) # make it a factor variable again
write.csv(predicted_swsi, "clutch_size/output/predicted_swsi_site.csv")

ggplot(predicted_swsi, aes(x = x, y = predicted, color = Site, fill = Site)) + 
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Site), 
              alpha = 0.2) +
  geom_line() +
  labs(
    x = "Standardized Winter Ice Index",
    y = "Predicted Clutch Size",
    color = "Site"
  ) +
  scale_color_manual(values = c("red","blue")) +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(legend.position = "bottom")
ggsave("clutch_size/output/clutch_wsi.jpg", width = 7, height = 5, dpi = 600)





##################### 3nd RANKED MODEL #########################

# c4 <- glmer(maxclutch ~ swsi + sInit + site + (1|year)

################################################################


####################################
############## 3rd ###############
#####################################

# maxclutch ~ sxt + sxw + Win_Hi + Init + site + (1|year)

# first let's extract beta coefficients along with their 95% CIs
beta_coeff3 <- fixef(c2) # extract fixed effects
print(beta_coeff3)

conf_int3 <- confint(c2, method = "profile")
print(conf_int3)
conf_int3 <- conf_int3[-1, ]
print(conf_int3)

# combine beta coeff with CIs
results3 <- data.frame(
  Estimate = beta_coeff3,
  low.95 = conf_int3[, 1],
  up.95 = conf_int3[, 2]
)
print(results3)

#save beta coeff and ci
write.csv(results3, "clutch_size/output/model3_fixedeffects.csv")

#### sxt

predicted_sxt_mmm <- ggpredict(c2, terms = "sxt [min, mean, max]") # predict for min mean and max
write.csv(predicted_sxt_mmm, "clutch_size/output/predicted_min_mean_max_sxt.csv")


predicted_sxt <- ggpredict(c2, terms = c("sxt", "site")) # predict for both sites


### Change names and values in the data
colnames(predicted_sxt)[colnames(predicted_sxt) == "group"] <- "Site" # change 'group' to 'Site'
predicted_sxt$Site <- as.character(predicted_sxt$Site) # so we can change the names below
predicted_sxt$Site[predicted_sxt$Site == "kig"] <- "Kigigak Island"
predicted_sxt$Site[predicted_sxt$Site == "utq"] <- "Utqiaġvik"
predicted_sxt$Site <- as.factor(predicted_sxt$Site) # make it a factor variable again
write.csv(predicted_sxt, "clutch_size/output/predicted_sxt_site.csv")

ggplot(predicted_sxt, aes(x = x, y = predicted, color = Site, fill = Site)) + 
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Site), 
              alpha = 0.2) +
  geom_line() +
  labs(
    x = "Spring Extreme Temp Days",
    y = "Predicted Clutch Size",
    color = "Site"
  ) +
  scale_color_manual(values = c("red","blue")) +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(legend.position = "bottom")
ggsave("clutch_size/output/clutch_sxt.jpg", width = 7, height = 5, dpi = 600)




#### sxw

predicted_sxw_mmm <- ggpredict(c2, terms = "sxw [min, mean, max]") # predict for min mean and max
write.csv(predicted_sxw_mmm, "clutch_size/output/predicted_min_mean_max_sxw.csv")


predicted_sxw <- ggpredict(c2, terms = c("sxw", "site")) # predict for both sites


### Change names and values in the data
colnames(predicted_sxw)[colnames(predicted_sxw) == "group"] <- "Site" # change 'group' to 'Site'
predicted_sxw$Site <- as.character(predicted_sxw$Site) # so we can change the names below
predicted_sxw$Site[predicted_sxw$Site == "kig"] <- "Kigigak Island"
predicted_sxw$Site[predicted_sxw$Site == "utq"] <- "Utqiaġvik"
predicted_sxw$Site <- as.factor(predicted_sxw$Site) # make it a factor variable again
write.csv(predicted_sxw, "clutch_size/output/predicted_sxw_site.csv")

ggplot(predicted_sxw, aes(x = x, y = predicted, color = Site, fill = Site)) + 
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Site), 
              alpha = 0.2) +
  geom_line() +
  labs(
    x = "Spring Extreme Wind Days",
    y = "Predicted Clutch Size",
    color = "Site"
  ) +
  scale_color_manual(values = c("red","blue")) +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(legend.position = "bottom")
ggsave("clutch_size/output/clutch_sxw.jpg", width = 7, height = 5, dpi = 600)






















### Plot mean clutch size for each year and site
mean_clutch <- cs_data %>%
  group_by(year, site) %>%
  summarise(mean_clutch = mean(maxclutch, na.rm = TRUE))

#scatterplot with connected lines
ggplot(mean_clutch, aes(x = year, y = mean_clutch, color = site, group = site)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Mean Clutch Size"
  ) +
  scale_color_manual(values = c("red", "blue")) +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(legend.position = "bottom")
ggsave("clutch_size/output/meanclutch_year.jpg", width = 7, height = 5, dpi = 600)
#








# c6 <- glmer(maxclutch ~ swin_hi + sInit + site + (1|year),
summary(c6)
summary(cs_data)

# estimate clutch size at min max and mean values of init
#predict(c6, data.frame(sInit = -2.54588574, site = "kig", year = 2003))
#predictInterval(c6, data.frame(sInit = -2.545886, site = "kig", year = 2003))

#predict(c6, newdata = clutch_predictions, data.frame(
        #site = "kig",
       # year = 2003,
        #type = "response"))

##############################################




### 
ggplot(cs_data, aes(x = maxclutch)) +
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue") +
  geom_text(stat = 'bin', aes(label = ..count..), vjust = -0.5, binwidth = 1) +
  theme_minimal()




















