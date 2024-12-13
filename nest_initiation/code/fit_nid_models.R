# Fit mixed effects models for nest initiation date

# Load Packages
library(lme4) #used for modeling mixed effects models
library(data.table) #for easy data manipulation and visuals
library(corrplot) #to create correlation plot
library(ggplot2)
library(dplyr)
library(tibble) #need for rownames

# Load in data
init_data <- read.csv("nest_initiation/data/initiation_wx.csv", header = TRUE)
head(init_data) #view data

# Make sure data was loaded properly
str(init_data)
summary(init_data)

# Change the site names, kig to Kigigak Island, this is for ggplot legend purposes
init_data <- init_data %>%
  mutate(Site = recode(Site, kig = "Kigigak Island", utq = "Utqiaġvik"))

# Check the data
summary(init_data)

# Make sure site and year are factor variables
is.factor(init_data$Site)
init_data$Site <- as.factor(init_data$Site)
is.factor(init_data$Site)

is.factor(init_data$Year)
init_data$Year <- as.factor(init_data$Year)
is.factor(init_data$Year)

# Check the data
head(init_data)
str(init_data)
summary(init_data)

# Visualize data and distributions
hist(init_data$Win_Hi)
hist(init_data$Init)
hist(init_data$wxt)
hist(init_data$sxt)
hist(init_data$wxw)
hist(init_data$sxw)

# Check for collinearity
pairs(init_data[6:11])
r <- cor(init_data[6:11])
r_plot <- corrplot(r, method = "number", type = "upper")
r_plot


###############################################################################

# Fit mixed effect models

# Init~ 1
fit_1 <- lmer(Init ~ 1 + (1|Year), data = init_data)
summary(fit_1)
##get output for calculating AIC (same method as what midazolam experiment)
loglike <- logLik(fit_1)[1]
betafit_1 <- coef(summary(fit_1))[ ,"Estimate"]
k <- length(betafit_1)
model <- "null"

# summary of model fit
f1.sum <- summary(fit_1)
print(f1.sum, digits=3)


# Init ~ sxt + sxw + Win_Hi + Site + (1|Year)
fit_2 <- lmer(Init ~ sxt + sxw + Win_Hi + Site + (1|Year), data = init_data) #Peterson & Douglas 
summary(fit_2)
plot(ranef(fit_2))
coef(fit_2) #mean estimates
##get output
loglike <- append(loglike, logLik(fit_2)[1])
betafit_2 <- coef(summary(fit_2))[ ,"Estimate"]
k <- append(k, length(betafit_2))
model <- append(model, "sxt+sxw+win_hi+site")

# summary of model fit
f2.sum <- summary(fit_2)
print(f2.sum, digits=3)


# Init ~ sxt + sxw + Win_Hi + I(Win_Hi^2) + Site + (1|Year)
fit_3 <- lmer(Init ~ sxt + sxw + Win_Hi + I(Win_Hi^2) + Site + (1|Year), data = init_data) #Peterson & Douglas
summary(fit_3)
plot(ranef(fit_3))
coef(fit_3)
##get output
loglike <- append(loglike, logLik(fit_3)[1])
betafit_3 <- coef(summary(fit_3))[ ,"Estimate"]
k <- append(k, length(betafit_3))
model <- append(model, "sxt+sxw+win_hi+win_hi2+site")

# summary of model fit
f3.sum <- summary(fit_3)
print(f3.sum, digits=3)


# Init ~ wsi + Site + (1|Year)
fit_4 <- lmer(Init ~ wsi + Site + (1|Year), data = init_data,) #Paul Flint
summary(fit_4)
plot(ranef(fit_4))
coef(fit_4)
#get output
loglike <- append(loglike, logLik(fit_4)[1])
betafit_4 <- coef(summary(fit_4))[ ,"Estimate"]
k <- append(k, length(betafit_4))
model <- append(model, "wsi+site")

# summary of model fit
f4.sum <- summary(fit_4)
print(f4.sum, digits=3)


# Init ~ Win_Hi + I(Win_Hi^2) + Site + (1|Year)
fit_5 <- lmer(Init ~ Win_Hi + I(Win_Hi^2) + Site + (1|Year), data = init_data) #Katie Christie
summary(fit_5)
plot(ranef(fit_5))
coef(fit_5)
#get output
loglike <- append(loglike, logLik(fit_5)[1])
betafit_5 <- coef(summary(fit_5))[ ,"Estimate"]
k <- append(k, length(betafit_5))
model <- append(model, "win_hi+win_hi2+site")

# summary of model fit
f5.sum <- summary(fit_5)
print(f5.sum, digits=3)


# Init ~ Win_Hi + Site + (1|Year)
fit_6 <- lmer(Init ~ Win_Hi + Site + (1|Year), data = init_data) #Paul Flint
summary(fit_6) #check model statistics
plot(ranef(fit_6)) #Residuals vs fitted values of the model
coef(fit_6)
# get output
loglike <- append(loglike, logLik(fit_6)[1])
betafit_6 <- coef(summary(fit_6))[ ,"Estimate"]
k <- append(k, length(betafit_6))
model <- append(model, "win_hi+site")

# summary of model fit
f6.sum <- summary(fit_6)
print(f6.sum, digits=3)


# AICc Model Selection

# create AICc table
table <- data.frame(model)
table$LL <- loglike
table$neg2LL <- loglike*-2 #calculate negative 2 loglikelihood from log likelihood
table$n <- nrow(init_data) #number of observations
table$k <- k+2 #add 2 parameters for the random year effect and the residual estimate
table$AIC <- table$neg2LL + (2*table$k) #calculate AIC (not corrected for small sample)
table$AICc <- table$AIC + ((2*table$k^2+2*table$k)/(table$n-table$k-1)) #calculate AICc
table$AICcmin <- min(table$AICc) #find smallest AIC value
table$deltaAICc <- table$AICc-table$AICcmin #calculate difference between smallest AIC and each AICc
table$odds <- exp(-0.5*table$deltaAICc) #calculate model odds, need for model weights
summodds <- sum(table$odds) #summary model odds
table$AICcwt <- table$odds/summodds #calculate AICc model weights=strength of support for each
print(table, digits=2)

##fit_6, win_hi+site model has the highest weight (0.45), indicating strong support

# Save model output to output folder
# Write the results output into excel (csv)
init_model_table <- table
write.csv(init_model_table, "nest_initiation/output/init_model_table.csv")


###############################################################################

# Model diagnostics
# Standardized residual versus fitted values by site
# Equal variance of residuals from the linear model
plot(fit_6, resid(., scaled=TRUE) ~ fitted(.) | Site, abline = 0)

# Normal distribution of residuals from the linear model
require("lattice")
qqmath(fit_6, id=0.025)

# fit vs. predicted to asses model fit from model
plot(fit_6, Init ~ fitted(.), abline = c(0,1))


###############################################################################

# Graphing preparation

# Add model fits to dataframe
init_data$fit <- predict(fit_6) #this gives out the mean initiation date for each site and year

# Create a new data frame with values for which model estimates
data1 <- data.frame(c("Kigigak Island","Utqiaġvik"), ice = rep(rep(0:79),each=2),1)
names(data1) <- c("Site","Win_Hi")
is.factor(data1$Site)
data1$Site <- as.factor(data1$Site)
summary(data1)
head(data1)
str(data1)

# Load package
library(merTools) # to use predictInterval to get fitted values

# Find random intercept year that is average
averageObs(fit_6) #this is the reference level year
# Add to the nre data set for prediction
colyear <- rep("2003", 160)
# Add random effect to new data frame
data1$Year <- colyear

fit6.CI <- predictInterval(fit_6, newdata = data1, which = "fixed",
                           level = 0.95, n.sims = 1000, stat = "median",
                           include.resid.var = FALSE)

# Combine the predictinterval with data1
fit6.data <- cbind(data1,fit6.CI)

# Get overall mean intiation date for each site
kig_initdata <- init_data[init_data$Site == "Kigigak Island",] #subset kigigak data only
utq_initdata <- init_data[init_data$Site == "Utqiaġvik",] #subset utqiagvik data only
kig_fit6data <- fit6.data[fit6.data$Site == "Kigigak Island",]
utq_fit6data <- fit6.data[fit6.data$Site == "Utqiaġvik",]
mean(kig_initdata$Init) #148.9, 148.8976
mean(utq_initdata$Init) #169.3, 169.3879
#mean(kig_fit6data$fit) #149.6
#mean(utq_fit6data$fit) #169.3
aggregate(Init ~ Site, data = init_data, FUN = mean) #more simple and faster way

# Get confidence interval for overall mean initiation dates (kig and utq)
# Load package
library(plotrix) #to get standard error
std.error(kig_initdata$Init) #0.1427982
std.error(utq_initdata$Init) #0.4993409
kig.m.uci <- 148.8976 + 1.96*0.1427982 #149.177484472
kig.m.lci <- 148.8976 - 1.96*0.1427982 #148.617715528
utq.m.uci <- 169.3879 + 1.96*0.4993409 #170.366608164
utq.m.lci <- 169.3879 - 1.96*0.4993409 #168.409191836


# To interpret the slope (winter ice days)
summary(fit_6)
#Beta estimate = 0.03
#std.error = 0.047
win.uci <- 0.03 + 1.96*0.047 #-0.06212
win.lci <- 0.03 - 1.96*0.047 #0.12212 
confint.merMod(fit_6)

# Use the predict function to get fitted values for certain ice days
# Create new data frame
lm6data <- data.frame(init_data$Init, init_data$Win_Hi, init_data$Site, init_data$Year)
names(lm6data) <- c("Init", "Win_Hi", "Site", "Year")
head(lm6data)
averageObs(fit_6, varList = "Kigigak Island")#used to get the average year for all fitted values. Using year=2003 as ref level
#averageObs(fit_6,varList = "Utqiaġvik")#same as code above... just have it here for learning purposes

# Which years had highes ice and lowest ice?
# 1. retrieve ice data table
library(readr)
ice_wx <- read_csv("nest_survival/data/ice_wx.csv")
View(ice_wx) #Year 2000 has lowest count of ice days. use 2000 as reference level
#highest is 2005


# Estimate mean init date at min and max winter ice days
predict(fit_6, data.frame(Win_Hi=0,Site="Kigigak Island",Year=2003))
predictInterval(fit_6, data.frame(Win_Hi=0,Site="Kigigak Island",Year=2003))
#fit=146 (26 May)
#upr(154)= 03 June
#lwr(138)= 18 May
predict(fit_6, data.frame(Win_Hi=79,Site="Kigigak Island",Year=2003))
predictInterval(fit_6, data.frame(Win_Hi=79,Site="Kigigak Island",Year=2003))
#fit=149 (29 May)
#upr(156)= 05 June
#lwr(141)= 21 May
predict(fit_6, data.frame(Win_Hi=0,Site="Utqiaġvik",Year=2003))
predictInterval(fit_6, data.frame(Win_Hi=0,Site="Utqiaġvik",Year=2003))
#fit=166 (14 June)
#upr(174)= 23 June
#lwr(158)= 07 June
predict(fit_6, data.frame(Win_Hi=79,Site="Utqiaġvik",Year=2003))
predictInterval(fit_6, data.frame(Win_Hi=79,Site="Utqiaġvik",Year=2003))
#fit=168 (16 June)
#upr(175)= 22 June
#lwr(161)= 10 June

# Convert julian date to normal date to two different data frames. for fitted, upper and lower CIs.
fit6.data$dfit <- as.Date(fit6.data$fit, origin = "1994-01-01")
fit6.data$dlwr <- as.Date(fit6.data$lwr, origin = "1994-01-01")
fit6.data$dupr <- as.Date(fit6.data$upr, origin = "1994-01-01")
init_data$ndate <- as.Date(init_data$fit, origin = "1994-01-01")
kig_fit6data$dfit <- as.Date(kig_fit6data$fit, origin = "1994-01-01")
utq_fit6data$dfit <- as.Date(utq_fit6data$fit, origin = "1994-01-01")


# Plot the fitted values and CI with data
ggplot() +
  geom_line(data = fit6.data, aes(x = Win_Hi, y = fit, color = Site)) +
  geom_line(data = fit6.data, aes(x = Win_Hi, y = lwr, color = Site)) +
  geom_line(data = fit6.data, aes(x = Win_Hi, y = upr, color = Site)) +
  geom_ribbon(data = fit6.data, aes(x = Win_Hi, ymin = lwr, ymax = upr, fill = Site),
              alpha = 0.2,
              linetype = 0,
              size = 0.5) +
  geom_point(data = init_data, aes(x = Win_Hi, y = fit, color = Site)) +
  scale_color_manual(values = c("red", "blue")) +
  geom_hline(yintercept = mean(kig_fit6data$fit), color = "red", lty = "dashed") +
  geom_hline(yintercept = mean(utq_fit6data$fit), color = "blue", lty = "dashed") +
  xlab("Winter Ice Days") + ylab("Nest Initiation Date") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(legend.position = "bottom")
ggsave("nest_initiation/output/init_ice.jpg",width = 7, height = 5, dpi = 600)

# Plot with normal date and not julian date. making another one because fitted, lwr and upr CIs may have been rounded
ggplot() +
  geom_line(data = fit6.data, aes(x = Win_Hi, y = dfit, color = Site)) +
  geom_line(data = fit6.data, aes(x = Win_Hi, y = dlwr, color = Site)) +
  geom_line(data = fit6.data, aes(x = Win_Hi, y = dupr, color = Site)) +
  geom_ribbon(data = fit6.data, aes(x = Win_Hi, ymin = dlwr, ymax = dupr, fill = Site),
              alpha = 0.2,
              linetype = 0,
              size = 0.5) +
  geom_point(data = init_data, aes(x = Win_Hi, y = ndate, color = Site)) +
  scale_color_manual(values = c("red", "blue")) +
  geom_hline(yintercept = mean(kig_fit6data$dfit), color = "red", lty = "dashed") +
  geom_hline(yintercept = mean(utq_fit6data$dfit), color = "blue", lty = "dashed") +
  xlab("Winter Ice Days") + ylab("Nest Initiation Date") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(legend.position = "bottom")
ggsave("nest_initiation/output/init_ice_normaldate.jpg",width = 7, height = 5, dpi = 600)
