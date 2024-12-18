# Fit mixed effects models for clutch size

# Load Packages
library(lme4) #used for modeling mixed effects models
library(data.table) #for easy data manipulation and visuals
library(optimx) #another method for optimzation in model fitting

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

# Standardize variables
cs_data$cwin_hi <- cs_data$win_hi - mean(cs_data$win_hi)
cs_data$swin_hi <- scale(cs_data$win_hi, center = TRUE, scale = TRUE) #standardized, to center only set scale=FALSE
# 
cs_data$cwsi <- cs_data$wsi - mean(cs_data$wsi)
cs_data$swsi <- scale(cs_data$wsi, center = TRUE, scale = TRUE) #standardized

head(cs_data)



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


# maxclutch ~ sxt + sxw + Win_Hi + site + (1|year)
c2 <- glmer(maxclutch ~ sxt + sxw + swin_hi[,1] + site + (1|year), 
            data = cs_data, family = poisson(link = "log")) #Peterson & Douglas
summary(c2)
loglike <- append(loglike, logLik(c2)[1])
beta_c2 <- coef(summary(c2))[ ,"Estimate"]
k <- append(k, length(beta_c2))
model <- append(model, "sxt+sxw+win_hi+site")

# summary of model fit
c2.sum <- summary(c2)
print(c2.sum, digits=3)
plot(ranef(c2)) #QQ plot
plot(c2)

logLik(c2)
AIC(c2)
# maxclutch ~ sxt + sxw + Win_Hi + I(Win_Hi^2) + site + (1|year)
c3 <- glmer(maxclutch ~ sxt + sxw + swin_hi[,1] + I(swin_hi[,1]^2) + site + (1|year), 
            data = cs_data, family = poisson(link = "log")) #Peterson & Douglas
summary(c3)
loglike <- append(loglike, logLik(c3)[1])
beta_c3 <- coef(summary(c3))[ ,"Estimate"]
k <- append(k, length(beta_c3))
model <- append(model, "sxt+sxw+win_hi+win_hi2+site")

# summary of model fit
c3.sum <- summary(c3)
print(c3.sum, digits=3)
plot(ranef(c3))



# maxclutch ~ wsi + site + (1|year)
c4 <- glmer(maxclutch ~ swsi[,1] + site + (1|year), 
            data = cs_data, family = poisson(link = "log")) #Paul Flint
summary(c4)
loglike <- append(loglike, logLik(c4)[1])
beta_c4 <- coef(summary(c4))[ ,"Estimate"]
k <- append(k, length(beta_c4))
model <- append(model, "wsi+site")

# summary of model fit
c4.sum <- summary(c4)
print(c4.sum, digits=3)
plot(ranef(c4))




# maxclutch ~ Win_Hi + I(Win_Hi^2) + site + (1|year)
c5 <- glmer(maxclutch ~ swin_hi[,1] + I(swin_hi[,1]^2) + site + (1|year), 
            data = cs_data, family = poisson(link = "log")) #Katie Christie
summary(c5)
loglike <- append(loglike, logLik(c5)[1])
beta_c5 <- coef(summary(c5))[ ,"Estimate"]
k <- append(k, length(beta_c5))
model <- append(model, "win_hi+win_hi2+site")

# summary of model fit
c5.sum <- summary(c5)
print(c5.sum, digits=3)
plot(ranef(c5))



# maxclutch ~ Win_Hi + site + (1|year)
c6 <- glmer(maxclutch ~ swin_hi[,1] + site + (1|year), 
            data = cs_data, family = poisson(link = "log")) #Paul Flint
summary(c6)
loglike <- append(loglike, logLik(c6)[1])
beta_c6 <- coef(summary(c6))[ ,"Estimate"]
k <- append(k, length(beta_c6))
model <- append(model, "win_hi+site")

# summary of model fit
c6.sum <- summary(c6)
print(c6.sum, digits=3)
plot(ranef(c6))

#c7 <- glm(maxclutch ~ Init, data = cs_data)
#summary(c7)
# get output
#loglike <- logLik(c7)[1]
#beta_c7 <- coef(summary(c7))[ ,"Estimate"]
#k <- length(beta_c7)
#model <- "init"

# summary of model fit
c7.sum <- summary(c7)
print(c7.sum, digits=3)



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
clutch_model_table <- table
write.csv(clutch_model_table, "clutch_size/output/clutch_model_table.csv")



###Get mean clutch size and standard errors for both sites
kig_csdata <- cs_data[cs_data$site == "kig",]
utq_csdata <- cs_data[cs_data$site == "utq",]
mean(kig_csdata$maxclutch) #4.8
mean(utq_csdata$maxclutch) #4.3

# Activate package
library(plotrix) #used to get standard error
std.error(kig_csdata$maxclutch) #0.021
std.error(utq_csdata$maxclutch) #0.10
aggregate(maxclutch ~ site, data = cs_data, FUN = mean)
klci <- mean(kig_csdata$maxclutch) - 1.96*std.error(kig_csdata$maxclutch) #4.7478
kuci <- mean(kig_csdata$maxclutch) + 1.96*std.error(kig_csdata$maxclutch) #4.8308

ulci <- mean(utq_csdata$maxclutch) - 1.96*std.error(utq_csdata$maxclutch) #4.1123
uuci <- mean(utq_csdata$maxclutch) + 1.96*std.error(utq_csdata$maxclutch) #4.5030




mean(cs_data$maxclutch) #4.761596
std.error(cs_data$maxclutch) #0.02085693
lci <- mean(cs_data$maxclutch) - 1.96*std.error(cs_data$maxclutch) #4.72
uci <- mean(cs_data$maxclutch) + 1.96*std.error(cs_data$maxclutch) #4.80

# Get overall mean
m_clutch <- mean(cs_data$maxclutch)
# get sample size
n <- length(cs_data$maxclutch)
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
aggregate(maxclutch ~ year, data = cs_data, FUN = mean)

# average clutch sizes per year at each site
kig_eggs <- aggregate(maxclutch ~ year, data = kig_csdata, FUN = mean)
utq_eggs <- aggregate(maxclutch ~ year, data = utq_csdata, FUN = mean)

plot(kig_eggs$year, kig_eggs$maxclutch)
library(ggplot2)
ggplot(kig_eggs, aes(x = year, y = maxclutch)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))



c7 <- glmer(maxclutch ~ 1 + (1|year) + Init, data = cs_data,
            family = poisson(link = "log"))
summary(c7)
# get output
loglike <- logLik(c7)[1]
beta_c1 <- coef(summary(c1))[ ,"Estimate"]
k <- length(beta_c7)
model <- "null"

# summary of model fit
c1.sum <- summary(c7)
print(c7.sum, digits=3)

#
