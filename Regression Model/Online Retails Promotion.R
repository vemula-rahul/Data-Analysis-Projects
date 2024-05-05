#' SDM A4: Online Retail Campaign

setwd("C:/Users/12149/Desktop/Data Analysis Projects/Regression Model/OnlineRetailPromotions.xlsx")
library(readxl)
d <- read_excel("OnlineRetailPromotions.xlsx", sheet="Data")
str(d)                                    # 64,000 obs


#' Data Preprocessing & Feature Engineering
#'
#' Since multichannel includes both phone and web, we must disentangle this variable
#' into its two constitutent channels

colSums(is.na(d))                         # Check for missing values
d$historysegment <- NULL                  # Less granular than history
d$recency  <- 13 - d$recency              # Higher value indicate more recent purchases

d$campaign <- factor(d$campaign)
levels(d$campaign) <- list(None="No E-Mail", Men="Mens E-Mail", Women="Womens E-Mail")
levels(d$campaign)                        # Simplify factor labels
d$campaign <- relevel(d$campaign, "None")

d$zipcode  <- factor(d$zipcode)
d$zipcode  <- relevel(d$zipcode, "Urban")

d$channelphone <- ifelse(d$channel=="Phone" | d$channel=="Multichannel", 1, 0)
d$channelweb   <- ifelse(d$channel=="Web"   | d$channel=="Multichannel", 1, 0)
d$channel <- NULL

View(d)
str(d)
dvisit <- subset(d, visit==1)             # People who visited the website
str(dvisit)                               # 9,394 obs
dconvert <- subset(d1, conversion==1)     # People who made a purchase
str(dconvert)                             # 578 obs
View (dconvert)


#' Data Visualization

hist(d$spend)
hist(log(d$spend))
summary(d$spend)

hist(dconvert$spend)
hist(log(dconvert$spend))
summary(dconvert$spend)

#' log(spend) shows a lot of excess zeros; without the excess zeros, rest of the data looks normal

library(PerformanceAnalytics)
temp <- d2[, c(1, 2, 10)]
chart.Correlation(temp)                   # Correlations < 0.3

#' Variable Selection
#' 
#' campaign:    Customers receiving the promotional campaign are expected to spend more
#' recency:     Recent customers may be predisposed to spending more
#' history:     Customers with a history of high prior purchases may be expected to
#'              spend more
#' mens/womens: Customers who purchased mens (womens) products are more likely to respond to mens (womens) campaign
#' zipcode:     Urban shoppers may have a different spending pattern than rural 
#'              or suburban customers
#' newcustomer: New customers may be more excited about online purchases
#' channel:     Some shoppers may prefer web or online channels; but since we have
#'              some shoppers that used both channels, we have to split this data
#'              into separate variables for web and online channel shoppers
#' visit/conversion: Constants: we are studying converted shoppers only


#' Regression Models

m1 = lm(log(spend) ~ campaign + history + recency + mens + womens + zipcode +
          newcustomer + channelphone + channelweb, data=dconvert)

m0 = lm(log(spend) ~ history + recency + mens + womens + zipcode +
          newcustomer + channelphone + channelweb, data=dconvert)

m2 = lm(log(spend) ~ campaign*mens + campaign*womens + campaign*newcustomer + 
          campaign*history + recency + zipcode + campaign*channelphone + 
          campaign*channelweb, data=dconvert)

library(stargazer)
stargazer(m0, m1, m2, type="text", single.row=TRUE)

#' Regression Assumptions 

plot(m1)                                  # Linearity: Passed, has omitted variables

library("car")
norm <- rnorm(500)
ks.test(norm, m1$fit)                     # K-S test: Failed multivariate normality

library("lmtest")
bptest(m1)                                # Breusch-Pagan test: Passed homoskedasticity

vif(m1)                                   # VIF test: Passed multi-collinearity

library(lmtest)                           # Durbin-Watson test: Passed autocorrelation
dwtest(m1)                          


#' Observation: All of the above OLS models have adj R-sq ~ 0. 
#' But more importantly, since normality failed, what's our Plan B?
#' Perhaps we should try GLM with a different (Poisson) dist. Since Poisson regression
#' requires integer DV, we can round off spend to the nearest integer. We may have to 
#' go back to the original dataframe d becase dconvert has no zeros.

dconvert$spend <-round(dconvert$spend, 0)
summary(dconvert$spend)

m3 = glm(spend ~ campaign*mens + campaign*womens + campaign*newcustomer + 
           campaign*history + recency + zipcode + campaign*channelphone + 
           campaign*channelweb, family=poisson (link=log), data=dconvert)
summary(m3)

library(AER)
dispersiontest(m3)                       

#' Because of overdispersion (lambda=93), Poisson estimates are not reliable.
#' Negative binomial model is more appropriate for overdispersed data.

library(MASS) 
m4 <- glm.nb(spend ~ campaign*mens + campaign*womens + campaign*newcustomer + 
               campaign*history + recency + zipcode + campaign*channelphone + 
               campaign*channelweb, data=dconvert)

#' Altenatively, we can make this really simple by using hurdle or zero-inflated or 
#' hurdle models to correct for excess zeros in the original dataset

d$spend <- round(d$spend, 0)

library(pscl)
m5 <- hurdle(spend ~ campaign*mens + campaign*womens + campaign*newcustomer + 
               campaign*history + recency + zipcode + campaign*channelphone + 
               campaign*channelweb | visit + conversion, 
             data=d, link="logit", dist="negbin")

m6 <- zeroinfl(spend ~ campaign*mens + campaign*womens + campaign*newcustomer + 
                 campaign*history + recency + zipcode + campaign*channelphone + 
                 campaign*channelweb | visit + conversion, 
               data=d, link="logit", dist="negbin")

stargazer(m4, m5, m6, type="text", single.row=TRUE)

#' Since hurdle estimates are pretty consistent with zeroinfl and negative binomial
#' estimates, we can use the hurdle model as our best model. But we also need a model
#' without interactions for comparison purposes. These would be our THREE BEST MODELS.

m7 <- hurdle(spend ~ campaign + history + recency + mens + womens + zipcode +
               newcustomer + channelphone + channelweb | visit + conversion, 
             data=d, link="logit", dist="negbin")

stargazer(m4, m7, m5, type="text", single.row=TRUE)
vif(m7)              
dwtest(m7) 

