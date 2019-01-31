# Sjplot
set.seed(123)
library(sjPlot)
library(stargazer)
cph_data <- airbnb_clean[,c("price_dkk","index","strict_cancel",
                            "home","cleaning_fee_dkk","superhost", 
                            "listing_duration", "instant",
                            "security_deposit_dkk",
                            "accommodates","minimum_nights" ,
                            "bathrooms", "distance", "dist_centrum")] %>% na.omit()

# Correlation Matrix
res <- cor(cph_data)
round(res, 2)


mod_no_log <- lm(price_dkk ~ distance + dist_centrum,data = airbnb_clean )
plot_mod_no_log <- plot_model(mod_no_log, type = "diag") %>% plot_grid()

# Thus we take the log

mod1 <- lm(log(price_dkk) ~ distance + dist_centrum,data = airbnb_clean )
plot_mod1 <- plot_model(mod1, type = "diag") %>% plot_grid()
summary(mod1)

mod2 <- lm(log(price_dkk) ~ distance + dist_centrum+
             home+ accommodates + bathrooms 
             ,data = airbnb_clean )
summary(mod2)
plot_mod2 <-plot_model(mod2, type = "diag") %>% plot_grid()

mod3 <- lm(log(price_dkk) ~  distance + dist_centrum 
           +home+ accommodates + bathrooms+
             strict_cancel+ instant+ minimum_nights+cleaning_fee_dkk, 
           data = cph_data)
summary(mod3)
plot_mod3 <-plot_model(mod3, type = "diag") %>% plot_grid()


mod4 <- lm(log(price_dkk) ~  distance + dist_centrum 
           +home+ accommodates + bathrooms+
             strict_cancel+ instant+ minimum_nights+cleaning_fee_dkk+
             index + superhost+
             listing_duration,
           data = cph_data)
summary(mod4)
plot_mod4 <-plot_model(mod4, type = "diag") %>% plot_grid()

library(lmtest)
library(sandwich)

library(lmtest)
library(sandwich)
mod1 <- lm(price_dkk %>% log() ~ distance + dist_centrum,data = cph_data )
mod2 <- lm(price_dkk %>% log() ~ distance + dist_centrum+ home+ accommodates + bathrooms, data = cph_data)
mod3 <- lm(price_dkk %>% log() ~ distance + dist_centrum +
             strict_cancel + instant + minimum_nights + cleaning_fee_dkk, data = cph_data)
mod4 <- lm(price_dkk %>% log() ~ distance + dist_centrum + +
             index + superhost+
             listing_duration, data = cph_data)
mod5 <- lm(price_dkk %>% log() ~  distance + dist_centrum +
             home + accommodates + bathrooms + strict_cancel+ instant + minimum_nights+cleaning_fee_dkk+
             index + superhost + listing_duration, data = cph_data)
mod1_rob <- coeftest(mod1, vcov=vcovHC(mod1))
mod2_rob <- coeftest(mod2, vcov=vcovHC(mod2))
mod3_rob <- coeftest(mod3, vcov=vcovHC(mod3))
mod4_rob <- coeftest(mod4, vcov=vcovHC(mod4))
mod5_rob <- coeftest(mod5, vcov=vcovHC(mod5))

stargazer(mod1_rob, mod2_rob, mod3_rob, mod4_rob,mod5_rob, #regression models 
          type = "html", # character vector (eg. "text" / "html" / "latex")
          title = "Linear Regression Model (Robust SE)",  # header
          style = "ajs",  # style (choice see below)
          summary = NULL,  # logical vector: output summary statistics when given data.frame# path and output of file
          out.header = FALSE, # logical vector: should output file contain code-header?
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "model5"), # column labels for mod1/mod2
          column.separate = c(1,1),  # how column labels should be assigned (label over sev. columns possible)
          covariate.labels = c("Distance Metro",  # Covariate Labels
                               "Distance Centre (Proxy)",
                               "Apartment (Dummy)",
                               "Accomodates",
                               "Number of Bathrooms",
                               "Strict Cancel",
                               "Instant Booking",
                               "Minimum nights",
                               "Cleaning Fee",
                               "Review Index",
                               "Superhost",
                               "Listings duration"),
          dep.var.caption = "Dep. Var", # Caption (Top) of dependent variable
          star.cutoffs = c(0.05,0.01,0.001),
          dep.var.labels = c("Log Price per night in DKK"))








stargazer(mod1, mod2, mod3, mod4,mod5,#regression models 
          type = "text", # character vector (eg. "text" / "html" / "latex")
          title = "Linear Regression Model",  # header
          style = "ajs",  # style (choice see below)
          summary = NULL,  # logical vector: output summary statistics when given data.frame# path and output of file
          out.header = FALSE, # logical vector: should output file contain code-header?
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "All"), # column labels for mod1/mod2
          column.separate = c(1,1),  # how column labels should be assigned (label over sev. columns possible)
          covariate.labels = c("Distance Metro",  # Covariate Labels
                               "Distance Centre (Proxy)",
                               "Apartment (Dummy)",
                               "Accomodates",
                               "Number of Bathrooms",
                               "Strict Cancel",
                               "Instant Booking",
                               "Minimum nights",
                               "Cleaning Fee",
                               "Review Index",
                               "Superhost",
                               "Listings duration"),
          dep.var.caption = "Dep. Var", # Caption (Top) of dependent variable
          star.cutoffs = c(0.05,0.01,0.001),
          dep.var.labels = c("Log Price per night in DKK"))
