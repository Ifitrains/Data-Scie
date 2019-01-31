---
title: "Regression"
output:
bookdown::html_document2: default
bibliography: bibfile.bib
---
## Preperation


Sharing economy based acc














## Calculate the distance


```r
library(geosphere)

metro_sp <- metro_df[,c("lat", "long")]

# Distance measure to measure the distance between a station and an accommodation
#
hav.dist <- function(long1, lat1, long2, lat2) {
  R <- 6371
  diff.long <- (long2 - long1)
  diff.lat <- (lat2 - lat1)
  a <- sin(diff.lat/2)^2 + cos(lat1) * cos(lat2) * sin(diff.long/2)^2
  b <- 2 * asin(pmin(1, sqrt(a))) 
  d = R * b
  return(d)
}


# # we get a matrix that contains in the first row the distances between the first accomodation 
# and all metro stations:

dist_metacc <- geosphere::distm(cbind(airbnb_clean$longitude, airbnb_clean$latitude),
                                cbind(metro_sp$lon, metro_sp$lat), 
                                fun = distHaversine)
summary(dist_metacc[,1]) # for the first metro
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##    89.36  4783.91  5756.16  5674.86  7011.88 12692.54
```

```r
head(dist_metacc[,1:3]) # Distances from the first six accomodations to the first three metros
```

```
##          [,1]     [,2]     [,3]
## [1,] 4079.600 2145.826 4228.463
## [2,] 5727.915 3982.683 6256.679
## [3,] 4978.247 3021.508 3912.291
## [4,] 6294.354 4322.552 5105.278
## [5,] 3314.377 1476.948 4080.976
## [6,] 5324.121 3534.447 5771.846
```

```r
# now get the index of the smallest value in each row (margin = 1)
# in other words: for each of the accomodations, get the index of the metro station that is closest
minidx <- apply(dist_metacc, 1, which.min)
print(minidx[1:5])
```

```
## [1] 13 17  9 10  4
```

```r
# add result to data

library(matrixStats)
airbnb_clean$distance <- rowMins(as.matrix(dist_metacc))

head(airbnb_clean$distance)
```

```
## [1]  640.8608 1167.9521 1347.5282  656.8279  261.9853  684.2862
```

```r
# Lets create a proxy "central" in order to control for that
# lat =55.679687, lng = 12.590659 (Coordinates of Nyhaven)

airbnb_clean$dist_centrum <- geosphere::distm(cbind(airbnb_clean$longitude,
                                       airbnb_clean$latitude),
                                c(12.590659, 55.679687), 
                                fun = distHaversine)
```




#### Regression #### 



```r
cph_data <- airbnb_clean[,c("price_dkk","index","strict_cancel",
                            "home","cleaning_fee_dkk","superhost", "listing_duration","price_person", "instant", "security_deposit_dkk",
                            "accommodates","minimum_nights" ,"cancellation_policy",
                            "bathrooms", "distance", "dist_centrum")]



mod1 <- lm(log(price_dkk) ~ distance + dist_centrum,data = airbnb_clean )

mod2 <- lm(log(price_dkk) ~  distance + dist_centrum + index +accommodates+ bathrooms + home + cleaning_fee_dkk +superhost+ instant + strict_cancel + security_deposit_dkk   , data = cph_data)

library(stargazer)
stargazer(mod1, mod2,#regression models 
          type = "html", # character vector (eg. "text" / "html" / "latex")
          title = "Linear Regression Model",  # header
          style = "default",  # style (choice see below)
          summary = NULL,  # logical vector: output summary statistics when given data.frame# path and output of file
          out.header = FALSE, # logical vector: should output file contain code-header?
          column.labels = c("Model 1"), # column labels for mod1/mod2
          column.separate = c(1,1),  # how column labels should be assigned (label over sev. columns possible)
          covariate.labels = c("Distance Metro",  # Covariate Labels
                               "Distance Centre (Proxy)",
                               "Review Index",
                               "Accomodates",
                               "Number of Bathrooms",
                               "Apartment (Dummy)",
                               "Cleaning Fee",
                               "Superhost",
                               "Instant Booking"),
          dep.var.caption = "Dep. Var", # Caption (Top) of dependent variable
          star.cutoffs = c(0.05,0.01,0.001),
          dep.var.labels = c("Price per night in DKK"))
```


<table style="text-align:center"><caption><strong>Linear Regression Model</strong></caption>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="2">Dep. Var</td></tr>
<tr><td></td><td colspan="2" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="2">Price per night in DKK</td></tr>
<tr><td style="text-align:left"></td><td>Model 1</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Distance Metro</td><td>-0.00003<sup>***</sup></td><td>-0.00001</td></tr>
<tr><td style="text-align:left"></td><td>(0.00001)</td><td>(0.00001)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Distance Centre (Proxy)</td><td>-0.0001<sup>***</sup></td><td>-0.0001<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.00000)</td><td>(0.00000)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Review Index</td><td></td><td>0.093<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.017)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Accomodates</td><td></td><td>0.111<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.004)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Number of Bathrooms</td><td></td><td>0.214<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.022)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Apartment (Dummy)</td><td></td><td>0.414<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.017)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Cleaning Fee</td><td></td><td>0.0004<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.00003)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Superhost</td><td></td><td>0.030<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.014)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Instant Booking</td><td></td><td>0.004</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.013)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">strict_cancel</td><td></td><td>0.012</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.011)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">security_deposit_dkk</td><td></td><td>0.00001<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.00000)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>6.881<sup>***</sup></td><td>4.910<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.013)</td><td>(0.169)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>7,534</td><td>3,869</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.094</td><td>0.565</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.093</td><td>0.564</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.463 (df = 7531)</td><td>0.329 (df = 3857)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>389.268<sup>***</sup> (df = 2; 7531)</td><td>455.367<sup>***</sup> (df = 11; 3857)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="2" style="text-align:right"><sup>*</sup>p<0.05; <sup>**</sup>p<0.01; <sup>***</sup>p<0.001</td></tr>
</table>


## Limitations

There are some limitations concerning the data used in the analysis. However benficial the provision of data from AirbnbInside.com is, it is clear that there will be a difference between advertised listings and booked listings. The problem is that some of the advertised listings, which is the basis for the InsideAirbnb-dataset, are outdated or created by accident and are thus not booked. I tried to control for this by setting a minimum requirement concerning the number of reviews as well as the verification of the host as a necessary condition. "Scrapers that track updates to host calendars cannot distinguish real bookings from
dates host block for other reasons." @coles2017airbnb

@coles2017airbnb also point to the fact that there is a discrepancy between the price listed and the actual transaction price. The scraping method does not include any corrections for specific discounts, i.e. weekly discount. 




## Final Findings Summarized:

the most expensive neighborhoods.
Most frequent words in summaries show that more hosts talk about the surrounding area rather than the listing itself.
Listings with prices around $200 - 300 get the most reviews, meaning that they are booked most often.
Cancellation policies are fairly spread out, but it doesn’t make a big difference for most people.


Caps on number of short-term rental nights
