######## Ecological Modelling #######

#install.packages("dismo")
library(dismo)
library(dplyr)
library(ggplot2)

hood_spp <- read.csv("hooded_warb_locations.csv")
str(hood_spp)

envt_infl <- stack("env_current.grd")
envt_forcast <- stack("env_forecast.grd")

plot(envt_infl$tmin,main="Min Temp")
plot(envt_infl$precip, main="Precipitation of the Location")

hood_location <- hood_spp %>% select(lon,lat)
hood_enviro <- envt_infl %>% extract(hood_location)

hood_data <- cbind(hood_spp,hood_enviro)
head(hood_data)

hood_scatter <- ggplot(hood_data, aes(tmin, precip, color= present))+
  geom_point()
hood_scatter

logistic_model <- glm(present~tmin+precip, family = "binomial", data = hood_data)
summary(logistic_model)

presence_data <- filter(hood_data, present==1)
absence_data <- filter(hood_data, present==0)
evaluation <- evaluate(presence_data, absence_data, logistic_model)
evaluation
plot(evaluation, 'ROC')

prediction_area <- predict(envt_infl, 
                           logistic_model,
                           type= "response")
plot(prediction_area,ext=extent(-140,-50, 25, 60))
points(presence_data[c("lon", "lat")],pch="+", cex= 0.5)

plot(prediction_area > 0.5,ext=extent(-140,-50, 25, 60))
tre <- threshold(evaluation, stat="prevalence")
plot(prediction_area > tre,ext=extent(-140,-50, 25, 60))
points(presence_data[c("lon", "lat")],pch="+", cex= 0.5)


forecast <- predict(envt_forcast, 
                    logistic_model,
                    type="response")
plot(forecast,ext=extent(-140,-50, 25, 60))
plot(forecast > tre,ext=extent(-140,-50, 25, 60))


plot(forecast-prediction_area,ext=extent(-140,-50, 25, 60))






