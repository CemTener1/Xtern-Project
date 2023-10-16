library(ggmap)
library(ggalt)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)

mydata = read.csv("/Users/newyorker/Desktop/food truck.csv")
api_key = #Enter your API key
head(mydata)

#Initial data wrangling- Extact the columns you need and rename them
food <- mydata %>%
  select(title,rating,category,address,rating,website,saturday,sunday,latitude,longitude) %>%
  filter(rating > 4, #I do not want to take my colleagues to plaeces with low ratings
         category != "Caterer", category != "Grocery store") %>%
  reframe(Name = title,
          Address = address,
          Rating = rating,
          Website = website,
          saturday_hour = saturday,
          sunday_hour = sunday,
          Cuisine = category,
          lat = latitude,
          long = longitude)

#Near Indianapolis
lat1 = 39.63281260759963
long1 = -86.32364964238258

lat2 = 39.92147128788137
long2 = -86.05929112265731

food_final <-food %>%
  filter(lat != "NA",
         between(lat, lat1,lat2),
         between(long, long1,long2))
head(food_final)

#Conenct to Google API
ggmap::register_google(key = api_key, write = TRUE)
get_googlemap(center = "Indianapolis") %>% ggmap()

#Create a df containing the potential food truck locations
point_df <- round(data.frame(
  x = jitter(food_final$long, amount = 0.05),
  y = jitter(food_final$lat, amount = 0.05)
), digits = 2)

#map of Indy and potential food trucks
get_googlemap(center = "Indianapolis", zoom = 11, markers = point_df, scale = 2) %>% ggmap() %>% + labs(
  title = "Food Trucks in Indianapolis",
  caption = "Circled locations will be considered for foodie plan ") + 
  xlab("Longitude") + ylab("Latitude") + 
  geom_encircle(aes(x=long, y=lat), data = food_final, size = 2, color = "blue")

food_final

#open on saturdays
sat_food <- food_final %>%
  filter(saturday_hour != "Closed", 
         saturday_hour != "")

#Saturday program
#First location the Grub house- not a food truck, a small meeting before the event - 9am
#Kone Heads FoodTruck 1pm
#FELIX TACOS TRUCK 7pm

#put all these together
sat_plan <- sat_food %>%
  select(Name, Address, Cuisine, lat,long) %>%
  filter(Name %in% c("The Grub House","Kone Heads FoodTruck","FELIX TACOS TRUCK") )

#put them in order
sat_plan[c(1, 3), ] <- sat_plan[c(3, 1), ]
sat_plan[c(2, 3), ] <- sat_plan[c(3, 2), ]

#add the starting address information to the df
starting <- data.frame(
  Name = "UIPUI",
  Address = "420 University Blvd, Indianapolis, IN 46202",
  Cuisine = NA,
  lat = 39.77445673633859,
  long = -86.1762176846559
)
sat_plan<- bind_rows(starting, sat_plan)

sat_plan <- sat_plan %>%
  mutate(Distance_KM = 0)

for (i in 2:nrow(sat_plan)) {
  distance <- distVincentySphere(sat_plan[i - 1, c("long", "lat")], sat_plan[i, c("long", "lat")]) / 1000
  sat_plan$Distance_KM[i] <- distance
}

sat_plan

sat_plan_final <- sat_plan %>%
  slice(-1) %>%
  select(Name, Address, Cuisine, Distance_KM) %>%
  mutate(Time = c("9 am", "2 pm", "8pm"))

sat_plan_final


#Sunday Program
#Start the day with hotdogs - Garcia's Hot Dogs - 10.30 am
#King gyros food truck - 2pm
#Quick bite 8pm

sun_food <- food_final %>%
  filter(sunday_hour != "Closed", 
         sunday_hour != "")

sun_plan <- sun_food %>%
  select(Name, Address, Cuisine, lat,long) %>%
  filter(Name %in% c("Garcia's Hot Dogs","King gyros food truck","Quick bite") )

sun_plan[c(1, 3), ] <- sun_plan[c(3, 1), ]

starting <- data.frame(
  Name = "UIPUI",
  Address = "420 University Blvd, Indianapolis, IN 46202",
  Cuisine = NA,
  lat = 39.77445673633859,
  long = -86.1762176846559
)
sun_plan<- bind_rows(starting, sun_plan)

sat_plan <- sat_plan %>%
  mutate(Distance_KM = 0)


for (i in 2:nrow(sun_plan)) {
  distance <- distVincentySphere(sun_plan[i - 1, c("long", "lat")], sun_plan[i, c("long", "lat")]) / 1000
  sun_plan$Distance_KM[i] <- distance
}
sun_plan

sun_plan_final <- sun_plan %>%
  slice(-1) %>%
  select(Name, Address, Cuisine, Distance_KM) %>%
  mutate(Time = c("10.30 am", "1 pm", "7pm"))

sun_plan_final

#plot the routes
#Plot for Saturday
point_df2 <- data.frame(
  x = sat_plan$long,
  y = sat_plan$lat)

map <- get_googlemap(center = "Indianapolis", zoom = 11, markers = point_df2, scale = 2) 
m<-ggmap(map)

m+ geom_path(
  aes(x = x, y = y), color = "blue", size = 1, data = point_df2
) + labs(
  title = "Foodie Travel plan for Saturday") + xlab("Longitude") + ylab("Latitude")

#Plot for Sunday
point_df3 <- data.frame(
  x = sun_plan$long,
  y = sun_plan$lat)
map2<- get_googlemap(center = "Indianapolis", zoom = 11, markers = point_df3, scale = 2) 

m2<-ggmap(map2)

m2+ geom_path(
  aes(x = x, y = y), color = "blue", size = 1, data = point_df3
)+ labs(
  title = "Foodie Travel plan for Saturday") + xlab("Longitude") + ylab("Latitude")

#Add travel method for saturday
sat_plan_final$transport_type = ifelse(sat_plan_final$Distance_KM > .6,"Car","Walk")
sun_plan_final$transport_type = ifelse(sun_plan_final$Distance_KM > .6,"Car","Walk")
sun_plan_final
#Update the df for transport and travel time - saturday
sat_plan_final$travel_time = NA
sat_plan_final

#Calculate travel time based on coordinates and google api estimate
for (i in 2:nrow(sat_plan)) {
  time <- google_distance(
    origin = c(sat_plan[i -1, "lat"], sat_plan[i -1, "long"]),
    destination = c(sat_plan[i, "lat"], sat_plan[i, "long"]),
    key = api_key,  # Replace with your actual API Key
    mode = "driving"
  )
  print(time$rows)
}

#Insert the estimates into the df
for (i in 1:3){
  time = c("11 mins", "20 mins", "22 mins")
  sat_plan_final$travel_time[i] = time[i]
}

sat_plan_final
#Update the df for transport and travel time -sunday

sun_plan_final$travel_time = NA
sun_plan

#Calculate travel time based on coordinates and google api estimate

for (i in 2:nrow(sun_plan)) {
  time <- google_distance(
    origin = c(sun_plan[i -1, "lat"], sun_plan[i -1, "long"]),
    destination = c(sun_plan[i, "lat"], sun_plan[i, "long"]),
    key = api_key,  # Replace with your actual API Key
    mode = "driving"
  )
  print(time$rows)
}

#Insert the estimates into the df
for (i in 1:3){
  time = c("13 mins", "18 mins", "17 mins")
  sun_plan_final$travel_time[i] = time[i]
}
sun_plan_final

#Further explaration of the data

food_analysis <- mydata %>%
  select(title,rating,category,address,rating,website,saturday,sunday,latitude,longitude) %>%
  filter(category != "Caterer", category != "Grocery store") %>%
  reframe(Name = title,
          Address = address,
          Rating = rating,
          Website = website,
          saturday_hour = saturday,
          sunday_hour = sunday,
          Cuisine = category,
          lat = latitude,
          long = longitude)


# Source: Frequency table
custom_palette <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
  "#bcbd22", "#17becf", "#393b79", "#5254a3", "#6b6ecf", "#9c9ede", "#637939", "#8ca252",
  "#b5cf6b", "#8c6d31", "#bd9e39", "#e7ba52", "#ad494a", "#d6616b"
)

df <- as.data.frame(table(food_analysis$Cuisine))
theme_set(theme_classic())
colnames(df) <- c("Cuisine", "freq")

pie <- ggplot(df, aes(x = "", y=freq, fill = factor(Cuisine))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  scale_fill_manual(values = custom_palette) +
  labs(fill="Cuisine", 
       title="Food Trucks in Indianapolis"
  )

pie + coord_polar(theta = "y", start=0) + theme_wsj()

avg_rating_by_cuisine2 <- food_analysis %>%
  group_by(Cuisine) %>%
  summarize(avg_rating = mean(Rating)) %>%
  filter(!is.na(avg_rating))

ggplot(avg_rating_by_cuisine2, aes(x=Cuisine, y=avg_rating)) + 
  geom_bar(stat="identity", width=.5, fill="#d62728") + 
  labs(title="Rating by Cuisine")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  ylab("Average Rating")























