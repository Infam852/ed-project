library(shiny)
library(readr)
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)
library(rworldmap)
library(RColorBrewer)


fn = "hotel_bookings.csv"  # source: https://www.kaggle.com/jessemostipak/hotel-booking-demand
colTypes = cols(
  hotel=col_factor(),
  is_canceled=col_logical(),
  lead_time=col_number(),
  arrival_date_year=col_factor(),
  arrival_date_month=col_factor(),
  arrival_date_week_number=col_factor(),
  arrival_date_day_of_month=col_factor(),
  stays_in_weekend_nights=col_number(),
  stays_in_week_nights=col_number(),
  adults=col_number(),
  children=col_number(),
  babies=col_number(),
  meal=col_factor(),
  country=col_factor(),
  market_segment=col_factor(),   # TA - Travel Agents, TO - Tour Operators
  distribution_channel=col_factor(),
  is_repeated_guest=col_logical(),
  previous_cancellations=col_number(),
  previous_bookings_not_canceled=col_number(),
  reserved_room_type=col_factor(),
  assigned_room_type=col_factor(),
  booking_changes=col_number(),
  deposit_type=col_factor(),
  agent=col_factor(),
  company=col_factor(),
  days_in_waiting_list=col_number(),
  customer_type=col_factor(),
  adr=col_number(),
  required_car_parking_spaces=col_number(),
  total_of_special_requests=col_number(),
  reservation_status=col_factor(),
  reservation_status_date=col_date(format="%Y-%m-%d")
)

booking_df <- read_csv(fn, col_types = colTypes)
# group_by(booking_df, country) %>% summarise()
booking_df %>% count(country) -> visit_per_country

booking_df %>% filter(is_canceled == F) -> filtered_df

count(booking_df, country) -> visit_per_country
worldMap <- getMap()
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom","Norway","Switzerland","Ukraine",
                   "Belarus","Serbia","Bosnia and Herz.", "Macedonia", "Albania",
                   "Montenegro", "Moldavia")
indEU <- which(worldMap$NAME%in%europeanUnion)
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  df$country = as.character(worldMap$ISO3.1[i])
  colnames(df) <- list("long", "lat", "region", "country")
  return(df)
})
europeCoords <- do.call("rbind", europeCoords)
# Add some data for each member
value <- sample(x = seq(0,3,by = 0.1), size = length(europeanUnion),
                replace = TRUE)
europeanUnionTable <- data.frame(country = europeanUnion, value = value)
joined <- left_join(x=europeCoords, y=visit_per_country, by="country")
# europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]
# Plot the map
breaks_seq <- seq(0, 10000, by=1000)
ggplot() + geom_polygon(data = joined, aes(x = long, y = lat, group = region, fill = n),
                        colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71)) +
  scale_fill_gradient(name = "Number of visits", low = "#FF0000FF", high = "#FFFF00FF", na.value = "#FFFF00FF", breaks=breaks_seq, limits=c(0, 10000), labels=format(breaks_seq)) +
  theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
    #panel.background = element_rect(fill = NA, colour = NA),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(), axis.title = element_blank(),
    #rect = element_blank(),
    plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))

