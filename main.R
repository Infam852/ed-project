library(shiny)
library(readr)

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
  reservation_status_date=col_date()
)

data <- read_csv(fn, col_types = colTypes)
