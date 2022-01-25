library(shiny)
library(readr)
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)
library(rworldmap)
library(RColorBrewer)


MAP_HEIGHT = 720

theme_set(theme_bw())

# !TODO skip unused cols col_skip()
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

worldMap <- getMap()
fn <- "hotel_bookings.csv"  # source: https://www.kaggle.com/jessemostipak/hotel-booking-demand
df_booking <- read_csv(fn, col_types = colTypes)
euCountries <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                 "Czech Rep.","Denmark","Estonia","Finland","France",
                 "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                 "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                 "Portugal","Romania","Slovakia","Slovenia","Spain",
                 "Sweden","United Kingdom","Norway","Switzerland","Ukraine",
                 "Belarus","Serbia","Bosnia and Herz.","Macedonia","Albania",
                 "Montenegro","Moldavia")
euCC <- c("ALB","AUT","BEL","BGR","BIH","BLR","CHE","CYP","CZE","DEU","DNK",
          "ESP","EST","FIN","FRA","GBR","GRC","HRV","HUN","IRL","ITA","LTU",
          "LUX","LVA","MKD","MNE","NLD","NOR","POL","PRT","ROU","SRB","SVK",
          "SVN","SWE","UKR","MLT")

# Dorzucenie pozostałych kraji do mapowania skrótów 
countries <- c("Spain", "Sweden")
acronyms <- c("ESP", "SWE")
countriesMapping <- setNames(as.list(acronyms), countries)

monthMapping <- c(
  "styczeń"="January",
  "luty"="February",
  "marzec"="March",
  "kwiecień"="April",
  "maj"="May",
  "czerwiec"="June",
  "lipiec"="July",
  "sierpień"="August",
  "wrzesień"="September",
  "październik"="October",
  "listopad"="November",
  "grudzień"="December"
)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Eksploracja danych - projekt wizualizacji danych"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    sidebarPanel(
      checkboxInput(
        inputId="isCanceledCheck",
        label="Nie uwzględniaj rezerwacji które zostały wycofane"
      ),
      
      checkboxInput(
        inputId="percentValues",
        label="Przeskaluj względem maksymalnej wartości"
      ),

      sliderInput(
        inputId = "mapMaxValue",
        label = "Dostosuj nacycenie mapy",
        min = 1000,
        max = 10000,
        step = 1000,
        value = 7000
      ),

      selectInput(
        inputId = "roomRequested",
        label = "Rodzaj żądanego pokoju",
        choices = c("dowolny", "A", "B", "C", "D", "E", "F", "G", "H", "L", "H")
      ),

      selectInput(
        inputId = "monthRequested",
        label = "Miesiąc",
        choices = c("cały rok", "styczeń", "luty", "marzec", "kwiecień", "maj",
                    "czerwiec", "lipiec", "sierpień", "wrzesień", "październik",
                    "listopad", "grudzień")
      ),
      fluidRow(
        column(width = 12, verbatimTextOutput("hover_info"))
      )

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      #plotOutput(outputId = "distPlot")
      plotOutput(outputId = "mapPlot", height = MAP_HEIGHT, hover = hoverOpts(id ="plot_hover")),
      dataTableOutput(outputId = "tablePlot")
    )
  )
)

server <- function(input, output) {
  
  output$mapPlot <- renderPlot({
    df_filtered <- df_booking

    # filter data based on user input
    if (input$isCanceledCheck){
      df_filtered %>%
        filter(!is_canceled) -> df_filtered
    }

    roomRequestedType <- input$roomRequested
    if(roomRequestedType != "dowolny"){
      df_filtered %>%
        filter(reserved_room_type == roomRequestedType) -> df_filtered
    }

    monthRequested <- input$monthRequested
    if(monthRequested != "cały rok"){
      df_filtered %>%
        filter(arrival_date_month == monthMapping[monthRequested]) -> df_filtered
    }

    breaks_seq <- seq(0, input$mapMaxValue, by=1000)
    count(df_filtered, country) -> visit_per_country
    legendTitle <- "Liczba wizyt"
    if (input$percentValues){
      visit_per_country %>%
        mutate(n=100*n/sum(visit_per_country$n)) -> visit_per_country
      breaks_seq <- seq(0, 100, by=20)
      legendTitle <- "Procent\nwszystkich\nwizyt"
    }

    worldMap <- getMap()
    indEU <- which(worldMap$NAME%in%euCountries)
    europeCoords <- lapply(indEU, function(i){
      df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
      df$region =as.character(worldMap$NAME[i])
      df$country = as.character(worldMap$ISO3.1[i])
      colnames(df) <- list("long", "lat", "region", "country")
      return(df)
    })
    europeCoords <- do.call("rbind", europeCoords)
    joinedEU <- left_join(x=europeCoords, y=visit_per_country, by="country")
    joinedEU$n <- replace_na(joinedEU$n, 0)
    print(unique(joinedEU$country))
    # Plot the map
    ggplot() + geom_polygon(data = joinedEU, aes(x = long, y = lat, group = region, fill = n),
                            colour = "black", size = 0.1) +
      coord_map(xlim = c(-13, 35),  ylim = c(32, 71)) +
      scale_fill_gradient(name = legendTitle, low = "#FF0000FF", high = "#FFFF00FF", na.value = "#FFFF00FF",
                          breaks=breaks_seq, limits=c(0, breaks_seq[length(breaks_seq)]), labels=format(breaks_seq)) +
      theme(
        #panel.grid.minor = element_line(colour = NA),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), axis.title = element_blank(),
        #rect = element_blank(),
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"),
        legend.key.size = unit(1, 'cm'),
        text = element_text(size=20))
  }, height = MAP_HEIGHT)

  output$tablePlot <- renderDataTable({
    df_filtered <- df_booking
    df_filtered %>%
      filter(country %in% euCC) -> df_filtered

    # filter data based on user input
    if (input$isCanceledCheck){
      df_filtered %>%
        filter(!is_canceled) -> df_filtered
    }

    roomRequestedType <- input$roomRequested
    if(roomRequestedType != "dowolny"){
      df_filtered %>%
        filter(reserved_room_type == roomRequestedType) -> df_filtered
    }

    monthRequested <- input$monthRequested
    if(monthRequested != "cały rok"){
      df_filtered %>%
        filter(arrival_date_month == monthMapping[monthRequested]) -> df_filtered
    }

    breaks_seq <- seq(0, input$mapMaxValue, by=1000)
    df_filtered %>% count(country) %>% arrange(desc(n)) -> visit_per_country
    visit_per_country
  }, options = list(pageLength=10))
  
  
  output$hover_info <- renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      hlat_min <- hover$y - 0.05
      hlong_min <- hover$x - 0.5
      hlong_max <- hover$x + 0.5
      hcountry <- worldMap[worldMap$LON < hlong_max &
                             worldMap$LON > hlong_min &
                             worldMap$LAT > hlat_min
                           ,]
      foundCountry <- toString(hcountry$NAME[1])
      if(foundCountry != "NA"){
        print(foundCountry)
        # Wypisanie statystyk
        acronym <- countriesMapping[foundCountry]
        if(acronym != 'NULL'){
          country_stats <- df_booking[df_booking$country == acronym, ]
          print("Ilość wybranego rodzaju pobytu:")
          print(table(country_stats$hotel))
          print("Ilość anulowanych rezerwacji:")
          print(table(country_stats$is_canceled))
          print("Ilość dokonanych rezerwacji w danych latach:")
          print(table(country_stats$arrival_date_year))
          print("Ilość dokonanych rezerwacji w danych miesiącach:")
          print(table(country_stats$arrival_date_month))
          print("Ilość wybranych typów zakwaterowania:")
          print(table(country_stats$reserved_room_type))
        }
      }
      else{
        print("Nie wykryłem kraju :/")
      }
    }
  })
}

shinyApp(ui = ui, server = server)
