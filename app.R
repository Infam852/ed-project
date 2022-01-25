library(shiny)
library(readr)
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)
library(rworldmap)
library(RColorBrewer)
library(ranger)


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
  titlePanel("Eksploracja danych - projekt wizualizacji danych"),
  tabsetPanel(
    tabPanel(
      "Mapa", 
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "visulizationType",
            label = "Wybierz co pokazać",
            choices = c("Liczbę klientów", "Średni dochód")
          ),
          
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
          )
        ),
        mainPanel(
          plotOutput(outputId = "mapPlot", height = MAP_HEIGHT),
          dataTableOutput(outputId = "tablePlot")
        )
      )
    ),
    tabPanel("Wykresy", fluid = TRUE,
       sidebarLayout(
         sidebarPanel(
           actionButton("run", "Trenuj!")),
         mainPanel(
           plotOutput(outputId = "rfPlot"),
         )
       )
    )
  )
)

server <- function(input, output) {
  
  output$mapPlot <- renderPlot({
    df_filtered <- df_booking

    visulizationType <- input$visulizationType
    observeEvent(visulizationType, {
      if (visulizationType == "Średni dochód"){
        updateSliderInput(inputId = "mapMaxValue", min=60, max=150, step=10)
      } else {
        updateSliderInput(inputId = "mapMaxValue", min=0, max=10000, step=1000)
      }
    })

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

    legendTitle <- "Liczba wizyt"
    
    if(visulizationType == "Średni dochód"){
      legendTitle <- "Średni dochód"
      df_filtered %>%
        group_by(country) %>%
        summarize(n=mean(adr, na.rm=T)) -> visit_per_country
      visit_per_country$n[visit_per_country$n < 60] <- 60
      breaks_seq <- seq(60, input$mapMaxValue, by=10)
    } else {
      count(df_filtered, country) -> visit_per_country
      breaks_seq <- seq(0, input$mapMaxValue, by=1000)
    }

    if (input$percentValues){
      visit_per_country %>%
        mutate(n=100*n/sum(visit_per_country$n)) -> visit_per_country
      breaks_seq <- seq(0, 100, by=20)
      legendTitle <- "Procent"
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
    # Plot the map
    ggplot() + geom_polygon(data = joinedEU, aes(x = long, y = lat, group = region, fill = n),
                            colour = "black", size = 0.1) +
      coord_map(xlim = c(-13, 35),  ylim = c(32, 71)) +
      scale_fill_gradient(name = legendTitle, low = "#FF0000FF", high = "#FFFF00FF", na.value = "#FFFF00FF",
                          breaks=breaks_seq, limits=c(breaks_seq[1], breaks_seq[length(breaks_seq)]), labels=format(breaks_seq)) +
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
    visulizationType <- input$visulizationType

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

    if(visulizationType == "Średni dochód"){
      df_filtered %>%
        group_by(country) %>%
        summarize(n=mean(adr, na.rm=T)) -> df_summarized
    } else {
      count(df_filtered, country) -> df_summarized
    }

    df_summarized %>%
      arrange(desc(n)) %>%
      mutate(Kraj=country) %>%
      mutate("Wartość"=n) %>%
      mutate(Procent=round(100*n/sum(visit_per_country$n), 2)) %>%
      select(-n, -country) -> visit_per_country
    visit_per_country
  }, options = list(pageLength=10))

  # second tab
  output$rfPlot <- renderPlot({
    SPLIT_RATIO <- 0.8
    set.seed(123)

    df_booking %>% select(-reservation_status_date,
                          -reservation_status,
                          -arrival_date_year,
                          -arrival_date_month,
                          -arrival_date_week_number,
                          -arrival_date_day_of_month) -> df_filtered
    
    df_filtered<-drop_na(df_filtered)

    # split data
    smp_size <- floor(SPLIT_RATIO * nrow(df_filtered))
    train_ind <- sample(seq_len(nrow(df_filtered)), size = smp_size)
    train <- df_filtered[train_ind, ]
    test <- df_filtered[-train_ind, ]

    observeEvent(input$run, {
      model_rf <- ranger(
        is_canceled ~ .,
        data = train,
        importance='impurity')
      df_filtered$is_canceled <- as.factor(df_filtered$is_canceled)
      
      pred_rf <- predict(model_rf, test)
      test$pred <- pred_rf$predictions
      
      cm_rf <- table(test$is_canceled, pred_rf$predictions)
      
      acc_rf <- sum(test$is_canceled == test$pred) / nrow(test)
      cat("Accurracy: ", acc_rf)
      df_importance <- data.frame(attributes(model_rf$variable.importance),
                                  as.vector(model_rf$variable.importance))
      colnames(df_importance) <- c("name", "importance")
      df_importance$name <- factor(
        df_importance$name,
        levels = df_importance$name[order(df_importance$importance)])
      ggplot(df_importance, aes(x=name, y=importance, fill=importance)) + 
        geom_bar(stat="identity") +
        coord_flip()
    })
  })
}

shinyApp(ui = ui, server = server)

