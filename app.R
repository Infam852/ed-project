library(shiny)
library(shinythemes)
library(readr)
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)
library(rworldmap)
library(RColorBrewer)
library(ranger)
library(crayon)
library(plotly)


MAP_HEIGHT = 720

theme_set(theme_bw())
shinyOptions(plot.autocolors=TRUE)

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
euCountries <- c("Albania", "Austria","Belgium","Bulgaria","Belarus","Croatia","Cyprus",
                 "Czech Rep.","Denmark","Estonia","Finland","France",
                 "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                 "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                 "Portugal","Romania","Slovakia","Slovenia","Spain",
                 "Sweden","United Kingdom","Norway","Switzerland","Ukraine",
                 "Serbia","Bosnia and Herz.","Macedonia",
                 "Montenegro","Moldavia")
euCC <- c("ALB","AUT","BEL","BGR","BLR","HRV","CYP","CZE","DNK","EST",
          "FIN","FRA","DEU", "GRC", "HUN","IRL","ITA","LVA","LTU","LUX","MAL",
          "NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE","GBR","NOR","CHF",
          "UKR","SRB","BIH","MAC", "MTG", "MOL")

countriesMapping <- setNames(as.list(euCC), euCountries)

roomMapping <- c("A", "B", "C", "D", "E", "F", "G", "L", "P")

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
smp_size_reduce <- floor(0.2 * nrow(df_booking))
reduced_idx <- sample(seq_len(nrow(df_booking)), size = smp_size_reduce)
df_reduced <- df_booking[reduced_idx, ]


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  fluidPage(theme = shinytheme("superhero"),
  tags$head(
    tags$style(
    HTML('
      #sidebar {
        background-color: transparent;
      }')
  )),
  h1("Eksploracja danych - wizualizacja danych", style = "color: #F5F36C;"),
  tabsetPanel(
    tabPanel(
      "Mapa", 
      sidebarLayout(
        sidebarPanel(id="sidebar",
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
            label = "Dostosuj nasycenie mapy",
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
        mainPanel(
          plotOutput(outputId = "mapPlot", height = MAP_HEIGHT, hover = hoverOpts(id ="plot_hover")),
          dataTableOutput(outputId = "tablePlot")
        )
      )
    ),
    tabPanel("Wykresy", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(id="sidebar",
          fluidRow(
            column(width = 6),
            h4("Dane wejściowe do wykresów kołowych:")
          ),
          
          fluidRow(
            selectInput(
              inputId = "circleCountry",
              label = "Kraj",
              choices = c(euCountries)
            ),
            selectInput(
              inputId = "circelMonth",
              label = "Miesiąc",
              choices = c("wszystkie", monthMapping)
            ),
            selectInput(
              inputId = "circelYear",
              label = "Rok",
              choices = c("wszystkie", "2015", "2016", "2017")
            )
          )
        ),
        mainPanel(
          p(),
          plotlyOutput(outputId = "plotCirclesCountry"),
          p(),
          plotlyOutput(outputId = "plotCirclesYear"),
          p(),
          plotlyOutput(outputId = "plotCirclesMonth")
        )
      )
    ),
    tabPanel("Lasy losowe", fluid = TRUE,
       sidebarLayout(
         sidebarPanel(id="sidebar",
           checkboxInput(
             inputId="random_seed",
             label="Losowe ziarno",
             value=T
           ),
           br(),
           strong("Zaznacz cechy których chcesz użyć do trenowania"),
           checkboxInput(
             inputId="deposit_type",
             label="deposit_type",
             value=T
           ),
           checkboxInput(
             inputId="lead_time",
             label="lead_time",
             value=T
           ),
           checkboxInput(
             inputId="country",
             label="country",
             value=T
           ),
           checkboxInput(
             inputId="adr",
             label="adr",
             value=T
           ),
           checkboxInput(
             inputId="agent",
             label="agent",
             value=T
           ),
           checkboxInput(
             inputId="stays_in_weekend_nights",
             label="stays_in_weekend_nights",
             value=T
           ),
           checkboxInput(
             inputId="adults",
             label="adults",
             value=T
           ),
           checkboxInput(
             inputId="children",
             label="children",
             value=T
           ),
           checkboxInput(
             inputId="babies",
             label="babies",
             value=T
           ),
           checkboxInput(
             inputId="market_segment",
             label="market_segment",
             value=T
           ),
           checkboxInput(
             inputId="distribution_channel",
             label="distribution_channel",
             value=T
           ),
           checkboxInput(
             inputId="is_repeated_guest",
             label="is_repeated_guest",
             value=T
           ),
           checkboxInput(
             inputId="previous_cancellations",
             label="previous_cancellations",
             value=T
           ),
           checkboxInput(
             inputId="booking_changes",
             label="booking_changes",
             value=T
           ),
           checkboxInput(
             inputId="hotel",
             label="hotel",
             value=T
           ),
           checkboxInput(
             inputId="customer_type",
             label="customer_type",
             value=T
           ),
           checkboxInput(
             inputId="required_car_parking_spaces",
             label="required_car_parking_spaces",
             value=T
           ),
           checkboxInput(
             inputId="total_of_special_requests",
             label="total_of_special_requests",
             value=T
           ),
           
           actionButton("run", "Trenuj!")),
         mainPanel(
           plotOutput(outputId = "rfPlot", height = MAP_HEIGHT),
           dataTableOutput(outputId = "rfResults")
         )
       )
      )
    )
  )
)

server <- function(input, output) {
  thematic::thematic_shiny(font = "auto")
  
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
      visit_per_country$n[visit_per_country$n < 60] <- 61
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
        panel.border = element_blank(),
        #rect = element_blank(),
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"),
        legend.key.size = unit(1, 'cm'),
        text = element_text(size=20))
  }, height = MAP_HEIGHT)
  
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
          print("Ilość wszystkich rezerwacji:")
          cat(blue(nrow(country_stats),"\n"))
          print("Ilość wybranego rodzaju pobytu:")
          print(table(country_stats$hotel))
          print("Ilość procentowa anulowanych rezerwacji:")
          procent <- table(country_stats$is_canceled)["TRUE"] / table(country_stats$is_canceled)["FALSE"]
          cat(blue(procent, "%\n"))
          print("Ilość dokonanych rezerwacji w danych latach:")
          print(table(country_stats$arrival_date_year))
          print("Ilość dokonanych rezerwacji w danych miesiącach:")
          print(table(country_stats$arrival_date_month)[monthMapping])
          print("Ilość wybranych typów zakwaterowania:")
          print(table(country_stats$reserved_room_type)[roomMapping])
        }
      }
      else{
        print("Nie wykryłem kraju :/")
      }
    }
  })

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
      mutate(Procent=round(100*n/sum(df_summarized$n), 2)) %>%
      select(-n, -country) -> df_summarized
    df_summarized
  }, options = list(pageLength=10))

  observeEvent(
    input$run, {
      output$rfPlot <- renderPlot({
        SPLIT_RATIO <- 0.8
        if (input$random_seed){
          set.seed(sample.int(10000, 1))
        } else{
          set.seed(1234)
        }
        # drop always
        df_reduced %>% select(-reservation_status_date,
                              -reservation_status,
                              -arrival_date_year,
                              -arrival_date_month,
                              -arrival_date_week_number,
                              -arrival_date_day_of_month) -> df_filtered
        
        # filter by user input
        if(!input$deposit_type){
          df_filtered %>% select(-deposit_type) -> df_filtered
        }
        if(!input$lead_time){
          df_filtered %>% select(-lead_time) -> df_filtered
        }
        if(!input$country){
          df_filtered %>% select(-country) -> df_filtered
        }
        if(!input$adr){
          df_filtered %>% select(-adr) -> df_filtered
        }
        if(!input$agent){
          df_filtered %>% select(-agent) -> df_filtered
        }
        if(!input$stays_in_weekend_nights){
          df_filtered %>% select(-stays_in_weekend_nights) -> df_filtered
        }
        if(!input$adults){
          df_filtered %>% select(-adults) -> df_filtered
        }
        if(!input$children){
          df_filtered %>% select(-children) -> df_filtered
        }
        if(!input$babies){
          df_filtered %>% select(-babies) -> df_filtered
        }
        if(!input$market_segment){
          df_filtered %>% select(-market_segment) -> df_filtered
        }
        if(!input$distribution_channel){
          df_filtered %>% select(-distribution_channel) -> df_filtered
        }
        if(!input$is_repeated_guest){
          df_filtered %>% select(-is_repeated_guest) -> df_filtered
        }
        if(!input$previous_cancellations){
          df_filtered %>% select(-previous_cancellations) -> df_filtered
        }
        if(!input$booking_changes){
          df_filtered %>% select(-booking_changes) -> df_filtered
        }
        if(!input$hotel){
          df_filtered %>% select(-hotel) -> df_filtered
        }
        if(!input$customer_type){
          df_filtered %>% select(-customer_type) -> df_filtered
        }
        if(!input$required_car_parking_spaces){
          df_filtered %>% select(-required_car_parking_spaces) -> df_filtered
        }
        if(!input$total_of_special_requests){
          df_filtered %>% select(-total_of_special_requests) -> df_filtered
        }
        df_filtered<-drop_na(df_filtered)
        
        # split data
        smp_size <- floor(SPLIT_RATIO * nrow(df_filtered))
        train_ind <- sample(seq_len(nrow(df_filtered)), size = smp_size)
        train <- df_filtered[train_ind, ]
        test <- df_filtered[-train_ind, ]

        model_rf <- ranger(
          is_canceled ~ .,
          data = train,
          importance='impurity')
        df_filtered$is_canceled <- as.factor(df_filtered$is_canceled)
        
        pred_rf <- predict(model_rf, test)
        test$pred <- pred_rf$predictions
        
        
        df_importance <- data.frame(attributes(model_rf$variable.importance),
                                    as.vector(model_rf$variable.importance))
        colnames(df_importance) <- c("name", "importance")
        df_importance$name <- factor(
          df_importance$name,
          levels = df_importance$name[order(df_importance$importance)])
        
        output$rfResults <- renderDataTable({
          cm_rf <- table(test$is_canceled, pred_rf$predictions)
          acc_rf <- sum(test$is_canceled == test$pred) / nrow(test)
          prec_rf <- cm_rf[2, 2] / (cm_rf[2, 2] + cm_rf[2, 1])
          recall_rf <- cm_rf[2, 2] / (cm_rf[2, 2] + cm_rf[1, 2])
          cat("Accurracy: ", acc_rf)
          result_df = data.frame(
            "Dokładność"=acc_rf,
            "Precyzja"=prec_rf,
            "Czułość"=recall_rf
          )
          result_df
        })

        ggplot(df_importance, aes(x=name, y=importance, fill=importance)) + 
          geom_bar(stat="identity") +
          xlab("Cecha") +
          ylab("Istnotność") +
          coord_flip() +
          theme(axis.text = element_text(size = 16),
                axis.title = element_text(size = 18),
                legend.key.height= unit(2, 'cm'),
                legend.key.width= unit(1, 'cm'))
      }, height = MAP_HEIGHT)
    }
  )
  output$plotCirclesCountry <- renderPlotly({
    font_layout <- list(
      family = "Open Sans",
      size = 12,
      color = 'white')
    
    if(!input$circleCountry == "wybierz"){
      acronym <- countriesMapping[input$circleCountry]
      country_data <- df_booking[df_booking$country == acronym, ]
      country_data <- country_data[country_data["is_canceled"] == "FALSE", ]
      pie_data <- table(country_data$arrival_date_month)[monthMapping]
      pie_data <- data.frame("months"=rownames(pie_data), pie_data)
      data <- pie_data[,c('months', 'Freq')]
      # print(data)
      fig <- plot_ly(data, labels = ~months, values = ~Freq, 
                     textposition = 'inside', textinfo = 'label+percent')
      fig <- fig %>% add_pie(hole = 0.6)
      fig <- fig %>% layout(title = paste('Ilość wszystkich zrealizowanych rezerwacji w danych miesiącach dla', input$circleCountry),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            autosize = F, width = 1000, height = 400, paper_bgcolor='transparent', 
                            font=font_layout)
      print(fig)
    }
  })
  output$plotCirclesYear <- renderPlotly({
    font_layout <- list(
      family = "Open Sans",
      size = 12,
      color = 'white')
    if((input$circelMonth == "wszystkie") && (input$circelYear == "wszystkie")){
      # acronym <- countriesMapping[input$circleCountry]
      country_data <- df_booking
      country_data <- country_data[country_data["is_canceled"] == "FALSE", ]
      pie_data <- table(country_data$country)
      pie_data <- data.frame("countries"=rownames(pie_data), pie_data)
      data <- pie_data[,c('countries', 'Freq')]
      # print(data)
      fig1 <- plot_ly(data, labels = ~countries, values = ~Freq, 
                      textposition = 'inside', textinfo = 'label+percent')
      fig1 <- fig1 %>% add_pie(hole = 0.6)
      fig1 <- fig1 %>% layout(title = paste('Ilość rezerwacji w danych krajach w wybranym okresie'),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            autosize = F, width = 1000, height = 400, paper_bgcolor='transparent', 
                            font=font_layout)
      print(fig1)
    }
    else if((input$circelMonth == "wszystkie") && (!input$circelYear == "wszystkie")){
      country_data <- df_booking
      country_data <- country_data[country_data["is_canceled"] == "FALSE", ]
      country_data <- country_data[country_data["arrival_date_year"] == input$circelYear, ]
      pie_data <- table(country_data$country)
      pie_data <- data.frame("countries"=rownames(pie_data), pie_data)
      data <- pie_data[,c('countries', 'Freq')]
      # print(data)
      fig1 <- plot_ly(data, labels = ~countries, values = ~Freq, 
                      textposition = 'inside', textinfo = 'label+percent')
      fig1 <- fig1 %>% add_pie(hole = 0.6)
      fig1 <- fig1 %>% layout(title = paste('Ilość rezerwacji w danych krajach w okresie', input$circelMonth, input$circelYear),
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              autosize = F, width = 1000, height = 400, paper_bgcolor='transparent', 
                              font=font_layout)
      print(fig1)
    }
    else if((!input$circelMonth == "wszystkie") && (input$circelYear == "wszystkie")){
      country_data <- df_booking
      country_data <- country_data[country_data["is_canceled"] == "FALSE", ]
      country_data <- country_data[country_data["arrival_date_month"] == input$circelMonth, ]
      pie_data <- table(country_data$country)
      pie_data <- data.frame("countries"=rownames(pie_data), pie_data)
      data <- pie_data[,c('countries', 'Freq')]
      # print(data)
      fig1 <- plot_ly(data, labels = ~countries, values = ~Freq, 
                      textposition = 'inside', textinfo = 'label+percent')
      fig1 <- fig1 %>% add_pie(hole = 0.6)
      fig1 <- fig1 %>% layout(title = paste('Ilość rezerwacji w danych krajach w okresie', input$circelMonth),
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              autosize = F, width = 1000, height = 400, paper_bgcolor='transparent', 
                              font=font_layout)
      print(fig1)
    }
    else if((!input$circelMonth == "wszystkie") && (!input$circelYear == "wszystkie")){
      country_data <- df_booking
      country_data <- country_data[country_data["is_canceled"] == "FALSE", ]
      country_data <- country_data[country_data["arrival_date_year"] == input$circelYear, ]
      country_data <- country_data[country_data["arrival_date_month"] == input$circelMonth, ]
      pie_data <- table(country_data$country)
      pie_data <- data.frame("countries"=rownames(pie_data), pie_data)
      data <- pie_data[,c('countries', 'Freq')]
      #print(data)
      if(sum(data$Freq) != 0 ){
        fig1 <- plot_ly(data, labels = ~countries, values = ~Freq, 
                        textposition = 'inside', textinfo = 'label+percent')
        fig1 <- fig1 %>% add_pie(hole = 0.6)
        fig1 <- fig1 %>% layout(title = paste('Ilość rezerwacji w danych krajach w okresie', input$circelMonth, input$circelYear),
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                autosize = F, width = 1000, height = 400, paper_bgcolor='transparent', 
                                font=font_layout)
        print(fig1)
      }
      else{
        fig1 <- plot_ly(data, labels = ~countries, values = ~Freq, 
                        textposition = 'inside', textinfo = 'label+percent')
        fig1 <- fig1 %>% add_pie(hole = 0.6)
        fig1 <- fig1 %>% layout(title = paste('Brak danych dla', input$circelMonth, input$circelYear),
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                autosize = F, width = 1000, height = 400, paper_bgcolor='transparent', 
                                font=font_layout)
        print(fig1)
      }
    }
  })
}

shinyApp(ui = ui, server = server)

