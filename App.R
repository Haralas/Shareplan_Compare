library(shiny)
library(quantmod)
library(dplyr)
library(ggplot2)
library(lubridate)
library(highcharter)
library(DT)
library(data.table)

ticker             = "CS.PA"
start_date         = "1990-01-01"
end_date           = "2025-09-01"
investment_period  = 5
initial_investment = 100
decote_classique   = 0.2
decote_garantie    = 0.064

read_data <<- FALSE
source('Functions.R')


ui <- fluidPage(
  titlePanel("Comparaison des Formules Shareplan 2025"),
  div(
    style = "background-color:#f8f9fa; padding:15px; border-radius:8px; margin-bottom:20px; 
           font-size:15px; color:#2C3E50; border:1px solid #ddd;",
    HTML("<b>ℹ️ Notes méthodologiques :</b><br>
       • Plutôt que de prendre un prix de référence, on prend le <b>dernier cours disponible à date</b>.<br>
       • Pour chaque jour, la valeur du cours est définie comme la <b>moyenne de l'ouverture, clôture, max et min</b> du jour.<br>
       • Les <b>splits</b> d'actions sont déjà pris en compte dans les données de base (Yahoo Finance).<br>
       • La comparaison historique des payoffs (partie 'Payoff pour plusieurs dates de départ') <b>n’est pas dynamique</b> en fonction des paramètres de l’UI : 
         elle se base sur les paramètres officiels du Shareplan pour un investissement de <b>100 €</b>.<br>
       • Le script complet est accessible sur GitHub : 
         <a href='https://github.com/Haralas/Shareplan_Compare' target='_blank'>
         https://github.com/Haralas/Shareplan_Compare</a> (branche master).")
  ),
  
  sidebarLayout(
    sidebarPanel(width = 2,
                 textInput("ticker", "Ticker (dummy input)", value = ticker),
                 dateInput("global_start","Date début graphique", value = "2007-01-01" ,
                           min = start_date, max = ymd(end_date) - years(5)),
                 numericInput("investment_period", "Durée détention (années)", value = investment_period, min = 1),
                 numericInput("initial_investment", "Capital initial (€)", value = initial_investment, min = 1),
                 numericInput("decote_classique", "Décote formule classique", value = decote_classique, min = 0, max = 1, step = 0.01),
                 numericInput("decote_garantie", "Décote formule garantie", value = decote_garantie, min = 0, max = 1, step = 0.001),
                 actionButton("run", "Lancer la comparaison historique"),
    ),
    mainPanel(width = 10,
              fluidRow(
                column(12,
                       wellPanel(
                         h3("📈 Payoff pour la date de départ spécifiée"),
                         tabPanel("Graph - Comparaison", highchartOutput("hc_full", height = "600px")),
                         tabPanel("Graph - Classique", highchartOutput("hc_classique", height = "600px"))
                       )
                )
              ),
              fluidRow(
                column(12,
                       wellPanel(
                         h3("💰 Payoff pour plusieurs dates de départ"),
                         tabPanel("Graph - Payoffs hebdos", highchartOutput("hc_payoffs", height = "600px")),
                         tabPanel("Table - Payoffs", DTOutput("tbl_payoffs"))
                       )
                )
              )
    )
  )
)

server <- function(input, output, session) {
  
  output$hc_payoffs <- renderHighchart({
    comp <- comp_data
    hc <- highchart() %>%
      hc_add_series(comp, "line", hcaes(x = StartDate, y = Classique_TotalValue), name = "Classique") %>%
      hc_add_series(comp, "line", hcaes(x = StartDate, y = Garantie_Employee),   name = "Garantie (employé)") %>%
      hc_add_series(comp, "line", hcaes(x = StartDate, y = Garantie_TotalValue), name = "Garantie (total)", dashStyle = "ShortDot") %>%
      hc_title(text = "Payoff final en fonction de la date de départ (pas annuel)") %>%
      hc_yAxis(title = list(text = "Valeur (€)")) %>%
      hc_xAxis(type = "datetime") %>%
      hc_tooltip(shared = TRUE, pointFormat = "{series.name}: {point.y:.2f} €")
    hc
  })
  
  
  simulations <- eventReactive(input$run, {
    list(
      simulation_1 = simulate_investment_immediate(stock_data, dividend_data,
                                                   initial_investment = input$initial_investment, input$global_start, input$investment_period, 
                                                   reinvest = TRUE,
                                                   decote   = input$decote_classique),
      
      simulation_2 = simulate_investment_immediate(stock_data, dividend_data,
                                                   initial_investment = input$initial_investment, input$global_start, input$investment_period,
                                                   reinvest = TRUE, decote   = 0),
      
      simulation_3 = simulate_investment_immediate(stock_data, dividend_data,
                                                   initial_investment = input$initial_investment, input$global_start, input$investment_period,
                                                   reinvest = FALSE, decote   = 0),
      
      simulate_investment_garantie_ =  simulate_investment_garantie(stock_data, dividend_data,
                                                                    initial_investment = input$initial_investment, input$global_start, input$investment_period, 
                                                                    reinvest = TRUE,
                                                                    decote   = input$decote_garantie)
      
    )
  })
  
  output$hc_full <- renderHighchart({
    sims <- simulations()  
    
    hc <- highchart() %>%
      hc_add_series(sims$simulate_investment_garantie_, "line", hcaes(x = Date, y = TotalValueTotal),
                    name = "Garantie - Payoff total (salarie + banque)") %>%
      hc_add_series(sims$simulate_investment_garantie_, "line", hcaes(x = Date, y = TotalValueEmployee),
                    name = "Garantie - Payoff salarie") %>%
      hc_add_series(sims$simulate_investment_garantie_, "line", hcaes(x = Date, y = TotalValueTotal - TotalValueEmployee),
                    name = "Garantie - Payoff banque (decote + dividendes)", dashStyle = "ShortDot") %>%
      hc_add_series(sims$simulation_1, "line", hcaes(x = Date, y = TotalValue),
                    name = "Classique - Valeur totale (réinvesti + décote)") %>%
      hc_title(text = "Évolution de la valeur totale et des changements d’actions") %>%
      hc_yAxis(title = list(text = "Valeur (€)")) %>%
      hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
      hc_legend(enabled = TRUE, layout = "vertical", align = "right", verticalAlign = "top",
                borderWidth = 1, backgroundColor = "#FFFFFF") %>%
      hc_tooltip(shared = TRUE,
                 pointFormat = "Valeur: {point.y:.2f} €")
    hc
    
  })
  
  output$hc_classique <- renderHighchart({
    sims <- simulations()  
    
    shares_changes <- sims$simulation_1 %>%
      mutate(Change = Shares != lag(Shares, default = first(Shares))) %>%
      filter(Change) %>%
      select(Date, Shares, TotalValue)

    shares_changes = rbind(sims$simulation_1[1,c('Date','Shares','TotalValue')], shares_changes)

    plot_lines <- lapply(1:nrow(shares_changes), function(i) {
      list(
        value = datetime_to_timestamp(as.POSIXct(shares_changes$Date[i])),
        color = "#7cb5ec",
        width = 2,
        dashStyle = "ShortDash",
        label = list(
          text = paste0("Actions: ", round(shares_changes$Shares[i], 2)),
          rotation = 0,
          align = "left",
          style = list(color = "#7cb5ec", fontSize = "10px")
        )
      )
    })

    # graphique Highcharter
    hc <- highchart() %>%
      hc_add_series(sims$simulation_1, "line", hcaes(x = Date, y = TotalValue),
                    name = "Valeur totale (réinvesti + décote)") %>%
      hc_add_series(sims$simulation_2, "line", hcaes(x = Date, y = TotalValue),
                    name = "Valeur totale (réinvesti + sans décote)") %>%
      hc_add_series(sims$simulation_3, "line", hcaes(x = Date, y = TotalValue),
                    name = "Valeur totale (non réinvesti + sans décote)", dashStyle = "ShortDot") %>%
      hc_title(text = "Évolution de la valeur totale et des changements d’actions") %>%
      hc_yAxis(title = list(text = "Valeur (€)")) %>%
      hc_xAxis(type = "datetime", title = list(text = "Date"), plotLines = plot_lines) %>%
      hc_legend(enabled = TRUE, layout = "vertical", align = "right", verticalAlign = "top",
                borderWidth = 1, backgroundColor = "#FFFFFF") %>%
      hc_tooltip(shared = TRUE,
                 pointFormat = "Valeur: {point.y:.2f} €")

    hc
  })

  
  output$tbl_payoffs <- renderDT({
    dat <- comp_data %>% mutate(across(where(is.numeric), ~round(., 2)))
    datatable(dat, options = list(pageLength = 10))
  })
}

shinyApp(ui, server)