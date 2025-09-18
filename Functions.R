

# Simulation avec réinvestissement immédiat des dividendes
simulate_investment_classique <- function(stock_data, dividend_data, 
                                          initial_investment, start_date, investment_period,
                                          reinvest = TRUE, decote = 0.2) {
  end_date = ymd(start_date) + years(investment_period)
  
  stock_data_      <- stock_data[paste0(start_date, "/", end_date)]
  dividend_data_   <- dividend_data[paste0(start_date, "/", end_date)]
  # split_data_      <- split_data[paste0(start_date, "/", end_date)]
  
  stock_data_$MeanPrice <- (Op(stock_data_) + Hi(stock_data_) + Lo(stock_data_) + Cl(stock_data_)) / 4
  
  
  # achat initial
  initial_price <- as.numeric(first(stock_data_$MeanPrice))
  shares        <- initial_investment / (initial_price*(1-decote))
  
  # suivi de l’évolution du nombre d’actions
  shares_evolution <- data.frame(Date = index(stock_data_), 
                                 Shares = NA)
  shares_evolution$Shares[1] <- shares
  shares_evolution$Value[1]  <- initial_price
  shares_evolution$TotalValue[1]  <- shares*initial_price
  head(shares_evolution)
  
  # boucle journalière
  for (i in 2:NROW(stock_data_)) {
    d <- index(stock_data_)[i]
    
    # appliquer dividende si présent
    if ((d %in% index(dividend_data_)) & reinvest==TRUE) {
      dividend <- as.numeric(dividend_data_[d])
      price    <- as.numeric(stock_data_$MeanPrice[i])
      shares   <- shares + (dividend * shares) / price
    }
    
    shares_evolution$Shares[i]     <- shares
    shares_evolution$Value[i]      <- as.numeric(stock_data_$MeanPrice[i])
    shares_evolution$TotalValue[i] <- shares*as.numeric(stock_data_$MeanPrice[i])
  }
  
  return(shares_evolution)
}

simulate_investment_garantie <- function(stock_data, dividend_data, 
                                         initial_investment, start_date, investment_period,
                                         reinvest = TRUE, decote = 0.064, taux_garanti = 0.03) {
  end_date        = ymd(start_date) + years(investment_period)
  
  stock_data_    <- stock_data[paste0(start_date, "/", end_date)]
  start_date_real = first(index(stock_data_))
  
  dividend_data_ <- dividend_data[paste0(start_date, "/", end_date)]
  
  stock_data_$MeanPrice <- (Op(stock_data_) + Hi(stock_data_) + Lo(stock_data_) + Cl(stock_data_)) / 4
  
  # achat initial
  initial_price    <- as.numeric(first(stock_data_$MeanPrice))
  shares_total     <- initial_investment / (initial_price*(1-decote))
  shares_employee  <- initial_investment / (initial_price)
  shares_bank      <- shares_total-shares_employee 
  
  # suivi de l’évolution du nombre d’actions
  shares_evolution <- data.frame(Date = index(stock_data_), 
                                 SharesTotal    = NA,
                                 #SharesEmployee = NA,
                                 SharesBank     = NA,
                                 Value          = NA,
                                 TotalValueTotal    = NA,
                                 TotalValueEmployee = NA,
                                 TotalValueBank     = NA)
  shares_evolution$SharesTotal[1]     <- shares_total
  shares_evolution$SharesBank[1]      <- shares_bank
  shares_evolution$Value[1]           <- initial_price
  shares_evolution$TotalValueTotal[1]     <- shares_total*initial_price
  shares_evolution$TotalValueEmployee[1]  <- initial_investment
  shares_evolution$TotalValueBank[1]      <- shares_bank*initial_price
  
  # boucle journalière
  for (i in 2:NROW(stock_data_)) {
    d <- index(stock_data_)[i]
    
    # appliquer dividende si présent et réinvesti
    if ((d %in% index(dividend_data_)) & reinvest==TRUE) {
      dividend <- as.numeric(dividend_data_[d])
      price    <- as.numeric(stock_data_$MeanPrice[i])
      shares_total   <- shares_total + (dividend * shares_total) / price
      shares_bank    <- shares_bank  + (dividend * shares_bank)  / price
    }
    
    shares_evolution$SharesTotal[i]     <- shares_total
    shares_evolution$SharesBank[i]      <- shares_bank
    shares_evolution$Value[i]           <- as.numeric(stock_data_$MeanPrice[i])
    shares_evolution$TotalValueTotal[i]     <- shares_total*as.numeric(stock_data_$MeanPrice[i])
    shares_evolution$TotalValueBank[i]      <- shares_bank*as.numeric(stock_data_$MeanPrice[i])
  }
  
  # Calcul des hausses protégées mensuelles 
  start <- ymd(start_date_real)
  end   <- ymd(end_date)
  current <- start
  monthly_prices <- c(initial_price)
  monthly_dates  <- c()
  
  while (current < end) {
    # subset des prix jusqu'au prochain mois
    next_month      <- current %m+% months(1) - days(1)
    prices_in_month <- stock_data_$MeanPrice[paste0(current, "/", next_month)]
    
    if (length(prices_in_month) > 0) {
      monthly_prices <- c(monthly_prices, as.numeric(last(prices_in_month)))
      monthly_dates  <- c(monthly_dates,  last(index(prices_in_month)))
      # print(last(index(prices_in_month)))
    }
    
    current <- current %m+% months(1)
  }
  monthly_dates = as.Date(monthly_dates)
  monthly_dates = c(as.Date(start_date_real),monthly_dates)
  
  monthly_returns   <- monthly_prices - monthly_prices[1]
  protected_returns <- pmax(monthly_returns, 0)
  
  #Payoff par pas de temps
  avg_protected_return <- sapply(1:length(protected_returns), function(x) mean(protected_returns[1:x], na.rm=TRUE))
  
  payoff <- sapply(2:length(protected_returns), function(x) max(((1+taux_garanti/12)^(x-1))*initial_investment,
                                                                4*avg_protected_return[x]*shares_employee+initial_investment, na.rm=TRUE))
  payoff <- c(initial_investment, payoff)
  details_payoff = data.frame(Date                   = monthly_dates,
                              PF_Cours_initial          = monthly_prices[1],
                              PF_Cours_mois             = monthly_prices,
                              PF_Return_protege_du_mois = protected_returns,
                              PF_Return_protege_moy     = avg_protected_return,
                              PF_Payoff                 = payoff)
  
  #Recap general
  shares_evolution = merge(shares_evolution, details_payoff, by.x = c('Date'), by.y = c('Date'), all.x = TRUE)
  shares_evolution[] <- lapply(shares_evolution, function(x) na.locf(x, na.rm = FALSE))
  shares_evolution$TotalValueEmployee <- shares_evolution$PF_Payoff
  
  return(shares_evolution)
}

print(read_data)
if(read_data){
  stock_data    = getSymbols(ticker,    from = start_date, to = end_date, auto.assign = FALSE)
  dividend_data = getDividends(ticker,  from = start_date, to = end_date)
  
  weekly_dates <- seq.Date(as.Date(start_date), as.Date(ymd(end_date) - years(5)), by = "month")
  
  classique_list <- lapply(weekly_dates, function(x) {
    
    sim <- simulate_investment_immediate(stock_data = stock_data, dividend_data = dividend_data,
                                         initial_investment = initial_investment, start_date = x, investment_period = investment_period,
                                         reinvest = TRUE, decote   = decote_classique)
    last(sim)
  })
  
  classique_ <- do.call(rbind, classique_list)
  classique_$StartDate <- weekly_dates
  
  garantie_list <- lapply(weekly_dates, function(x) {
    sim <- simulate_investment_garantie(stock_data = stock_data, dividend_data = dividend_data,
                                        initial_investment = initial_investment, start_date = x, investment_period = investment_period,
                                        reinvest = TRUE, decote   = decote_garantie)
    last(sim)
  })
  garantie_ <- do.call(rbind, garantie_list)
  garantie_$StartDate <- weekly_dates
  
  # construire table comparée
  comp_data <- data.frame(StartDate = weekly_dates,
                          Classique_TotalValue     = classique_$TotalValue,
                          Garantie_TotalValue      = garantie_$TotalValueTotal,
                          Garantie_EmployeeValue   = garantie_$TotalValueEmployee)
  comp_data$StartDate    <- as.Date(comp_data$StartDate)
  
  fwrite(stock_data,    "./Data/stock_data.csv")
  fwrite(dividend_data, "./Data/dividend_data.csv")
  fwrite(comp_data,     "./Data/comp_data.csv")
}else{
  print("go")
  
  stock_data    = read.csv("./Data/stock_data.csv")
  stock_data    = xts(stock_data[,-1], order.by = as.Date(stock_data[,1]))
  
  dividend_data = read.csv("./Data/dividend_data.csv")
  dividend_data = xts(dividend_data[,-1], order.by = as.Date(dividend_data[,1]))
  
  comp_data     = fread( "./Data/comp_data.csv")
}



