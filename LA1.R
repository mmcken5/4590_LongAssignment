# ===========================================================================================================================#
# TODO                                                                                                                       #
# ===========================================================================================================================#
# DONE # 1. Logic if 'is firm publicly traded?' is FALSE/TRUE                                                                #
# DONE # 2. Logic if 'Will the cost of debt change in the stable period?' is FALSE/TRUE                                      #
# DONE # 3. Logic for 'Do you want to enter cost of equity directly'                                                         #
# DONE # 4. Logic for 'To change the beta in the stable period, enter the beta for stable period'                                   #
# 5. Logic for 'To change the debt ratio (debtratio_stable) in the stable growth period enter the debt ratio for the stable growth period'      #
# DONE # 8. Properly have input arguments to the function                                                                    #
# ===========================================================================================================================#


##Inputing Data

###Financial Data
inputs <- list(
    c_revenue = 108249,
    c_capex = 7696,
    c_depreciation = 1814,
    tax_rate = .2422,
    
    book_debt = 0,
    book_equity = 76615,
    NOL = 200000,
    
    public = TRUE,
    m_price = 500,
    m_shares = 929.3,
    m_debt = .1,
    debt_capital = 0,
    
    ###Cost of Equity
    #capm = FALSE,
    e_rate = .07,
    
    ####CAPM Model
    beta_growth = 1,
    #####beta to change in stable period
    beta_stable = 1,
    rf_rate = .02,
    rm_rate = .05,
    capm_rate_growth = rf_rate + (beta_growth * (rm_rate - rf_rate)),
    capm_rate_stable = rf_rate + (beta_stable * (rm_rate - rf_rate)),
    
    ###Cost of Debt
    d_rate = .025,
    
    excess_period = 20L,
    cap_period = 5L,
    
    fast_period = 7L,
    stable_period = excess_period - cap_period,
    
    ###Assumptions
    growth_fast = .06,
    growth_slow = .03,
    revenue_growth = c(rep(growth_fast, fast_period), rep(growth_slow, stable_period)),
    cogs_asrev = .8165,
    workingcap_asrev = .05,
    workingcap_stable = .01,
    
    #####debt ratio in the stable period
    debtratio = TRUE,
    debtratio_stable = .15,
    
    #####will cost of debt change in stable period?
    d_rate_change = TRUE,
    d_rate_stable = .08,
    
    #% of capex to depreciation in terminal
    capex_depreciation = 1.1,
    
    
    # ASSUMPTION: Assuming same fast and slow growth rate as revenue growth rate
    ###Depreciation Growth
    depreciation_growth = c(-0.0683317211644177, rep(growth_fast, 5), rep(growth_slow, 14)),
    
    ###CAPEX Growth
    capex_growth = c(.023939668853227, rep(growth_fast, 4), rep(growth_slow, 15))
)

DCF_value <- function(inputs){
  # Initialize local variables from input arguments
  c_revenue <- inputs$c_revenue
  c_capex <- inputs$c_capex
  c_depreciation <- inputs$c_depreciation
  tax_rate <- inputs$tax_rate
  
  book_debt <- inputs$book_debt
  book_equity <- inputs$book_equity
  NOL <- inputs$NOL
  
  public <- inputs$public
  m_price <- inputs$m_price
  m_shares <- inputs$m_shares
  m_debt <- inputs$m_debt
  debt_capital <- inputs$debt_capital
  
  ###Cost of Equity
  #capm <- inputs$capm
  e_rate <- inputs$e_rate
  
  ####CAPM Model
  beta_growth <- inputs$beta_growth
  beta_stable <- inputs$beta_stable
  rf_rate <- inputs$rf_rate
  rm_rate <- inputs$rm_rate
  capm_rate_stable <- inputs$capm_rate_stable
  capm_rate_growth <- inputs$capm_rate_growth
  
  ###Cost of Debt
  d_rate <- inputs$d_rate
  
  ###Assumptions
  revenue_growth <- inputs$revenue_growth
  growth_fast <- inputs$growth_fast
  growth_slow <- inputs$growth_slow
  cogs_asrev <- inputs$cogs_asrev
  workingcap_asrev <- inputs$workingcap_asrev
  workingcap_stable <- inputs$workingcap_stable
  
  #####debt ratio in the stable period
  debtratio <- inputs$debtratio
  debtratio_stable <- inputs$debtratio_stable
  
  #####will cost of debt change in stable period?
  d_rate_change <- inputs$d_rate_change
  d_rate_stable <- inputs$d_rate_stable
  
  #% of capex to depreciation in terminal
  capex_depreciation <- inputs$capex_depreciation
  
  excess_period <- inputs$excess_period
  cap_period <- inputs$cap_period
  
  fast_period <- inputs$fast_period
  stable_period <- inputs$stable_period
  
  ###Depreciation Growth
  depreciation_growth <- inputs$depreciation_growth
  
  ###CAPEX Growth
  capex_growth <- inputs$capex_growth
  
  
  # Initialize local variables
  debt_val <- 0
  debt_rate_stable_calc <- 0
  equity_to_capital <- 0
  
  # Initialize vectors (for two-stage DCF)
  beta_vector <- c(rep(beta_growth, fast_period), rep(beta_stable, stable_period))
  capm_vector <- c(rep(capm_rate_growth, fast_period), rep(capm_rate_stable, stable_period))
  
  
  # Determine if equity rate will be entered directly, or need to calculated value
  if (is.null(e_rate)){
    e_rate <- capm_vector
  }
  else{
    e_rate <- c(rep(e_rate, excess_period))
  }
  
  # Determining debt value by checking if firm is public
  if (public){
    debt_val <- m_debt 
  }
  else{
    debt_val <- book_debt
  }
  
  # Determine equity to capital ratio
  if(public){
    market_cap <- m_price * m_shares
    equity_to_capital <- market_cap / (market_cap + m_debt)
  }
  else{
    if(debt_capital != 0){
      equity_to_capital <- 1 - debt_capital
    }
    else{
      equity_to_capital <- 1 - book_debt / (book_debt + book_equity)
    }
  }
  
  # Check if the cost of debt will change in the stable (terminal) period
  if(d_rate_change){
    debt_rate_stable_calc <- debt_rate_stable * (1 - tax_rate)
  }
  else{
    debt_rate_stable_calc <- d_rate * (1 - tax_rate)
  }
  
  #Revenue
  dcf_revenue <- c_revenue * (1 + revenue_growth[1])
  for (i in 2:fast_period){
    dcf_revenue <- c(dcf_revenue, (dcf_revenue[i - 1] * (1 + revenue_growth[i])))
  }
  for (i in (fast_period + 1):excess_period){
    dcf_revenue <- c(dcf_revenue, (dcf_revenue[i - 1] * (1 + revenue_growth[i])))
  }
  
  #COGS
  dcf_cogs <- cogs_asrev * dcf_revenue
  
  #EBIT
  dcf_ebit <- dcf_revenue - dcf_cogs
  
  #EBIT * T
  if (NOL > dcf_ebit){
    dcf_taxes <- 0
  } else {
    dcf_taxes <- tax_rate
  }
  for (i in 2:excess_period){
    if (dcf_nol[i - 1] > dcf_ebit[i]){
      dcf_taxes <- c(dcf_taxes, 0)
    } else {
      dcf_taxes <- c(dcf_taxes, ((dcf_nol[i - 1] - dcf_ebit[i]) * tax_rate))
    }
  }
  
  #NOL 
  dcf_nol <- NOL - dcf_revenue[1]
  for (i in 2:excess_period){
    if (dcf_nol[i - 1] > dcf_ebit){
      dcf_nol <- c(dcf_nol, dcf_nol[i - 1] - dcf_ebit)
    } else {
      dcf_nol <- c(dcf_nol, 0)
      }
    }
  
  #Tax Rate
  dcf_trates <- dcf_taxes / dcf_ebit
  
  #EBIT After Taxes
  dcf_ebit_sub <- dcf_ebit - dcf_taxes
  
  #Depreciation
  dcf_depreciation <- c_depreciation * (1 + depreciation_growth[1])
  for (i in 2:excess_period){
    dcf_depreciation <- c(dcf_depreciation, dcf_depreciation[i - 1] * (1 + depreciation_growth[i]))
  }
  
  #CAPEX
  dcf_capex <- c_capex * (1 + capex_growth[1])
  for (i in 2:excess_period){
    dcf_capex <- c(dcf_capex, dcf_capex[i - 1] * (1 + capex_growth[i]))
  }
  
  #Change in Working Capital
  workingcapital <- c(workingcap_asrev * c_revenue, workingcap_asrev * dcf_revenue)
  dcf_workingcapital <- NULL
  for (i in 1:excess_period){
    dcf_workingcapital <- c(dcf_workingcap, (workingcap[i + 1] - workingcap[i]))
  }
  
  #FCFF
  dcf_fcff <- dcf_ebit_sub + dcf_depreciation - dcf_capex - dcf_workingcapital
  
  #Cost of Debt
  dcf_costdebt <- c(rep(debt_rate_stable_calc, excess_period))
  
  #Cost of Equity/Beta/debt ratio/cost of capital
  dcf_beta <- beta_vector
  dcf_costequity <- e_rate
  dcf_debtratio <- c(rep(1 - equity_to_capital), excess_period)
  dcf_costcapital <- (dcf_costequity * (1 - dcf_debtratio)) + (dcf_costdebt * dcf_debtratio)
  
  #Cum. WACC
  for (i in 1:excess_period){
    if (i == 1){
      dcf_wacc <- 1 + dcf_costcapital
    } else {
      dcf_wacc <- c(dcf_wacc, dcf_wacc[i - 1] * (1 + dcf_costcapital))
    }
  }
  
  #Present Value
  dcf_pv <- dcf_fcff / dcf_wacc
  
  #Stable Growth
  dcf_stablegrowth <- growth_stable
  
  #Terminal FCFF
  tn_revenue <- dcf_revenue[excess_period] * (1 + dcf_stablegrowth)
  tn_cogs <- tn_revenue * cogs_asrev
  tn_ebit <- tn_revenue - tn_cogs
  if (dcf_nol[excess_period] > tn_ebit){
    tn_taxes <- 0
  } else {
    tn_taxes <- (tn_ebit - dcf_nol[excess_period]) * tax_rate
  }
  tn_ebit_sub <- tn_ebit - tn_taxes
  tn_depreciation <- dcf_depreciation[excess_period]
  tn_capex <- capex_depreciation * tn_depreciation
  tn_workingcapital <- workingcap_asrev * (tn_revenue - dcf_revenue[excess_period])
  tn_fcff <- tn_ebit_sub + tn_depreciation - tn_capex - tn_workingcapital
  tn_costequity <- e_rate
  #Equity/(Debt+Equity)
  tn_formula <- 1 - debtratio_stable
  tn_costdebt <- dcf_costdebt
  #Flip of above formula
  tn_flip <- 1 - tn_formula
  tn_costcapital <- tn_costequity * tn_formula + tn_costdebt * tn_flip
  
  value_postgrowth <- tn_fcff / (tn_costcapital - dcf_stablegrowth)
  pv_allfcff <- sum(dcf_pv)
  pv_terminal <- value_postgrowth / dcf_wacc[excess_period]
  firmvalue <- pv_allfcff + pv_terminal
  DebtMarketValue <- debt_val
  EquityMarketValue <- firmvalue - DebtMarketValue
  ValuePerShare <- EquityMarketValue / m_shares
  
  #######################
  #retVals <- c(DebtMarketValue, EquityMarketValue, ValuePerShare)
  return(ValuePerShare)
}

DCF_value(inputs)
