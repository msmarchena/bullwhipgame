library(shiny)
library(forecast)
library(plotly)

## mylag function
mylag <- function(x, lag) {
  n <- length(x)
  xnew <- x[n-(lag-1)]
  return(xnew)
}

function(input, output, session) {

##Here are defined the initial inputs of our example
  t <- c(1:5)
  d<- c(100,100,105,105,110)
  rc<- c(100,100,105,105,110)
  o <- c(100,100,105,105,110)
  m<- c(100,100,105,105,110)
  sd<- rep(0,5)
  ns<- rep(0,5)
  ltd<- c(100,100,105,105,110)
  ss <- rep(0,5)
  out<- c(100,100,105,105,110)
  cost <- rep(0,5)
  bw <- rep(NA,5)
  
  #reactive customer demand value
  cust_demand <- reactive({ as.numeric(input$c1)  })

  output$default_sl <- renderPrint({ servl() })
  
  #reactive service level value
  servl <- reactive({as.numeric(input$sl)})
  output$default_sl <- renderPrint({ servl() })
  
  #reactive leadt time value
  lt <- reactive({as.numeric(input$L)})
  output$default_lt<-renderPrint({ lt() })
  
  #reactive functions for the forecast method and receive
  forcast_model <- reactive({input$forecast})

  #reactive simple moving average periods 
  sma_periods <- reactive({as.numeric(input$periods)})
  output$p<-renderPrint({ sma_periods() })
   
  #reactive exponential smoothing parameter
  es_alpha <- reactive({as.numeric(input$alpha)})
  output$esparameter<-renderPrint({ es_alpha() })
  
  #reactive ordering cost value
  #orderingcost <- reactive({as.numeric(input$ord_cost)})
  
  #reactive holding cost value
  holdingcost <- reactive({as.numeric(input$h_cost)})
  
  #reactive backlog cost value
  backlogcost <- reactive({as.numeric(input$backlog_cost)})
 

  
######################################################################################################################
#Reactive participants results
######################################################################################################################  
# stores the current retailer data frame, called by values() and set by values(new_values)
  #values <- reactiveVal(data.frame(Time=t, Demand=d, Receive=rc, Mean=m, std=sd,  NS=ns, LTD=ltd, SS= ss, OUT=out, Order=o))
  values <- reactiveVal(data.frame( Demand=d, Receive=rc, Forecast=m, NS=ns, LTD=ltd, SS= ss, OUT=out, Order=o, Cost=cost, Bullwhip=bw))
# stores the current wholesale data frame, called by W_values() and set by W_values(new_W_values)
  #W_values <- reactiveVal(data.frame(W_D=o, W_Rec=rc, W_MD=m, W_SD=sd,  W_NS=ns, W_LTD=ltd, W_SS= ss, W_OUT=out, W_Ord=o))
  W_values <- reactiveVal(data.frame( Demand=o, Receive=rc, Forecast=m, NS=ns, LTD=ltd, SS= ss, OUT=out, Order=o, Cost=cost, Bullwhip=bw))
# stores the current distribution data frame, called by D_values() and set by D_values(new_D_values)
  D_values <- reactiveVal(data.frame(Demand=o, Receive=rc, Forecast=m, NS=ns, LTD=ltd, SS= ss, OUT=out, Order=o, Cost=cost, Bullwhip=bw))
# stores the current factory data frame, called by F_values() and set by F_values(new_F_values)
  F_values <- reactiveVal(data.frame(Demand=o, Receive=rc, Forecast=m, NS=ns, LTD=ltd, SS= ss, OUT=out, Order=o, Cost=cost, Bullwhip=bw))
# stores the perceived demand of all participants in a data frame, called by bullwhip_values() and set by values(new_bullwhip_values)
  perceived_demand <- reactiveVal(data.frame(Customer_demand=d, Retailer=o, Wholesaler=o, Distributor=o, Factory=o))
# update values table on button click
  observeEvent(input$update,{
########################################################################################################################  
## Retailer reactive table
######################################################################################################################## 
 
    old_values <- values()

    d_new <- cust_demand() 
    rc_new <- mylag(old_values$Order, lt()) 
    ns_new <- tail(old_values$NS, 1) + rc_new - d_new  
    #m_new <- mean(tail(old_values$Demand, sma_periods() )) 
      if(forcast_model() =='SMA')isolate({m_new <- mean(tail(old_values$Demand, sma_periods() )) })
      if(forcast_model() =='ES')isolate({ m_new <- es_alpha()*tail(old_values$Demand, 1) + (1-es_alpha() )*tail(old_values$Forecast, 1) })
      if(forcast_model() =='MMSE')isolate({ r_model<- arima( old_values$Demand, order=c(1,0,0),method="ML" )
                                            ar1_r<- predict(r_model, 1) 
                                            m_new <-round(ar1_r$pred[1],2)
                                  })#endisolate
    
    sd_new <- round(sd(tail(old_values$Demand,sma_periods() )),2)
    ltd_new <- lt()*m_new
    ss_new <- round(qnorm(p = servl(), mean = 0, sd = 1)*sd_new*sqrt(lt()),2)
    out_new <- ltd_new + ss_new
    o_new <- out_new - ns_new 
    cost_new <- if(is.na(ns_new)){ return() }
                else{if(ns_new > 0){ns_new*holdingcost() }
                       else{  abs(ns_new*backlogcost()) }
                }  
 
    ##simulated bullwhip measure
    var_demand<- var(old_values$Demand)
    var_order<- var(old_values$Order)
    bw_new <-   var_order/var_demand

    new_values <- data.frame(Demand=d_new, Receive=rc_new, Forecast=m_new, NS=ns_new, LTD=ltd_new, SS= ss_new, OUT=out_new, 
                              Order=o_new, Cost=cost_new, Bullwhip=bw_new)
    
    # attach the new line to the old data frame here:
    new_df <- round(rbind(old_values, new_values),3)
    
    #store the result in values variable
    values(new_df)

########################################################################################################################  
## Wholesaler reactive table
########################################################################################################################

  old_W_values <- W_values()

  wd_new <- tail(old_values$Order, 1)
  wrc_new <- mylag(old_W_values$Order, lt())   #tail(old_W_values$Order, 1)
  wns_new <- tail(old_W_values$NS, 1) + wrc_new - wd_new
  #wm_new <- mean(tail(old_W_values$Demand,sma_periods()  ))
  
  if(forcast_model() =='SMA')isolate({wm_new <- mean(tail(old_W_values$Demand, sma_periods() )) })
  if(forcast_model() =='ES')isolate({ wm_new <- es_alpha()*tail(old_W_values$Demand, 1) + (1-es_alpha() )*tail(old_W_values$Forecast, 1) })
  if(forcast_model() =='MMSE')isolate({ w_model <- arima( old_W_values$Demand, order=c(1,0,0),method="ML" )
                                        ar1_w <- predict(w_model, 1)
                                        wm_new <-round(ar1_w$pred[1],2)
                               })#endisolate

  wsd_new <- round(sd(tail(old_W_values$Demand,sma_periods() )),2)
  wltd_new <- lt()*wm_new
  wss_new <- round(qnorm(p = servl(), mean = 0, sd = 1)*wsd_new*sqrt(lt()),2)
  wout_new <- wltd_new + wss_new
  wo_new <- wout_new - wns_new
  
  wcost_new <- if(wns_new > 0){wns_new*holdingcost()
  }else{  abs(wns_new*backlogcost()) }
  
  ##simulated wholesaler bullwhip measure
  wvar_demand<- var(old_W_values$Demand)
  wvar_order<- var(old_W_values$Order)
  wbw_new <- wvar_order/wvar_demand

  new_W_values <- data.frame(Demand=wd_new, Receive=wrc_new, Forecast=wm_new, NS=wns_new, LTD=wltd_new, SS= wss_new,
                          OUT=wout_new, Order=wo_new, Cost=wcost_new, Bullwhip=wbw_new)

# attach the new line to the old data frame here:
  new_wdf <- round(rbind(old_W_values, new_W_values),2)

#store the result in values variable
  W_values(new_wdf)

########################################################################################################################  
## Distributor reactive table
########################################################################################################################

  old_D_values <- D_values()
 
  dd_new <- tail(old_W_values$Order, 1)
  drc_new <-  mylag(old_D_values$Order, lt())  #tail(old_D_values$Order, 1)
  dns_new <- tail(old_D_values$NS, 1) + drc_new - dd_new
 # dm_new <- mean(tail(old_D_values$Demand,4))
  
  if(forcast_model() =='SMA')isolate({dm_new <- mean(tail(old_D_values$Demand, sma_periods() )) })
  if(forcast_model() =='ES')isolate({ dm_new <- es_alpha()*tail(old_D_values$Demand, 1) + (1-es_alpha() )*tail(old_D_values$Forecast, 1) })
  if(forcast_model() =='MMSE')isolate({ d_model <- arima( old_D_values$Demand, order=c(1,0,0),method="ML" )
                                                   ar1_d <- predict(d_model, 1)
                                                   dm_new <-round(ar1_d$pred[1],2)
                                       })#endisolate
  
  dsd_new <- round(sd(tail(old_D_values$Demand, sma_periods() )),2)
  dltd_new <- lt()*dm_new
  dss_new <- round(qnorm(p = servl(), mean = 0, sd = 1)*dsd_new*sqrt(lt()),2)
  dout_new <- dltd_new + dss_new
  do_new <- dout_new - dns_new

  dcost_new <- if(dns_new > 0){dns_new*holdingcost()
  }else{  abs(dns_new*backlogcost()) }
  
  # ##simulated distributor bullwhip measure
  dvar_demand<- var(old_D_values$Demand)
  dvar_order<- var(old_D_values$Order)
  dbw_new <- dvar_order/dvar_demand
  
  new_D_values <- data.frame(Demand=dd_new, Receive=drc_new, Forecast=dm_new, NS=dns_new, LTD=dltd_new, SS= dss_new,
                           OUT=dout_new, Order=do_new, Cost=dcost_new, Bullwhip=dbw_new)

# attach the new line to the old data frame here:
  new_ddf <- round(rbind(old_D_values, new_D_values),2)

#store the result in values variable
  D_values(new_ddf)

########################################################################################################################  
## Factory reactive table
########################################################################################################################

  old_F_values <- F_values()
 
  fd_new <- tail(old_D_values$Order, 1)
  frc_new <- mylag(old_F_values$Order, lt())  ## tail(old_F_values$Order, 1)
  fns_new <- tail(old_F_values$NS, 1) + frc_new - fd_new
  #fm_new <- mean(tail(old_F_values$Demand,4))
  
  
  if(forcast_model() =='SMA')isolate({fm_new <- mean(tail(old_F_values$Demand, sma_periods() )) })
  if(forcast_model() =='ES')isolate({ fm_new <- es_alpha()*tail(old_F_values$Demand, 1) + (1-es_alpha() )*tail(old_F_values$Forecast, 1) })
  #Here the MLE (maximum likelihood estimation) method is used to have a stationary model. 
  #It is slower but gives better estimates.
  if(forcast_model() =='MMSE')isolate({ f_model <- arima( old_F_values$Demand, order=c(1,0,0),method="ML" )
                                        ar1_f <- predict(f_model, 1)
                                        fm_new <-round(ar1_f$pred[1],2)
                                      })#endisolate
  
  fsd_new <- round(sd(tail(old_F_values$Demand, sma_periods() )),2)
  fltd_new <- lt()*fm_new
  fss_new <- round(qnorm(p = servl(), mean = 0, sd = 1)*fsd_new*sqrt(lt()),2)
  fout_new <- fltd_new + fss_new
  fo_new <- fout_new - fns_new
  
  fcost_new <- if(fns_new > 0){fns_new*holdingcost()
  }else{  abs(fns_new*backlogcost()) }
  
  # ##simulated factory bullwhip measure
  fvar_demand<- var(old_F_values$Demand)
  fvar_order<- var(old_F_values$Order)
  fbw_new <- fvar_order/fvar_demand

  new_F_values <- data.frame(Demand=fd_new, Receive=frc_new, Forecast=fm_new, NS=fns_new, LTD=fltd_new, SS= fss_new,
                           OUT=fout_new, Order=fo_new, Cost=fcost_new, Bullwhip=fbw_new)

# attach the new line to the old data frame here:
  new_fdf <- round(rbind(old_F_values, new_F_values),2)

#store the result in values variable
  F_values(new_fdf)

################################################################
# Perceived demand dataframe
##################################################################
  old_perceived_demand  <- perceived_demand ()

  new_perceived_demand  <- data.frame(Customer_demand=d_new, Retailer=o_new, Wholesaler=wo_new,  
                                   Distributor=do_new, Factory=fo_new)
  
  new_perceived_demand_df <- round(rbind(old_perceived_demand, new_perceived_demand),2)
#store the perceived demand of all participants
  perceived_demand(new_perceived_demand_df)
  
#reset the numeric input to NA  
  updateNumericInput(session, "c1", "Actual customer demand", NA)
})#endobserveEvent

####################################################################################################################
# delete the last row of all tables
####################################################################################################################  
  deleteEntry <-observeEvent(input$reset,{    
    values( values()[-nrow(values()),])
    W_values( W_values()[-nrow(W_values()),])
    D_values( D_values()[-nrow(D_values()),])
    F_values( F_values()[-nrow(F_values()),])
    perceived_demand( perceived_demand()[-nrow(perceived_demand()),])
})#endobserveEvent
  
####################################################################################################################
# restart all tables
####################################################################################################################  
  deleteEntry <-observeEvent(input$restart,{    
    values( values()[1:5,])
    W_values( W_values()[1:5,])
    D_values( D_values()[1:5,])
    F_values( F_values()[1:5,])
    perceived_demand( perceived_demand()[1:5,])
  })#endobserveEvent  
  
  
  
  #Restart
  defaultvalues <- observeEvent(input$restart, {
    isolate(updateCounter$i == 0)
    values( values()[1:5,])
    W_values( W_values()[1:5,])
    D_values( D_values()[1:5,])
    F_values( F_values()[1:5,])
    updateCounter$i <- 0 
  }) 
  
  
  
  
####################################################################################################################  
#reactive counter modified
####################################################################################################################  
  updateCounter <- reactiveValues(i = 0)
  
  output$count <- renderText({
    paste0("Iteractions: ", updateCounter$i)
  })
  
  observe({
    input$update
    
    isolate({
      updateCounter$i <- updateCounter$i + 1
    })
  })
  
  observe({
    input$reset
    isolate(updateCounter$i <- updateCounter$i - 1)
  })
##################################################################################################################
# Display main results
##################################################################################################################
  r_invcost <- reactive({ colSums(values()[9]) })
  w_invcost <- reactive({ colSums(W_values()[9]) })
  d_invcost <- reactive({ colSums(D_values()[9]) })
  f_invcost <- reactive({ colSums(F_values()[9]) })
  
  output$display  <- renderUI({ 
    r_sum <-r_invcost()
    w_sum <-w_invcost()
    d_sum <-d_invcost()
    f_sum <-f_invcost()
    if(updateCounter$i==10) {
      r10 <<- r_sum
      w10 <<- w_sum
      d10 <<- d_sum
      f10 <<- f_sum
      sc_partialcost10 <<-r_sum + w_sum + d_sum + f_sum
    
      str_pc <- paste("Partial cost:", sc_partialcost10)
      str1 <- paste("Retailer:", r10)
      str2 <- paste("Wholesaler:", w10)
      str3 <- paste("Distributor:", d10)
      str4 <- paste("Factory:", f10)
      partial_cost<<- HTML(paste(str_pc, str1, str2, str3, str4, sep = '<br/>'))
      return(partial_cost)
                          
    } 
    if(updateCounter$i<10){
      return()
    }
    if(updateCounter$i>10) {
      sc_totalcost <-r_sum + w_sum + d_sum + f_sum
      str_tc <- paste("Total cost", sc_totalcost)
      str5 <- paste("Retailer:", r_sum)
      str6 <- paste("Wholesaler:", w_sum)
      str7 <- paste("Distributor:", d_sum)
      str8 <- paste("Factory:", f_sum)
      total_cost<- HTML(paste(str_tc, str5, str6, str7, str8, sep = '<br/>'))
      return( HTML(paste(partial_cost, total_cost, sep = '<br/><br/>')))
    } 
  })

####################################################################################################################    
## Tables outputs
####################################################################################################################  
 # output$Retailertab <- DT::renderDataTable({  return(values())  })
  output$Retailertab <- DT::renderDataTable({  return(values())  }
                                             ,
                                            colnames = c('Time' = 1),
                                            options = list(lengthMenu = c(5, 25, 50), pageLength = 25)
                                            #rownames = FALSE
                                            # options = list(
                                            #   autoWidth = TRUE,
                                            #   columnDefs = list(list(width = '80px', targets = "_all"))
                                            #)
                                            )
  # # Print the wholwsale table  stored in W_values$df, 
   output$Wholesalertab <- DT::renderDataTable({  return(W_values())  },
                                               colnames = c('Time' = 1),
                                               options = list(lengthMenu = c(5, 25, 50), pageLength = 25)
                                               )
  # # Print the distribution table stored in D_values$df, 
  output$Distributortab <- DT::renderDataTable({  return(D_values())  },
                                               colnames = c('Time' = 1),
                                               options = list(lengthMenu = c(5, 25, 50), pageLength = 25)
                                               )
  # # Print the factory table stored in F_values$df, 
   output$Factorytab <- DT::renderDataTable({  return(F_values())  },
                                            colnames = c('Time' = 1),
                                            options = list(lengthMenu = c(5, 25, 50), pageLength = 25)
                                            )
  # # Bullwhip effect plot
  output$perceivedTab<- DT::renderDataTable({  return(perceived_demand())  },
                                            colnames = c('Time' = 1),
                                            options = list(lengthMenu = c(5, 25, 50), pageLength = 25)
                                            )  
  
####################################################################################################################    
## Bullwhip  line graph
####################################################################################################################  

output$bullwhip_plot<- renderPlotly({ 
  plot_ly(perceived_demand(), x = 1:nrow(perceived_demand()), y = ~Customer_demand, name = 'Customer Demand', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~Retailer, name = 'Retailer', mode = 'lines') %>%
    add_trace(y = ~Wholesaler, name = 'Wholesaler', mode = 'lines') %>%
    add_trace(y = ~Distributor, name = 'Distributor', mode = 'lines') %>%
    add_trace(y = ~Factory, name = 'Factory', mode = 'lines') %>%
  layout(xaxis = list(title = "Time"),
         yaxis = list(title = "Order"),
         legend = list(x = 0.1, y = 0.9)
         )
  
#command for horizontal legend
# layout(legend = list(orientation = 'h')) 
  })
  
###################################################################################################################    
#Glossary
#####################################################################################################################
  output$notation <- renderUI({
  fluidRow(
    column(12, 
      strong('Back orders (B),'), 'orders that can not be filled at current time.',
      br(),   
      strong('Bullwhip,'), ' simulated measure of the increase in demand variability that occurs at each echelon of the supply chain',
      br(), 
      strong('Cost,'), 'the inventory cost in current period.',
      br(), 
     strong('Demand (D),'), 'current demand.',
     br(),    
  strong('Forecast'), 'refers to the mean demand estimation.',
         br(),
  strong('Lead time (L)'), 'is the number of days between the time an order is placed and the time it is received.',
  br(),
  strong('Lead time demand (LTD)'), 'is the average demand during lead time.',
  br(),
  strong('Net stock (NS)'), 'is defined here as a current inventory level. ',
  br(),
  strong('Order (O),'), 'stocks ordered but not yet arrived. ',
  br(),
  strong('Order Up to Level (OUT)'), 'is a replenishment policy. Each period companies review stock levels and place an order to bring its stock levels up to a target level.',
  br(),
  strong(' Receive'), 'refers to the arrival of orders placed L periods ago.' ,
  br(),
  strong('Safety factor (z)'), 'is a constant associated with the service level, also called the z-score.',
  br(),
  strong('Safety stock (SS),'), 'the amount of inventory that companies keep in order to protect theirselves against stockout situations during lead time. ',
  br(),
  strong('Service level (SL),'), 'the probability of not running out of stock during the next replenishment cycle.',
  br(),  
  strong('Standart deviation (Std)'), 'of demand.'
  )#endcolumn     
 )#endfluidRow
})#endrenderUI


  output$formulas <- renderUI({
    fluidRow(
      column(4, 
             withMathJax('Receive = O(-L) '),  
             br(),br(),
             withMathJax('NS = NS(-1) + Receive - D'),
             br(),br(),
            # withMathJax('b=\\frac{1}{2)'), 
             withMathJax('OUT = LTD + SS')
      ),#endcolumn
      column(4, 
             withMathJax('O = OUT - NS'),
             br(),br(),
             withMathJax('LTD = Forecast*L'),
             br(),br(),
             withMathJax('SS = z*Std*\\(\\sqrt{L}\\)')
             
      ),#endcolumn 
      column(4, 
             withMathJax('Bullwhip = Var(O)/Var(D)'),
             br(),br()
             
      )#endcolumn 
    )#endfluidRow
  })#endrenderUI
}