library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(r2d3)
library(plotly)
library(treemap)
library(stringr)
library(scales)
library(DT)

shinyServer(function(input, output, session) {
  
  loan_rate<- data_Jay %>% group_by(loan_status) %>% summarise(n = n()) %>% mutate(rate = paste0(round(100 * n/sum(n), 0), "%"))
  ## Approval Rate 
  aprroval_rate<- loan_rate %>% filter(loan_status=="Fully Paid") %>% select(rate)
  ## Declined Rate: 
  decline_rate<- loan_rate %>% filter(loan_status=="Declined") %>% select(rate)
  ## Average DTI 
  avg_dti<-round(mean(data_Jay$dti),2)
  
  # show the total number of applicants
  output$value1 <- renderValueBox({
    valueBox(
      nrow(data_Jay),
      'Total Applications',
      icon = icon("user",lib='glyphicon'),
      color = "purple")  
  })
  
  ## loan approval rate
  output$value2 <- renderValueBox({ 
    valueBox(
      aprroval_rate,
      'Approval Rate',
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green")  
  })
  
  ## loan decline rate
  output$value3 <- renderValueBox({
    valueBox(
      decline_rate,
      'Declined Rate:',
      icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red")   
  })
  
  ## average dti
  output$value4 <- renderValueBox({
    valueBox(
      avg_dti,
      'Average DTI:',
      icon = icon("menu-hamburger",lib='glyphicon'),
      color = "olive")   
  })
  
  ## see if the value of type of laon status gets change
  observeEvent(input$loanstatus, {
    
    ## get the selected option from ui
    loanStats<-input$loanstatus

    ## filter the data as per the selected option
    data_Jay <- data_Jay %>% filter(loan_status==loanStats)

    ## get the home_ownership count 
    ownership_rate<- data_Jay %>% group_by(home_ownership) %>% summarise(n = n()) %>% mutate(rate = round(100 * n/sum(n), 0))
    
    ## get the title count
    purpose_per<- data_Jay %>% group_by(title) %>% summarise(n = n()) %>% 
      mutate(rate = round(100 * n/sum(n), 0)) %>% filter(rate>0)
    
   ## get the empCategory category count
    emp_length_per <- data_Jay %>% group_by(empCategory) %>% summarise(n = n()) %>% mutate(rate = round(100 * n/sum(n), 0))
    
    ## group by category and count the numbers under each category
    dti_val <- data_Jay %>% group_by(dtiCategory) %>% summarise(n = n())
    
    ## group by rvlCategory and count under each category
    rvl_val <- data_Jay %>% group_by(rvlCategory) %>% summarise(n = n())
    
    ## top 6 states in the data
    topstates<-data_Jay %>% group_by(addr_state) %>% summarise(n=n()) %>% arrange(desc(n)) %>% head(5)
    
    ## color choices for hist/pie
    colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
    
    ## percentage of homeownership
    output$homeowenership <- renderPlotly({
      plot_ly(
        x = ownership_rate$home_ownership,
        y = ownership_rate$rate,
        name = "",
        type = "bar",
        text = paste0(ownership_rate$rate,"%"), textposition = 'auto',
        marker = list(color = colors[1])
      ) %>% layout(yaxis = list(range = c(0,max(ownership_rate$rate)+10),title = 'Percentage')) 
    })
    
    ## pecentage of purpose for each category
    output$loanpurpose <- renderPlotly({
      plot_ly(
        x = purpose_per$title,
        y = purpose_per$rate,
        name = "",
        type = "bar",
        text = paste0(purpose_per$rate,"%"), textposition = 'auto',
        marker = list(color = colors[2])) %>% 
        layout(yaxis = list(range = c(0,max(purpose_per$rate)+10),title = 'Percentage')) 
    })
    
    ## percentage of emp_length for under category
    output$employment <- renderPlotly({
      plot_ly(
        x = emp_length_per$empCategory,
        y = emp_length_per$rate,
        name = "",
        type = "bar",
        colors = emp_length_per$rate,
        text = paste0(emp_length_per$rate,"%"), textposition = 'auto',
        marker = list(color = colors[3])
      ) %>% layout(yaxis = list(range = c(0,max(emp_length_per$rate)+10),title = 'Percentage')) 
    })
    
    ## draw warm-colored index treemap
    palette.HCL.options <- list(hue_start=270, hue_end=360+150)
    # treemap for the 5-top states
    output$topstate <- renderPlot({
      treemap(topstates,
              index=c("addr_state"),
              vSize="n",
              type="index",
              palette = "RdYlGn",
              #palette.HCL.options=palette.HCL.options,
              fontsize.labels=c(10,10),
              fontsize.title=0,
              aspRatio=1:1
      ) 
    })
    
    ## count under each category
    output$dtirange <- renderPlotly({
      plot_ly(dti_val, labels = ~dtiCategory, values = ~n, type = 'pie',sort = FALSE,
              marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1))) %>% 
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend = list(
                 #orientation = "h",   # show entries horizontally
                            # xanchor = "center"  # use center of legend as anchor
                            font = list(size = 10)
                             ))
    })
    
    ## count under each revol_util category
    output$revolvinguitlity <- renderPlotly({
      plot_ly(rvl_val, labels = ~rvlCategory, values = ~n, type = 'pie',sort = FALSE,
              marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1))) %>% 
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend = list(
                 #orientation = "h",   # show entries horizontally
                             # xanchor = "center"  # use center of legend as anchor
                             font = list(size = 10)
               )) 
    })
    
  })
  
  ## observe the numeric input
  observeEvent(input$click,{
    # if(!str_detect(input$lnamt, "\\$") & input$lnamt!="")
    if(input$lnamt!="") updateTextInput(session, "lnamt", value = dollar(as.numeric(gsub("[//$//,]","",input$lnamt))))
    if(input$anlincm!="") updateTextInput(session, "anlincm", value = dollar(as.numeric(gsub("[//$//,]","",input$anlincm))))
  })
  
  tableData<-data_Jay %>% select(Probability,loan_amnt,funded_amnt,funded_amnt_inv,term,int_rate,installment,grade,
                                 sub_grade,emp_title,emp_length,home_ownership,annual_inc,verification_status,
                                 loan_status,purpose,zip_code,addr_state,dti,open_acc,revol_bal,revol_util,total_acc,
                                 application_type,tot_cur_bal,total_bal_il,max_bal_bc,total_rev_hi_lim,avg_cur_bal,
                                 bc_open_to_buy,bc_util,mo_sin_old_il_acct,mo_sin_old_rev_tl_op,pct_tl_nvr_dlq,
                                 tot_hi_cred_lim,total_bal_ex_mort,total_bc_limit,total_il_high_credit_limit)
  
  output$predData <- DT::renderDataTable({
    datatable(tableData,rownames= FALSE,filter='top',options = list(scrollX = TRUE,pageLength = 15,autoWidth = TRUE,columnDefs = list(list(width = '100px', targets = "_all"),list(width = '100px', targets = "_all")))) %>% 
      formatStyle(
        'Probability',
        target = 'row',
        backgroundColor = styleInterval(c(.35, .70), c('red', 'orange','green')),
        fontWeight = 'bold'
      )
  })
})
