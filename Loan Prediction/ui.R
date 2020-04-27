library(shinydashboard)
library(plotly)
library(shinyWidgets)

header <- dashboardHeader(title = "Basic Dashboard")  

## switch between 2 tabs
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    convertMenuItem(menuItem("Overview", tabName = "tab_overview", icon = icon("dashboard"),
                             selectInput("loanstatus", label = h5("Select one or more types"), choices = c("Approved"="Fully Paid","Declined"="Declined","Under Review"="Partial Approved"), multiple=FALSE, selectize=TRUE)
                             ), tabName = "tab_overview"),
    convertMenuItem(menuItem("Predictions", tabName = "tab_prediction", icon = icon("trophy"),
             textInput("lnamt", "Loan Amount:"),
             textInput("terms", "Term:"),
             textInput("emplttl", "Employment Title:"),
             textInput("hmownrsp", "Home Ownership:"),
             textInput("anlincm", "Annual Income (Without Tax):"),
             textInput("pol", "Purpose of Loan:"),
             textInput("rsc", "Residential State Code:"),
             textInput("cbtir", "Current Debt To Income Ratio (%):"),
             textInput("toa", "Total Open Account:"),
             textInput("rvlutl", "Revolving Utility (%):"),
             actionButton("click", "submit")
             ), tabName = "tab_prediction")
  )
)

frow1 <- fluidRow(
  valueBoxOutput("value1",width=3),
  valueBoxOutput("value2",width=3),
  valueBoxOutput("value3",width=3),
  valueBoxOutput("value4",width=3)
)

frow2 <- fluidRow( 
  box(width = 4,
      title = "Home Ownership",
      status = "primary",
      solidHeader = TRUE, 
      collapsible = TRUE, 
      plotlyOutput("homeowenership", height = "250px")
  )
  ,box(width = 4,
       title = "Loan Purpose",
       status = "primary",
       solidHeader = TRUE, 
       collapsible = TRUE, 
       plotlyOutput("loanpurpose", height = "250px")
  ),
  box(width = 4,
      title = "Employment",
      status = "primary",
      solidHeader = TRUE, 
      collapsible = TRUE, 
      plotlyOutput("employment", height = "250px")
  ) 
)

frow3 <- fluidRow( 
  box(width = 4,
      title = "Top States",
      status = "primary",
      solidHeader = TRUE, 
      collapsible = TRUE, 
      plotOutput("topstate", height = "300px")
  ),
  box(width = 4,
      title = "DTI Range",
      status = "primary",
      solidHeader = TRUE, 
      collapsible = TRUE, 
      plotlyOutput("dtirange", height = "300px")
  ),
  box(width = 4,
      title = "Revolving Utility",
      status = "primary",
      solidHeader = TRUE, 
      collapsible = TRUE, 
      plotlyOutput("revolvinguitlity", height = "300px")
  ) 
)


# combine the two fluid rows to make the body
body <- dashboardBody( 
  
  tabItems(
    tabItem("tab_overview",frow1,frow2,frow3),
    tabItem("tab_prediction", 
            fluidRow(
              box(width = 12, DT::dataTableOutput('predData')))
            )
    ),
  tags$head(tags$style(HTML('
                      .skin-red .left-side, .skin-red .main-sidebar, .skin-red .wrapper {
                          background-color: #808585;
                      }
                      .skin-red .main-header .navbar {
                          background-color: #D35E60;
                      }
                      .skin-red .main-header .logo {
                          background-color: #D35E60;
                      }
                      .skin-red .sidebar-menu>li.active>a, .skin-red .sidebar-menu>li:hover>a {
                          background: #D35E60;
                          border-left-color: #D35E60;
                      }
                      section.sidebar .shiny-input-container {
                          padding: 0px 15px 0px 15px;
                      }
                      section.sidebar .shiny-bound-input.action-button, section.sidebar .shiny-bound-input.action-link {
                          margin: 6px 5px 15px 140px;
                          display: block;
                      }
                      .skin-red .sidebar-menu>li>.treeview-menu {
                          background: #808585;
                      }
                    ')))
  )

dashboardPage(title = 'Loan Data Analysis', header, sidebar, body, skin='red')