##################################################
# Shiny app to predict successful ctDNA sequencing
##################################################
library(arm)
library(shiny)
library(shinydashboard)

load('multivariate_logistic_regression.RData')

nice_percent <- function(value, fmt='%3.1f%%'){
    truncated <- max(min(value, 1), 0)
    sprintf(fmt, 100*truncated)
}

Header <- dashboardHeader(title='UW OncoPlex prediction',
                          titleWidth=250)

Sidebar <- dashboardSidebar(width=250,
             sidebarMenu(
               menuItem('Home', tabName='home'),
               menuItem('Help', tabName='help')))

Body <- dashboardBody(
           tags$head(tags$style(HTML('
                                     .skin-green .main-sidebar {
                                         background-color: #ffffff;
                                     }
                                     .skin-green .main-sidebar .sidebar .sidebar-menu .active a{
                                         background-color: #008d4c;
                                     }
                                     .skin-green .main-sidebar .sidebar .sidebar-menu a{
                                         background-color: #00a65a;
                                     }
                                     .content-wrapper,
                                     .right-side {
                                         background-color: #ffffff;
                                     }'))),
           tabItems(
           tabItem(tabName='home',
                   fluidRow(
                            box(width=12,
                                solidHeader=TRUE,
                                h3('Predict the probability of successful ctDNA sequencing')),
                            column(width=4,
                                   box(width=NULL,
                                       title='Patient characteristics',
                                       solidHeader=TRUE,
                                      numericInput('psa.value',
                                                   'PSA level (ng/mL):',
                                                   value=10,
                                                   min=0,
                                                   max=5000),
                                       selectInput('castration.resistant.flag',
                                                   'Castration resistant PC:',
                                                   c('No'='No', 'Yes'='Yes')),
                                       #selectInput('met.before.ctdna.flag',
                                       #            'Confirmed metastasis:',
                                       #            c('No'='No', 'Yes'='Yes')),
                                       selectInput('chaarted.high.volume.flag',
                                                   'CHAARTED high volume PC:',
                                                   c('No'='No', 'Yes'='Yes')))),
                            column(width=8,
                                   box(width=NULL,
                                       title='Predicted probability',
                                       solidHeader=TRUE,
                                       h1(textOutput('pred')))))),
           # Help
           tabItem(tabName='help',
                   fluidRow(
                     box(width=12,
                         solidHeader=TRUE,
                         h3('Details of the underlying multivariate logistic regression model are described here:'),
                         '(manuscript in preparation)')
                     )
                   )
           ))

ui <- dashboardPage(Header, Sidebar, Body, skin='green')

server <- function(input, output){
    output$pred <- renderText({
        dset <- data.frame(log.psa.value=log(input$psa.value),
                           castration.resistant.flag=input$castration.resistant.flag,
                           #met.before.ctdna.flag=input$met.before.ctdna.flag,
                           chaarted.high.volume.flag=input$chaarted.high.volume.flag)
        pred <- predict(fit, newdata=dset, type='response', se.fit=TRUE)
        nice_pred <- with(pred, paste0(nice_percent(fit),
                                       ' (',
                                       nice_percent(fit-1.96*se.fit),
                                       '-',
                                       nice_percent(fit+1.96*se.fit),
                                       ')'))
    })
}

shinyApp(ui, server)

if(FALSE){
    dset <- data.frame(log.psa.value=log(10),
                       castration.resistant.flag='No',
                       #met.before.ctdna.flag='No',
                       chaarted.high.volume.flag='No')
    pred <- predict(fit, newdata=dset, type='response', se.fit=TRUE)
    nice_pred <- with(pred, paste0(nice_percent(fit),
                                   ' (',
                                   nice_percent(fit-1.96*se.fit),
                                   '-',
                                   nice_percent(fit+1.96*se.fit),
                                   ')'))
}
