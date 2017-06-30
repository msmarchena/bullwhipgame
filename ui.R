library(shiny)

fluidPage(navbarPage("Bullwhip game", id= "mainNavbarPage", theme= "flatly.css",
########################################################################################################################                     
          tabPanel("Description",
                   
                   tabsetPanel(
                      tabPanel("Overview",
                          br(), 
                         p(strong('Bullwhip game')),         
                       
p('The Bullwhip Game simulates the distribution process of a single product that use a four stages supply chain: 
reailer, wholesaler, distributor and factory.'),

p('Members of the supply chain need to
meet customer demand with minimal shortage situations and inventory cost, while satisfying service level requirements. All
participants use the same inventory replenishment policy, forecast method, delivery time and service level.
Holding and shortage cost are fixed and information sharing and cooperation is not allowed.'),

p('As a main result of the simulation is the bullwhip effect, the increase in demand variability along the supply chain. 
   The bullwhip effect is pointed out as a key driver of inefficiencies associated with the supply chain. In the presence of this phenomenon,
participants involved in the manufacture of a product and its distribution to final customer face unstable
production schedules or excessive inventory.
  ')
                             ),#endtabPanel
                      tabPanel("Rules",
                               
                               sidebarPanel(
                                 p(strong('Players:')),  
                                 p('One player which has the role of the final customer. '),
                                 p(strong('Dynamics of the game:')),   
                               p('Each company in the system is the customer of upstream and a supplier of downstream company in
the supply chain. For instance, the retailer observes customer demand and based on its current inventory situation places orders to its supplier, 
in this case the wholesaler. He receives orders made after a delivery lead time.
 '),
                               
                               p(strong('Goal:')),  
                               p('To minimize the total inventory cost in the supply chain. '),
                               
                               p(strong('Results:')),  
                               p('After 10 interactions the total cost is displayed. A line graph of demand variability
                                in the supply chain (bullwhip effect) is also shown')
                         
                               ),#endsidebarPanel
                              mainPanel(
                                br(), br(),
                              # img(src='chain3_425x550.png',height=450, width=640)
                              div(img(src='chain_425x550.png',height=450, width=640), style="text-align: center;")
                              )#endmainPanel


                      ),
                      tabPanel("How To Play",
                               br(),
              p(' - Player needs to set up the participants inputs tab before start to play. All 
                 companies follow same parameters and use the same Order Up to Level (OUT) replenishment policy. '),
              p(' - There are 3 forecast methods: Simple moving average, Exponential smoothing and Autorregresive model order one. Each 
method has its corresponding parameter. Default values for lead time, service level, holding and shortage cost can be changed. '),
              p('- In the play tab, insert a value for the customer demand and click the "Update" button.'),
              p('- Each time a value is updated, the reult tables of all participants is presented in the main panel.  
Note that there are alredy initial values which are necessary to apply forecast methods.
                 '),
               p('- The reults are displyed after 10 interactions, but you can continue to play. '),
               p('- Use the glossary tab to understand how variables are calculated.  ')
               
                               )#endtabPanel
                   )#endtabsetPanel
           
           ),#endtabPanel
#######################################################################################################################
tabPanel("Participants inputs",
         
         fluidRow(
           column(12,
              helpText('Before to start playing you need to setup the parameters below. All the participants follow the same parameters')
           )#endcolumn
         ),#endfluidRow
         br(),
         fluidRow(
           column(6, 
                  selectInput(inputId = "forecast", label = "Forecast method", choices = list("Simple Moving Average (SMA)"="SMA", 
                                                                                              "Exponential Smoothing (ES)"="ES", "Autorregressive model - AR(1)"="MMSE"), selected="SMA")
           ),#endcolumn         
           column(4, 
                  conditionalPanel(condition = "input.forecast == 'SMA'",
                                   numericInput("periods","Number of periods to be used", 5)
                  ),#endconditionalPanel 
                  conditionalPanel(condition = "input.forecast == 'ES'", numericInput("alpha","Smoothing parameter", 0.20) 
                  )
            )#endcolumn
         ),#endfluidRow
         br(),
         fluidRow(
           column(4,
                  selectInput(inputId = "L",label = "Lead time",choices = c(1, 2, 3, 4, 5), selected=1)
           ),#endcolumn
           column(2),
           column(4, 
                  selectInput(inputId = "sl",label = "Service level", choices = c(0.95, 0.97, 0.99), selected=0.95)
           )#endcolumn
         ),#endfluidRow
         br(),
         fluidRow(
           # column(4,
           #         numericInput("ord_cost","Ordering cost", 0)
           # ),#endcolumn
           column(4, 
                  numericInput("h_cost","Holding cost", 0.5)
           ),#endcolumn
           column(2),
           column(4, 
                  numericInput("backlog_cost","Shortage Penalty cost", 2)
           )#endcolumn
         )#endfluidRow
),#endtabPanel
#######################################################################################################################
          tabPanel("Play",
                  
                   
                    sidebarPanel(width = 3,
                              
                              #helpText("Customer demand"),      
                              
                              numericInput("c1","Customer demand", NA),
                              tags$p(actionButton("update", "Update")),
                              tags$p(actionButton("reset", "Clear")),
                              textOutput("count"),
                             #You can use renderUI and htmlOutput instead of renderText and textOutput( cointainer=pre) to display multiple lines
                              htmlOutput("display")
                         
                  ),#endsiderbarPanel #uiOutput("tb"))  #tableOutput("example")
                  mainPanel(  tabsetPanel(tabPanel("Retailer", DT::dataTableOutput("Retailertab")),
                                          tabPanel("Wholesaler", DT::dataTableOutput("Wholesalertab")),
                                          tabPanel("Distributor", DT::dataTableOutput("Distributortab")),
                                          tabPanel("Factory", DT::dataTableOutput("Factorytab")),
                                          tabPanel("Perceived demand", DT::dataTableOutput("perceivedTab")),
                                          tabPanel("Bullwhip plot", plotOutput("bullwhip_plot"))
                             )#endtabsetPanel    
                  )#endmainPanel  

          ),#endtabPanel

#######################################################################################################################
                   
          tabPanel("Glossary",
                  h3( 'Formulas and definitions'),
                  br(),
                   uiOutput('notation'),
                   tags$hr(),
                   uiOutput('formulas')
          ),#endtabPanel

####################################################################################################################
          tabPanel("About", 
                   fluidRow(
                     column(12,
                            h4('About Bullwhip game'), 
                         
                            p('The Bullwhip Game is an Open Source project developed to ilustrate and to explore the bullwhip effect. 
The main goal of our interactive tool is to present the distribution dynamics of a product and show typical problems that arise 
from a non-coordinated system, specially the bullwhip effect. Our interactive tool use R programming language and Shiny to give an easy and friendly user experience. '),

                           p(' Created by ', strong('Marlene Silva Marchena')),
                          strong('Version: 0.1'), 
                            br(),br(),
                          ' Code on  ', a("Github", href="https://github.com/msmarchena/Bullwhip-game", 
                            target="_blank"),
                          br(),br(),
                           strong('License:'), 'GPL3',
                          br(),br(),
                          img(src='gplv3-127x51.png',height=51, width=127),
                          br(),br(),
                          strong('Contact'),
                          br(),
                          'Comments, suggestions, bug report or just want to contribute to this project please send an email:',
                          img(src='email_Marlene.png'),
                          br(),br(),
                          strong('Acknowledgements'),
                          br(),
                            HTML(paste0(
                              'Bullwhip game is built using ', 
                              a("R", 
                                href="https://www.r-project.org/", 
                                target="_blank"),
                              ' and ',
                              a("R Shiny", 
                                href="http://shiny.rstudio.com", 
                                target="_blank"),
                              ' framework',
                              ', with CSS from ',
                              a("Bootswatch", 
                                href="http://bootswatch.com", 
                                target="_blank"),
                              ' .'
                            ))
                            
                     ) # end column
                   ) # end fluidRow
                   )#endtabPanel
                  

))  


