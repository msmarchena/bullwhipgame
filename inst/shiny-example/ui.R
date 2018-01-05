library(shiny)
library(plotly)

fluidPage(navbarPage("bullwhipgame", id= "mainNavbarPage", theme= "flatly.css",
########################################################################################################################
          tabPanel("Description",

                   tabsetPanel(
                      tabPanel("Overview",
                          br(),
                         p(strong('bullwhipgame')),

    'The bullwhipgame is an educational game that has as purpose the illustration and exploration of
      the', em('bullwhip effect,'), 'i.e, the increase in demand variability along the supply chain.',

    br(), br(),

    p('The game simulates the distribution process of a single product that uses a four stages supply chain:
reailer, wholesaler, distributor and factory. The members of the supply chain need to
      meet customer demand with minimal shortage situations and inventory cost, while satisfying service level requirements. All
      participants use the same inventory replenishment policy, forecast method, delivery lead time and service level.
      Holding and shortage cost are fixed and information sharing and cooperation is not allowed.')



                             ),#endtabPanel
                      tabPanel("Rules",

                               sidebarPanel(
                                 p(strong('Players:')),
                                 p('One player which has the role of the final customer. '),
                                 p(strong('Dynamics of the game:')),
                               p('Each company in the system is the customer of upstream and a supplier of downstream company in
the supply chain. For instance, the retailer observes customer demand and based on its current inventory situation places orders to its supplier,
in this case the wholesaler. She receives orders made after a delivery lead time.
 '),

                               p(strong('Goal:')),
                               p('To minimize the total inventory cost in the supply chain. '),

                               p(strong('Results:')),
                               'After 10 interactions the total cost is displayed. A line graph of demand variability
                                in the supply chain ', em('(bullwhip effect)'), ' is also shown'

                               ),#endsidebarPanel
                              mainPanel(
                                br(), br(),
                               div(img(src='chain_425x550.png',height=450, width=540), style="text-align: center;")
                               )#endmainPanel
                      ),
                      tabPanel("How To Play",

       br(),
       p(strong('Instructions')),

       p(' - You need to set up the participants inputs tab before start to play. All
                 companies follow the same parameters and use the same Order Up to Level (OUT) replenishment policy. '),
              p(' - The mean demand forecast is calculated using one of the following methods: simple moving average, exponential smoothing
or autoregressive model order one . The default simple moving average method requires the number of periods to be used to calculate
the mean demand. In the case of the exponential smoothing method, you need to choose the smoothing parameter.
                Default values for lead time, service level, holding and shortage cost may be changed. '),
              p('- In the play tab, insert a value for the customer demand and click the "Update" button.'),
              p('- Each time a value is updated, the reult tables of all participants are displayed in the main panel.
Note that there are alredy initial values.
                 '),
               p('- The results are displayed after 10 interactions, but you can continue to play. '),
               p('- Use the glossary tab to understand how variables are calculated.  ')

                               )#endtabPanel
                   )#endtabsetPanel

           ),#endtabPanel
#######################################################################################################################
tabPanel("Participants inputs",

         fluidRow(
           column(12,
              h4('Before to start playing you need to setup the parameters below. All the participants follow the same parameters')
           )#endcolumn
         ),#endfluidRow
         br(),
         fluidRow(
           column(6,
                  selectInput(inputId = "forecast", label = "Forecast method", choices = list("Simple Moving Average (SMA)"="SMA",
                                                                                              "Exponential Smoothing (ES)"="ES", "Autoregressive model - AR(1)"="MMSE"), selected="SMA")
           ),#endcolumn
           column(4,
                  conditionalPanel(condition = "input.forecast == 'SMA'",
                                   numericInput("periods","Number of periods to be used", 5)
                  ),#endconditionalPanel
                  conditionalPanel(condition = "input.forecast == 'ES'",
                                   numericInput("alpha","Smoothing parameter", 0.20),
                                   helpText('A number between 0 and 1')
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
           column(4,
                  numericInput("h_cost","Holding cost", 0.5)
           ),#endcolumn
           column(2),
           column(4,
                  numericInput("backlog_cost","Shortage Penalty cost", 2)
           )#endcolumn
         ),#endfluidRow
         helpText('Holding and shortage penalty cost must be positive numbers')
),#endtabPanel
#######################################################################################################################
          tabPanel("Play",


                    sidebarPanel(width = 3,

                              numericInput("c1","Customer demand", NA),
                              tags$p(actionButton("update", "Update")),
                              tags$p(actionButton("reset", "Clear")),
                              tags$p(actionButton("restart", "Restart")),
                              textOutput("count"),
                              br(),
                             #You can use renderUI and htmlOutput instead of renderText and textOutput( cointainer=pre) to display multiple lines
                              htmlOutput("display")

                  ),#endsiderbarPanel #uiOutput("tb"))  #tableOutput("example")
                  mainPanel( width = 9,
                            tabsetPanel(tabPanel("Retailer",
                                                br(),
                                                h4(' Retailer main results'),
                                                br(),
                                                DT::dataTableOutput("Retailertab")),
                                          tabPanel("Wholesaler",
                                                  br(),
                                                  h4(' Wholesaler main results'),
                                                  br(),
                                                  DT::dataTableOutput("Wholesalertab")),
                                          tabPanel("Distributor",
                                                  br(),
                                                  h4('Distributor main results'),
                                                  br(),
                                          DT::dataTableOutput("Distributortab")),
                                          tabPanel("Factory",
                                                  br(),
                                                  h4('Factory main results'),
                                                  br(),
                                          DT::dataTableOutput("Factorytab")),
                                          tabPanel("Orders",
                                                  br(),
                                                  h4('Resume of orders'),
                                                  br(),
                                          DT::dataTableOutput("perceivedTab")),
                                          tabPanel("Graph",
                                                   br(),
                                                   h4('Orders plot'),
                                                   plotlyOutput("bullwhip_plot"))
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
                            h4('About bullwhipgame'),

                            p('The bullwhipgame is an Open Source project developed to illustrate and explore the', em('bullwhip effect.'),
'The main goal of our interactive tool is to present the dynamics of distribution of a product and to show typical problems arising
from a non-coordinated system. Our interactive tool use R programming language and Shiny
to offer an easy and friendly user experience.'),
                            br(), br(),

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


