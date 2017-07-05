# SCbullwhip

The SCbullwhip is an educational game that has as purpose the illustration and exploration of 
the *bullwhip effect*, i.e. the increase in demand variability along the supply chain.
The game simulates the distribution process of a single product that uses a four stages supply chain: 
reailer, wholesaler, distributor and factory. The members of the supply chain need to
meet customer demand with minimal shortage situations and inventory cost, while satisfying service level requirements. All
participants use the same inventory replenishment policy, forecast method, delivery lead time and service level.
Holding and shortage cost are fixed and information sharing and cooperation is not allowed




**Installation:**

To play the bullwhip game locally you must have installed R, Shiny and the packages used in the server file.<br>
Copy the "server.R" and "ui.R" files and "www" folder in the same directory on your machine.<br>
Open one of the files in RStudio and click the "runApp" button.

From Github, the following command will download and run the application

`shiny::runGitHub('bullwhipgame', 'msmarchena')`

To play the bullwhip game online visit https://marchenamarlene.shinyapps.io/bullwhipgame/
