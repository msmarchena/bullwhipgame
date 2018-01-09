\name{bullwhipgame}
\alias{bullwhipgame}
\title{Bullwhip Effect Demo in Shiny}
\usage{
bullwhipgame()
}
\value{
  Nothing
}
\description{
bullwhipgame is a Shiny app to illustrate and visualize the bullwhip effect, i.e, the increase in demand variability along the supply chain. The game simulates the distribution process of a single product that uses a four stages supply chain: reailer, wholesaler, distributor and factory. The members of the supply chain need to meet customer demand with minimal shortage situations and inventory cost, while satisfying service level requirements. All participants use the same inventory replenishment policy, forecast method, delivery lead time and service level. Holding and shortage cost are fixed and information sharing and cooperation is not allowed.
}

\examples{
if(interactive()){
 bullwhipgame()
}

}
