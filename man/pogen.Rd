\name{pogen}
\alias{pogen}

\title{pogen
}
\description{
If an exponentially distributed variable is modeled as the waiting time before an arrival, the Poisson distributed variable can be modeled as the number of arrivals during a period of time of length t. So we define parameter 'sum', which is sort of a timer and its initial value is zero. then we keep generating an exponentially distributed number, adding that to 'sum' and incrementing the number of occurrences(Poisson random variable) for each addition to 'sum', until 'sum' reaches the time limit which is passed to the function as an argument. Finally, it returns the total count of occurrences.
}
\usage{
pogen(x)
}

\arguments{
landa   parameter of the poisson variable
t       time duration whithin which the events happen
}

\value{
returns number of occurr
}



\examples{

a <- pogen(250, 1000)
a
visualpogen(a)
}
