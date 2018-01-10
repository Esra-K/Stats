\name{expgen}
\alias{expgen}

\title{expgen
}
\description{
This function returns a positive number following exponential distribution. Using the inverse transform method, it can be proven that if variable x in the interval of (0,1) follows uniform distribution , -1/λ *lnx follows exponential distribution with parameter λ. So it simply uses cugen to generate a number between 0 and 1, and returns 1/ λ * logarithm of that number.
}
\usage{
expgen(x)
}

\arguments{ 'λ' (the exponential distribution's parameter) is the function's argument,
}

\value{
Returns a positive number following exponential distribution

}

\examples{
a <- expgen(250)
a
visualexpgen(a)
}

