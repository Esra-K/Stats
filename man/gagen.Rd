\name{gagen}
\alias{gagen}
\title{gagen
}
\description{
This function generates a number following gamma distribution. Summation of k i.i.d exponential random variables leads to a gamma-distribution-following random variable.(k is one of this function’s arguments). So we a)define a sum and set it to zero, b) use the expgen function to produce an exponential number, c) add the number to ‘sum’, d) repeat b) and c) k times, and then ‘sum’ would be a gamma distributed number, that will be returned.
}
\usage{
gagen(x)
}
\arguments{ 
k(number of exponential i.i.d random variables, 
landa(parameter of the underlying exponential distribution))
}

\value{
This function generates a number following gamma distribution
}


\examples{
a <- gagen(100, 0.76)
a
visualgagen(a)
}

