\name{Brgen}
\alias{Brgen}
\title{
brgen
}
\description{
To produce a random Bernoulli number(which is 1 with the probability of p and zero with the probability of 1-p)  this function produce one random number between 0 and 1 with uniform distribution (For this purpose we already have a dugen function).
Then the function returns one if this number is less than p and zero otherwise.
(for the uniform distribution with minimum a and maximum b, the probability of  variable X being within a specified interval is the ratio of the interval’s length to the subtraction of  a from b, so if 0<X<1, P(X <P) = (p-0)/(1-0) = p, hence we return a 1 if(X<p) and zero otherwise)
}
\usage{
Brgen(x)
}
\arguments{ 
p (the probability of the bernoulli variable being a one)
}

\value{

}


\examples{

}

