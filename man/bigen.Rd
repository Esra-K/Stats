\name{bigen}
\alias{bigen}
\title{bigen
}
\description{
This function returns the number of successful experiments out of n Bernoulli experiments. So we define a sum(number of successes) and initialize it to zero. In a loop that is repeated n times, we put the output of brgen( the Bernoulli function) in variable ‘a’, and add it to sum(‘a’ is one in case of success and zero otherwise, so it makes sense)
}
\usage{
bigen(x)
}

\arguments{
n, number of experiments conducted, 
p, probability of success in one of them.
}

\value{
returns the number of successes( the binomial random variable)of n experiments
}

\examples{

a <- bigen(500, 0.4)
a
visualbigen(a)
}

