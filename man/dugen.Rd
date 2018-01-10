\name{dugen}
\alias{dugen}

\title{
dugen
}
\description{
We???ve got a function callled rgenerator that produces a vector containing n random numbers between zero and 2^32. So if the i???th element of the vector is rng2[i] we have:
0 < Rng2[i] < 2^32
=> 0 < Rng2[i] / 2 ^32 < 1
=> 0 < (Rng2[i] / 2 ^32 )* (upperbound- lowerbound) < (upperbound - lowerbound)
=> lowerbound < (Rng2[i] / 2 ^32 )* (upperbound- lowerbound) + lowerbound< (upperbound -lowerbound) + lowerbound
=> lowerbound < (Rng2[i] / 2 ^32 )*(upperbound- lowerbound) + lowerbound< upperbound
Thus, for i???s between 1 and n, we change Rng2[i] to  (Rng2[i] / 2 ^32 )*(upperbound- lowerbound) + lowerbound , and now we have n numbers between lowerbound and upperbound following  the uniform distribution.
the cugen function is the special case of dugen with minimum 0 and maximum 1.

}
\usage{
dugen(x)
}
\arguments{ the arguments are n(number of integers needed), lowerbound and upperbound(bounds of the interval within which the numbers should be generated).
}
\value{
a vector of n numbers between upperbound and lowerbound is returned.
}

\examples{
a <- dugen(100, 0, 1000)
a
visualdugen(a)
}
