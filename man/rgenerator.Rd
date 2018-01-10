\name{rgenerator}
\alias{rgenerator}

\title{
rgenerator

}
\description{
Generates pseudo random numbers using the linear congruential method. 
First we create a vector to write the generated numbers to it later on.
We give the generator a seed value, which is what th d <- as.numeric(Sys.time()) * 1000 line does.
Then, in a loop that is executed n times(N is the number of numbers requested), you apdate the value of d to (a*d + c)mod m(It's important to choose the right values for a and d and c)
So d is a random number between zero and m-1. The d produced in the i'th loop is stored in the i'th index of the vector. 
Done.

}
\usage{
rgenerator(x)
}

\arguments{
The only argument, n, is the number of random number you need to get
}

\value{
The return value is a vetor with n elements(i.e. random numbers)

}

\examples{
a <- rgenerator(1000)
a
}
