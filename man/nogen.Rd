\name{nogen}
\alias{nogen}

\title{
nogen
}
\description{
This function returns a normal random variable with mean 'mean' and standard deviation 'sd'.
The Poisson (lambda) distribution can be considered as an approximation of N(lambda, lambda). Using this we first generate 'a',a Poisson random variable with parameter lambda ( we take advantage of the 'pogen' function for this stage). This is almost equivalent to a normal random variable with mean lambda and standard deviation lambda. 
So ('a'-lambda)/lambda is a standard random variable, and ('a' * 'sd') + mean is the required random variable, which is returned.
}
\usage{
nogen(x)
}

\arguments{
mean: mean of the normal distrubution
sd: standard deviation of the normal distrubution
}
}

\value{
Returns a normal random variable with mean 'mean' and standard deviation 'sd'.
}


\examples{
nogen(10, 3)
}

