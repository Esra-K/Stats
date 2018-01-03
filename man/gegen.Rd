\name{gegen}
\alias{gegen}

\title{gegen
}
\description{
Geometric random variable producer: the function has to do the Bernoulli experiment and count the number of failures( when Bernoulli function’s output is zero) before the first success. It puts the result of brgen (Bernoulli generator) in variable ‘a’. First it assigns 0 to a and repeats calling the brgen function until the result is one, for the sake of refreshment. Then we start by initializing a counter to zero, assigning the output of brgen to ‘a’ again in a while loop, until a gets a success( 1) from the brgen function. Till then we increment the counter for each zero ‘a; gets.
}
\usage{
gegen(x)
}

}

\value{ the geometric random variable(number of failures before a success)
}

\examples{

}

