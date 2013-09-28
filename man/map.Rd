\name{map}
\alias{test}
\title{
map<String,T>
}
\description{
Implements a fast & ready map with strings and generic values
}
\usage{
test(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Giacomo Bergami

Maintainer: Giacomo Bergami <giacomo90@libero.it>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
    m = newMap()              # Defines a new data type
    map_AddKeyVal(m,"tui",5)  # Adds 5 of key "tui". The keys should be of type string, otherwise a NULL element will be returned
    map_getVal(m,"tui")       # Returns 5
    map_hasKey(m,"tui")       # Returns 1 since the key "tui" exists
    rmMap(m)                  # Frees the map
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
