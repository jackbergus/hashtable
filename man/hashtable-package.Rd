\name{hashtable-package}
\alias{hashtable-package}
\alias{hashtable}
\docType{package}
\title{
This package implements an HashTable in R using the C++11 data types.
}
\description{
It occupies 2n memory, since we want to find the elemnts by value (and hence
 a digest function is used over generic types)
}
\details{
\tabular{ll}{
Package: \tab hashtable\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-09-27\cr
License: \tab GPLv2\cr
}
~~ An overview of how to use the package, including the ~~
~~ most important functions ~~
}
\author{
Giacomo Bergami

Maintainer: Giacomo Bergami <giacomo90@libero.it>
}
\keyword{ package }
\examples{
 i = newHashT()  #Generates a new HashTable
 IdRM_AddElem(i,5)   #Inserts an element by value
 IdRM_RemElem(i,5)   #Removes an element by value
 #IdRM_UpdateElem(i,5,6)   removes 5 and adds 6
 rmHashtable(i)      #C++11 frees the HashTable
 
 #Some utilities
 i = getPtr(c(1,2,3))    #Returns the Ptr of the brand-new R object
 #gc()
 ptr_ToObj(i)            #The proof that the object is still in memory... :P
}
