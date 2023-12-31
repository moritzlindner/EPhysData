% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Get_Set_EPhysData.R
\docType{methods}
\name{Get_Set_EPhysData}
\alias{Get_Set_EPhysData}
\alias{Rejected}
\alias{Rejected<-}
\alias{FilterFunction}
\alias{FilterFunction<-}
\alias{AverageFunction}
\alias{AverageFunction<-}
\alias{TimeTrace}
\alias{StimulusTrace}
\title{Get/Set methods for EPhysData objects}
\usage{
Rejected(X, return.fx = F, ...)

Rejected(X, ...) <- value

FilterFunction(X, ...)

FilterFunction(X, ...) <- value

AverageFunction(X, ...)

AverageFunction(X, ...) <- value

TimeTrace(X)

StimulusTrace(X)
}
\arguments{
\item{X}{An \code{EPhysData} object}

\item{return.fx}{For \code{Rejected()}: Whether to return the function or the resulting logical vector. Default is \code{FALSE}, i.e. to return the function.}

\item{...}{Currently unused.}

\item{value}{A value (usually a function) to set.}
}
\description{
These methods are used to get and set the non-data slots from \link{EPhysData} objects.
}
\details{
\code{Rejected}: These functions set or get a function returning a logical vector indicating which of the repeated measurements stored in an \link{EPhysData} object to exclude from averaging. The following function from the EPhysMethods package may be helpful: \link[EPhysMethods:autoreject.by.distance]{EPhysMethods::autoreject.by.distance}, \link[EPhysMethods:autoreject.by.signalfree]{EPhysMethods::autoreject.by.signalfree}

\code{FilterFunction}: Set  a function for filtering each individual of the repeated measurements in the \link{EPhysData} object. Could be downsampling or noise removal, for instance. The following functions from the EPhysMethods package may be helpful: \link[EPhysMethods:filter.bandpass]{EPhysMethods::filter.bandpass}, \link[EPhysMethods:filter.detrend]{EPhysMethods::filter.detrend},

\code{AverageFunction}: Set a function describing how averaging across repeated measurement should be performed in the \link{EPhysData} object. Usually, \link[base:mean]{mean} can be a good start.

\code{TimeTrace}: These functions set or get the time trace belonging to the measurements stored in an \link{EPhysData} object.
}
\section{Functions}{
\itemize{
\item \code{Rejected()}: Rejected

\item \code{Rejected(X, ...) <- value}: Rejected<-

\item \code{FilterFunction()}: FilterFunction

\item \code{FilterFunction(X, ...) <- value}: FilterFunction<-

\item \code{AverageFunction()}: AverageFunction

\item \code{AverageFunction(X, ...) <- value}: AverageFunction<-

\item \code{TimeTrace()}: TimeTrace

\item \code{StimulusTrace()}: StimulusTrace

}}
\examples{
# Create an EPhysData object with example data
myEPhysData <- makeExampleEPhysData()

# Get the "Rejected" slot
Rejected(myEPhysData)

# Set the "Rejected" slot
Rejected(myEPhysData) <- sample(c(TRUE,FALSE), dim(myEPhysData)[2], TRUE)

# Get the "filter.fx" slot
FilterFunction(myEPhysData)

# Set the "filter.fx" slot
FilterFunction(myEPhysData) <- function(x) x^2

# Get the "average.fx" slot
AverageFunction(myEPhysData)

# Set the "average.fx" slot
AverageFunction(myEPhysData) <- median

}
\seealso{
\link[EPhysMethods:autoreject.by.distance]{EPhysMethods::autoreject.by.distance}, \link[EPhysMethods:autoreject.by.signalfree]{EPhysMethods::autoreject.by.signalfree}, \link[EPhysMethods:filter.bandpass]{EPhysMethods::filter.bandpass}, \link[EPhysMethods:filter.detrend]{EPhysMethods::filter.detrend},
}
