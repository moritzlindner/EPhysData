#' EPhysData: Structured Index and Introduction for the EPhysData R Package
#'
#' There are two classes for storing the data. The `EPhysData` class stores  data from a single recording (possibly with multiple trials). It is usually generated from imported raw data using `newEPhysSet()`. Information on how to filter or average the data contained in this class can be stored in it, but the data is left unchanged. The `EPhysSet` class contains a collection of associated  `EPhysData` classes (e.g. data from different channels recorded in parallel or in response to increasing stimulus intensities). Each `EPhysData` stored is associated with metadata describing the nature of the particular recodring. This class can be subsetted, extended or modified in other ways.
#'
## usethis namespace: start
#'
#' @section Object creation:
#' * \link[=newEPhysData]{newEPhysData} (for \link[=EPhysData]{EPhysData} objects) \cr
#' * \link[=newEPhysSet]{newEPhysSet} (for \link[=EPhysSet]{EPhysSet} objects) \cr\cr
#'
#' @section Object manipulation:
#' * \link[=AverageFunction<-]{AverageFunction<-} (for \link[=EPhysData]{EPhysData} objects) \cr
#' * \link[=FilterFunction<-]{FilterFunction<-} (for \link[=EPhysData]{EPhysData} objects) \cr
#' * \link[=Rejected<-]{Rejected<-} (for \link[=EPhysData]{EPhysData} objects) \cr\cr
#' #'
#' * \link[=BestSIPrefix]{BestSIPrefix} (for \link[=EPhysData]{EPhysSet} and \link[=EPhysData]{EPhysSet}  objects) \cr
#' * \link[=SetSIPrefix]{SetSIPrefix} (for \link[=EPhysData]{EPhysSet} and \link[=EPhysData]{EPhysSet}  objects) \cr\cr
#'
#' * \link[=AddData]{AddData<-} (for \link[=EPhysSet]{EPhysSet} objects) \cr
#' * \link[=Metadata]{Metadata<-} (for \link[=EPhysSet]{EPhysSet} objects) \cr
#' * \link[=AddMetadata]{AddMetadata} (for \link[=EPhysSet]{EPhysSet} objects) \cr
#' * \link[=lapply]{lapply} (for \link[=EPhysSet]{EPhysSet} objects) \cr\cr
#'
#' @section Accession functions:
#' * \link[=Subset]{Subset} (for \link[=EPhysData]{EPhysData} and \link[=EPhysData]{EPhysSet}  objects) \cr
#' * \link[=[]{[} (for \link[=EPhysData]{EPhysData}) \cr
#' * \link[=[[]{[[} and \link[=[[<-]{[[<-} (for \link[=EPhysSet]{EPhysSet}) \cr
#' * \link[=as.data.frame]{as.data.frame} (for \link[=EPhysData]{EPhysSet} and \link[=EPhysData]{EPhysSet}  objects) \cr\cr
#'
#' * \link[=GetData]{GetData} (for \link[=EPhysData]{EPhysData} objects) \cr\cr
#'
#' * \link[=AverageFunction]{AverageFunction} (for \link[=EPhysData]{EPhysData} objects) \cr
#' * \link[=FilterFunction]{FilterFunction} (for \link[=EPhysData]{EPhysData} objects) \cr
#' * \link[=StimulusTrace]{StimulusTrace} (for \link[=EPhysData]{EPhysData} objects) \cr
#' * \link[=TimeTrace]{TimeTrace} (for \link[=EPhysData]{EPhysData} objects) \cr
#' * \link[=Rejected]{Rejected} (for \link[=EPhysData]{EPhysData} objects) \cr\cr
#'
#' * \link[=Metadata]{Metadata} (for \link[=EPhysSet]{EPhysSet} objects) \cr\cr
#'
#' @section Helper functions:
#' * \link[=makeExampleEPhysData]{makeExampleEPhysData} \cr
#' * \link[=makeExampleEPhysSet]{makeExampleEPhysSet} \cr
#' * \link[=length]{length} (for \link[=EPhysSet]{EPhysSet} objects) \cr
#' * \link[=dim]{dim} (for \link[=EPhysData]{EPhysData} objects) \cr\cr
#' * \link[=Save]{Save} Saves EPhysData or EPhySet objects to an HDF5 file \cr
#' * \link[=Load]{Load} Loads EPhysData or EPhySet objects from an HDF5 file \cr
#'
#'
#' @examples
#' # Load the EphysData package
#' library(EPhysData)
#'
#' # Create an example EPhysData object
#' data <- makeExampleEPhysData()
#'
#' # Get data
#' GetData(data, Raw = TRUE)
#'
#' # Get time trace data
#' TimeTrace(data)
#'
#' # Set Averaging and function for the data
#' FilterFunction(data)<-function(x){x/max(x)}
#' GetData(data, Raw = FALSE)
#' AverageFunction(data)<-mean
#' GetData(data, Raw = FALSE)
#'
#' # Create an EPhysSet object
#' ephysSet <-
#'   newEPhysSet(
#'     Data = list(
#'       makeExampleEPhysData(),
#'       makeExampleEPhysData(),
#'       makeExampleEPhysData(),
#'       makeExampleEPhysData()
#'     ),
#'     Metadata = data.frame(StepID = c("A1", "A2", "A3", "A4"))
#'     )
#' ephysSet
#'
#'
#' @author \href{https://www.lindnerlab.de}{Moritz Lindner}
#' @aliases EPhysData-package
#' @docType package
#' @name EPhysData-package
NULL
