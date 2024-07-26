#' Load/Save EPhysData or EPhysSet objects to or from  HDF5 files
#'
#' These functions load and save \linkS4class{EPhysData} or \linkS4class{EPhysSet} objects from or into HDF5 files.
#' @param X A \linkS4class{EPhysData} or \linkS4class{EPhysSet} object.
#' @param filename Path the data is read from or written to.
#' @param overwrite Should existing files be overwritten?
#' @return
#' * Save: Does not return any values.
#' * Load: An \linkS4class{EPhysData} or \linkS4class{EPhysSet} object.
#' @examples
#' fn<-tempfile()
#' SampleData<-makeExampleEPhysData()
#' Save(SampleData, fn, overwrite = TRUE)
#' SampleData
#' rm(SampleData)
#' SampleData<-Load(fn)
#' SampleData
#' require(EPhysMethods)
#' Rejected(SampleData)<-autoreject.by.distance
#' Save(SampleData, fn, overwrite = TRUE)
#' SampleData
#' rm(SampleData)
#' SampleData<-Load(fn)
#' SampleData
#' rm(SampleData)
#' SampleData<-makeExampleEPhysSet()
#' Save(SampleData, fn, overwrite = TRUE)
#' SampleData
#' rm(SampleData)
#' SampleData<-Load(fn)
#' SampleData
#' @seealso \link[=EPhysData]{EPhysData}, \link[=EPhysSet]{EPhysSet}
#' @name LoadSave
#' @rdname LoadSave-methods
#' @docType methods
NULL
#'
#' @describeIn LoadSave-methods Save \linkS4class{EPhysData} or \linkS4class{EPhysSet} objects to an HDF5 file
#' @export
setGeneric(
  name = "Save",
  def = function(X,
                 filename,
                 overwrite = F)
  {
    standardGeneric("Save")
  }
)

#' @importFrom hdf5r H5File
#' @importFrom stringr str_detect
#' @rdname LoadSave-methods
#' @aliases Save,EPhysData,EPhysSet,ANY-method
setMethod("Save",
          "EPhysData",
          function(X,
                   filename,
                   overwrite) {
            if (file.exists(filename) & overwrite == F) {
              stop(paste("File", filename, "already exists."))
            } else{
              if (file.exists(filename)) {
                warning(paste("File", filename, "already exists. Overwriting."))
              }
            }
            if(exists("con")){
              if(con$is_valid){
                con$close_all()
              }
            }
            tryCatch({
              con <- H5File$new(filename, mode = "w")
            }, error= function(e){
              if (str_detect(e$message,"unable to create file")){
                out<-e
                stop("File '", filename,"' could not be created. Check path.")
              } else {
                stop(e)
              }
            })
            con <- Save.EPhysData(con, X)
            con$close_all()
          })

#' @importFrom hdf5r H5File
#' @importFrom stringr str_detect
#' @rdname LoadSave-methods
setMethod("Save",
          "EPhysSet",
          function(X,
                   filename,
                   overwrite) {
            if (file.exists(filename) & overwrite == F) {
              stop(paste("File", filename, "already exists."))
            } else{
              if (file.exists(filename)) {
                warning(paste("File", filename, "already exists. Overwriting."))
              }
            }
            if(exists("con")){
              if(con$is_valid){
                con$close_all()
              }
            }
            tryCatch({
              con <- H5File$new(filename, mode = "w")
            }, error= function(e){
              if (str_detect(e$message,"unable to create file")){
                out<-e
                stop("File '", filename,"' could not be created. Check path.")
              } else {
                stop(e)
              }
            })
            con <- Save.EPhysSet(con, X)
            con$close_all()
          })

#' @importFrom hdf5r H5File h5attr `h5attr<-`
#' @importFrom units drop_units deparse_unit
#' @noMd
#' @keywords internal
Save.EPhysData <- function (con, X) {
  con$create_dataset("Data",X@Data,gzip_level = 9)
  con[["Units"]] <- deparse_unit(X@Data)
  con[["TimeTrace"]] <- drop_units(X@TimeTrace)
  con[["TimeUnit"]] <- deparse_unit(X@TimeTrace)
  con[["StimulusTrace"]] <- drop_units(X@StimulusTrace)
  con[["StimulusUnit"]] <- deparse_unit(X@StimulusTrace)
  tmp.fx<-deparse(X@Rejected)
  con[["Rejected"]] <- iconv(paste(tmp.fx, collapse = "\n"), "UTF-8", "UTF-8", sub = '')
  tmp.fx<-deparse(X@filter.fx)
  con[["filter.fx"]] <- paste(tmp.fx, collapse = "\n")
  tmp.fx<-deparse(X@average.fx)
  con[["average.fx"]] <- paste(tmp.fx, collapse = "\n")
  con[["Created"]] <- X@Created
  con[["Type"]] <- "EPhysData"
  con[["EPhysDataVersion"]] <- as.character(packageVersion('EPhysData'))
  #con[["Description"]] <- readLines(file = paste0(path.package("EPhysData"), "/Hdf5_EPhysData_desc.txt"))
  con
}

#' @importFrom hdf5r H5File h5attr `h5attr<-`
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @noMd
#' @keywords internal
Save.EPhysSet <- function (con, X) {
  SER_SLOT <- con$create_group("Data")
  pb = txtProgressBar(min = 0, max = length(X@Data), initial = 0)
  for (i in 1:length(X)) {
    Recordings <-
      Save.EPhysData(SER_SLOT$create_group(as.character(i)),
                     X[[i]])
    setTxtProgressBar(pb,i)
  }
  close(pb)
  con$create_dataset("Metadata",X@Metadata)
  con[["Type"]] <- "EPhysSet"
  con[["EPhysDataVersion"]] <- as.character(packageVersion('EPhysData'))
  con
}

#' @importFrom hdf5r H5File
#' @importFrom utils packageVersion compareVersion
#' @describeIn LoadSave-methods Load \linkS4class{EPhysData} or \linkS4class{EPhysSet} objects from an HDF5 file
#' @export
Load <- function(filename) {
  if (file.exists(filename)) {
    con <- H5File$new(filename, mode = "r")
  } else {
    stop("File '", filename, "' does not exist. ")
  }

  if (compareVersion(con$open("EPhysDataVersion")$read(),
                     as.character(packageVersion('EPhysData')))>0) {
    warning("This file was created using a newer version of EPhysData (",con$open("EPhysDataVersion")$read(),")")
  }

  if (con$open("Type")$read() == "EPhysData") {
    out <- Load.EPhysData(con)
  }
  if (con$open("Type")$read() == "EPhysSet") {
    out <- Load.EPhysSet(con)
  }
  con$close_all()
  return(out)
}

#' @importFrom hdf5r H5File
#' @keywords internal
#' @noMd
Load.EPhysData <- function(con) {
  if (con$open("Type")$read() != "EPhysData") {
    warning("This Hdf5 file does not seem to contain EPhysData.")
  }


  Rejected = con$open("Rejected")$read()
  filter.fx = con$open("filter.fx")$read()
  average.fx = con$open("average.fx")$read()

  warnstring1 <-
    ", probably because it was too complex or not formatted correctly ('"
  warnstring2 <-
    "'). Replacing by default. For storing and loading multiline functions, make sure each line of code ends with a ';'."
  fxstr<-Rejected
  Rejected <- tryCatch({
    eval(parse(text = Rejected))
  }, error = function(e) {
    warning(
      "Function stored for rejection of recordings could not be imported",
      warnstring1,
      fxstr,
      warnstring2
    )

    function(x) {
      rep(FALSE, ncol(x))
    }
  })
  fxstr<-filter.fx
  filter.fx <- tryCatch({
    eval(parse(text = filter.fx))
  }, error = function(e) {
    warning(
      "Function stored for filtering of individual recordings could not be imported",
      warnstring1,
      fxstr,
      warnstring2
    )
    function(x) {
      x
    }
  })
  fxstr<-average.fx
  average.fx <- tryCatch({
    eval(parse(text = average.fx))
  }, error = function(e) {
    function(x) {
      x
    }
    warning(
      "Function stored for averaging of recordings could not be imported",
      warnstring1,
      fxstr,
      warnstring2,
      ". One reason could be that not all packages that the function depends on are loaded. e.g. Try 'require(EPhysMethods)' before loading the data. "
    )
  })

  out <- EPhysData(
    Data = as_units(con$open("Data")$read(), con$open("Units")$read()),
    TimeTrace = as_units(con$open("TimeTrace")$read(), con$open("TimeUnit")$read()),
    StimulusTrace = as_units(
      con$open("StimulusTrace")$read(),
      con$open("StimulusUnit")$read()
    ),
    Rejected = Rejected,
    filter.fx = filter.fx,
    average.fx = average.fx
  )
  out@Created <-
    as.POSIXct(con$open("Created")$read(), origin = "1970-01-01")

  if (validObject(out)) {
    return(out)
  } else{
    stop("Importing file results in an invalid 'EPhysData' object. Is Hdf5 file corrupted?")
  }

}

#' @importFrom hdf5r H5File
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @keywords internal
#' @noMd
Load.EPhysSet <- function(con) {
  if(con$open("Type")$read() != "EPhysSet"){
    warning("This Hdf5 file does not seem to contain an EPhysSet.")
  }

  Data.con <- con$open("Data")
  DATA<-list()
  pb = txtProgressBar(min = 0, max = length(names(Data.con)), initial = 0)
  j=0
  for (i in names(Data.con)){
    Data.curr<-Data.con$open(i)
    DATA[[as.integer(i)]]<-Load.EPhysData(con = Data.curr)
    setTxtProgressBar(pb,j)
    j=j+1
  }
  close(pb)

  out <- EPhysSet(Data = DATA,
                  Metadata = con$open("Metadata")$read())
  if (validObject(out)) {
    return(out)
  } else{
    stop("Importing file results in an invalid 'EPhysSet' object. Is Hdf5 file corrupted?")
  }
}
