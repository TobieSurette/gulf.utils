#' Import Data
#'
#' @description Import from one object into another.
#'
#' @param x Target object.
#' @param value Object containing catch data to be assigned.
#' @param by Character string(s) specifying which variables use as an index key.
#' @param variables Character string(s) specifying the fields to import from the source object.
#' @param fill.na Value to be used to fill in target field entries with no matching source entries.
#' @param ... Other parameters (not used).
#'
#' @seealso \code{\link[gulf.utils]{match}}
#'
#' @examples
#' library(gulf.data)
#'
#' # Import snow crab survey by-catch data:
#' x <- read.scsset(year = 2020, valid = 1)
#' y <- read.scscat(year = 2020, species = c("cod", "plaice"))
#' y$species <- species(y$species)
#' import(x, fill = 0, var = "number.caught", group = "species") <- y
#'
#' # Import snow crab category data:
#' x <- read.scsset(year = 2020, valid = 1)
#' b <- read.scsbio(2020)
#' import(x, fill = 0) <- catch(b, category = c("M", "COM", "MF"))
#' print(x$M)

#' @export
"import<-" <- function(x, ...) UseMethod("import<-")

#' @describeIn import-set Import assignment method for data frames.
#' @export
"import<-.data.frame" <- function(x, value, variables, fill.na, group, ...){

   # Parse 'group' argument:
   if (!missing(group)){
      if (is.character(group)){
         if ((group %in% names(value)) & length(group) == 1){
            group <- value[group]
         }else{
            group <- NULL
         }
      }
      value <- value[setdiff(names(value), names(group))]
      if (!is.null(group)) group <- as.data.frame(group)
   }else{
      group <- NULL
   }

   # Define variable to merge:
   if (missing(variables)) variables <- names(value)

   # Define indexing variables:
   key <- c(list(...)$by, list(...)$key, key(value), key(x))
   if (is.null(key)) key <- intersect(variables, names(x))

   # Remove indexing variables:
   variables <- setdiff(variables, key)
   variables <- variables[variables %in% names(value)]
   if (length(variables) == 0) stop("No matching variables to import from source object.")

   # Append results:
   if (length(variables) > 0){
      if (is.list(group)){
         g <- unique(group[,1])
         if (length(g) > 0){
            na.var <- NULL
            for (i in 1:length(g)){
               for (j in 1:length(variables)){
                  v <- value[which(group[,1] == g[i]), ]
                  if (length(v) > 0){
                     ix <- gulf.utils::match(x, v, by = key, ...)
                     str <- as.character(g[i])
                     if (length(variables) > 1) str <- paste0(str, ".", variables[j])
                     str <- gsub(" ", ".", str)
                     x[str] <- NA
                     x[str] <- v[ix, variables[j]]
                     na.var <- c(na.var, str)  # Accumulate which variables to use for 'fill.na'.
                  }
               }
            }
         }
      }else{
         ix <- gulf.utils::match(x, value, by = key, ...)
         x[variables] <- NA
         x[variables] <- value[ix, variables]
         na.var <- variables
      }

      # Fill NA values:
      if (!missing(fill.na)){
         tmp <- x[na.var]
         tmp[is.na(tmp)] <- fill.na
         x[na.var] <- tmp
      }
   }

   return(x)
}
