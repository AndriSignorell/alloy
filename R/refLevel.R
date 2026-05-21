
refLevel <- function(x){
  
  refCat <- function(model, var) {
    cs <- attr(model.matrix(model), "contrasts")[[var]]
    if (is.character(cs)) {
      if (cs == "contr.treatment")
        ref <- 1
      else stop("No treatment contrast")
    }
    else {
      zeroes <- !cs
      ones <- cs == 1
      stopifnot(all(zeroes | ones))
      cos <- colSums(ones)
      stopifnot(all(cos == 1))
      ros <- rowSums(ones)
      stopifnot(sum(!ros) == 1 && sum(ros) != ncol(cs))
      ref <- which(!ros)
    }
    return(levels(model$model[[var]])[ref])
  }
  
  # find factor predvals
  fpred <- names(grep("factor|ordered", attr(x[["terms"]], "dataClasses"), value=TRUE))
  resp <- all.vars(formula(x))[1]
  fpred <- fpred[fpred != resp]
  
  # list( complicated=
  # sapply(fpred , function(z) refCat(x, z))
  #
  # # or simply?
  # , simple=sapply(x$xlevels, "[", 1))
  
  sapply(fpred , function(z) refCat(x, z))
  
}
