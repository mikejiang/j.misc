#' unlock a package namespace
#' @param env the package namespace to unlock
#' @useDynLib j.misc
unlockNamespace <- function(env) {
  .Call('unlockNamespace', PACKAGE = 'j.misc', env)
}

.updateFunc_old <- function(name, pkgName){
  funcSym <- as.symbol(name)
  #update the environment of the global function with the package namespace
  eval(substitute(environment(v1) <- getNamespace(v2), list(v1 = funcSym, v2 = pkgName)), envir = parent.frame(2))
  #copy the global function into package namespace
  assignInNamespace(name, eval(funcSym), ns = pkgName)
}

#' update a function within a package namespace
#' 
#' This is useful for testing a function patch without reinstalling the package or loading entire package into global session
#' The function has to pre-exist within the package namespace.
#' 
#' @param name \code{character} the function name defined in the global session
#' @param pkgName \code{character} the package name that the patched function will be writing to
#' 
#' @examples 
#' \dontrun{
#' updateFunc("read.ncdfFlowSet", "ncdfFlow")
#' }
#' @export 
updateFunc <-function(name, pkgName){
  funcSym <- as.symbol(name)
  
  #update the environment of the global function with the package namespace
  eval(substitute(environment(v1) <- getNamespace(v2), list(v1 = funcSym, v2 = pkgName)), envir = parent.frame(2))
  
  #insert the global function into package namespace
  env1 <- getNamespace(pkgName)
  unlockNamespace(env1)
  unlockBinding(name, env1)
  env1[[name]] <- eval(funcSym)
  lockBinding(name, env1)
  lockEnvironment(env1)
}

#' pretty layout of proto objects hierarchy
#' 
#' default layout does not play well with the bigger tree
#' @param e environment or proto object
#' @export 
plot_proto <- function(e){
  g <- proto::graph.proto(e,child.to.parent = F)
#   browser()
  if(length(nodes(g)) == 0)
    message("No proto node to graph!")
  else
    Rgraphviz::renderGraph(Rgraphviz::layoutGraph(g, attrs = list(graph = list(rankdir="LR")
                                                  , node = list(fontsize = 40,fixedsize = F, shape = "none")
#                                                  , edge = list(style = "dashed")
                                              )
                            )
  )
}