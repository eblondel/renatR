#' RenaterFederation
#' @docType class
#' @export
#' @keywords federation
#' @return Object of \code{\link{R6Class}} for modelling an RenaterFederation
#' @format \code{\link{R6Class}} object.
#'
#' @note Main user class to be used with \pkg{renatR}
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
RenaterFederation <-  R6Class("RenaterFederation",
  inherit = renatRLogger,
  private = list(),
  public = list(

    #'@field id federation id
    id = NA,

    #'@description Initialize the federation
    #'@param id federation id
    #'@param logger logger
    initialize = function(id = c("renater", "edugain", "test"), logger = NULL){
      super$initialize(logger = logger)
	    id <- match.arg(id)
      self$id = id
    },

  	#'@description Get identity providers (IdPs)
  	#'@param pretty if output should be prettified as \link{data.frame}
  	#'@return an object of class \link{list} or \link{data.frame}
  	getIdentityProviders = function(pretty = FALSE){
  		idps = jsonlite::read_json(sprintf("https://discovery.renater.fr/%s/api.php/idps", self$id))[[1]]
  		if(pretty) idps = do.call("rbind.fill", lapply(1:length(idps$children), function(i){
  		  as.data.frame(t(unlist(idps$children[[i]])))
  		}))
  		return(idps)
  	}
  )
)
