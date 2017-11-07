#'@title A function to set up a design for a given set of factors with their specific levels using the MBR-design method.
#'@name mbrdsim
#'@aliases mbrdsim
#'@description The multi-level binary replacement (MBR) design approach is used here in order to facilitate the investigation of the effects of
#'the data properties on the performance of estimation/prediction methods. The mbrdsim function
#'takes as input a list containing a set of factors with their levels. The output is an MBR-design
#'with the combinations of the factor levels to be run.
#'@usage mbrdsim(simlist, fraction, gen=NULL)
#'@param simlist A named list containing the levels of a set of (multi-level) factors.
#'@param fraction Design fraction at bit-level. Full design: fraction=0, half-fraction: fraction=1, and so on.
#'@param gen Generators for the fractioning at the bit level. Default is \code{NULL} for which the generators are chosen
#'  automatically by the \code{FrF2} function. See documentation of \code{FrF2} for details on how to set the generators.
#'@return
#'  \item{BitDesign }{The design at bit-factor level. The object is of class design, as output from FrF2. Function design.info()
#'    can be used to get extra design info of the bit-design. The bit-factors are named.numbered if the input factor list is named.}
#'  \item{Design }{The design at original factor level, non-randomized. The factors are named if the input factor list is named.}%% ...
#'@references Martens, H., Måge, I., Tøndel, K., Isaeva, J., Høy, M. and Sæbø¸, S., 2010, Multi-level binary replacement (MBR) design for computer experiments in high-dimensional nonlinear systems, \emph{J, Chemom}, \bold{24}, 748--756.
#'@author Solve Sæbø
#'@examples
#'  # Input: A list of factors with their levels (number of levels must be a multiple of 2).
#'  simlist <- list(R2=c(0.5,0.9),
#'                  pos = c(12,45),
#'                  gamma = c(0.1, 0.9),
#'                  comp = 1:8,
#'                  alpha = c(0.01, 0.05, 0.1, 0.25))
#'
#'  # A 1/8 design
#'  des <- mbrdsim(simlist, fraction=3)
#'  # Setting generators manually as interactions between base bit-level factors.
#'  des <- mbrdsim(simlist, fraction=3, gen=list(c(1,2,4,5),c(2,3,5),c(3,4,5)))
#'
#'  # The MBRD-design at original factor level
#'  des$Design
#'
#'  # Info about the bit-design including bit-level aliasing (and resolution if \code{gen = NULL})
#'  \dontrun{
#'  # library(DoE.base)
#'  # design.info(des$BitDesign)
#'  }
#'@keywords MBRD
#'@keywords Design
#'@export

mbrdsim <- function(simlist, fraction, gen = NULL) {
  nlev       <- unlist(lapply(simlist, length))
  l2lev      <- log2(nlev)
  repnames   <- rep(names(l2lev), times = l2lev)
  ext        <- sapply(l2lev, function(x) paste0(".", x))
  bitnames   <- paste0(repnames, ext)
  res        <- mbrd(l2lev, fraction = fraction, gen = gen,
                     fnames2 = names(simlist), fnames1 = bitnames)
  runDesign  <- as.data.frame(res$Design)
  runDesign  <- sapply(names(simlist), function(x){
    simlist[[x]][runDesign[[x]]]
  })
  res$Design           <- runDesign
  colnames(res$Design) <- names(simlist)
  return(res)
}
