#' @title Make a (Congifured) Drake Plan
#' @description Make a \code{drake} plan using the project's speficic configuration setup.
#' @return The master internal configuration list, mostly containing arguments to make() and important objects constructed along the way. See drake_config() for more details.
#' @note See the \code{drake} manual for details: \url{https://ropenscilabs.github.io/drake-manual/hpc.html#on-your-local-machine-1}.
#' @export
make_with_config_project <- function(plan){

  # future::plan(future::multiprocess)

  drake::make(plan = plan,
              garbage_collection = TRUE)
}
