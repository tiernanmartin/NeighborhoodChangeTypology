#' @title Make a (Congifured) Drake Plan
#' @description Make a \code{drake} plan using the project's speficic configuration setup.
#' @param plan drake plan
#' @param .garbage_collection logical (default = FALSE), whether to run \code{gc()} on the plan
#' @param .beep logical (default = TRUE), whether to play the "ping" sound after the plan is made
#' @param .beep_on_error logical (default = TRUE), whether to play the "facebook" sound if the plan returns an error
#' @return The master internal configuration list, mostly containing arguments to make() and important objects constructed along the way. See drake_config() for more details.
#' @note See the \code{drake} manual for details: \url{https://ropenscilabs.github.io/drake-manual/hpc.html#on-your-local-machine-1}.
#' @export
make_with_config_project <- function(plan, .garbage_collection = FALSE, .beep = TRUE,.beep_on_error = TRUE){

  # future::plan(future::multiprocess)

  make_plan_expr <- function(){drake::make(plan = plan, garbage_collection = .garbage_collection)}

  # beep on either success or error
  if(.beep & .beep_on_error){
    beepr::beep_on_error(sound = 10,
                         expr = beepr::beep(sound = 1,
                                            expr = make_plan_expr()))
  } else

    # beep on error only

    if(.beep_on_error){
      beepr::beep_on_error(sound = 10,
                           expr = make_plan_expr())
    } else

      # beep on success only

      if(.beep){
        beepr::beep(sound = 1,
                    expr = make_plan_expr())
      } else

        # don't beep

        make_plan_expr()


}
