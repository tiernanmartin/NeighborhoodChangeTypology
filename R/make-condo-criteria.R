#' @title Make the Criteria for Condominium Units
#' @description Make the criteria used to identify the condominium
#'   units that will be used to identify housing market characteristics.
#' @return a list
#' @importFrom units set_units
#' @importFrom dplyr filter
#' @importFrom dplyr transmute
#' @importFrom dplyr pull
#' @importFrom stringr str_detect
#' @importFrom purrr map
#' @importFrom purrr flatten_chr
#' @export
make_condo_criteria <- function(condo_unit_type_key){


  # Find the Unit Types associated with residential condominiums

  check_condo_categories <- function(){
    condo_unit_type_key %>% print(n=Inf)

  }

  condo_unit_types <- c("Flat",
                  "Townhouse",
                  "Penthouse,Flat",
                  "Penthouse,Townhouse",
                  "Floating Home, Flat",
                  "Mobile Home",
                  "Floating Home, Townhouse",
                  "Live/Work",
                  "Apartments"
                  )


  condo_criteria <-
    list(
      "condo_unit_types" = condo_unit_types,
      "tax_years" = c(2005, 2010, 2018),
      "remained_condo" = TRUE,
      "min_impr_value" = 5000 # this is a bit arbitrary
    )

  return(condo_criteria)
}
