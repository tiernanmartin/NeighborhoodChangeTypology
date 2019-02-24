
#' @title Make Housing Market Parcel Value Appreciation Data
#' @description Temporary description
#' @param present_use_key Tibble, Temporary description
#' @param single_family_criteria Tibble, Temporary description.
#' @param cpi Tibble, Temporary description.
#' @param parcel_value Tibble, Temporary description.
#' @param parcel_info_2005 Tibble, Temporary description.
#' @param parcel_info_2010 Tibble, Temporary description.
#' @param parcel_info_2018 Tibble, Temporary description.
#' @return a `tibble`
#' @export
make_housing_market_parcel_appr <- function(housing_market_parcel_value){


  housing_market_parcel_appr <- housing_market_parcel_value %>%
    dplyr::select(PIN,
                  HOME_TYPE,
                  TAX_YEAR,
                  VALUE_TOTAL,
                  tidyselect::matches("LGL")) %>%
    dplyr::mutate(TAX_YEAR = stringr::str_c("YEAR_",TAX_YEAR,sep = "")) %>%
    tidyr::spread(TAX_YEAR, VALUE_TOTAL) %>%
    dplyr::mutate(YEARS_05_10 = YEAR_2010/YEAR_2005 - 1,
                  YEARS_05_18 = YEAR_2018/YEAR_2005 - 1,
                  YEARS_10_18 = YEAR_2018/YEAR_2010 - 1) %>%
    tidyr::gather(TAX_YEAR_RANGE, APPRECIATION, tidyselect::matches("YEARS")) %>%
    dplyr::select(PIN, HOME_TYPE, tidyselect::matches("LGL"), TAX_YEAR_RANGE, APPRECIATION )

  check_distribution_by_time <- function(){
    hist_data <- housing_market_parcel_appr %>%
    dplyr::mutate(TAX_YEAR_RANGE = factor(TAX_YEAR_RANGE,
                                   levels = c("YEARS_05_10","YEARS_10_18", "YEARS_05_18"),
                                   ordered = TRUE)) %>%
    dplyr::filter(CONDO_LGL|SF_LGL) %>%
    dplyr::filter(APPRECIATION < 2.5)


    label_data <- hist_data %>%
      dplyr::group_by(HOME_TYPE, TAX_YEAR_RANGE) %>%
      dplyr::summarise(N = paste0("n = ",scales::comma(sum(!is.na(APPRECIATION))))) %>%
      dplyr::ungroup()


    ggplot2::ggplot(data = hist_data,
           ggplot2::aes(x = APPRECIATION)) +
    ggplot2::geom_histogram() +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::comma) +
    ggplot2::facet_grid(HOME_TYPE ~ TAX_YEAR_RANGE,scales = "fixed") +
    geom_text(data = label_data, aes(x = 2, y = 150000, label = N), inherit.aes = FALSE) +
    ggplot2::labs(title = "Distribution of Value Appreciation by Time Period",
         subtitle = "Parcels and Condominiums in King County, WA",
         caption = "Data: King County Department of Assessments")

  }

  check_distribution_by_type <- function(){
    hist_data <- housing_market_parcel_appr %>%
    dplyr::mutate(TAX_YEAR_RANGE = factor(TAX_YEAR_RANGE,
                                   levels = c("YEARS_05_10","YEARS_10_18", "YEARS_05_18"),
                                   ordered = TRUE)) %>%
    dplyr::filter(CONDO_LGL|SF_LGL) %>%
    dplyr::filter(APPRECIATION < 2.5)


    label_data <- hist_data %>%
      dplyr::group_by(HOME_TYPE, TAX_YEAR_RANGE) %>%
      dplyr::summarise(N = paste0("n = ",scales::comma(sum(!is.na(APPRECIATION))))) %>%
      dplyr::ungroup()


    ggplot2::ggplot(data = hist_data,
           ggplot2::aes(x = APPRECIATION)) +
    ggplot2::geom_histogram() +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::comma) +
    ggplot2::facet_grid(TAX_YEAR_RANGE ~ HOME_TYPE,scales = "fixed") +
    geom_text(data = label_data, aes(x = 2, y = 130000, label = N), inherit.aes = FALSE) +
    ggplot2::labs(title = "Distribution of Value Appreciation by Residence Type",
         subtitle = "Parcels and Condominiums in King County, WA",
         caption = "Data: King County Department of Assessments")

  }

  return(housing_market_parcel_appr)

}
