make_housing_market_parcels <- function(present_use_key,
                                        single_family_criteria,
                                        parcel_value,
                                        parcel_info_2005,
                                        parcel_info_2010,
                                        parcel_info_2018){

  # Create the function that will prepare each year's parcel data,
  # and the two lists to map over

  prep_parcels  <- function(p, tax_year){

      p %>% dplyr::transmute(PIN = make_pin(MAJOR,MINOR),
                    PRESENT_USE = PRESENTUSE,
                    SQFT_LOT = units::set_units(SQFTLOT,"ft^2"),
                    TAX_YEAR = tax_year)

  }

  p <- list( parcel_info_2005,
             parcel_info_2010,
             parcel_info_2018)

  tax_year <- list(2005,2010,2018)


  # Run the function on all three years of parcel data and then bind the results together

  p_all <- purrr::pmap_df(list(p, tax_year), prep_parcels) %>%
    dplyr::left_join(present_use_key, by = "PRESENT_USE") %>%
    dplyr::transmute(PIN,
              TAX_YEAR,
              PRESENT_USE = PRESENT_USE_DESC,
              SQFT_LOT = units::set_units(SQFT_LOT,"ft^2"))

  # Prepare parcel_value: where a parcel has multiple values for a given year, add them together

  parcel_value_ready <- parcel_value %>%
    dplyr::group_by(PIN, TAX_YEAR) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::ungroup()

  # Filter for single family parcels with complete records and appropriate sizes

  is_single_family <- function(TAX_YEAR, PRESENT_USE, SQFT_LOT, VALUE_IMPROVEMENT){

    if(any(is.na(c(TAX_YEAR, PRESENT_USE, SQFT_LOT, VALUE_IMPROVEMENT)))){return(FALSE)}


    years <- all(length(single_family_criteria$tax_years) == length(TAX_YEAR), single_family_criteria$tax_years %in% TAX_YEAR )

    use <- all(PRESENT_USE %in% single_family_criteria$present_uses)

    size <- all( max(SQFT_LOT, na.rm = TRUE) < single_family_criteria$parcel_area$upper,
                 min(SQFT_LOT, na.rm = TRUE) > single_family_criteria$parcel_area$lower)

    impr_val <- all(VALUE_IMPROVEMENT >= single_family_criteria$min_impr_value)

    all(years, use, size, impr_val)


  }

  # ~10 minute operation

  p_sf <- p_all %>%
    dplyr::inner_join(parcel_value_ready, by = c("PIN", "TAX_YEAR" )) %>%
    dplyr::group_by(PIN) %>%
    dplyr::summarise_all(list) %>%
    dplyr::mutate(SF_LGL = purrr::pmap_lgl(list(TAX_YEAR, PRESENT_USE, SQFT_LOT, VALUE_IMPROVEMENT),
                             is_single_family)) %>%
    tidyr::unnest() %>%
    dplyr::filter(SF_LGL) %>%
    dplyr::select(-SF_LGL)

return(p_sf)

}
