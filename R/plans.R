# PROJECT TEMPLATES PLAN --------------------------------------------------------------
#' @title Get the Project Templates Plan
#' @description Use \code{\link[drake]{drake_plan}} to create the project's
#'   templates plan.
#' @return a `drake` plan
#' @examples
#'
#' # Print one of the external data plans
#'
#' get_templates_plan()
#'
#'
#' # Make the plan, load a target, print the target
#'
#' \dontrun{
#'
#' make(get_templates_plan())
#'
#' readd(preliminary_model_plan)
#'
#' }

#' @export
get_templates_plan <- function(){

  pkgconfig::set_config("drake::strings_in_dots" = "literals")

  templates_plan <- drake::drake_plan(
    model_table = make_model_table(),
    indicator_template = make_indicator_template()

  )

  return(templates_plan)

}


# DATA PLANS -----------------------------------------------------


#' @title Get the Data Plans
#' @description Use \code{\link[drake]{drake_plan}} to create the data plans.
#' @return a `drake` plan
#' @examples
#'
#' # Print one of the external data plans
#'
#' get_data_cache_plan()
#'
#'
#' # Make the plan, load a target, print the target
#'
#' \dontrun{
#'
#' data_plans <- bind_plans(get_data_source_plan(), get_data_cache_plan())
#'
#' make(data_plans)
#'
#' readd(kc_boundary)
#' }

#' @export
get_data_source_plan <- function(){

  pkgconfig::set_config("drake::strings_in_dots" = "literals")

  # PREP PLAN NOTE: these should be *manually* triggered to prevent unnecessary,
  #                 long-running uploads to OSF.io

  prep_plan <- drake::drake_plan(
    acs_tables = make_acs_tables(),
    acs_data_prep_status = target(command = prepare_acs_data(indicator_template, model_table, acs_tables, path = file_out("extdata/source/acs-data.csv")),
                                  trigger = trigger(mode = "condition", condition = FALSE)),
    hud_chas_data_prep_status = target(command = prepare_hud_chas_data(zip_path = file_out("extdata/source/hud-chas-data.zip")),
                                       trigger = trigger(mode = "condition", condition = FALSE)),
    ltdb_data_prep_status = target(command = prepare_ltdb_data(path = file_out("extdata/source/ltdb-data.csv")),
                                   trigger = trigger(mode = "condition", condition = FALSE)),
    kc_boundary_prep_status = target(prepare_kc_boundary(path = file_out("extdata/source/kc-boundary.gpkg")),
                                     trigger = trigger(mode = "condition", condition = FALSE)),
    white_center_place_prep_status = target(prepare_white_center_place(path = file_out("extdata/source/white-center-place.gpkg")),
                                            trigger = trigger(mode = "condition", condition = FALSE)),
    waterbodies_prep_status = target(prepare_waterbodies(path = file_out("extdata/source/ECY_WAT_NHDWAMajor.zip")),
                                     trigger = trigger(mode = "condition", condition = FALSE)),
    parcel_boundaries_prep_status = target(prepare_parcel_boundaries(path = file_out("extdata/source/parcel_SHP.zip")),
                                           trigger = trigger(mode = "condition", condition = FALSE)),
    parcel_data_prep_status = target(prepare_parcel_data(model_table, acs_tables, zip_path = file_out("extdata/source/kc-assessor-parcels-2005-2010-2018.zip")),
                                     trigger = trigger(mode = "condition", condition = FALSE)),
    census_tracts_2016_prep_status = target(prepare_census_tracts_2016(path = file_out("extdata/source/census-tracts-2016.gpkg")),
                                            trigger = trigger(mode = "condition", condition = FALSE))

  )

  upload_plan <- drake::drake_plan(
    has_osf_access = check_osf_access(project_title = "Neighborhood Change Typology"),
    acs_data_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                         project_id = "sj7n9",
                                                         file_id = "xhzv8",
                                                         path = file_in("extdata/source/acs-data.csv")),
                                    trigger = trigger(command = FALSE)),
    hud_chas_data_upload_status = target(command = osf_upload_or_update(has_osf_access = has_osf_access,
                                                                        project_id = "sj7n9",
                                                                        file_id = "rc8wk",
                                                                        path = file_in("extdata/source/hud-chas-data.zip")),
                                         trigger = trigger(command = FALSE)),
    ltdb_data_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                          project_id = "sj7n9",
                                                          file_id = "7xwga",
                                                          path = file_in("extdata/source/ltdb-data.csv")),
                                     trigger = trigger(command = FALSE)),
    kc_boundary_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                            project_id = "sj7n9",
                                                            file_id = "mzd5v",
                                                            path = file_in("extdata/source/kc-boundary.gpkg")),
                                       trigger = trigger(command = FALSE)),
    white_center_place_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                   project_id = "sj7n9",
                                                                   file_id = "ctbqp",
                                                                   path = file_in("extdata/source/white-center-place.gpkg")),
                                              trigger = trigger(command = FALSE)),
    waterbodies_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                            project_id = "sj7n9",
                                                            file_id = "gevkt",
                                                            path = file_in("extdata/source/ECY_WAT_NHDWAMajor.zip")),
                                       trigger = trigger(command = FALSE)),
    parcel_boundaries_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                  project_id = "sj7n9",
                                                                  file_id = "2ufmh",
                                                                  path = file_in("extdata/source/parcel_SHP.zip")),
                                             trigger = trigger(command = FALSE)),
    parcel_data_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                            project_id = "sj7n9",
                                                            file_id = "t7b8v",
                                                            path = file_in("extdata/source/kc-assessor-parcels-2005-2010-2018.zip")),
                                       trigger = trigger(command = FALSE)),
    census_tracts_2016_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                   project_id = "sj7n9",
                                                                   file_id = "cagvu",
                                                                   path = file_in("extdata/source/census-tracts-2016.gpkg")),
                                              trigger = trigger(command = FALSE))
  )

  target_archive_plan <- drake::drake_plan(
    cpi_prep_status = prepare_cpi(path= file_out("extdata/source/cpi-2000-2018.csv")),
    cpi_upload_status = target(command = osf_upload_or_update(has_osf_access = has_osf_access,
                                                              project_id = "sj7n9",
                                                              file_id = "8y3cj",
                                                              path = file_in("extdata/source/cpi-2000-2018.csv")),
                               trigger = trigger(condition = FALSE))
  )


  data_source_plan <- drake::bind_plans(prep_plan, upload_plan)

  return(data_source_plan)

}


#' @export
get_data_cache_plan <- function(){

  pkgconfig::set_config("drake::strings_in_dots" = "literals")

  download_plan <- drake::drake_plan(
    acs_data_filepath = target(command = osf_download_files(id = "xhzv8", path = file_out("extdata/osf/acs-data.csv")),
                               trigger = trigger(change = get_osf_version("sj7n9","acs-data.csv"))),
    hud_chas_data_filepath = target(command = osf_download_files(id = "rc8wk", path = file_out("extdata/osf/hud-chas-data.zip")),
                                    trigger = trigger(change = get_osf_version("sj7n9","hud-chas-data.zip"))),
    ltdb_data_filepath = target(command = osf_download_files(id = "7xwga", path = file_out("extdata/osf/ltdb-data.csv")),
                                trigger = trigger(change = get_osf_version("sj7n9","ltdb-data.csv"))),
    kc_boundary_filepath = target(command = osf_download_files(id = "mzd5v", path = file_out("extdata/osf/kc-boundary.gpkg")),
                                  trigger = trigger(change = get_osf_version("sj7n9", "kc-boundary.gpkg"))),
    white_center_place_filepath = target(command = osf_download_files(id = "ctbqp", path = file_out("extdata/osf/kc-boundary.gpkg")),
                                         trigger = trigger(change = get_osf_version("sj7n9", "white-center-place.gpkg"))),
    waterbodies_filepath = target(command = osf_download_files(id = "gevkt", path = file_out("extdata/osf/ECY_WAT_NHDWAMajor.zip")),
                                  trigger = trigger(change = get_osf_version("sj7n9", "ECY_WAT_NHDWAMajor.zip"))),
    parcel_boundaries_filepath = target(command = osf_download_files(id = "2ufmh", path = file_out("extdata/osf/parcel_SHP.zip")),
                                        trigger = trigger(change = get_osf_version("sj7n9", "parcel_SHP.zip"))),
    census_tracts_2016_filepath = target(command = osf_download_files(id = "cagvu", path = file_out("extdata/osf/census-tracts-2016.gpkg")),
                                         trigger = trigger(change = get_osf_version("sj7n9", "census-tracts-2016.gpkg"))),
    parcel_data_filepath = target(command = osf_download_files(id = "t7b8v", path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2018.zip")),
                                  trigger = trigger(change = get_osf_version("sj7n9", "kc-assessor-parcels-2005-2010-2018.zip"))),
    cpi_filepath = target(command = osf_download_files(id = "8y3cj", path = file_out("extdata/osf/cpi-2000-2018.csv")),
                          trigger = trigger(change = get_osf_version("sj7n9", "cpi-2000-2018.csv")))
  )

  ready_plan <- drake::drake_plan(
    acs_data = make_acs_data(path = file_in("extdata/osf/acs-data.csv")),
    hud_chas_data = make_hud_chas_data(zip_path = file_in("extdata/osf/hud-chas-data.zip"),
                                       file_path = file_out("extdata/osf/hud-chas-data.csv")),
    ltdb_data = make_ltdb_data(path = file_in("extdata/osf/ltdb-data.csv")),
    kc_boundary = make_kc_boundary(path = file_in("extdata/osf/kc-boundary.gpkg")),
    white_center_place = make_white_center_place(path = file_in("extdata/osf/white-center-place.gpkg")),
    waterbodies = make_waterbodies(path = file_in("extdata/osf/ECY_WAT_NHDWAMajor.zip")),
    parcel_boundaries = make_parcel_boundaries(path = file_in("extdata/osf/parcel_SHP.zip")),
    census_tracts_2016 = make_census_tracts_2016(path = file_in("extdata/osf/census-tracts-2016.gpkg")),
    parcel_value =  make_parcel_value(zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2018.zip"),
                                      file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_ValueHistory_V.csv")),
    parcel_sales =  make_parcel_sales(zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2018.zip"),
                                      file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_RPSale.csv")),
    parcel_info_2005 = make_parcel_info_2005(zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2018.zip"),
                                             file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_Parcel_2005.csv")),
    parcel_info_2010 = make_parcel_info_2010(zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2018.zip"),
                                             file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_Parcel_2010.csv")),
    parcel_info_2018 = make_parcel_info_2018(zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2018.zip"),
                                             file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_Parcel_2018.csv")),
    parcel_lut_2005 = make_parcel_lut_2005(zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2018.zip"),
                                           file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_LookUp_2005.csv")),
    parcel_lut_2018 = make_parcel_lut_2018(zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2018.zip"),
                                           file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_LookUp_2018.csv")),
    condo_info_2005 =  make_condo_info_2005(zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2018.zip"),
                                            file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_Condo_Unit_2005.csv")),
    condo_info_2010 =  make_condo_info_2010(zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2018.zip"),
                                            file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_Condo_Unit_2010.csv")),
    condo_info_2018 =  make_condo_info_2018(zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2018.zip"),
                                            file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_CondoUnit2_2018.csv")),
    res_bldg_2005 = make_res_bldg_2005(zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2018.zip"),
                                       file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_ResBldg_2005.csv")),
    res_bldg_2010 = make_res_bldg_2010(zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2018.zip"),
                                       file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_ResBldg_2010.csv")),
    res_bldg_2018 = make_res_bldg_2018(zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2018.zip"),
                                       file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2018/EXTR_ResBldg_2018.csv")),
    cpi = make_cpi(path = file_in("extdata/osf/cpi-2000-2018.csv"))
  )

  data_cache_plan <- drake::bind_plans(download_plan, ready_plan)

  return(data_cache_plan)

}


# INDICATOR PLAN ------------------------------------------------------

#' @title Get the Indicator Plan
#' @description Use \code{\link[drake]{drake_plan}} to create the indicator plan.
#' @return a `drake` plan
#' @export
#' @examples
#'
#' # Print the plan
#'
#' get_indicator_plan()
#'
#'
#' # Make the plan, load a target, print the target
#'
#' \dontrun{
#'
#' make(get_indicator_plan())
#'
#' loadd(acs_indicators)
#'
#' print(acs_indicators)
#' }
get_indicator_plan <- function(){

  pkgconfig::set_config("drake::strings_in_dots" = "literals")

  ind_prep_plan <- drake::drake_plan(
    parcel_tract_overlay = make_parcel_tract_overlay(parcel_boundaries, census_tracts_2016),
    census_tracts_2016_trimmed = make_census_tracts_2016_trimmed(census_tracts_2016, waterbodies),
    present_use_key = make_present_use_key(parcel_lut_2005, parcel_lut_2018),
    condo_unit_type_key = make_condo_unit_type_key(parcel_lut_2005, parcel_lut_2018),
    sales_lut_key_list = make_sales_lut_key_list(parcel_lut_2018),
    single_family_criteria = make_single_family_criteria(present_use_key),
    condo_criteria = make_condo_criteria(condo_unit_type_key),
    sales_criteria = make_sales_criteria(),
    excluded_tract_geoids = make_excluded_tract_geoids()
  )

  ind_plan <- drake::drake_plan(
    # indicators_cnt = make_indicators_cnt(acs_data, hud_chas_data, acs_tables),
    # indicators_pct = make_indicators_pct(acs_data, hud_chas_data, acs_tables),
    # housing_market_parcel_value = make_housing_market_parcel_value(present_use_key,
    #                                                                condo_unit_type_key,
    #                                                                single_family_criteria,
    #                                                                condo_criteria,
    #                                                                cpi,
    #                                                                parcel_value,
    #                                                                parcel_info_2005,
    #                                                                parcel_info_2010,
    #                                                                parcel_info_2018,
    #                                                                condo_info_2005,
    #                                                                condo_info_2010,
    #                                                                condo_info_2018),
    # housing_market_parcel_appr = make_housing_market_parcel_appr(housing_market_parcel_value),
    # housing_market_sales = make_housing_market_sales(parcel_sales,
    #                                                  sales_lut_key_list,
    #                                                  sales_criteria,
    #                                                  present_use_key,
    #                                                  single_family_criteria,
    #                                                  condo_unit_type_key,
    #                                                  condo_criteria,
    #                                                  cpi,
    #                                                  parcel_info_2005,
    #                                                  parcel_info_2010,
    #                                                  parcel_info_2018,
    #                                                  condo_info_2005,
    #                                                  condo_info_2010,
    #                                                  condo_info_2018,
    #                                                  res_bldg_2005,
    #                                                  res_bldg_2010,
    #                                                  res_bldg_2018),
    # housing_market_indicators = make_housing_market_indicators(census_tracts_2016,
    #                                                            excluded_tract_geoids,
    #                                                            parcel_boundaries,
    #                                                            parcel_tract_overlay,
    #                                                            housing_market_parcel_value,
    #                                                            housing_market_parcel_appr),
    # vulnerability_indicators = make_vulnerability_indicators(acs_indicators),
    # demo_change_indicators = make_demo_change_indicators(acs_indicators),
    tmp = c("placeholder")
  )

  indicator_plan <- drake::bind_plans(ind_prep_plan, ind_plan)

  return(indicator_plan)

}


# MODEL PLAN --------------------------------------------------------------

#' @title Get the Model Plan
#' @description Use \code{\link[drake]{drake_plan}} to create the typology plan.
#' @return a `drake` plan
#' @export
#' @examples
#'
#' # Print the plan
#'
#' get_model_plan()
#'
#'
#' # Make the plan, load a target, print the target
#'
#' \dontrun{
#'
#' make(get_model_plan())
#'
#' loadd(acs_indicators)
#'
#' print(acs_indicators)
#' }
get_model_plan <- function(){

  pkgconfig::set_config("drake::strings_in_dots" = "literals")

  bates_plan <- drake::drake_plan(


    # typology = make_typology(vulnerability_indicators, demo_change_indicators, housing_market_indicators, census_tracts_2016_trimmed)
  )

  original_typology_plan <- drake::drake_plan(
    typology = make_typology(vulnerability_indicators, demo_change_indicators, housing_market_indicators, census_tracts_2016_trimmed)
  )


  return(model_plan)
}


# WORKFLOW PLAN --------------------------------------------------------------

#' @title Get the Workflow Plan
#' @description Use \code{\link[drake]{drake_plan}} to create the meta plan.
#' @return a `drake` plan
#' @export
#' @examples
#'
#' # Print the plan
#'
#' get_workflow_plan()
#'
#'
#' # Make the plan, load a target, print the target
#'
#' \dontrun{
#'
#' make(get_workflow_plan())
#'
#' loadd(acs_indicators)
#'
#' print(acs_indicators)
#' }
get_workflow_plan <- function(){

  pkgconfig::set_config("drake::strings_in_dots" = "literals")

  workflow_plan <- drake::bind_plans(
    get_templates_plan(),
    get_data_source_plan(),
    get_data_cache_plan(),
    get_indicator_plan()
  )

  return(workflow_plan)

}
