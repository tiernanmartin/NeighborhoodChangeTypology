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
    metadata_template = make_metadata_template(),
    data_template = make_data_template(),
    variable_template = make_variable_template(),
    indicator_template = make_indicator_template(),
    indicator_topic_template = make_indicator_topic_template(),
    indicator_type_template = make_indicator_type_template(),
    acs_tables = make_acs_tables()

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
    acs_data_prep_status = target(command = prepare_acs_data(data_template, model_table, acs_tables, path = file_out("extdata/source/acs-data.csv")),
                                  trigger = trigger(mode = "condition", condition = FALSE)),
    hud_chas_data_prep_status = target(command = prepare_hud_chas_data(zip_path = file_out("extdata/source/hud-chas-data.zip")),
                                       trigger = trigger(mode = "condition", condition = FALSE)),
    ltdb_data_prep_status = target(command = prepare_ltdb_data(data_template, acs_tables, path = file_out("extdata/source/ltdb-data.csv")),
                                   trigger = trigger(mode = "condition", condition = FALSE)),
    factfinder_data_prep_status = target(command = prepare_factfinder_data(data_template, acs_tables, path = file_out("extdata/source/factfinder-data.csv")),
                                         trigger = trigger(mode = "condition", condition = FALSE)),
    kc_boundary_prep_status = target(prepare_kc_boundary(path = file_out("extdata/source/kc-boundary.gpkg")),
                                     trigger = trigger(mode = "condition", condition = FALSE)),
    white_center_place_prep_status = target(prepare_white_center_place(path = file_out("extdata/source/white-center-place.gpkg")),
                                            trigger = trigger(mode = "condition", condition = FALSE)),
    waterbodies_prep_status = target(prepare_waterbodies(path = file_out("extdata/source/ECY_WAT_NHDWAMajor.zip")),
                                     trigger = trigger(mode = "condition", condition = FALSE)),
    parcel_boundaries_prep_status = target(prepare_parcel_boundaries(path = file_out("extdata/source/parcel_SHP.zip")),
                                           trigger = trigger(mode = "condition", condition = FALSE)),
    parcel_data_prep_status = target(prepare_parcel_data(model_table, acs_tables, zip_path = file_out("extdata/source/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip")),
                                     trigger = trigger(mode = "condition", condition = FALSE)),
    census_tracts_2016_prep_status = target(prepare_census_tracts_2016(path = file_out("extdata/source/census-tracts-2016.gpkg")),
                                            trigger = trigger(mode = "condition", condition = FALSE)),
    cpi_prep_status = target(prepare_cpi(path= file_out("extdata/source/cpi-2000-2018.csv")),
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
    factfinder_data_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                project_id = "sj7n9",
                                                                file_id = "9cvqf",
                                                                path = file_in("extdata/source/factfinder-data.csv")),
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
                                                            file_id = "9t5vc",
                                                            path = file_in("extdata/source/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip")),
                                       trigger = trigger(command = FALSE)),
    census_tracts_2016_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                   project_id = "sj7n9",
                                                                   file_id = "cagvu",
                                                                   path = file_in("extdata/source/census-tracts-2016.gpkg")),
                                              trigger = trigger(command = FALSE)),
    cpi_upload_status = target(command = osf_upload_or_update(has_osf_access = has_osf_access,
                                                              project_id = "sj7n9",
                                                              file_id = "8y3cj",
                                                              path = file_in("extdata/source/cpi-2000-2018.csv")),
                               trigger = trigger(condition = FALSE))
  )

  # target_archive_plan <- drake::drake_plan(
  #
  #
  # )


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
    factfinder_data_filepath = target(command = osf_download_files(id = "9cvqf", path = file_out("extdata/osf/factfinder-data.csv")),
                                      trigger = trigger(change = get_osf_version("sj7n9","factfinder-data.csv"))),
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
    parcel_data_filepath = target(command = osf_download_files(id = "9t5vc", path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip")),
                                  trigger = trigger(change = get_osf_version("sj7n9", "kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"))),
    cpi_filepath = target(command = osf_download_files(id = "8y3cj", path = file_out("extdata/osf/cpi-2000-2018.csv")),
                          trigger = trigger(change = get_osf_version("sj7n9", "cpi-2000-2018.csv")))
  )

  ready_plan <- drake::drake_plan(
    acs_data = make_acs_data(path = file_in("extdata/osf/acs-data.csv")),
    hud_chas_data = make_hud_chas_data(data_template,
                                       zip_path = file_in("extdata/osf/hud-chas-data.zip"),
                                       file_path = file_out("extdata/osf/hud-chas-data.csv")),
    hud_chas_data_lut = make_hud_chas_data_lut(zip_path = file_in("extdata/osf/hud-chas-data.zip"),
                                               file_path = file_out("extdata/osf/hud-chas-data/hud-chas-datadictionary-table7.csv")),
    ltdb_data = make_ltdb_data(path = file_in("extdata/osf/ltdb-data.csv")),
    factfinder_data = make_factfinder_data(path = file_in("extdata/osf/factfinder-data.csv")),
    kc_boundary = make_kc_boundary(path = file_in("extdata/osf/kc-boundary.gpkg")),
    white_center_place = make_white_center_place(path = file_in("extdata/osf/white-center-place.gpkg")),
    waterbodies = make_waterbodies(path = file_in("extdata/osf/ECY_WAT_NHDWAMajor.zip")),
    parcel_boundaries = make_parcel_boundaries(path = file_in("extdata/osf/parcel_SHP.zip")),
    census_tracts_2016 = make_census_tracts_2016(path = file_in("extdata/osf/census-tracts-2016.gpkg")),
    parcel_value =  make_parcel_value(data_template,
                                      zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                      file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_ValueHistory_V.csv")),
    parcel_sales =  make_parcel_sales(data_template,
                                      zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                      file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_RPSale.csv")),
    parcel_info_2005 = make_parcel_info_2005(metadata_template,
                                             zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                             file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_Parcel_2005.csv")),
    parcel_info_2010 = make_parcel_info_2010(metadata_template,
                                             zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                             file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_Parcel_2010.csv")),
    parcel_info_2013 = make_parcel_info_2013(metadata_template,
                                             zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                             file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_Parcel_2013.csv")),
    parcel_info_2014 = make_parcel_info_2014(metadata_template,
                                             zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                             file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_Parcel_2014.csv")),
    parcel_info_2015 = make_parcel_info_2015(metadata_template,
                                             zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                             file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_Parcel_2015.csv")),
    parcel_info_2016 = make_parcel_info_2016(metadata_template,
                                             zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                             file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_Parcel_2016.csv")),
    parcel_info_2017 = make_parcel_info_2017(metadata_template,
                                             zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                             file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_Parcel_2017.csv")),
    parcel_info_2018 = make_parcel_info_2018(metadata_template,
                                             zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                             file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_Parcel_2018.csv")),
    parcel_lut_2005 = make_parcel_lut_2005(metadata_template,
                                           zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                           file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_LookUp_2005.csv")),
    parcel_lut_2018 = make_parcel_lut_2018(metadata_template,
                                           zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                           file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_LookUp_2018.csv")),
    condo_info_2005 =  make_condo_info_2005(metadata_template,
                                            zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                            file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_Condo_Unit_2005.csv")),
    condo_info_2010 =  make_condo_info_2010(metadata_template,
                                            zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                            file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_Condo_Unit_2010.csv")),
    condo_info_2013 =  make_condo_info_2013(metadata_template,
                                            zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                            file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_Condo_Unit_2013.csv")),
    condo_info_2014 =  make_condo_info_2014(metadata_template,
                                            zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                            file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_Condo_Unit_2014.csv")),
    condo_info_2015 =  make_condo_info_2015(metadata_template,
                                            zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                            file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_Condo_Unit_2015.csv")),
    condo_info_2016 =  make_condo_info_2016(metadata_template,
                                            zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                            file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_Condo_Unit_2016.csv")),
    condo_info_2017 =  make_condo_info_2017(metadata_template,
                                            zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                            file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_CondoUnit2_2017.csv")),
    condo_info_2018 =  make_condo_info_2018(metadata_template,
                                            zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                            file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_CondoUnit2_2018.csv")),
    res_bldg_2005 = make_res_bldg_2005(metadata_template,
                                       zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                       file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_ResBldg_2005.csv")),
    res_bldg_2010 = make_res_bldg_2010(metadata_template,
                                       zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                       file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_ResBldg_2010.csv")),
    res_bldg_2013 = make_res_bldg_2013(metadata_template,
                                       zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                       file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_ResBldg_2013.csv")),
    res_bldg_2014 = make_res_bldg_2014(metadata_template,
                                       zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                       file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_ResBldg_2014.csv")),
    res_bldg_2015 = make_res_bldg_2015(metadata_template,
                                       zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                       file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_ResBldg_2015.csv")),
    res_bldg_2016 = make_res_bldg_2016(metadata_template,
                                       zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                       file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_ResBldg_2016.csv")),
    res_bldg_2017 = make_res_bldg_2017(metadata_template,
                                       zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                       file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_ResBldg_2017.csv")),
    res_bldg_2018 = make_res_bldg_2018(metadata_template,
                                       zip_path = file_in("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018.zip"),
                                       file_path = file_out("extdata/osf/kc-assessor-parcels-2005-2010-2013-2014-2015-2016-2017-2018/EXTR_ResBldg_2018.csv")),
    cpi = make_cpi(path = file_in("extdata/osf/cpi-2000-2018.csv"))
  )

  data_cache_plan <- drake::bind_plans(download_plan, ready_plan)

  return(data_cache_plan)

}

# VARIABLE PLAN ------------------------------------------------------

#' @title Get the Variable Plan
#' @description Use \code{\link[drake]{drake_plan}} to create the variable plan.
#' @return a `drake` plan
#' @export
#' @examples
#'
#' # Print the plan
#'
#' get_variable_plan()
#'
#'
#' # Make the plan, load a target, print the target
#'
#' \dontrun{
#'
#' make(get_variable_plan())
#'
#' loadd(parcel_tract_overlay)
#'
#' print(parcel_tract_overlay)
#' }
get_variable_plan <- function(){

  pkgconfig::set_config("drake::strings_in_dots" = "literals")

  var_prep_plan <- drake::drake_plan(
    parcel_tract_overlay = make_parcel_tract_overlay(parcel_boundaries, census_tracts_2016),
    census_tracts_2016_trimmed = make_census_tracts_2016_trimmed(census_tracts_2016, waterbodies),
    present_use_key = make_present_use_key(parcel_lut_2005, parcel_lut_2018),
    condo_unit_type_key = make_condo_unit_type_key(parcel_lut_2005, parcel_lut_2018),
    sales_lut_key_list = make_sales_lut_key_list(parcel_lut_2018),
    single_family_criteria = make_single_family_criteria(present_use_key),
    condo_criteria = make_condo_criteria(condo_unit_type_key),
    sales_criteria = make_sales_criteria(),
    excluded_tract_geoids = make_excluded_tract_geoids(),
    census_geography_metadata = make_census_geography_metadata(acs_data),
    community_metadata = make_community_metadata(),
    county_community_tract_all_metadata = make_county_community_tract_all_metadata(acs_data, community_metadata),
    parcel_all_metadata = make_parcel_all_metadata(present_use_key,
                                                   condo_unit_type_key,
                                                   parcel_tract_overlay,
                                                   parcel_info_2005,
                                                   parcel_info_2010,
                                                   parcel_info_2013,
                                                   parcel_info_2014,
                                                   parcel_info_2015,
                                                   parcel_info_2016,
                                                   parcel_info_2017,
                                                   parcel_info_2018,
                                                   condo_info_2005,
                                                   condo_info_2010,
                                                   condo_info_2013,
                                                   condo_info_2014,
                                                   condo_info_2015,
                                                   condo_info_2016,
                                                   condo_info_2017,
                                                   condo_info_2018,
                                                   res_bldg_2005,
                                                   res_bldg_2010,
                                                   res_bldg_2013,
                                                   res_bldg_2014,
                                                   res_bldg_2015,
                                                   res_bldg_2016,
                                                   res_bldg_2017,
                                                   res_bldg_2018,
                                                   parcel_sales,
                                                   variable_template),
    tmp = c("placeholder")
  )

  var_plan <- drake::drake_plan(
    acs_variables = make_acs_variables(acs_data, acs_tables, variable_template),
    hud_chas_variables = make_hud_chas_variables(hud_chas_data, hud_chas_data_lut, model_table, census_geography_metadata, variable_template),
    ltdb_variables = make_ltdb_variables(ltdb_data, census_geography_metadata, variable_template),
    factfinder_variables = make_factfinder_variables(factfinder_data, variable_template, census_geography_metadata),
    # parcel_sales_variables = make_parcel_sales_variables(parcel_sales,
    #                                                      parcel_all_metadata,
    #                                                      sales_lut_key_list,
    #                                                      sales_criteria,
    #                                                      single_family_criteria,
    #                                                      condo_criteria,
    #                                                      cpi,
    #                                                      variable_template),
    # parcel_value_variables_part1 = make_parcel_value_variables_part1(parcel_all_metadata,
    #                                                                  single_family_criteria,
    #                                                                  condo_criteria,
    #                                                                  cpi,
    #                                                                  parcel_value),
    # parcel_value_variables_part2 = make_parcel_value_variables_part2(parcel_value_variables_part1),
    # parcel_value_variables = make_parcel_value_variables(parcel_value_variables_part2,
    #                                                      variable_template),
    tmp = c("placeholder")
  )

  variable_plan <- drake::bind_plans(var_prep_plan, var_plan)

  return(variable_plan)

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
    indicators_cnt_pct_acs_chas = make_indicators_cnt_pct_acs_chas(acs_variables,
                                                                   hud_chas_variables,
                                                                   county_community_tract_all_metadata,
                                                                   community_metadata),
    indicators_cnt_pct_value = make_indicators_cnt_pct_value(parcel_value_variables,
                                                             parcel_tract_overlay,
                                                             county_community_tract_all_metadata,
                                                             community_metadata),
    indicators_cnt_pct_sales = make_indicators_cnt_pct_sales(parcel_sales_variables,
                                                             parcel_tract_overlay,
                                                             county_community_tract_all_metadata,
                                                             community_metadata),
    indicators_median_acs_ltdb_ff = make_indicators_median_acs_ltdb_ff(acs_variables,
                                                                       ltdb_variables,
                                                                       factfinder_variables,
                                                                       county_community_tract_all_metadata,
                                                                       community_metadata),
    indicators_median_value = make_indicators_median_value(parcel_value_variables,
                                                           parcel_tract_overlay,
                                                           county_community_tract_all_metadata,
                                                           community_metadata),
    indicators_median_sales = make_indicators_median_sales(parcel_sales_variables,
                                                           parcel_tract_overlay,
                                                           county_community_tract_all_metadata,
                                                           community_metadata),
    change_endyears = make_change_endyears(),
    tmp = c("placeholder")
  )

  ind_plan <- drake::drake_plan(
    indicators_cnt_pct = make_indicators_cnt_pct(indicators_cnt_pct_acs_chas,
                                                 indicators_cnt_pct_value,
                                                 indicators_cnt_pct_sales,
                                                 indicator_template),
    indicators_median = make_indicators_median(indicators_median_acs_ltdb_ff,
                                               indicators_median_value,
                                               indicators_median_sales,
                                               indicator_template),
    indicators_by_topic = make_indicators_by_topic(indicators_cnt_pct,
                                                   indicators_median,
                                                   model_table,
                                                   indicator_topic_template),
    # sample_size_metadata = make_sample_size_metadata(indicators_cnt_pct,
    #                              indicators_median),
    # indicators = make_indicators(indicators_cnt_pct,
    #                              indicators_median,
    #                              sample_size_metadata),


    tmp2 = c("placeholder")
  )

  # sample_size_plan <- drake::drake_plan(
  #   sample_metadata = make_sample_metadata(indicators_cnt_pct,
  #                                          indicators_median)
  # )

  ind_type_plan <- drake::drake_plan(
    indicators_comparison = make_indicators_comparison(indicators_by_topic,
                                                       change_endyears,
                                                       indicator_type_template),
    indicators_comparison_of_change = make_indicators_comparison_of_change(indicators_by_topic,
                                                                           change_endyears,
                                                                           indicator_type_template),
    indicators_change_in_comparison = make_indicators_change_in_comparison(indicators_comparison,
                                                                           change_endyears,
                                                                           indicator_type_template),
    tmp3 = c("placeholder")
  )



  indicator_plan <- drake::bind_plans(ind_prep_plan, ind_plan, ind_type_plan)

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

  coo_original_plan <- drake::drake_plan()

  portland_plan <- drake::drake_plan(
    portland_model_vulnerability = make_portland_model_vulnerability(),
    tmp_portland_plan = c("tmp")
  )

  coo_original_updated_plan <- drake::drake_plan()

  original_revised <- drake::drake_plan(
    # typology = make_typology(vulnerability_indicators, demo_change_indicators, housing_market_indicators, census_tracts_2016_trimmed)
  )

  model_plan <- bind_plans(coo_original_plan,
                           portland_plan,
                           coo_original_updated_plan,
                           original_revised
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
    # get_data_source_plan(),  # this only needs to be included when a data source is added or changed
    get_data_cache_plan(),
    get_variable_plan(),
    get_indicator_plan(),
    get_model_plan()
  )

  return(workflow_plan)

}
