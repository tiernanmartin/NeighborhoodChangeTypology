
# EXTERNAL DATA -----------------------------------------------------------

plan_external_data <- drake::drake_plan(
  kc_boundary = make_kc_boundary()
)

use_data(plan_external_data, overwrite = TRUE)
