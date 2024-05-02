
source("packages.R")
source("conflicts.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

library(future)
library(future.callr)
plan(callr)


manuscript_version <- 1

if(!dir.exists(glue("doc/manuscript-v{manuscript_version}"))){
  dir.create(glue("doc/manuscript-v{manuscript_version}"))
}


labels_tar <- tar_target(
  labels,
  list(variables = c(age_yrs = "Age, years",
                     race_ethnicity = "Race/ethnicity",
                     sex = "Sex",
                     treatment = "Treatment group",
                     glucose_fasting_mgdl = "Fasting glucose, mg/dl",
                     hba1c_percent = "HbA1c, %",
                     chol_trig_mgdl = "Triglycerides, mg/dl",
                     bmi = "Body mass index, kg/m2"))
)

dpp_init_tar <- tar_target(dpp_init, load_dpp())

manuscript_tar <- tar_render(
  manuscript,
  path = here::here("doc/manuscript.Rmd"),
  output_file = paste0("manuscript", "-v", manuscript_version, "/",
                       "manuscript-", basename(here()),
                       "-v", manuscript_version,
                       ".docx")
)


# step 1: load all the components of DPP
dpp_components_tar <- tar_target(
  dpp_components,
  dpp_load(format = "\\.sas7bdat$")
)

# step 2: merge, keeping data collected PRIOR to randomization
dpp_merged_tar <- tar_target(
  dpp_merged,
  dpp_merge(dpp_components)
)

# step 3, rename variables and derive some
dpp_formatted_tar <- tar_target(
  dpp_formatted,
  dpp_format(dpp_merged)
)

dpp_excluded_tar <- tar_target(
  dpp_excluded,
  dpp_exclude(dpp_formatted)
)

mesa_init_tar <- tar_target(
  mesa_init,
  mesa_load()
)

mesa_excluded_tar <- tar_target(
  mesa_excluded,
  mesa_exclude()
)



targets <- list(
  labels_tar,
  dpp_components_tar,
  dpp_merged_tar,
  dpp_formatted_tar,
  dpp_excluded_tar,
  mesa_init_tar,
  manuscript_tar
)

tar_hook_before(
  targets = targets,
  hook = {source("conflicts.R")},
  names = everything()
)
