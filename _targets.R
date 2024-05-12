
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
  list(
    variables = c(
      age_yrs = "Age",
      race_ethnicity = "Race/ethnicity",
      sex = "Sex",
      education = "Educational attainment",
      treatment = "Treatment group",
      glucose_fasting_mgdl = "Fasting glucose",
      homa_ir = "HOMA-Insulin Resistance",
      homa_beta = "HOMA-Beta cell function",
      hba1c_percent = "Glycated Hemoglobin",
      chol_trig_mgdl = "Triglycerides",
      chol_ldlc_mgdl = "Low-density lipoprotein cholesterol",
      chol_hdl_mgdl = "High-density lipoprotein cholesterol",
      bmi = "Body mass index"),
    units = c(
      age_yrs = "years",
      glucose_fasting_mgdl = "mg/dl",
      hba1c_percent = "%",
      chol_trig_mgdl = "mg/dl",
      bmi = "kg/m2",
      chol_ldlc_mgdl = "mg/dl",
      chol_hdl_mgdl = "mg/dl"
    ),
    levels = list(
      sex = c(
        "male" = "Male",
        "female" = "Female"
      ),
      race_ethnicity = c(
        "caucasian" = "Non-Hispanic White",
        "african_american" = "Non-Hispanic Black",
        "hispanic" = "Hispanic",
        "other" = "Other/Chinese"
      ),
      education = c(
        "less than high school" = "< High School",
        "high school" = "High School Graduate",
        "some college to college degree" = "Some College or College Graduate"
      )
    )
  )
)

dpp_init_tar <- tar_target(dpp_init, load_dpp())

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
  mesa_exclude(mesa_init)
)

tbl_exclusion_tar <- tar_target(
  tbl_exclusion,
  tabulate_exclusions(dpp_excluded, mesa_excluded)
)

tbl_characteristics_tar <- tar_target(
  tbl_characteristics,
  tabulate_characteristics(dpp_excluded, mesa_excluded, labels)
)

data_analysis_tar <- tar_target(
  data_analysis,
  bind_rows(DPP = dpp_excluded$included,
            MESA = mesa_excluded$included,
            .id = 'study')
)

analysis_tar <- tar_target(
  analysis,
  validate_preds(data_analysis)
)

fig_cuminc_tar <- tar_target(
  fig_cuminc,
  visualize_cuminc(data_analysis)
)

tbl_cuminc_tar <- tar_target(
  tbl_cuminc,
  tabulate_cuminc(data_analysis)
)


manuscript_tar <- tar_render(
  manuscript,
  path = here::here("doc/manuscript.Rmd"),
  output_file = paste0("manuscript", "-v", manuscript_version, "/",
                       "manuscript-", basename(here()),
                       "-v", manuscript_version,
                       ".docx")
)

targets <- list(
  labels_tar,
  dpp_components_tar,
  dpp_merged_tar,
  dpp_formatted_tar,
  dpp_excluded_tar,
  mesa_init_tar,
  mesa_excluded_tar,
  tbl_exclusion_tar,
  tbl_characteristics_tar,
  data_analysis_tar,
  analysis_tar,
  fig_cuminc_tar,
  tbl_cuminc_tar,
  manuscript_tar
)

tar_hook_before(
  targets = targets,
  hook = {source("conflicts.R")},
  names = everything()
)
