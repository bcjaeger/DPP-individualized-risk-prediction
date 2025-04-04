dpp_merge <- function(dpp_components, daysrand_max = 0) {

 # filter datasets with visits to contain only data at or before baseline
 for(i in seq_along(dpp_components$non_form)){

  if("visit" %in% names(dpp_components$non_form[[i]])){

   dpp_components$non_form[[i]] <- dpp_components$non_form[[i]] %>%
    filter(visit %in% c('BAS', 'RUN'), daysrand <= daysrand_max) %>%
    # NOTE: these ids appear to have duplicated data in the diet file
    # 10029375 10031210 10033391 10036250
    # the distinct call here prevents that from causing duplicates to be
    # brought into the merge statement below
    distinct()

   # drop these identifiers, merge should only use release_id
   dpp_components$non_form[[i]]$visit <- NULL
   dpp_components$non_form[[i]]$daysrand <- NULL

  }

 }

 dpp_nonform <- reduce(dpp_components$non_form, .f = left_join)

 # include forms data (don't worry about filtering to visit/daysrand)

 # blood pressure

 # dpp_bp <- dpp_components$form$s03

 # education
 dpp_edu <- dpp_components$form$s06 %>%
  transmute(
   release_id,
   edugrp = case_when(
    seeduc < 12  ~ "less than high school",
    seeduc == 12 ~ "high school",
    seeduc <= 16 ~ "some college to college degree"
   ),
   edugrp = factor(edugrp, levels = c("less than high school",
                                      "high school",
                                      "some college to college degree"))
  )

 dpp_nonform %>%
  left_join(dpp_edu) %>%
  relocate(edugrp, .before = weight)


}

