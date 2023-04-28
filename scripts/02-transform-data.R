# Introduction #### 
# Transform the data to prepare it for use in a flex dashboard 

# Package libraries #### 
library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)

# od rate of ed visits #### 
# read total ed visits 
ed_total <- read_rds(
  file = "data-tidy/essence-data-ed-visits-total.rds"
)

# read od related ed visits 
od_total <- read_rds(
  file = "data-tidy/essence-data-ed-visits-total-od.rds"
)

# join 
od_rate <- full_join(
  x = od_total,
  y = ed_total,
  by = "date"
) %>%
  mutate(
    rate = 100000*(`count.x`/`count.y`)
  ) %>%
  select(
    date,
    od_count = count.x,
    ed_total = count.y,
    rate
  ) 

od_rate %>%
  ggplot() +
  geom_line(mapping = aes(
    x = date,
    y = rate
  )) +
  ylim(0,NA)

# save to disk 
write_rds(
  x = od_rate,
  file = "data-tidy/essence-data-od-rate.rds"
)

# Read data details #### 
# numerator of the rate per all ED visits 
od_details <- read_rds(
  file = "data-raw/essence-data-details.rds"
)

# Transform 
od_details <- od_details %>%
  mutate(
    weekday = wday(date), 
    week = week(date),
    month = month(date),
    year = year(date),
    hospital_name = factor(hospital_name),
    region = factor(region),
    sex = factor(sex),
    age = as.numeric(age),
    c_ethnicity = factor(c_ethnicity),
    c_race = factor(c_race),
    c_race_c_eth_combined_narrow = factor(c_race_c_eth_combined_narrow),
    c_race_c_eth_combined_broad = factor(c_race_c_eth_combined_broad),
    admit_date_time = ymd_hms(admit_date_time),
    ccdd_category = factor(ccdd_category),
    age_group2 = factor(case_when(
      age >= 60 ~ ">=60 yrs",
      age >= 40 ~ "40-59 yrs",
      age >= 20 ~ "20-39 yrs",
      age >= 10 ~ "10-19 yrs",
      age < 10 ~ "<10 yrs",
      TRUE ~ NA_character_
    ),
    levels = c(
      "<10 yrs",
      "10-19 yrs",
      "20-39 yrs",
      "40-59 yrs",
      ">=60 yrs"
    )),
    age_group3 = factor(case_when(
      age >= 80 ~ "80-1000",
      age >= 70 ~ "70-79",
      age >= 60 ~ "60-69",
      age >= 50 ~ "50-59",
      age >= 40 ~ "40-49",
      age >= 30 ~ "30-39",
      age >= 20 ~ "20-29",
      age >= 10 ~ "10-19",
      age < 10 ~ "00-09",
      TRUE ~ NA_character_
    ),
    levels = c(
      "00-09",
      "10-19",
      "20-29",
      "30-39",
      "40-49",
      "50-59",
      "60-69",
      "70-79",
      "80-1000"
    ))
  ) 

# inspect 
glimpse(od_details)
od_details %>%
  count(ccdd_category)

# save to disk 
write_rds(
  x = od_details,
  file = "data-tidy/essence-data-details.rds"
)

# od rate of ed visits by hospital #### 
# read hosp denominator 
ed_by_hosp <- read_rds(
  file = "data-tidy/essence-data-ed-visits-total-od-by-hosp.rds"
) %>%
  mutate(
    week = week(date),
    year = year(date),
    week_year = str_c(
      year,
      if_else(
        condition = week < 10,
        true = str_c(
          "0",
          week,
          sep = ""
        ),
        false = as.character(week)
      ),
      sep = "-"
    )
  )

glimpse(ed_by_hosp)
glimpse(od_details)

od_rate_by_hosp <- od_details %>%
  group_by(week_year) %>%
  count(hospital_name) %>%
  ungroup() %>%
  left_join(
  y = ed_by_hosp,
  by = c(
    "week_year",
    "hospital_name" = "line_label"
  )
) %>% 
  mutate(
    rate = 100000*(n/count)
  ) %>%
  select(
    week_year,
    hospital_name,
    n,
    count,
    rate
  ) 

write_rds(
  x = od_rate_by_hosp,
  file = "data-tidy/essence-data-od-rate-by-hosp.rds"
)

od_rate_by_hosp %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = week_year,
      y = rate,
      color = hospital_name,
      group = hospital_name
    )
  )


# # total rate by month #### 
# # create data table where data detail count / total count 
# # indicates the rate of suicide related ed visits per all ed visits
# 
# # read total denominator of the rate per all ED visits 
# ed_totals <- read_rds(
#   file = "data-tidy/essence-data-ed-visits-total.rds"
# ) %>%
#   mutate(
#     year = year(date),
#     month = month(date)
#   )
# 
# # join numerator / denominator 
# (total_rate_by_month <- essence_df %>%
#   group_by(year, month) %>%
#   count() %>%
#   ungroup() %>%
#   full_join(
#     y = ed_totals
#   ) %>%
#   mutate(
#     rate = 100000*(n/count)
#   ) %>%
#   select(
#     date,
#     year,
#     month,
#     n,
#     count,
#     rate
#   ))
# 
# # inspect with plot 
# ggplot(data = total_rate_by_month) +
#   geom_line(
#     mapping = aes(
#       x = date,
#       y = rate
#     )
#   ) 
# 
# # save to disk 
# write_rds(
#   x = total_rate_by_month,
#   file = "data-tidy/essence-data-ed-monthly-rate.rds"
# )
# 
# # rate by month by hospital 
# ed_totals_by_hosp <- read_rds(
#   file = "data-tidy/essence-data-ed-visits-total-by-hosp.rds"
# ) %>%
#   mutate(
#     year = year(date),
#     month = month(date)
#   )
# 
# # join 
# rate_month_by_hosp <- essence_df %>%
#   group_by(year, month) %>%
#   count(hospital_name) %>%
#   ungroup() %>%
#   full_join(
#     y = ed_totals_by_hosp,
#     by = c(
#       "year",
#       "month",
#       "hospital_name" = "line_label"
#     )
#   ) %>%
#   mutate(
#     rate = 100000*(n/count)
#   ) %>%
#   select(
#     date,
#     year,
#     month,
#     hospital_name,
#     n,
#     count,
#     rate
#   ) 
# 
# # inspect with plot 
# rate_month_by_hosp %>%
#   ggplot() +
#   geom_line(
#     mapping = aes(
#       x = date,
#       y = rate,
#       color = hospital_name,
#       group = hospital_name
#     )
#   ) +
#   ylim(0,NA)
# 
# # save to disk 
# write_rds(
#   x = rate_month_by_hosp,
#   file = "data-tidy/essence-data-ed-monthly-rate-by-hosp.rds"
# )
  
# rate by month by sex ####
# read sex denominator 
ed_by_sex <- read_rds(
  file = "data-tidy/essence-data-ed-visits-total-od-by-sex.rds"
) %>%
  mutate(
    week = week(date),
    year = year(date),
    week_year = str_c(
      year,
      if_else(
        condition = week < 10,
        true = str_c(
          "0",
          week,
          sep = ""
        ),
        false = as.character(week)
      ),
      sep = "-"
    )
  )

glimpse(ed_by_sex)
glimpse(od_details)

od_rate_by_sex <- od_details %>%
  group_by(week_year) %>%
  count(sex) %>%
  ungroup() %>%
  left_join(
    y = ed_by_sex,
    by = c(
      "week_year",
      "sex" = "sex_id"
    )
  ) %>% 
  mutate(
    rate = 100000*(n/count)
  ) %>%
  select(
    week_year,
    sex,
    n,
    count,
    rate
  ) 

od_rate_by_sex %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = week_year,
      y = rate,
      color = sex,
      group = sex
    )
  )

write_rds(
  x = od_rate_by_sex,
  file = "data-tidy/essence-data-od-rate-by-sex.rds"
)


# # join 
# rate_month_by_sex <- essence_df %>%
#   filter(sex != "U") %>%
#   group_by(year, month) %>%
#   count(sex) %>%
#   ungroup() %>%
#   full_join(
#     y = ed_totals_by_sex,
#     by = c(
#       "year",
#       "month",
#       "sex" = "sex_id"
#     )
#   ) %>%
#   mutate(
#     rate = 100000*(n/count)
#   ) %>%
#   select(
#     date,
#     year,
#     month,
#     sex,
#     n,
#     count,
#     rate
#   ) 
# 
# # inspect with plot 
# rate_month_by_sex %>%
#   ggplot() +
#   geom_line(
#     mapping = aes(
#       x = date,
#       y = rate,
#       color = sex,
#       group = sex
#     )
#   ) +
#   ylim(0,NA)
# 
# # save to disk 
# write_rds(
#   x = rate_month_by_sex,
#   file = "data-tidy/essence-data-ed-monthly-rate-by-sex.rds"
# )

# rate by month by age ####
# read age denominator 
ed_by_age <- read_rds(
  file = "data-tidy/essence-data-ed-visits-total-od-by-age.rds"
) %>%
  mutate(
    week = week(date),
    year = year(date),
    week_year = str_c(
      year,
      if_else(
        condition = week < 10,
        true = str_c(
          "0",
          week,
          sep = ""
        ),
        false = as.character(week)
      ),
      sep = "-"
    )
  )

glimpse(ed_by_age)

ed_by_age %>%
  count(line_label)

glimpse(od_details)

od_rate_by_age <- od_details %>%
  mutate(
    age_group2_id = case_when(
      age >= 75 ~ "75+",
      age >= 65 ~ "65-74",
      age >= 55 ~ "55-64",
      age >= 26 ~ "26-54",
      age >= 18 ~ "18-25",
      age < 18 ~ "00-17"
    )
  ) %>%
  group_by(week_year) %>%
  count(age_group2_id) %>%
  ungroup() %>%
  left_join(
    y = ed_by_age,
    by = c(
      "week_year",
      "age_group2_id" = "age_group2_id"
    )
  ) %>% 
  mutate(
    rate = 100000*(n/count)
  ) %>%
  select(
    week_year,
    age_group2_id,
    n,
    count,
    rate
  ) 

od_rate_by_age %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = week_year,
      y = rate,
      color = age_group2_id,
      group = age_group2_id
    )
  )

write_rds(
  x = od_rate_by_age,
  file = "data-tidy/essence-data-od-rate-by-age.rds"
)

# ed_totals_by_age <- read_rds(
#   file = "data-tidy/essence-data-ed-visits-total-by-age.rds"
# ) %>%
#   mutate(
#     year = year(date),
#     month = month(date),
#     age_group3 = factor(case_when(
#       age_ten_year_id == "00-09" ~ "<10 yrs",
#       age_ten_year_id == "10-19" ~ "10-19 yrs",
#       age_ten_year_id == "20-29" ~ "20-39 yrs",
#       age_ten_year_id == "30-39" ~ "20-39 yrs",
#       age_ten_year_id == "40-49" ~ "40-59 yrs",
#       age_ten_year_id == "50-59" ~ "40-59 yrs",
#       age_ten_year_id == "60-69" ~ ">=60 yrs",
#       age_ten_year_id == "70-79" ~ ">=60 yrs",
#       age_ten_year_id == "80-1000" ~ ">=60 yrs"
#     ),
#     levels = c(
#       "<10 yrs",
#       "10-19 yrs",
#       "20-39 yrs",
#       "40-59 yrs",
#       ">=60 yrs"
#     ))
#   ) %>%
#   group_by(year, month, age_group3) %>%
#   summarise(count = sum(count)) %>%
#   ungroup()
# 
# # join 
# rate_month_by_age <- essence_df %>%
#   group_by(year, month) %>%
#   count(age_group2) %>%
#   ungroup() %>%
#   full_join(
#     y = ed_totals_by_age,
#     by = c(
#       "year",
#       "month",
#       "age_group2" = "age_group3"
#     )
#   ) %>%
#   mutate(
#     rate = 100000*(n/count),
#     date = ymd(str_c(
#       year,
#       if_else(
#         condition = month < 10,
#         true = str_c(
#           "0",
#           month
#         ),
#         false = as.character(month)
#       ),
#       "01",
#       sep = "-"
#     ))
#   ) %>%
#   select(
#     date,
#     year,
#     month,
#     "age_group" = age_group2,
#     n,
#     count,
#     rate
#   ) 
# 
# # inspect with plot 
# rate_month_by_age %>%
#   ggplot() +
#   geom_line(
#     mapping = aes(
#       x = date,
#       y = rate,
#       color = age_group,
#       group = age_group
#     )
#   ) +
#   ylim(0,NA)
# 
# # save to disk 
# write_rds(
#   x = rate_month_by_age,
#   file = "data-tidy/essence-data-ed-monthly-rate-by-age.rds"
# )

# rate by month by race ####
# read race denominator 
ed_by_race <- read_rds(
  file = "data-tidy/essence-data-ed-visits-total-od-by-race.rds"
) %>%
  mutate(
    week = week(date),
    year = year(date),
    week_year = str_c(
      year,
      if_else(
        condition = week < 10,
        true = str_c(
          "0",
          week,
          sep = ""
        ),
        false = as.character(week)
      ),
      sep = "-"
    )
  )

glimpse(ed_by_race)
glimpse(od_details)

od_rate_by_race <- od_details %>%
  group_by(week_year) %>%
  count(c_race_c_eth_combined_broad) %>%
  ungroup() %>%
  left_join(
    y = ed_by_race,
    by = c(
      "week_year",
      "c_race_c_eth_combined_broad" = "line_label"
    )
  ) %>% 
  mutate(
    rate = 100000*(n/count)
  ) %>%
  select(
    week_year,
    c_race_c_eth_combined_broad,
    n,
    count,
    rate
  ) 

od_rate_by_race %>%
  filter(n > 2) %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = week_year,
      y = rate,
      color = c_race_c_eth_combined_broad,
      group = c_race_c_eth_combined_broad
    ), size = 1
  ) +
  geom_point(
    mapping = aes(
      x = week_year,
      y = rate,
      color = c_race_c_eth_combined_broad,
      group = c_race_c_eth_combined_broad
    ), size = 2
  )

write_rds(
  x = od_rate_by_race,
  file = "data-tidy/essence-data-od-rate-by-race.rds"
)

# ed_totals_by_race <- read_rds(
#   file = "data-tidy/essence-data-ed-visits-total-by-race.rds"
# ) %>%
#   mutate(
#     year = year(date),
#     month = month(date)
#   )
# 
# # join 
# rate_month_by_race <- essence_df %>%
#   group_by(year, month) %>%
#   count(c_race_c_eth_combined_broad) %>%
#   ungroup() %>%
#   full_join(
#     y = ed_totals_by_race,
#     by = c(
#       "year",
#       "month",
#       "c_race_c_eth_combined_broad" = "c_race_eth_broad_id"
#     )
#   ) %>%
#   mutate(
#     rate = 100000*(n/count)
#   ) %>%
#   select(
#     date,
#     year,
#     month,
#     race = c_race_c_eth_combined_broad,
#     n,
#     count,
#     rate
#   ) 
# 
# # inspect with plot 
# rate_month_by_race %>%
#   ggplot() +
#   geom_line(
#     mapping = aes(
#       x = date,
#       y = rate,
#       color = race,
#       group = race
#     )
#   ) +
#   ylim(0,NA)
# 
# # save to disk 
# write_rds(
#   x = rate_month_by_race,
#   file = "data-tidy/essence-data-ed-monthly-rate-by-race.rds"
# )

# rate by month by ccdd category ####
# read ccdd denominator 
ed_by_ccdd <- read_rds(
  file = "data-tidy/essence-data-ed-visits-total-od-by-ccdd.rds"
) %>%
  mutate(
    week = week(date),
    year = year(date),
    week_year = str_c(
      year,
      if_else(
        condition = week < 10,
        true = str_c(
          "0",
          week,
          sep = ""
        ),
        false = as.character(week)
      ),
      sep = "-"
    )
  )

glimpse(ed_by_ccdd)
glimpse(od_details)

ed_by_ccdd_count <- ed_by_ccdd %>%
  group_by(week_year) %>%
  summarise(count = sum(count)) %>%
  ungroup()

od_rate_by_ccdd <- od_details %>%
  group_by(week_year) %>%
  count(ccdd_category) %>%
  ungroup() %>%
  left_join(
    y = ed_by_ccdd_count,
    by = c(
      "week_year"
      # "ccdd_category" = "line_label"
    )
  ) %>% 
  mutate(
    rate = 100000*(n/count)
  ) %>%
  select(
    week_year,
    ccdd_category,
    n,
    count,
    rate
  ) 

od_rate_by_ccdd %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = week_year,
      y = rate,
      color = ccdd_category,
      group = ccdd_category
    ), size = 1
  ) +
  geom_point(
    mapping = aes(
      x = week_year,
      y = rate,
      color = ccdd_category,
      group = ccdd_category
    ), size = 2
  )

write_rds(
  x = od_rate_by_race,
  file = "data-tidy/essence-data-od-rate-by-race.rds"
)

# ed_totals_by_ccdd <- read_rds(
#   file = "data-tidy/essence-data-ed-visits-total-by-ccdd.rds"
# ) %>%
#   mutate(
#     year = year(date),
#     month = month(date)
#   )

# ERROR? ####

# join 
rate_month_by_ccdd <- essence_df %>%
  group_by(year, month) %>%
  count(ccdd_category) %>%
  ungroup() %>%
  full_join(
    y = ed_totals,
    by = c(
      "year",
      "month"
    )
  ) %>%
  mutate(
    rate = 100000*(n/count)
  ) %>%
  select(
    date,
    year,
    month,
    ccdd_category,
    n,
    count,
    rate
  ) 

# inspect with plot 
rate_month_by_ccdd %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = date,
      y = rate,
      color = ccdd_category,
      group = ccdd_category
    )
  ) +
  ylim(0,NA)

# save to disk 
write_rds(
  x = rate_month_by_ccdd,
  file = "data-tidy/essence-data-ed-monthly-rate-by-ccdd.rds"
)

# HERE #### 


rate_month_by_sex <- essence_df %>%
  filter(sex != "U") %>%
  group_by(year, month) %>%
  count(sex) %>%
  ungroup() %>%
  full_join(
    y = ed_totals_by_age,
    by = c(
      "year",
      "month",
      "age_group3" = "sex_id"
    )
  ) %>%
  mutate(
    rate = 100000*(n/count)
  ) %>%
  select(
    date,
    year,
    month,
    sex,
    n,
    count,
    rate
  ) 

rate_month_by_sex %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = date,
      y = rate,
      color = sex,
      group = sex
    )
  ) +
  ylim(0,NA)

write_rds(
  x = rate_month_by_sex,
  file = "data-tidy/essence-data-ed-monthly-rate-by-sex.rds"
)


#### TO HERE ####

essence_df %>%
  group_by(year, month) %>%
  count() %>%
  ungroup() %>%
  full_join(
    y = ed_totals
  ) %>%
  mutate(
    rate = 100000*(n/count)
  ) %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = date,
      y = rate
    )
  ) +
  ylim(0,NA)

essence_var_list <- c(
  "hospital_name",
  "sex",
  "age_group",
  "c_ethnicity",
  "c_race",
  "c_race_c_eth_combined_narrow",
  "ccdd_category",
  "age_group2"
)

func_tabyl <- function(x){
  essence_df %>%
    tabyl(x, show_na = FALSE) %>%
    adorn_totals() %>%
    adorn_pct_formatting()
}

lapply(essence_var_list, func_tabyl)

func_count <- function(x){
  essence_df %>%
    group_by(month_year) %>%
    count(!!rlang::sym(x)) %>%
    ungroup()
}

lapply(essence_var_list, func_count)

func_plot <- function(x){
  plot <- essence_df %>%
    group_by(month_year) %>%
    count(!!rlang::sym(x)) %>%
    ungroup() %>%
    ggplot() +
    geom_col(
      mapping = aes(
        x = !!rlang::sym(x),
        y = n
      )
    ) +
    coord_flip() +
    theme_classic()
  
  ggsave(
    filename = str_c(
      "data-viz/count-",
      as.character(x),
      ".png"
    ),
    plot = plot
  )
}

lapply(essence_var_list, func_plot)

essence_df %>%
  tabyl(hospital_name) %>%
  adorn_totals() %>%
  adorn_pct_formatting()

essence_df %>%
  tabyl(hospital_name) %>%
  adorn_totals() %>%
  adorn_pct_formatting() 

essence_df %>%
  tabyl(age_group, sex) %>%
  adorn_totals() %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")

essence_df %>%
  tabyl(hospital_name, sex) %>%
  adorn_totals() %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")

essence_df %>%
  tabyl(ccdd_category, sex, hospital_name) %>%
  adorn_totals() %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")

essence_df %>%
  group_by(year) %>%
  count(hospital_name) %>%
  ungroup() %>%
  ggplot() +
  geom_col(mapping = aes(
    fill = factor(year),
    x = hospital_name,
    y = n,
    group = factor(year)
  ), position = "dodge"
  )

essence_df %>%
  group_by(year) %>%
  count(week, hospital_name) %>%
  ungroup() %>%
  ggplot() +
  geom_line(mapping = aes(
    color = factor(year),
    x = week,
    y = n
  )
  ) +
  ylim(0,NA) +
  facet_wrap(~hospital_name)

# can i replicate this? 
# https://www.cdc.gov/mmwr/volumes/69/wr/mm6904a3.htm?s_cid=mm6904a3_w
essence_df %>%
  group_by(year, month) %>%
  count(sex, age_group2) %>%
  ungroup() %>%
  mutate(
    sex = as.character(sex),
    month_year = str_c(
      year,
      if_else(
        condition = month < 10, 
        true = str_c("0", month),
        false = as.character(month)
      ),
      sep = "-"
    )
  ) %>%
  filter(sex != "U") %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = month_year,
      y = n,
      color = age_group2,
      group = age_group2
    ), size = 1
  ) +
  facet_wrap(~sex, nrow = 3) +
  scale_color_brewer(palette = "Accent") +
  ylim(0,NA)

essence_df %>%
  group_by(year, month) %>%
  count(sex, hospital_name) %>%
  ungroup() %>%
  mutate(
    sex = as.character(sex),
    month_year = str_c(
      year,
      if_else(
        condition = month < 10, 
        true = str_c("0", month),
        false = as.character(month)
      ),
      sep = "-"
    )
  ) %>%
  filter(sex != "U") %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = month_year,
      y = n,
      color = hospital_name,
      group = hospital_name
    ), size = 1
  ) +
  facet_wrap(~sex, nrow = 3) +
  scale_color_brewer(palette = "Accent") +
  ylim(0,NA)

# total hospitalizations for each month 
api_data_total_by_hosp <- api_data_total_by_hosp %>%
  mutate(
    date = ymd(date),
    month = month(date),
    year = year(date)
  )

essence_df %>%
  group_by(year, month) %>%
  count(hospital_name) %>%
  ungroup() %>%
  full_join(
    api_data_total_by_hosp,
    by = c(
      "year",
      "month",
      "hospital_name" = "er_facility_display"
    )
  ) %>%
  mutate(
    rate = 100000*(n/count),
    month_year = str_c(
      year,
      if_else(
        condition = month < 10, 
        true = str_c("0", month),
        false = as.character(month)
      ),
      sep = "-"
    )
  ) %>%
  select(
    month_year,
    year, 
    month,
    hospital_name,
    n,
    count,
    rate
  ) %>%
  filter(count > 6) %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = factor(month_year),
      y = rate,
      color = hospital_name,
      group = hospital_name,
      linetype = hospital_name
    ),
    size = 2
  ) +
  theme_classic() +
  labs(
    title = str_wrap("Monthly rate of emergency department visits related to suicidal ideation or suicide attempt grouped by hospital",
                     width = 80
    ),
    subtitle = "2021-2022",
    x = "Year-Month",
    y = "Rate per 100,000 ED visits"
  )

# total hospitalizations for each month by sex
api_data_total_by_sex <- api_data_total_by_sex %>%
  mutate(
    date = ymd(date),
    month = month(date),
    year = year(date)
  )

essence_df %>%
  group_by(year, month) %>%
  count(sex) %>%
  ungroup() %>%
  full_join(
    api_data_total_by_sex,
    by = c(
      "year",
      "month",
      "sex" = "sex_id"
    )
  ) %>%
  mutate(
    rate = 100000*(n/count),
    month_year = str_c(
      year,
      if_else(
        condition = month < 10, 
        true = str_c("0", month),
        false = as.character(month)
      ),
      sep = "-"
    )
  ) %>%
  select(
    month_year,
    year, 
    month,
    sex,
    n,
    count,
    rate
  ) %>%
  filter(count > 6) %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = factor(month_year),
      y = rate,
      color = sex,
      group = sex,
      linetype = sex
    ),
    size = 2
  ) +
  ylim(0,NA) +
  theme_classic() +
  labs(
    title = str_wrap("Monthly rate of emergency department visits related to suicidal ideation or suicide attempt grouped by sex",
                     width = 80
    ),
    subtitle = "2021-2022",
    x = "Year-Month",
    y = "Rate per 100,000 ED visits"
  )

# total hospitalizations for each month by age
api_data_total_by_age <- api_data_total_by_age %>%
  mutate(
    date = ymd(date),
    month = month(date),
    year = year(date)
  ) 

distinct(api_data_total_by_age, age_id)

essence_df %>%
  group_by(year, month) %>%
  count(age_group) %>%
  ungroup() %>%
  full_join(
    api_data_total_by_age,
    by = c(
      "year",
      "month",
      "age_group" = "age_id"
    )
  ) %>%
  mutate(
    rate = 100000*(n/count),
    month_year = str_c(
      year,
      if_else(
        condition = month < 10, 
        true = str_c("0", month),
        false = as.character(month)
      ),
      sep = "-"
    )
  ) %>%
  select(
    month_year,
    year, 
    month,
    age_group,
    n,
    count,
    rate
  ) %>%
  filter(count > 6) %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = factor(month_year),
      y = rate,
      color = age_group,
      group = age_group,
      linetype = age_group
    ),
    size = 2
  ) +
  ylim(0,NA) +
  theme_classic() +
  labs(
    title = str_wrap("Monthly rate of emergency department visits related to suicidal ideation or suicide attempt grouped by age",
                     width = 80
    ),
    subtitle = "2021-2022",
    x = "Year-Month",
    y = "Rate per 100,000 ED visits"
  )
