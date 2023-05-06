setwd ()
rm (list = ls())
clean = read.csv("data_clean.csv")

library ("tidyverse")
library ("stargazer")
library ("sjPlot")


out <- clean %>% 
  select (nationalistic_index, 
          patriotic_index,
          conservative_index,
          antimarket_index)

# TABLE 1 ----

cor_mat <- cor(out)
colnames (cor_mat) <- c("Nationalism",
                        "Patriotism",
                        "Conservativeness",
                        "Antimarket")

rownames (cor_mat) <- c("Nationalism",
                        "Patriotism",
                        "Conservativeness",
                        "Antimarket")
stargazer (cor_mat,
           out = "table1.html")

# TABLE 2 ----


x <- clean %>%
  select (male, edu_college_total)


control_both = clean %>%
  select (age, income_below_8k, minority, rural_growup, employed,
          job_public, job_private, job_none, student,
          party_aff, 
          tech_dummy_index, globalization_dummy_index, 
          tcom_favorable_index, data_concern_index)

control_gender = clean %>%
  select (age, income_below_8k, minority, rural_growup, employed,
          edu_below_hs, edu_hs, edu_college, edu_above_college, 
          job_public, job_private, job_none, student,
          party_aff, 
          tech_dummy_index, globalization_dummy_index, 
          tcom_favorable_index, data_concern_index)

control_college = clean %>%
  select (age, male, income_below_8k, minority, rural_growup, employed,
          job_public, job_private, job_none, student,
          party_aff, 
          tech_dummy_index, globalization_dummy_index, 
          tcom_favorable_index, data_concern_index)


result = list ()
for (i in 1:ncol(out)) {
  df <- data.frame (y=out[[i]], x, control_both)
  var_name <- names(out[i])
  print (var_name)
  model <- lm (y ~ ., df, na.action = na.exclude)
  print (summary(model))
  result [[i]] <- model
}


stargazer (result,
           out = "Table 2.html",
           column.labels = c("National identity",
                                "Patriotism",
                                "Political conservativeness",
                                "Antimarket-sentiment"))
