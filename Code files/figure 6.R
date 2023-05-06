setwd ()
rm (list = ls())
clean = read.csv("data_clean.csv")

library ("tidyverse")
library ("stargazer")
library ("sjPlot")
library ("ggplot2")
library ("broom")
library ("stringr")

### CREATE DUMMY VARIABLES

# Nationalism
clean$nationalism_above <- case_when (clean$nationalistic_index >= mean(clean$nationalistic_index) ~ 1,
                                      TRUE ~ 0)
clean$nationalism_below <- case_when (clean$nationalism_above==1 ~ 0,
                                      TRUE ~ 1)

# Patriotims
clean$patriotic_above <- case_when (clean$patriotic_index >= mean(clean$patriotic_index) ~ 1,
                                    TRUE ~ 0)
clean$patriotic_below <- case_when (clean$patriotic_above==1 ~ 0,
                                    TRUE ~ 1)


# Conservativeness
clean$conservative_above <- case_when (clean$conservative_index >= mean(clean$conservative_index) ~ 1,
                                       TRUE ~ 0)
clean$conservative_below <- case_when (clean$conservative_above ==1 ~ 0,
                                       TRUE ~ 1)

# Antimarket 
clean$antimarket_above <- case_when (clean$antimarket_index >= mean(clean$antimarket_index) ~1,
                                     TRUE ~ 0)
clean$antimarket_below <- case_when (clean$antimarket_above == 1 ~ 0,
                                     TRUE ~ 1)


### CREATE INTERACTION TERMS 

#Nationalism
clean$nat_above_treat1 = clean$nationalism_above * clean$treatment_treat1
clean$nat_above_treat2 = clean$nationalism_above * clean$treatment_treat2
clean$nat_above_treat3 = clean$nationalism_above * clean$treatment_treat3

clean$nat_below_treat1 = clean$nationalism_below * clean$treatment_treat1
clean$nat_below_treat2 = clean$nationalism_below * clean$treatment_treat2
clean$nat_below_treat3 = clean$nationalism_below * clean$treatment_treat3


# Patriotism
clean$pat_above_treat1 = clean$patriotic_above * clean$treatment_treat1
clean$pat_above_treat2 = clean$patriotic_above * clean$treatment_treat2
clean$pat_above_treat3 = clean$patriotic_above * clean$treatment_treat3

clean$pat_below_treat1 = clean$patriotic_below * clean$treatment_treat1
clean$pat_below_treat2 = clean$patriotic_below * clean$treatment_treat2
clean$pat_below_treat3 = clean$patriotic_below * clean$treatment_treat3


# Conservativeness
clean$con_above_treat1 = clean$conservative_above * clean$treatment_treat1
clean$con_above_treat2 = clean$conservative_above * clean$treatment_treat2
clean$con_above_treat3 = clean$conservative_above * clean$treatment_treat3

clean$con_below_treat1 = clean$conservative_below * clean$treatment_treat1
clean$con_below_treat2 = clean$conservative_below * clean$treatment_treat2
clean$con_below_treat3 = clean$conservative_below * clean$treatment_treat3


# Antimarket
clean$anti_above_treat1 = clean$antimarket_above * clean$treatment_treat1
clean$anti_above_treat2 = clean$antimarket_above * clean$treatment_treat2
clean$anti_above_treat3 = clean$antimarket_above * clean$treatment_treat3

clean$anti_below_treat1 = clean$antimarket_below * clean$treatment_treat1
clean$anti_below_treat2 = clean$antimarket_below * clean$treatment_treat2
clean$anti_below_treat3 = clean$antimarket_below * clean$treatment_treat3




### RUN REGRESSION

# Get list
model_list = clean%>%
  select (treatment_treat1,    #treatment
          treatment_treat2,
          treatment_treat3,
          age, male, income_below_8k, minority, rural_growup, employed, 
          edu_below_hs, edu_hs, edu_college, edu_above_college,
          job_public, job_private, job_none, student, party_aff,
          nationalistic_index,
          conservative_index,
          antimarket_index,
          patriotic_index,
          tech_dummy_index,
          globalization_dummy_index,
          tcom_favorable_index,
          data_concern_index,
          
          out7_scaled,
          out8_scaled,
          out9_scaled,
          out10_scaled,
          out11_scaled,
          
          nat_above_treat1 : nat_above_treat3,    #interactions
          nat_below_treat1 : nat_below_treat3,
          
          pat_above_treat1 : pat_above_treat3,
          pat_below_treat1 : pat_below_treat3,
          
          con_above_treat1 : con_above_treat3,
          con_below_treat1 : con_below_treat3,
          
          anti_above_treat1 : anti_above_treat3,
          anti_below_treat1 : anti_below_treat3,
          
          nationalism_above,    #dummy vars
          patriotic_above,
          conservative_above,
          antimarket_above)

out = model_list %>%   #outcome list
  select (out7_scaled,
          out8_scaled,
          out9_scaled,
          out10_scaled,
          out11_scaled)

treat_nat = model_list %>%   #nationalism
  select (nationalism_above,
          nat_above_treat1 : nat_above_treat3,
          nat_below_treat1 : nat_below_treat3)

treat_pat = model_list %>%    #patriotism
  select (patriotic_above,
          pat_above_treat1 : pat_above_treat3,
          pat_below_treat1 : pat_below_treat3)

treat_con = model_list %>%   #conservativeness
  select (conservative_above,
          con_above_treat1 : con_above_treat3,
          con_below_treat1 : con_below_treat3)


treat_anti = model_list %>%   #antiservativeness
  select (antimarket_above,
          anti_above_treat1 : anti_above_treat3,
          anti_below_treat1 : anti_below_treat3)

control_nat = model_list %>%  #control list
  select (age, male, income_below_8k, minority, rural_growup, employed,
          edu_below_hs, edu_hs, edu_college, edu_above_college,
          job_public, job_private, job_none, student,
          party_aff, 
          # nationalistic_index, 
          conservative_index, antimarket_index,
          patriotic_index, tech_dummy_index, globalization_dummy_index, 
          tcom_favorable_index, data_concern_index)

control_pat = model_list %>%  #control list
  select (age, male, income_below_8k, minority, rural_growup, employed,
          edu_below_hs, edu_hs, edu_college, edu_above_college,
          job_public, job_private, job_none, student,
          party_aff, 
          nationalistic_index, 
          conservative_index, antimarket_index,
          # patriotic_index, 
          tech_dummy_index, globalization_dummy_index, 
          tcom_favorable_index, data_concern_index)


control_con = model_list %>%  #control list
  select (age, male, income_below_8k, minority, rural_growup, employed,
          edu_below_hs, edu_hs, edu_college, edu_above_college,
          job_public, job_private, job_none, student,
          party_aff, 
          nationalistic_index, 
          # conservative_index, 
          antimarket_index,
          patriotic_index, tech_dummy_index, globalization_dummy_index, 
          tcom_favorable_index, data_concern_index)

control_anti = model_list %>%  #control list
  select (age, male, income_below_8k, minority, rural_growup, employed,
          edu_below_hs, edu_hs, edu_college, edu_above_college,
          job_public, job_private, job_none, student,
          party_aff, 
          nationalistic_index, 
          conservative_index, 
          # antimarket_index,
          patriotic_index, tech_dummy_index, globalization_dummy_index, 
          tcom_favorable_index, data_concern_index)



# Run regression

# Nationalism
result_nat = list()     #store results to a list to print table
nat_df <- data.frame()  #store results to a dataframe


for (i in 1:ncol(out)) {
  df_nat = data.frame(y=out[[i]], treat_nat, control_nat)
  var_name = names(out[i])
  print(var_name)
  model_nat <- lm(y ~., df_nat, na.action = na.exclude)
  print(summary(model_nat))
  result_nat[[i]] <- model_nat
  
  tidy_model_nat = tidy(model_nat)    #result from a a list to a df
  tidy_model_nat = mutate (tidy_model_nat, model = var_name)   #assign name of outcome to result
  nat_df = bind_rows(nat_df, tidy_model_nat)    #put all models in a df
}




# Patriotism
result_pat = list()     #store results to a list to print table
pat_df <- data.frame()  #store results to a dataframe


for (i in 1:ncol(out)) {
  df_pat = data.frame(y=out[[i]], treat_pat, control_pat)
  var_name = names(out[i])
  print(var_name)
  model_pat <- lm(y ~., df_pat, na.action = na.exclude)
  print(summary(model_pat))
  result_pat[[i]] <- model_pat
  
  tidy_model_pat = tidy(model_pat)    #result from a a list to a df
  tidy_model_pat = mutate (tidy_model_pat, model = var_name)   #assign name of outcome to result
  pat_df = bind_rows(pat_df, tidy_model_pat)    #put all models in a df
}





# Conservative
result_con = list()     #store results to a list to print table
con_df <- data.frame()  #store results to a dataframe


for (i in 1:ncol(out)) {
  df_con = data.frame(y=out[[i]], treat_con, control_con)
  var_name = names(out[i])
  print(var_name)
  model_con <- lm(y ~., df_con, na.action = na.exclude)
  print(summary(model_con))
  result_con[[i]] <- model_con
  
  tidy_model_con = tidy(model_con)    #result from a a list to a df
  tidy_model_con = mutate (tidy_model_con, model = var_name)   #assign name of outcome to result
  con_df = bind_rows(con_df, tidy_model_con)    #put all models in a df
}



# Antimarket
result_anti = list()     #store results to a list to print table
anti_df <- data.frame()  #store results to a dataframe


for (i in 1:ncol(out)) {
  df_anti = data.frame(y=out[[i]], treat_anti, control_anti)
  var_name = names(out[i])
  print(var_name)
  model_anti <- lm(y ~., df_anti, na.action = na.exclude)
  print(summary(model_anti))
  result_anti[[i]] <- model_anti
  
  tidy_model_anti = tidy(model_anti)    #result from a a list to a df
  tidy_model_anti = mutate (tidy_model_anti, model = var_name)   #assign name of outcome to result
  anti_df = bind_rows(anti_df, tidy_model_anti)    #put all models in a df
}




all_model = bind_rows(nat_df, pat_df, con_df, anti_df)

all_model = all_model %>%
  filter (grepl("treat", term))


all_model <- all_model%>%               #create CI (line)
  mutate(ci95 = 1.96*std.error)


all_model$treatment = case_when(grepl("treat1", all_model$term) ~ "US-China Competition (T1)",
                                grepl("treat2", all_model$term) ~ "Sanction by US (T2)",
                                TRUE ~ "Gov use of data (T3)")

all_model$stakeholder = case_when (grepl("out7", all_model$model) ~ "Private companies",
                                   grepl("out8", all_model$model) ~ "Central government",
                                   grepl("out9", all_model$model) ~ "Local government",
                                   grepl("out10", all_model$model) ~ "AI Competition",
                                   TRUE ~ "AI World Leader")

all_model$interaction = case_when (grepl ("nat_above", all_model$term)~ "Nationalism (equal or above mean)",
                                   grepl ("nat_below", all_model$term)~ "Nationalism (below mean)",
                                   grepl ("pat_above", all_model$term)~ "Patriotism (equal or above mean)",
                                   grepl ("pat_below", all_model$term)~ "Patriotism (below mean)",
                                   grepl ("con_above", all_model$term)~ "Conservativeness (equal or above mean)",
                                   grepl ("con_below", all_model$term)~ "Conservativeness (below mean)",
                                   grepl ("anti_above", all_model$term)~ "Antimarket (equal or above mean)",
                                   TRUE~ "Antimarket (below mean)")

model = rep(c("out10_scaled", "out11_scaled", "out7_scaled", "out8_scaled", "out9_scaled"), each = 8)

interaction = rep(c("Nationalism (equal or above mean)",
                    "Nationalism (below mean)",
                    "Patriotism (equal or above mean)",
                    "Patriotism (below mean)",
                    "Conservativeness (equal or above mean)",
                    "Conservativeness (below mean)",
                    "Antimarket (equal or above mean)",
                    "Antimarket (below mean)"), 5)

group1 = c(57.5, 57,
           55.5, 55,
           53.5, 53,
           51.5, 51)

position = c(group1, group1-10, group1-20, group1-30, group1-40)

position_df = bind_cols(model, interaction, position)

colnames (position_df) = c("model",
                           "interaction",
                           "position")

model_plot <- left_join (all_model, position_df)





# Reorder treatment and stakeholders so they appear as preferred order in the plotting
model_plot$treatment <- factor(model_plot$treatment, levels=c("US-China Competition (T1)", 
                                                              "Sanction by US (T2)", 
                                                              "Gov use of data (T3)")) 

model_plot$interaction <- factor (model_plot$interaction, 
                                  levels = c("Nationalism (equal or above mean)", 
                                             "Patriotism (equal or above mean)", 
                                             "Conservativeness (equal or above mean)",
                                             "Antimarket (equal or above mean)", 
                                             "Nationalism (below mean)",
                                             "Patriotism (below mean)",
                                             "Conservativeness (below mean)",
                                             "Antimarket (below mean)"))



model_plot %>%
  mutate (low = estimate-ci95, high = estimate+ci95) %>%
  ggplot (aes(x=position, y=estimate)) +
  
  
  # theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.background = element_rect(fill = '#F0F8FF'),
        strip.background =element_rect(fill="lightblue"),
        panel.border = element_rect(color = "black", fill = NA),
        axis.text.y = element_text (size = 18, hjust = 0),
        legend.text = element_text (size = 18),
        strip.text = element_text (size = 15),
        axis.title.x = element_text (size = 18), 
        plot.margin = unit (c(0, 0.5, 0, 0), "in")) +
  guides (shape = guide_legend(nrow = 4)) +
  geom_point (aes(shape = interaction)) +
  scale_shape_manual (values = c(16,15,17,8,1,0,2,5)) +
  geom_errorbar(aes(ymin=low, ymax=high, linetype = interaction) ,width=0) +
  scale_linetype_manual (values = c("solid", "solid", "solid", "solid",    #left column of legend
                                    "dashed", "dashed", "dashed", "dashed")) +  #right column of legend
  geom_hline(yintercept=0, linetype = 2)+
  facet_grid(~treatment, scales = "fixed") + 
  labs(x="", y="Treatment Effects") + 
  scale_x_continuous(breaks = c(57, 47, 37, 27, 17),
                     labels = c(str_wrap(c("Personal data critical for Chinese company competitiveness in AI development",
                                           "Personal data critical for central government's goal to become world leader in AI",
                                           "Trust private companies for responsible data use",
                                           "Trust central government for responsible data use",
                                           "Trust local government for responsible data use"), 
                                         width = 33))) + 
  
  
  coord_flip()

ggsave(filename = "Figure 6.png",
       height = 6, width = 8, scale = 16/9)

