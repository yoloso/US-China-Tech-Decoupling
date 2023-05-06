setwd ()   #set working directory
rm (list = ls())
clean = read.csv("data_clean.csv")    

#Load packages
library ("tidyverse")
library ("ggplot2")
library ("broom")
library ("stringr")


# REGRESSION ----
## Create interaction terms----

### For gender----
clean$male_treat1 = clean$male * clean$treatment_treat1
clean$male_treat2 = clean$male * clean$treatment_treat2
clean$male_treat3 = clean$male * clean$treatment_treat3

clean$female_treat1 = clean$female * clean$treatment_treat1
clean$female_treat2 = clean$female * clean$treatment_treat2
clean$female_treat3 = clean$female * clean$treatment_treat3

### For education level---

clean$college_treat1 = clean$edu_college_total * clean$treatment_treat1
clean$college_treat2 = clean$edu_college_total * clean$treatment_treat2
clean$college_treat3 = clean$edu_college_total * clean$treatment_treat3

clean$edu_below_college = case_when(clean$edu_college_total==1 ~ 0,
                                   TRUE ~1)

clean$below_college_treat1 = clean$edu_below_college * clean$treatment_treat1
clean$below_college_treat2 = clean$edu_below_college * clean$treatment_treat2
clean$below_college_treat3 = clean$edu_below_college * clean$treatment_treat3



## RUN REGRESSION----

model_list = clean%>%
  select (treatment_treat1,
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
          out1_scaled_index, 
          out3_scaled_index,
          out5_scaled_index,
          male_treat1 : male_treat3,
          female_treat1 : female_treat3,
          edu_college_total,
          college_treat1 : college_treat3,
          below_college_treat1 : below_college_treat3)
          

out = model_list %>%
  select (out1_scaled_index,
          out3_scaled_index,
          out5_scaled_index)


### Gender----
treat_gender = model_list %>%   #treatment list
  select (male, 
          male_treat1 : male_treat3,
          female_treat1 : female_treat3)

control_gender = model_list %>%    #control list
  select (age, income_below_8k, minority, rural_growup, employed,
          edu_below_hs, edu_hs, edu_college, edu_above_college, 
          job_public, job_private, job_none, student,
          party_aff, 
          nationalistic_index, conservative_index, antimarket_index,
          patriotic_index, tech_dummy_index, globalization_dummy_index, 
          tcom_favorable_index, data_concern_index)


result_gender = list()     #store results to a list to print table
gender_df <- data.frame()  #store results to a dataframe


#Run regression for genders
for (i in 1:ncol(out)) {
  df_gender = data.frame(y=out[[i]], treat_gender, control_gender)
  var_name = names(out[i])
  print(var_name)
  model_gender <- lm(y ~., df_gender, na.action = na.exclude)
  print(summary(model_gender))
  result_gender[[i]] <- model_gender
  
  tidy_model_gender = tidy(model_gender)    #result from a a list to a df
  tidy_model_gender = mutate (tidy_model_gender, model = var_name)   #assign name of outcome to result
  gender_df = bind_rows(gender_df, tidy_model_gender)    #put all models in a df
}




### College----
treat_edu = model_list %>%   #Treatment
  select (edu_college_total,
          college_treat1 : college_treat3,
          below_college_treat1 : below_college_treat3)


control_college = model_list %>%   #Control 
  select (age, male, income_below_8k, minority, rural_growup, employed,
          edu_below_hs, edu_hs,  
          job_public, job_private, job_none, student,
          party_aff, 
          nationalistic_index, conservative_index, antimarket_index,
          patriotic_index, tech_dummy_index, globalization_dummy_index, 
          tcom_favorable_index, data_concern_index)

#Run regression for education levels
result_edu = list()
edu_df <- data.frame ()

for (i in 1:ncol(out)) {
  df_edu = data.frame(y=out[[i]], treat_edu, control_college)
  var_name = names(out[i])
  print(var_name)
  model_edu <- lm(y ~., df_edu, na.action = na.exclude)
  print(summary(model_edu))
  result_edu[[i]] <- model_edu
  
  tidy_model_edu = tidy(model_edu)    #result from a a list to a df
  tidy_model_edu = mutate (tidy_model_edu, model = var_name)   #assign name of outcome to result
  edu_df = bind_rows(edu_df, tidy_model_edu)    #put all models in a df
}



# PLOTTING ----

## Prepare for plotting----

all_model = bind_rows(gender_df, edu_df)   #put results into a df

all_model = all_model %>%    #Select results to plot
  filter (grepl("treat", term))


all_model <- all_model%>%               #create CI (line)
  mutate(ci95 = 1.96*std.error)


all_model$treatment = case_when(grepl("treat1", all_model$term) ~ "US-China Competition (T1)",
                                grepl("treat2", all_model$term) ~ "Sanction by US (T2)",
                                TRUE ~ "Gov use of data (T3)")    #change value

all_model$stakeholder = case_when (grepl("out1", all_model$model) ~ "Private companies",
                                   grepl("out3", all_model$model) ~ "Central government",
                                   TRUE ~ "Local government")    # change value

all_model$interaction = case_when (grepl ("female_treat", all_model$term)~ "Female",
                                   grepl ("male_treat", all_model$term) ~ "Male",
                                   grepl ("below_college", all_model$term) ~ "Below college",
                                   TRUE ~ "College or above")    # change value


# Give each value a position

model = rep(c("out1_scaled_index", "out3_scaled_index", "out5_scaled_index"), each = 4)   

# treatment = rep(c("US-China Competition", "Sanction by US", "Gov use of data"), 9)

interaction = rep(c("Male", "Female",
                "College or above", "Below college"), 3)  

position = rev (c (1, 1.2,
                   1.5, 1.7,
                   2.2, 2.4,
                   2.7, 2.9,
                   3.4, 3.6,
                   3.9, 4.1 ))

position_df = data.frame (model, position, interaction)

model_plot <- left_join (all_model, position_df)  #merge two tables, now results have position


# Reorder treatment and stakeholders so they appear as preferred order in the plotting
model_plot$treatment <- factor(model_plot$treatment, levels=c("US-China Competition (T1)",
                                                              "Sanction by US (T2)", 
                                                              "Gov use of data (T3)")) 
model_plot$interaction <- factor (model_plot$interaction, 
                                  levels = c("Male", "Female",
                                             "College or above", "Below college"))


## Plot----

model_plot %>%
  mutate (low = estimate-ci95, high = estimate+ci95) %>%
  ggplot (aes(x=position, y=estimate)) +
  geom_point (aes(shape = interaction)) +
  geom_errorbar(aes(ymin=low, ymax=high, linetype = interaction) ,width=0) +
  geom_hline(yintercept=0, linetype = 2)+
  scale_shape_manual (values = c(16,1,15,0,17,2)) +
  scale_linetype_manual (values = c("solid", "dashed", 
                                    "solid", "dashed")) +
  facet_grid(~treatment, scales = "fixed") + 
  labs(x="", y="Treatment Effects")+
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.background = element_rect(fill = '#F0F8FF'),
        strip.background =element_rect(fill="lightblue"),
        panel.border = element_rect(color = "black", fill = NA),
        axis.text.y = element_text (size = 20, hjust = 0),
        axis.title.x = element_text (size = 20),
        legend.text = element_text (size = 20),
        strip.text = element_text (size = 15),
        plot.margin = unit(c(0,0.5,0,0), "in")) +
  scale_x_continuous(breaks = c(3.6, 2.4, 1.2),
                     labels = c(str_wrap(c("Willingness to share with private companies",
                                           "Willingness to share with central government",
                                           "Willingness to share with local government"), width = 25)
                                          ))+
  coord_flip()   #flip the plot

ggsave(filename = "Figure 3.png",   #save plot
       height = 6, width = 8, scale = 16/9)


