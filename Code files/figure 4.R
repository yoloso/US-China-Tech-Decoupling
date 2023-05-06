setwd ()
rm(list = ls())
clean = read.csv("data_clean.csv")

library ("tidyverse")
library ("ggplot2")
library ("broom")


# REGRESSION ----
##Create dummy variables----
clean$male_college_or_above_dummy = case_when (clean$male==1 & clean$edu_college_total==1~1,
                                               TRUE ~ 0)
clean$male_below_college_dummy = case_when (clean$male==1 & clean$edu_college_total==0~1,
                                            TRUE ~ 0)
clean$female_college_or_above_dummy = case_when (clean$female==1 & clean$edu_college_total==1~1,
                                                 TRUE ~0)
clean$female_below_college_dummy = case_when (clean$female==1 & clean$edu_college_total==0~1,
                                              TRUE ~0)

##Interaction terms----


clean$male_college_treat1 = clean$male_college_or_above_dummy * clean$treatment_treat1
clean$male_college_treat2 = clean$male_college_or_above_dummy * clean$treatment_treat2
clean$male_college_treat3 = clean$male_college_or_above_dummy * clean$treatment_treat3

clean$male_below_treat1 = clean$male_below_college_dummy * clean$treatment_treat1
clean$male_below_treat2 = clean$male_below_college_dummy * clean$treatment_treat2
clean$male_below_treat3 = clean$male_below_college_dummy * clean$treatment_treat3

clean$female_college_treat1 = clean$female_college_or_above_dummy * clean$treatment_treat1
clean$female_college_treat2 = clean$female_college_or_above_dummy * clean$treatment_treat2
clean$female_college_treat3 = clean$female_college_or_above_dummy * clean$treatment_treat3

clean$female_below_treat1 = clean$female_below_college_dummy * clean$treatment_treat1
clean$female_below_treat2 = clean$female_below_college_dummy * clean$treatment_treat2
clean$female_below_treat3 = clean$female_below_college_dummy * clean$treatment_treat3



## Get the lists----
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
          out1_scaled_index, out2_scaled_index, out3_scaled_index,
          out4_scaled_index, out5_scaled_index, out6_scaled_index,
          out7_scaled, out8_scaled, out9_scaled,
          out10_scaled, out11_scaled,
          male_college_or_above_dummy, male_below_college_dummy, female_college_or_above_dummy, female_below_college_dummy,
          male_college_treat1 : male_college_treat3,
          male_below_treat1 : male_below_treat3,
          female_college_treat1 : female_college_treat3,
          female_below_treat1 : female_below_treat3)          # Interaction terms

out = model_list %>%
  select (out1_scaled_index, out2_scaled_index, out3_scaled_index,
          out4_scaled_index, out5_scaled_index, out6_scaled_index,
          out7_scaled, out8_scaled, out9_scaled,
          out10_scaled, out11_scaled)

treat = model_list%>%
  select (male_college_or_above_dummy, male_below_college_dummy, female_college_or_above_dummy, female_below_college_dummy,
          male_college_treat1 : male_college_treat3,
          male_below_treat1 : male_below_treat3,
          female_college_treat1 : female_college_treat3,
          female_below_treat1 : female_below_treat3)

control = model_list%>%
  select (age,  income_below_8k, minority, rural_growup, employed, 
          
          job_public, job_private, job_none, student, party_aff,
          nationalistic_index,
          conservative_index,
          antimarket_index,
          patriotic_index,
          tech_dummy_index,
          globalization_dummy_index,
          tcom_favorable_index,
          data_concern_index)


## Run regressions----
result = list()     #store results to a list to print table
all_model <- data.frame()  #store results to a dataframe


for (i in 1:ncol(out)) {
  df = data.frame(y=out[[i]], treat, control)
  var_name = names(out[i])
  print(var_name)
  model <- lm(y ~., df, na.action = na.exclude)
  print(summary(model))
  result[[i]] <- model
  
  tidy_model = tidy(model)    #result from a a list to a df
  tidy_model = mutate (tidy_model, model = var_name)   #assign name of outcome to result
  all_model = bind_rows(all_model, tidy_model)    #put all models in a df
}


# PLOTTING----
## Prepare for plotting ----
all_model = all_model %>%
  filter (grepl("treat", term))


all_model <- all_model%>%               #create CI (line)
  mutate(ci95 = 1.96*std.error)


all_model$treatment = case_when(grepl("treat1", all_model$term) ~ "US-China Competition (T1)",
                                grepl("treat2", all_model$term) ~ "Sanction by US (T2)",
                                TRUE ~ "Gov use of data (T3)")

all_model$interaction <- case_when( grepl("female_college", all_model$term) ~ "Female above college",
                                    grepl("female_below", all_model$term) ~ "Female below college",
                                    grepl("male_college", all_model$term) ~ "Male above college",
                                    grepl("male_below", all_model$term) ~ "Male below college")



## Positioning values----

panel_1 = all_model %>%
  filter (model %in% c("out1_scaled_index", "out3_scaled_index", "out5_scaled_index"))

p1_model <- rep (c("out1_scaled_index", "out3_scaled_index", "out5_scaled_index"), each = 4)

p1_interaction <- rep (c("Male above college", "Male below college", 
                         "Female above college", "Female below college"), 3)
p1_position <- c (5.6, 5.4, 5.2, 5, 
                  3.6, 3.4, 3.2, 3, 
                  1.6, 1.4, 1.2, 1)

p1_position_df <- data.frame("model" = p1_model, 
                             "interaction" = p1_interaction, 
                             "position" = p1_position)

model_plot <- left_join (panel_1, p1_position_df)


# Reorder treatment and stakeholders so they appear as preferred order in the plotting
model_plot$treatment <- factor(model_plot$treatment, levels=c("US-China Competition (T1)", 
                                                              "Sanction by US (T2)", 
                                                              "Gov use of data (T3)")) 

model_plot$interaction <- factor (model_plot$interaction, 
                                  levels = c("Male above college", 
                                             "Female above college", 
                                             "Male below college", 
                                             "Female below college"))


## Plot----
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
        axis.text.y = element_text (size = 20, hjust = 0),
        legend.text = element_text (size = 20),
        strip.text = element_text (size = 15),
        axis.title.x = element_text (size = 20),
        plot.margin = unit (c(0, 0.5, 0, 0), "in")) +
  guides(shape = guide_legend(nrow = 2)) +
  geom_point (aes(shape = interaction)) +
  scale_shape_manual (values = c(16,15,1,0)) +
  geom_errorbar(aes(ymin=low, ymax=high, linetype = interaction) ,width=0) +
  scale_linetype_manual (values = c("solid", "solid", 
                                    "dashed", "dashed")) +
  geom_hline(yintercept=0, linetype = 2)+
  facet_grid(~treatment, scales = "fixed") + 
  labs(x="", y="Treatment Effects")+
  scale_x_continuous(breaks = c(5.5, 3.5, 1.5),
                     labels = c(str_wrap(c("Willingness to share data with private companies",
                                           "Willingness to share data with central government",
                                           "Willingness to share data with local government"), width = 27)
                     ))+
  
  
  coord_flip()


# save
ggsave (filename = "Figure 4.png",
        height = 6, width = 8, scale = 16/9)
