setwd ()  #change working directory
rm (list = ls())
clean = read.csv("data_clean.csv")

library ("tidyverse")
library ("ggplot2")
library ("broom")
library ("stringr")

# RUN REGRESSION----

## Get variables lists----
model_list = clean%>%    #get the list
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
          out1_scaled_1:out1_scaled_7, out1_scaled_index, 
          out2_scaled_1:out2_scaled_3, out2_scaled_index,
          out3_scaled_1:out3_scaled_7, out3_scaled_index,
          out4_scaled_1:out4_scaled_3, out4_scaled_index,
          out5_scaled_1:out5_scaled_7, out5_scaled_index,
          out6_scaled_1:out6_scaled_3, out6_scaled_index)

out = model_list %>%  #outcome vars
  select (out2_scaled_1:out2_scaled_3, out2_scaled_index,
          out4_scaled_1:out4_scaled_3, out4_scaled_index,
          out6_scaled_1:out6_scaled_3, out6_scaled_index)

treat = model_list %>%  #treatment vars
  select (treatment_treat1:treatment_treat3)

control = model_list %>%   #control vars
  select (age, male, income_below_8k, minority, rural_growup, employed,
          edu_below_hs, edu_hs, edu_college, edu_above_college, 
          job_public, job_private, job_none, student,
          party_aff, 
          nationalistic_index, conservative_index, antimarket_index,
          patriotic_index, tech_dummy_index, globalization_dummy_index, 
          tcom_favorable_index, data_concern_index)


## Run regressions ----
result = list()     #store results to a list to print table
all_model <- data.frame()  #store results to a dataframe

for (i in 1:ncol(out)) {
  df = data.frame(y=out[[i]], treat, control)    #put outcomes, treatments and controls into 1 df
  var_name = names(out[i])    
  print(var_name)
  model <- lm(y ~., df, na.action = na.exclude)   #run regressions
  print(summary(model))
  result[[i]] <- model    #put to the list
  tidy_model = tidy(model)    #result from a list to a df
  tidy_model = mutate (tidy_model, model = var_name)   #assign name of outcome to result
  all_model = bind_rows(all_model, tidy_model)    #put all models in a df
}


# PLOTTING----

## Prepare for plotting ----

all_model <- all_model %>%
  filter (grepl("treatment", term))       #only get treatment results

all_model <- all_model%>%               #create CI (line)
  mutate(ci95 = 1.96*std.error)

all_model$treatment = case_when(all_model$term == "treatment_treat1"~ "US-China Competition (T1)",
                                all_model$term == "treatment_treat2"~ "Sanction by US (T2)",
                                TRUE ~ "Gov use of data (T3)")    #change value

all_model$stakeholder = case_when (grepl("out2", all_model$model) ~ "Private companies",
                                   grepl("out4", all_model$model) ~ "Central government",
                                   TRUE ~ "Local government")     #change value



### Positioning values in the plot ----
# Create a position table for each value 
out_business = c()
out_central = c()
out_local = c()

for (i in 1:3){
  out_business = c(out_business, paste0 ("out2_scaled_", i))
  out_central = c(out_central, paste0 ("out4_scaled_", i))
  out_local = c(out_local, paste0 ("out6_scaled_", i))
}

out_list = c(out_business, 
             out_central, 
             out_local,
             "out2_scaled_index", "out4_scaled_index","out6_scaled_index")

out_list = rep(out_list, each = 3)        

position_list = rev(c(1, 1.5, 2,
                      seq (from = 4, to = 8, by = 2),
                      seq (from = 4.5, to = 8.5, by = 2),
                      seq (from = 10.5, to = 14.5, by = 2)))

position_list = rep (position_list, each = 3)

stakeholder_list = rep(c("Private companies", "Central government", "Local government"), each = 9)

stakeholder_list = c(stakeholder_list, rep(c("Private companies", "Central government", "Local government"), each= 3))

treatment_list = rep (c("US-China Competition (T1)", 
                        "Sanction by US (T2)", 
                        "Gov use of data (T3)"), 3)

position_df = data.frame ("model" = out_list, 
                          "position" = position_list,
                          "stakeholder" = stakeholder_list,
                          "treatment" = treatment_list)   #a df with position

model_plot <- merge (all_model, position_df, by = c("model", "stakeholder", "treatment"))  #merge two tables, now results have position


### Reorder values ----
model_plot$treatment <- factor(model_plot$treatment, levels=c("US-China Competition (T1)", 
                                                              "Sanction by US (T2)", 
                                                              "Gov use of data (T3)")) 
model_plot$stakeholder <- factor (model_plot$stakeholder, 
                                  levels = c("Private companies", "Central government", "Local government"))



## Plotting ----
model_plot %>%
  mutate (low = estimate-ci95, high = estimate+ci95) %>%    
  ggplot (aes(x=position, y=estimate)) +    
  geom_point (aes(shape = stakeholder)) +   #dot
  geom_errorbar(aes(ymin=low, ymax=high, linetype = stakeholder) ,width=0) +   #whisker
  geom_hline(yintercept=0, linetype = 2)+   #cutoff line
  facet_grid(~treatment, scales = "fixed") +   #3 panels
  labs(x="", y="Treatment Effects")+     
  scale_color_manual(values = c(6, 12, 17)) +   #change type of dots
  theme(plot.title = element_text(face = "bold"), 
        legend.position = "bottom",     #legends
        legend.title = element_blank(),
        panel.background = element_rect(fill = '#F0F8FF'),   #panels colors
        strip.background =element_rect(fill="lightblue"),
        panel.border = element_rect(color = "black", fill = NA) ,
        axis.text.y = element_text(size = 20, hjust = 0),   #text size
        axis.title.x = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 16),
        plot.margin = unit(c(0,0.5,0,0), "in")) +    #plot margin
  
  scale_x_continuous(breaks = rev(c(1.5, 4.25, 6.25, 8.25, 10.5, 12.5, 14.5)),   #position of labels
                     labels = c(str_wrap(c("Targeted ads to consumers",
                                "Differentiating prices for products",
                                "Better understand user preferences",
                                "Identify potential criminals and deter unlawful activities",
                                "Identify people with different views from goverment policies",
                                "Use location data to control the spread of outbreak"
                                 ), width = 30),
                     "Support use of personal data index" = expression(bold("Support use of personal data index"))
                     ))+
 
  
  coord_flip()   #flip the plot 

ggsave(filename = "Figure 2.png",   #save the plot
       height = 6, width = 8, scale = 16/9)


