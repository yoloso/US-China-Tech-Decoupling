setwd ()  #change the working directory
rm (list = ls())  #start fresh
clean = read.csv("data_clean.csv")


#Load library 
# If you don't have these packages, uncomment to install packages
#install.packages ("tidyverse")
#install.packages ("ggplot2")
#install.packages ("broom")

library ("tidyverse")
# library ("stargazer")
# library ("sjPlot")
library ("ggplot2")
library ("broom")


# RUN REGRESSION----


## Select variables ----

model_list = clean%>%     #select variables into list
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

out = model_list %>%    #outcome variables
  select (out1_scaled_1:out1_scaled_7, out1_scaled_index,
          out3_scaled_1:out3_scaled_7, out3_scaled_index,
          out5_scaled_1:out5_scaled_7, out5_scaled_index)

treat = model_list %>%  #treatment variables
  select (treatment_treat1:treatment_treat3)

control = model_list %>%   #control variables
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
  df = data.frame(y=out[[i]], treat, control)    #put variables into a data frame
  var_name = names(out[i]) 
  print(var_name)
  model <- lm(y ~., df, na.action = na.exclude)    #run regression
  print(summary(model)) 
  result[[i]] <- model     #put result into the list
  
# PLOTTING ----
  
## Select results to plot ----
  tidy_model = tidy(model)    #result from a a list to a df
  tidy_model = mutate (tidy_model, model = var_name)   #assign name of outcome to result
  all_model = bind_rows(all_model, tidy_model)    #put all models in a df
}

all_model <- all_model %>%
filter (grepl("treatment", term))       #only get treatment results

all_model <- all_model%>%               #create CI (line)
  mutate(ci95 = 1.96*std.error)

all_model$treatment = case_when(all_model$term == "treatment_treat1"~ "US-China Competition (T1)",
                                all_model$term == "treatment_treat2"~ "Sanction by US (T2)",
                                TRUE ~ "Gov use of data (T3)")  #Change value

all_model$stakeholder = case_when (grepl("out1", all_model$model) ~ "Private companies",
                                   grepl("out3", all_model$model) ~ "Central government",
                                   TRUE ~ "Local government")   #Change value



## give each result a position ----
out_business = c()
out_central = c()
out_local = c()

for (i in 1:7){
  out_business = c(out_business, paste0 ("out1_scaled_", i))    
  out_central = c(out_central, paste0 ("out3_scaled_", i))
  out_local = c(out_local, paste0 ("out5_scaled_", i))
}

out_list = c(out_business, "out1_scaled_index", 
             out_central, "out3_scaled_index",
             out_local, "out5_scaled_index")

out_list = rep(out_list, each = 3)  

position_list = rev(c(seq (from = 1, to = 22, by = 3),
                      seq (from = 1.5, to = 22.5, by = 3),
                      seq (from = 2, to = 23, by = 3)))

position_list = rep (position_list, each = 3)

stakeholder_list = rep(c("Private companies", "Central government", "Local government"), each = 24)

treatment_list = rep (c("US-China Competition (T1)", 
                        "Sanction by US (T2)", 
                        "Gov use of data (T3)"), 3)

position_df = data.frame ("model" = out_list, 
                 "position" = position_list,
                 "stakeholder" = stakeholder_list,
                 "treatment" = treatment_list)   #a df with position

model_plot <- merge (all_model, position_df, by = c("model", "stakeholder", "treatment"))  #merge two tables, now results have position


# Reorder treatment and stakeholders so they appear as preferred order in the plotting
model_plot$treatment <- factor(model_plot$treatment, levels=c("US-China Competition (T1)", 
                                                              "Sanction by US (T2)", 
                                                              "Gov use of data (T3)")) 
model_plot$stakeholder <- factor (model_plot$stakeholder, 
                                  levels = c("Private companies", "Central government", "Local government"))



## Plot----
model_plot %>%
  mutate (low = estimate-ci95, high = estimate+ci95) %>%
  ggplot (aes(x=position, y=estimate)) +   #get axes
  geom_point (aes(shape = stakeholder)) +   #get dots
  geom_errorbar(aes(ymin=low, ymax=high, linetype = stakeholder) ,width=0) +  #get whiskers 
  geom_hline(yintercept=0, linetype = 2)+   #get cutoff line
  facet_grid(~treatment, scales = "fixed") + #divide into 3 grids ~ 3 treatments
  labs(x="", y="Treatment Effects")+     #Get title for axes
  scale_color_manual(values = c(6, 15, 17)) +    #change shape of dots
  theme(legend.position = "bottom",           #legend 
        legend.title = element_blank(),
        panel.background = element_rect(fill = '#F0F8FF'),   #color of panel elements
        strip.background =element_rect(fill="lightblue"),
        panel.border = element_rect(color = "black", fill = NA),
        axis.text.y = element_text (size = 20, hjust = 0),   #size of labels
        axis.title.x = element_text (size = 20),
        legend.text = element_text (size = 20), 
        strip.text = element_text (size = 16),
        plot.margin = unit(c(0,0.5,0,0), "in")) +    #margin of figure
   scale_x_continuous(breaks = rev(seq(from = 1.5, to = 22.5, by = 3)),   
                     labels = c("Biological and facial data",
                                "Online shopping records",
                                "Web browsing history",
                                "Location and travel information",
                                "Driving records",
                                "Medical records",
                                "Financial information",
                                "Willingness to share index" = expression(bold("Willingness to share index"))
                                ))+

  coord_flip()    #flip the figure

ggsave(filename = "Figure 1.png",    #save figure
       height = 6, width = 8, scale = 16/9)


