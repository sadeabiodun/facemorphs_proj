# facemorphs

# Below is a description of each R script in the scr folder of this repository: 

#0_transform_data.R 
#-purpose: imports raw data and cleans/reverse codes necessary variables 
#-input file: data/base_data/Facemorph1_raw.csv and data/base_data/Facemorph2_raw.csv
#-output file: data/base_data/Faces1_Numeric_Data_reverse.csv and data/base_data/Faces2_Numeric_Data_reverse.csv

#1_summarize_data.R
#-purpose: creates summary tables with combined faces 1 and faces 2 ratings for analysis in JASP 
#-helper function: scr/SummarySE2.R
#-input file: data/base_data/Faces1_Numeric_Data_reverse.csv and data/base_data/Faces2_Numeric_Data_reverse.csv 
#-output file: data/all_faces_ratings.csv and data/ave_faces_ratings_aro_val.csv

#2a_visualize_data_arsl.R 
#-purpose: create plot visualizations for arousal rating trends (outputs 3 way interaction plot)
#-helper function: scr/SummarySE2.R
#-input file: data/ave_faces_ratings.csv 
#-output file: plots/emo_mag_age_legend_arsl.png 

#2b_visualize_Data_vln.R 
#-purpose: create plot visualizations for valence rating trends (outputs 3 way interaction plot)
#-helper function: scr/SummarySE2.R
#-input file: data/ave_faces_ratings.csv 
#-output file: plots/emo_mag_age_legend_vln.png 

#3_naturalness_analysis.R 
#-purpose: analyze naturalness-of-stimuli ratings given by participants 
#-helper function: scr/SummarySE2.R
#-input file: data/base_data/Faces2_Numeric_Data_reverse.csv
#-output file: data/nat_ratings.csv 

#SummarySE2.R
#-purpose: helper functions used to generate means and standard error values for groups of data 

#Below is a description of each directory inside this repository:

#data/ 
#- contains interim data sets and script outpu. All contents on first level of this directory can be deleted and regenerated from scripts. content in data/base_data are the raw data files 

#plots/
#- contains figures and tables for manuscript. bar plots in this directory can be deleted and regenerated from scripts.

#scr/
#- contains functions and other helper scripts.