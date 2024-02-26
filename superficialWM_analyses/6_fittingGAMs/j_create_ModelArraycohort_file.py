import csv
import os
from os.path import join as ospj
import pandas as pd
import re
import sys
import json
 

# This script creates cohort files for ModelArray with fsaverage5 dsistudio scalar data, GMprobseg data 
# and also creates cohort files for dsistudio scalar data that has had vertices NaN'd out based on that subject's GMprobseg data (>50% GM probability vertices are NaN'ed)

########################################
# Set directories
########################################
 
config_file = "/cbica/projects/luo_wm_dev/code/superficialWM_analyses/config_HCPD.json"

# Read config from the specified file
with open(config_file, "rb") as f:
    config = json.load(f)

dataset = config['dataset']
data_root = config['data_root']
outputs_root = config['outputs_root']
demographics = config['demographics_file']
qc = config['qc_file']

# make output cohortfiles directories
if not os.path.exists(ospj(f"{outputs_root}", "all_subjects", "cohortfiles")):
        os.makedirs(ospj(f"{outputs_root}", "all_subjects", "cohortfiles"))
        print(f"Directory 'cohortfiles' created.")
else:
        print(f"Directory 'cohortfiles' already exists.")

if not os.path.exists(ospj(f"{outputs_root}", "all_subjects", "cohortfiles", "dsistudio_scalars")):
        os.makedirs(ospj(f"{outputs_root}", "all_subjects", "cohortfiles", "dsistudio_scalars"))
        print(f"Directory 'cohortfiles/dsistudio_scalars' created.")
else:
        print(f"Directory 'cohortfiles/dsistudio_scalars' already exists.")

if not os.path.exists(ospj(f"{outputs_root}", "all_subjects", "cohortfiles", "GMprobseg")):
        os.makedirs(ospj(f"{outputs_root}", "all_subjects", "cohortfiles", "GMprobseg"))
        print(f"Directory 'cohortfiles/probseg' created.")
else:
        print(f"Directory 'cohortfiles/probseg' already exists.")




########################################
# Make Cohort Files
########################################

# load demographics and qc files
dem_df = pd.read_csv(demographics)
qc_df = pd.read_csv(qc)
merged_df = pd.merge(dem_df, qc_df, on="sub")
merged_df = merged_df.drop(columns=['t1_neighbor_corr', 'race', 'site']) 

# set scalars
scalars = ["ad", "dti_fa", "gfa", "ha", "iso", "md", "qa", "rd", "rd1", "rd2", "rdi"]

# define function for making cohort files (1 file per scalar; 1 file for GMprobseg) - vertices NOT filtered for GM probability
def make_cohort_df(depth, type):
    if type == "scalar":
        scalar_dataframes = {}
        # Iterate over scalars
        for scalar in scalars:
            print(scalar)
            if not os.path.exists(ospj(f"{outputs_root}", "all_subjects", "cohortfiles", "dsistudio_scalars", f"{scalar}")):
                os.makedirs(ospj(f"{outputs_root}", "all_subjects", "cohortfiles", "dsistudio_scalars", f"{scalar}"))
                print(f"Directory 'cohortfiles/dsistudio_scalars/scalar' created.")
            else:
                print(f"Directory 'cohortfiles/dsistudio_scalars/scalar' already exists.")
            # Add a new column 'scalar_name' with scalar name
            scalar_df = merged_df.copy()  # Create a copy of the original dataframe
            scalar_df['scalar_name'] = scalar
            
            # Create source_file column    
            scalar_df['source_file'] = scalar_df.apply(lambda row: f"{outputs_root}/{row['sub']}/vol_to_surf/dsistudio_scalars/fsaverage5/{scalar}/{row['sub']}_{scalar}_{depth}_fsaverage5.shape.gii", axis=1)
        

            # Create separate dataframe for each scalar
            scalar_dataframes[scalar] = scalar_df  # Add dataframe to dictionary with scalar name as key

            scalar_dataframes[scalar].to_csv(f'{outputs_root}/all_subjects/cohortfiles/dsistudio_scalars/{scalar}/{scalar}_{depth}_cohortfile.csv', index=False)
    else:
        probseg_dataframes = {}
        
        # Add a new column 'GMprobseg' 
        probseg_df = merged_df.copy()  # Create a copy of the original dataframe
        probseg_df['scalar_name'] = "GMprobseg"
        
        # Create source_file column    
        probseg_df['source_file'] = probseg_df.apply(lambda row: f"{outputs_root}/{row['sub']}/vol_to_surf/probseg_depths/fsaverage5/{row['sub']}_GMprobseg_{depth}_fsaverage5.shape.gii", axis=1)
     
        probseg_df.to_csv(f'{outputs_root}/all_subjects/cohortfiles/GMprobseg/GMprobseg_{depth}_cohortfile.csv', index=False)


# depth = 1 mm
make_cohort_df("depth_1", "scalar")
make_cohort_df("depth_1", "GMprobseg")

# depth = 1.25 mm
make_cohort_df("depth_1p25", "scalar")
make_cohort_df("depth_1p25", "GMprobseg")

# depth = 3 mm (GMprobseg only)
make_cohort_df("depth_3", "GMprobseg")


      
# define function for making cohort files (1 file per scalar) - dsistudio scalar vertices ARE filtered for GM probability and medial wall is removed
def make_cohort_df_filtered(depth):
        scalar_dataframes = {}
        # Iterate over scalars
        for scalar in scalars:
            print(scalar)
            if not os.path.exists(ospj(f"{outputs_root}", "all_subjects", "cohortfiles", "dsistudio_scalars", f"{scalar}")):
                os.makedirs(ospj(f"{outputs_root}", "all_subjects", "cohortfiles", "dsistudio_scalars", f"{scalar}"))
                print(f"Directory 'cohortfiles/dsistudio_scalars/scalar' created.")
            else:
                print(f"Directory 'cohortfiles/dsistudio_scalars/scalar' already exists.")
            # Add a new column 'scalar_name' with scalar name
            scalar_df = merged_df.copy()  # Create a copy of the original dataframe
            scalar_df['scalar_name'] = scalar
            
            # Create source_file column    
            scalar_df['source_file'] = scalar_df.apply(lambda row: f"{outputs_root}/{row['sub']}/vol_to_surf/dsistudio_scalars/fsaverage5/{scalar}/{row['sub']}_{scalar}_{depth}_fsaverage5_GMfiltered_noMW.shape.gii", axis=1)
        

            # Create separate dataframe for each scalar
            scalar_dataframes[scalar] = scalar_df  # Add dataframe to dictionary with scalar name as key

            scalar_dataframes[scalar].to_csv(f'{outputs_root}/all_subjects/cohortfiles/dsistudio_scalars/{scalar}/{scalar}_{depth}_cohortfile_GMfiltered_noMW.csv', index=False)
     
      


# depth = 1 mm
make_cohort_df_filtered("depth_1")
 
# depth = 1.25 mm
make_cohort_df_filtered("depth_1p25")
  

# depth = 3 mm
make_cohort_df_filtered("depth_3")
  

