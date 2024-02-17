import csv
import os
from os.path import join as ospj
import pandas as pd
import re
import sys
import json

########################################
# Set directories
########################################
# Parse command-line arguments
if len(sys.argv) != 2:
    print("Usage: python g_best_depths.py <config_file>")
    sys.exit(1)

config_file = sys.argv[1]

# Read config from the specified file
with open(config_file, "rb") as f:
    config = json.load(f)

dataset = config['dataset']
outputs_root = config['outputs_root']
 

# Initialize separate DataFrames for each hemisphere and measure type
lh_GM_combined = pd.DataFrame()
lh_WM_combined = pd.DataFrame()
rh_GM_combined = pd.DataFrame()
rh_WM_combined = pd.DataFrame()

# Regular expression pattern to match subject IDs
subject_id_pattern = re.compile(r'sub-\d+')

# Loop through directories for each subject
for root, dirs, files in os.walk(outputs_root):
    for subject_dir in dirs:
        # Extract subject ID from folder name
        match = subject_id_pattern.search(subject_dir)
        if match:
            subject_id = match.group()
            
            # Loop through hemispheres
            for hemisphere in ['lh', 'rh']:
                # Loop through measure types (GM and WM)
                for measure_type in ['GM', 'WM']:
                    file_name = f'{hemisphere}_{measure_type}probseg_diffdepths.csv'
                    file_path = os.path.join(root, subject_dir, 'vol_to_surf', 'probseg_depths', file_name)
                    
                    # Check if the file exists
                    if os.path.exists(file_path):
                        # Load CSV file into DataFrame
                        df = pd.read_csv(file_path)
                        
                        if measure_type == "GM":
                            # Rename 'Lowest_GM_probability' column to subject ID
                            df = df.rename(columns={'Lowest_GM_probability': subject_id})
                        else: 
                            df = df.rename(columns={'Highest_WM_probability': subject_id})
                        
                        # Select only 'depth' and subject ID columns
                        df = df[['Depth', subject_id]]
                        
                        # Combine with the corresponding DataFrame based on hemisphere and measure type
                        if hemisphere == 'lh' and measure_type == 'GM':
                            lh_GM_combined = pd.concat([lh_GM_combined, df], axis=1)
                            
                        elif hemisphere == 'lh' and measure_type == 'WM':
                            lh_WM_combined = pd.concat([lh_WM_combined, df], axis=1)
                            
                        elif hemisphere == 'rh' and measure_type == 'GM':
                            rh_GM_combined = pd.concat([rh_GM_combined, df], axis=1)
                            
                        elif hemisphere == 'rh' and measure_type == 'WM':
                            rh_WM_combined = pd.concat([rh_WM_combined, df], axis=1)
            print(f"combined df's done for {subject_id}")


# Drop duplicate "depth" columns, keeping only the first one
lh_GM_combined = lh_GM_combined.loc[:,~lh_GM_combined.columns.duplicated()]
lh_WM_combined = lh_WM_combined.loc[:,~lh_WM_combined.columns.duplicated()]
rh_GM_combined = rh_GM_combined.loc[:,~rh_GM_combined.columns.duplicated()]
rh_WM_combined = rh_WM_combined.loc[:,~rh_WM_combined.columns.duplicated()]
 

 

# Create copies of the combined DataFrames
lh_GM_count_df = lh_GM_combined.copy()
lh_WM_count_df = lh_WM_combined.copy()
rh_GM_count_df = rh_GM_combined.copy()
rh_WM_count_df = rh_WM_combined.copy()

# Sum across subject columns to get count for each row
lh_GM_count_df['count'] = lh_GM_combined.iloc[:, 1:].sum(axis=1)
lh_WM_count_df['count'] = lh_WM_combined.iloc[:, 1:].sum(axis=1)
rh_GM_count_df['count'] = rh_GM_combined.iloc[:, 1:].sum(axis=1)
rh_WM_count_df['count'] = rh_WM_combined.iloc[:, 1:].sum(axis=1)

# Drop subject ID columns
lh_GM_count_df = lh_GM_count_df.drop(columns=lh_GM_combined.columns[1:])
lh_WM_count_df = lh_WM_count_df.drop(columns=lh_WM_combined.columns[1:])
rh_GM_count_df = rh_GM_count_df.drop(columns=rh_GM_combined.columns[1:])
rh_WM_count_df = rh_WM_count_df.drop(columns=rh_WM_combined.columns[1:])

# Print first few rows of each count DataFrame
print("lh_GM_count_df:")
print(lh_GM_count_df.head())
print("\nlh_WM_count_df:")
print(lh_WM_count_df.head())
print("\nrh_GM_count_df:")
print(rh_GM_count_df.head())
print("\nrh_WM_count_df:")
print(rh_WM_count_df.head())


# make directory for csv output
if not os.path.exists(ospj(outputs_root, "all_subjects", "probseg_depths")):
    os.makedirs(ospj(outputs_root, "all_subjects", "probseg_depths"))

probseg_dir = ospj(outputs_root, "all_subjects", "probseg_depths")

# Save out csv's
lh_GM_count_df.to_csv(ospj(probseg_dir, "lh_GM_count.csv"), index=False)
lh_WM_count_df.to_csv(ospj(probseg_dir, "lh_WM_count.csv"), index=False)
rh_GM_count_df.to_csv(ospj(probseg_dir, "rh_GM_count.csv"), index=False)
rh_WM_count_df.to_csv(ospj(probseg_dir, "rh_WM_count.csv"), index=False)

