import csv
import json
import os
from os.path import join as ospj
import re
import sys

# This script identifies CUBIDS acquisition variants and non-variants to use for sample selection for a given dataset

###########################
## Set variables and dirs #
###########################
dataset = sys.argv[1]
config_file = sys.argv[2]

# Read config from the specified file
with open(config_file, "rb") as f:
    config = json.load(f)

data_root = config['data_root']
manuscript_input_root = config['manuscript_input_root']

qc_dir = ospj(data_root, "raw", "qc_files")
sample_selection_dir = ospj(manuscript_input_root, "sample_selection_files")
if not os.path.exists(sample_selection_dir):
    os.makedirs(sample_selection_dir)

# Create a list to store subject names
subject_names_variant = []
subject_names_nonvariant = []

# iterate through qc_files directory
if dataset == "HCPD":
    for filename in os.listdir(qc_dir):
        subject_name = filename.split('_')[0]
        print(subject_name)
        if "acq-dir98" in filename:
            # If "acq-dir98" is in the filename, add the subject name to the variant list
            subject_names_variant.append(subject_name)
        else :
            subject_names_nonvariant.append(subject_name) # otherwise, add to nonvariants :)
else: 
    for filename in os.listdir(qc_dir):
        subject_name = filename.split('_')[0]
        print(subject_name)
        if "VARIANT" in filename:
            # If "VARIANT" is in the filename, add the subject name to the variant list
            subject_names_variant.append(subject_name)
        else:
            subject_names_nonvariant.append(subject_name) # otherwise, add to nonvariants :)

# write the variant subject names to a csv file
csv_file_variants = os.path.join(sample_selection_dir, f'{dataset}_VariantDWI_participantlist.csv')
with open(csv_file_variants, 'w', newline='') as csvfile: # the with statement sets up context to perform file operations within this indented block
    csv_writer = csv.writer(csvfile) # creates csv writer object
    csv_writer.writerow(['Subject Name'])   
    csv_writer.writerows([[subject] for subject in subject_names_variant])

print(f'Subject names saved to {csv_file_variants}')

# write the non-variant subject names to a csv file
csv_file_nonvariants = os.path.join(sample_selection_dir, f'{dataset}_NonVariantDWI_participantlist.csv')
with open(csv_file_nonvariants, 'w', newline='') as csvfile: # the with statement sets up context to perform file operations within this indented block
    csv_writer = csv.writer(csvfile) # creates CSV writer object
    csv_writer.writerow(['Subject Name'])  # Write header
    csv_writer.writerows([[subject] for subject in subject_names_nonvariant])

print(f'Subject names saved to {csv_file_nonvariants}')