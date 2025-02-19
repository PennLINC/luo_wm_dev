#!/bin/bash
 

 cd ~/two_axes_manuscript/code/run_babs_qsirecon/
./babs_init_replication.sh # initializes babs for PNC, HCP-D, and HBN for 3 test subjects 
# in each dataset 


cd ~/two_axes_manuscript/code/run_babs_qsirecon/
./copy_json_files.sh # run code to copy json files to respective BABS project


cd ~/two_axes_manuscript/code/run_babs_qsirecon/
./data_description_files.sh # run code to make data description files for respective BABS project