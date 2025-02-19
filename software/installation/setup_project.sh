#!/bin/bash

project_root="/cbica/projects/luo_wm_dev/two_axes_manuscript/"
mkdir -p ${project_root}/code/logs

if [ ! -d ${project_root}/input/ ]; then
    mkdir -p ${project_root}/input/PNC/derivatives
    mkdir -p ${project_root}/input/PNC/sample_selection_files/final_sample
    mkdir -p ${project_root}/input/PNC/subject_list/final_sample
     
    mkdir -p ${project_root}/input/HCPD/derivatives
    mkdir -p ${project_root}/input/HCPD/sample_selection_files/final_sample
    mkdir -p ${project_root}/input/HCPD/subject_list/final_sample
     
    mkdir -p ${project_root}/input/HBN/derivatives
    mkdir -p ${project_root}/input/HBN/sample_selection_files/final_sample
    mkdir -p ${project_root}/input/HBN/subject_list/final_sample
     
fi 

if [ ! -d ${project_root}/output/ ]; then
    mkdir -p ${project_root}/output/PNC/tract_profiles
    mkdir -p ${project_root}/output/HCPD/tract_profiles
    mkdir -p ${project_root}/output/HBN/tract_profiles    
fi 

