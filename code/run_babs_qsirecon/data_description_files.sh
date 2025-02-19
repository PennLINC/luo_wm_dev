#!/bin/bash


# PNC
json_content='{
    "Acknowledgements": "",
    "Authors": [],
    "BIDSVersion": "1.0.2",
    "DatasetDOI": "",
    "Funding": [],
    "HowToAcknowledge": "",
    "License": "",
    "Name": "RBC_PNC",
    "ReferencesAndLinks": [],
    "template": "project"
}'


PNC_json="/cbica/projects/luo_wm_dev/input/PNC/derivatives/babs_qsirecon_pyafq_act/analysis/code/dataset_description.json"
echo "$json_content" > "$PNC_json"
echo "JSON file '$PNC_json' has been created successfully."


# HCPD
json_content='{
    "Acknowledgements": "",
    "Authors": [],
    "BIDSVersion": "1.0.2",
    "DatasetDOI": "",
    "Funding": [],
    "HowToAcknowledge": "",
    "License": "",
    "Name": "RBC_HCPD",
    "ReferencesAndLinks": [],
    "template": "project"
}'

HCPD_json="/cbica/projects/luo_wm_dev/input/HCPD/derivatives/babs_qsirecon_pyafq_act/analysis/code/dataset_description.json"
echo "$json_content" > "$HCPD_json"
echo "JSON file '$HCPD_json' has been created successfully."



# HBN: with ACT (sensitivity analysis)
json_content='{
    "Acknowledgements": "",
    "Authors": [],
    "BIDSVersion": "1.0.2",
    "DatasetDOI": "",
    "Funding": [],
    "HowToAcknowledge": "",
    "License": "",
    "Name": "RBC_HBN",
    "ReferencesAndLinks": [],
    "template": "project"
}'

HBN_json="/cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_qsirecon_pyafq_act/analysis/code/dataset_description.json"
echo "$json_content" > "$HBN_json"
echo "JSON file '$HBN_json' has been created successfully."