#!/bin/bash


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

HCPD_json="/cbica/projects/luo_wm_dev/input/HCPD/derivatives/babs_noddi/analysis/code/dataset_description.json"
echo "$json_content" > "$HCPD_json"
echo "JSON file '$HCPD_json' has been created successfully."

HCPD_json="/cbica/projects/luo_wm_dev/input/HCPD/derivatives/babs_mapmri/analysis/code/dataset_description.json"
echo "$json_content" > "$HCPD_json"
echo "JSON file '$HCPD_json' has been created successfully."

# HBN
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

HBN_json="/cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_noddi/analysis/code/dataset_description.json"
echo "$json_content" > "$HBN_json"
echo "JSON file '$HBN_json' has been created successfully."

HBN_json="/cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_mapmri/analysis/code/dataset_description.json"
echo "$json_content" > "$HBN_json"
echo "JSON file '$HBN_json' has been created successfully."