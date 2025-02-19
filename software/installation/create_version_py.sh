#!/bin/bash

# need to make a version.py file for BABS 
# BABS expects a version.py file, but the patched version we used for pyAFQ doesn't have that file.
version_content='version = "1.3.2-dev"'
output_file="/cbica/projects/luo_wm_dev/two_axes_manuscript/software/pyAFQ/AFQ/version.py"
echo "$version_content" > "$output_file"
echo "File '$output_file' has been created with version 1.3.2-dev."