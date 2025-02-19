
#!/bin/bash

# main analyses
cp ~/two_axes_manuscript/code/run_babs_qsirecon/qsirecon_json_files/mrtrix_singleshell_ss3t_ACT-hsvs_pyafq_dti.json ~/input/PNC/derivatives/babs_qsirecon_pyafq_act/analysis/code
cp ~/two_axes_manuscript/code/run_babs_qsirecon/qsirecon_json_files/mrtrix_multishell_msmt_ACT-hsvs_pyafq_dti.json ~/input/HCPD/derivatives/babs_qsirecon_pyafq_act/analysis/code
cp ~/two_axes_manuscript/code/run_babs_qsirecon/qsirecon_json_files/mrtrix_multishell_msmt_pyafq_tractometry_dti.json ~/input/HBN/derivatives/babs_qsirecon_pyafq_allsubs_noACT/analysis/code

# HBN sensitivity analysis
cp ~/two_axes_manuscript/code/run_babs_qsirecon/qsirecon_json_files/mrtrix_multishell_msmt_ACT-hsvs_pyafq_dti.json ~/input/HBN/derivatives/babs_qsirecon_pyafq_act/analysis/code