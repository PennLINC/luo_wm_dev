#build a singularity image for qsiprep
#docker site: https://hub.docker.com/r/pennbbl/qsiprep/tags
#tag: https://hub.docker.com/layers/pennbbl/qsiprep/0.22.0/images/sha256-597df19a6268b975bf8b94dda96bd9f7ba79d9981a9cf7ecfc5bf6ab74471941


cd /cbica/projects/luo_wm_dev/two_axes_manuscript/software/
mkdir -p qsiprep
singularity build qsiprep/qsiprep_0.22.0.sif docker://pennbbl/qsiprep:0.22.0

