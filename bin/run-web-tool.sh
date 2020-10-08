#! /bin/bash

# docker image import https://tool.transitionmonitor.com/downloads/2diirunner_1.3.0.tar.xz 2diirunner:1.3.0

# TODO: Change directory to PACTA_analysis/
analysis=$(pwd)
data=$(dirname $(pwd))/pacta-data
report=$(dirname $(pwd))/create_interactive_report
stresstest=$(dirname $(pwd))/StressTestingModelDev

# To commit changes to this image, remove the flag --rm
docker run --rm -ti \
  --mount type=bind,source=$analysis,target=/bound \
  --mount type=bind,source=$data,target=/pacta-data \
  --mount type=bind,source=$report,target=/create_interactive_report \
  --mount type=bind,source=$stresstest,target=/StressTestingModelDev \
  2diirunner:1.3.0 /bound/bin/run-scripts.sh

