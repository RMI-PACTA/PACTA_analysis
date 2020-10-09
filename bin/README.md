## Running the web tool on transitionmonitor's docker-container

Import the base images with:

```bash
docker image import https://tool.transitionmonitor.com/downloads/2diirunner_1.3.0.tar.xz 2diirunner:1.3.0
```

Ensure your working directory is PACTA_analysis, then run one of these workflows:

* Run the web tool in a single step

```bash
./bin/run-web-tool
```

* Run the web tool in two steps:

```bash
./bin/start-container
/bound/bin/run-r-scripts
```
