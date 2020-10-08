## Running the web tool on transitionmonitor's docker-container

Ensure your working directory is PACTA_analysis, then run one of these workflows:

* Run the web tool in a single step

```bash
./bin/run-web-tool.sh
```

* Run the web tool in two steps:

```bash
./bin/start-container.sh
/bound/bin/run-r-scripts.sh
```
