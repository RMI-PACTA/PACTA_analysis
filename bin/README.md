## Running the web tool on transitionmonitor's docker-container

Ensure your working directory is PACTA_analysis, then run one of these workflows:

* Run the web tool in a single step

```bash
# Use the default portfolio name: TestPortfolio_Input
./bin/run-web-tool

# Or use a specific portfolio name
./bin/run-web-tool TestPortfolio_Input
```

* Run the web tool in two steps:

```bash
./bin/start-container

# Use the default portfolio name: TestPortfolio_Input
/bound/bin/run-r-scripts

# Or use a specific portfolio name
/bound/bin/run-r-scripts TestPortfolio_Input
```
