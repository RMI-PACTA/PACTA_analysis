This Dockerfile extend the image 2diirunner, imported with:

```bash
docker image import https://tool.transitionmonitor.com/downloads/2diirunner_1.3.0.tar.xz 2diirunner:1.3.0
```

To build this image locally, change directory to this folder, then run:

```bash
docker build --tag 2diirunner-with-packages .
```

Reference: <https://ropenscilabs.github.io/r-docker-tutorial/04-Dockerhub.html>
