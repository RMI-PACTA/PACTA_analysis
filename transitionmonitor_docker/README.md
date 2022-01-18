# Description

The Dockerfile in this directory creates an image containing a freshly
cloned copy of PACTA_analysis and all the repositories it depends on.

The tree of the docker container looks like this:

``` {.bash}
/bound  # contents of PACTA_analysis
/create_interactive_report
/pacta-data
/r2dii.climate.stress.test
/r2dii.stress.test.data
```

# Notes

Note that PACTA_analysis and friends are not mounted but copied into the
container, so they will be frozen in the state they are when you build
the image.

# Usage

You must have SSH authentication to your GitHub account setup to use this tool.

Before running the script, you will need to choose the tag that you
want to use for the release. You should use [semantic
versioning](https://semver.org), and you should choose a tag that
follows in sequence from previously existing tags in the PACTA_analysis 
and friends repos. You can see existing tags in the relevant repos
here:\
<https://github.com/2DegreesInvesting/PACTA_analysis/tags>\
<https://github.com/2DegreesInvesting/create_interactive_report/tags>\
<https://github.com/2DegreesInvesting/r2dii.climate.stress.test/tags>\
<https://github.com/2DegreesInvesting/r2dii.stress.test.data/tags>\
<https://github.com/2DegreesInvesting/pacta-data/tags>\

Run the build_with_tag.sh script, specifying a tag to assign to it.

``` {.bash}
./build_with_tag.sh -t 0.1.14
```

The default target platform architecture is "linux/x86_64", and that is **required** for the TM website, however, you may use the `-x` option to set an alternate target platform architecture explicitly. For example, you may desire to build for "linux/arm64" in order to do local testing on an M1 mac, which will run much faster than running tests through a "linux/x86_64" image through emulation. Example:

``` {.bash}
./build_with_tag.sh -t 0.1.14 -x "linux/arm64"
```

Additionally, by default `./build_with_tag.sh` will *not* export the built image to a `*.tar.gz` file. Saving/exporting the docker image is a time consuming process, so it may make more sense to build the image (which will then automatically be loaded into your local loaded docker images), then do some testing, and only if you're sure it's working properly then export it to a `*.tar.gz` file with (replace `<tag>` with the appropriate version number) `docker save 2dii_pacta:<tag> | gzip -q > '$image_tar_gz'`. However, if you know you want to export the built docker image, you can do it all at once by adding the `-s` option like so:

``` {.bash}
./build_with_tag.sh -t 0.1.14 -s
```

The script will:

- clone the repos locally, only copying the current version of the files
- build a `2dii_pacta:<tag>` and `2dii_pacta:latest` docker image (where `<tag>` is the tag you provided, e.g. `0.1.14`). The image builds from the Dockerfile in this directory, which will

  - use [`r-base:<version>`](https://hub.docker.com/_/r-base) as the base (where <version> is the R version specified in the Dockerfile)
  - install system dependencies
  - install latex and dependencies
  - install dependent TeX packages
  - install dependent R packages and system dependencies
  - copy in the freshly cloned repos
  - make some necessary permissions changes
  - set the `build_version` environment variable with the specified tag

If the build is successful, the new 2dii_pacta docker image will already be 
loaded in your local docker images.

#  Testing

There are two scripts in this directory to facilitate testing of the docker 
image: `run-like-constructiva-flags.sh` and `run-all-tests.sh`

# Releasing

To release a new version of the software, push all tags to GitHub. The
tag you provide will affect the image, the repository containing the
Dockerfile that builds the image (PACTA_analysis), and the pacta siblings repos. 
That consistent tag make the process reproducible.

To push the tags, start the container with something like

``` {.bash}
docker run --rm -ti -v "$HOME/.ssh":/root/.ssh 2dii_pacta:latest bash
```

This example starts an ephemeral container (`run --rm`) from the image
`2dii_pacta:latest`, and creates a volume that makes your .ssh key
available to the container (`-v "$HOME/.ssh":/root/.ssh`), which
you'll need to interact with GitHub. If instead of ssh you use https
protocol, you may omit the volume argument and provide your username and
password when prompted.

Once inside the container, you can push any tag as you would normally
push a branch `git push <remote> <tag>`, for example:

``` {.bash}
cd /bound
# If origin = https://github.com/2DegreesInvesting/PACTA_analysis
git push origin 0.1.14
```

# For the web

That shared docker image can be loaded into the new machine with, for example:

``` {.bash}
docker load --input 2dii_pacta_v0.1.14.tar.gz
```

The docker image can then be used as intended with a script such as...

``` {.bash}
portfolio_name="TestPortfolio"
userFolder="$(pwd)"/working_dir/TestPortfolio
resultsFolder="$(pwd)"/user_results/4

docker run -ti \
  --rm \
  --pull=never \
  --network none \
  --user 1000:1000 \
  --memory=8g \
  --memory-swappiness=0 \
  --mount type=bind,source=${userFolder},target=/bound/working_dir \
  --mount type=bind,readonly,source=${resultsFolder},target=/user_results \
  2dii_pacta:latest \
  /bound/bin/run-r-scripts "$portfolio_name"
```

where you set `userFolder` to the path to the directory that contains
the user specific portfolio info on the server (typical PACTA output directory 
structure), and you set `resultsFolder` to the path to the directory that 
contains the survey (and other) results that are relevant to the specific user 
on the server. Those directories will then be mounted inside of the docker
container in the appropriate locations.
