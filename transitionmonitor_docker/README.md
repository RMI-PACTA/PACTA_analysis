# Description

The Dockerfile in this directory creates an image containing a freshly
cloned copy of PACTA_analysis and all the repositories it depends on.

The tree of the docker container looks like this:

``` {.bash}
/bound  # contents of PACTA_analysis
/pacta-data
/create_interactive_report
# ... more siblings of PACTA_analysis
```

# Notes

Note that PACTA_analysis and friends are not mounted but copied into the
container, so they will be frozen in the state they are when you build
the image.

# Usage

You must have SSH authentication to your GitHub account setup to use this tool.

Before running the script, you will need to remove any existing
2dii_pacta docker images that are loaded. You can do that, for instance,
with `docker rmi --force $(docker images -q '2dii_pacta' | uniq)`.

Also before running the script, you will need to choose the tag that you
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
./build_with_tag.sh 0.0.4
```

The script will:
- clone the repos locally, only copying the current version of the files
- build a "2dii_pacta:<tag>" and "2dii_pacta:latest" docker image (where <tag> is the tag you provided, e.g. 0.0.4). The image builds from the Dockerfile in this directory, which will
  - use 2dii/r-packages as a base
  - copy in the freshly cloned repos
  - make some necessary permissions changes
  - export the freshly made docker image gzipped (2dii_pacta.tar.gz)

If the build is successful, load the image interactively and push the
tags created for each of the PACTA_analysis and friends repos inside of
the docker container.

# Releasing

To release a new version of the software, push all tags to GitHub. The
tag you provide will affect the image, the repository containing the
dockerfile that builds the image, and the pacta siblings. That
consistent tag make the process reproducible.

To push the tags, start the container with something like

``` {.bash}
cd ~
docker run --rm -ti -v "$(pwd)/.ssh":/root/.ssh 2dii_pacta:latest
```

This example starts an ephemeral container (`run --rm`) from the image
`2dii_pacta:latest`, and creates a volume that makes your .ssh key
available to the container (`-v "$(pwd)/.ssh":/bound/.ssh`), which
you'll need to interact with GitHub. If instead of ssh you use https
protocol, you may omit the volume argument and provide your username and
password when prompted.

Once inside the container, you can push any tag as you would normally
push a branch `git push <remote> <tag>`, for example:

``` {.bash}
cd /bound
# If origin = https://github.com/2DegreesInvesting/PACTA_analysis
git push origin 0.0.4
```

# For the web

That shared docker image can be loaded into the new machine with...

``` {.bash}
docker load --input 2dii_pacta.tar.gz
```

The docker image can then be used as intended with a script such as...

``` {.bash}
portfolio_name="TestPortfolio"
working_dir="$(pwd)"/working_dir
user_results="$(pwd)"/user_results

docker run --rm -ti \
  --network none \
  --user 1000:1000 \
  --memory="4g" \
  --mount type=bind,source="$working_dir",target=/bound/working_dir \
  --mount type=bind,source="$user_results",target=/user_results \
  2dii_pacta \
  /bound/bin/run-r-scripts "$portfolio_name"
```

where you set `working_dir` to the path to the directory that contains
the user specific portfolio info on the server, and you set
`user_results` to the path to the directory that contains the survey
(and other) results that are relevant to the specific user on the
server. Those directories will then be mounted inside of the docker
container in the appropriate locations.
