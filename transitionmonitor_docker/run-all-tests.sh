#! /bin/bash

usage() {
  echo "Usage: $0" 1>&2
  echo "Optional flags:" 1>&2
  echo "[-t <docker image tag>] (default latest)" 1>&2
  # x for architecture?
  echo "[-x <target platform>] (default linux/x86_64)" 1>&2
  exit 1;
}

while getopts t:x: flag
do
    case "${flag}" in
        t) dockertag=${OPTARG};;
        x) target_platform=${OPTARG};;
        *) usage;;
    esac
done

if [ -z "${dockertag}" ]; then
    dockertag="latest"
fi

if [ -z "${target_platform}" ]; then
    target_platform="linux/x86_64"
fi

set -e

# https://unix.stackexchange.com/a/86724
for path in working_dirs/*/ ; do
    [ -L "${path%/}" ] && continue
    portfolioid=$(basename "${path}")
    echo ""
    echo "#### $portfolioid ####"
    echo ""
    ./run-like-constructiva-flags.sh -t ${dockertag} -p ${portfolioid} -x ${target_platform}
done

exit 0
