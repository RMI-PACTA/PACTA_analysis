#! /bin/bash

usage() {
  echo "Usage: $0  -p <portfolio name string> -t <docker image tag>" 1>&2
  echo "Optional flags:" 1>&2
  echo "[-u <userId>] (default 4)" 1>&2
  # a for analysis
  echo "[-a <path to local PACTA_analysis repo>] (default docker internal)" 1>&2
  # d for data
  echo "[-d <path to local pacta-data repo>] (default docker internal)" 1>&2
  # c for create
  echo "[-c <path to local create_interactive_report repo>] (default docker internal)" 1>&2
  # s for stresss test
  echo "[-s <path to local r2dii.climate.stress.test repo>] (default docker internal)" 1>&2
  # E because d is already taken for pacta-data
  echo "[-e <path to local r2dii.stress.test.data repo>] (default docker internal)" 1>&2
  # r for run
  echo "[-r <container command>] (default /bound/bin/run-r-scripts "portfolio name string")" 1>&2
  # x for architecture?
  echo "[-x <target platform>] (default linux/x86_64)" 1>&2
  # v for verbose
  echo "[-v] (verbose mode)" 1>&2
  echo "[-i] run container in interactive, tty mode (docker run -it)" 1>&2
  exit 1;
}

while getopts p:t:u:a:d:c:d:s:e:r:x:vi flag
do
    case "${flag}" in
        u) userId=${OPTARG};;
        p) portfolioIdentifier=${OPTARG};;
        t) tag=${OPTARG};;
        a) pa_repo=${OPTARG};;
        d) data_repo=${OPTARG};;
        c) cir_repo=${OPTARG};;
        s) st_repo=${OPTARG};;
        e) stdata_repo=${OPTARG};;
        r) docker_command=${OPTARG};;
        x) target_platform=${OPTARG};;
        v) verbose=1;;
        i) interactive=1;;
        *) usage;;
    esac
done

if [ -z "${portfolioIdentifier}" ] || [ -z "${tag}" ]; then
    usage
fi

if [ -z "${userId}" ]; then
    userId="4"
fi

if [ -z "${target_platform}" ]; then
    target_platform="linux/x86_64"
fi

userFolder="$(pwd)"/working_dirs/"$portfolioIdentifier"
resultsFolder="$(pwd)"/user_results/"$userId"

if [ -n "${verbose}" ]; then
  echo userId="$userId"
  echo portfolioIdentifier="$portfolioIdentifier"
  echo tag="$tag"
  echo userFolder="$userFolder"
  echo resultsFolder="$resultsFolder"
  echo ""
fi

if [ -z "${docker_command}" ]; then
  docker_command="/bound/bin/run-r-scripts"
  docker_command_args="${portfolioIdentifier}"
fi

args=(
  "--rm"
  --platform $target_platform
  "--pull=never"
  --network none
  --user 1000:1000
  "--memory=8g"
  "--memory-swappiness=0"
)

if [ -n "${interactive}" ]; then
  args+=("-it")
fi

if [ -n "${pa_repo}" ]; then
  args+=(--mount "type=bind,source=${pa_repo},target=/bound")
fi

if [ -n "${data_repo}" ]; then
  args+=(--mount "type=bind,source=${data_repo},target=/pacta-data")
fi

if [ -n "${cir_repo}" ]; then
  args+=(--mount "type=bind,source=${cir_repo},target=/create_interactive_report")
fi

if [ -n "${st_repo}" ]; then
  args+=(--mount "type=bind,source=${st_repo},target=/r2dii.climate.stress.test")
fi

if [ -n "${stdata_repo}" ]; then
  args+=(--mount "type=bind,source=${stdata_repo},target=/r2dii.stress.test.data")
fi


args+=(
  --mount "type=bind,source=${userFolder},target=/bound/working_dir"
  --mount "type=bind,readonly,source=${resultsFolder},target=/user_results"
)
args+=("2dii_pacta:$tag")
args+=("${docker_command}")

echo Running Docker Container

if [ -n "${verbose}" ]; then
  for arg in "${args[@]}"; do
    echo "$arg"
  done
fi

# I can't finsd a better way to work around however docker is parsing
# arguments.
if [ -n "${docker_command_args}" ]; then
  docker run "${args[@]}" "${docker_command_args}"
else
  docker run "${args[@]}"
fi

echo "Done :-)"
