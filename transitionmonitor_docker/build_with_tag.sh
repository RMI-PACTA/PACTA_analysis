#! /bin/bash

# Examples:
# # The tag is enforced
# build_with_tag 0.0.0.999
#
# # Optionally give the names of the repos as trailing arguments
# build_with_tag 0.0.0.999 PACTA_analysis r2dii.climate.stress.test
#
# # With the help of pacta-find, you don't need to know the names
# # of pacta siblings -- it's enough to know the path to the parent.
# pacta_siblings="$(basename $(pacta-find ~/git))"
# ./build_with_tag.sh 0.0.0.999 "$pacta_siblings"

red () {
    printf "\033[31m${1}\033[0m\n"
}

yellow () {
    printf "\033[33m${1}\033[0m\n"
}

green () {
    printf "\033[32m${1}\033[0m\n"
}

dir_start="$(pwd)"
dir_temp="$(mktemp -d)"
cleanup () {
  rm -rf $dir_temp
  cd $dir_start
}
trap cleanup EXIT

url="git@github.com:2DegreesInvesting/"
tag="$1"
repos="${@:2}"
if [ -z "$repos" ]
then
    repos="PACTA_analysis create_interactive_report r2dii.climate.stress.test r2dii.stress.test.data pacta-data"
fi

if [ -z "$tag" ]
then
    red "Please give a tag."
    exit 2
fi

ssh -T git@github.com
if [ $? -ne 1 ];
then
  red "You must have SSH authentication to GitHub setup properly to use this tool." && exit 1
fi

remotes="$(echo $repos | tr ' ' ',')"
remotes=$(eval "echo $url{$remotes}.git")
tags=""
for i in $remotes
do
    tags_i="$(git ls-remote --tags --ref $i | cut -d / -f 3)"
    tags="$tags $tags_i"
done

tags="$(echo $tags | tr ' ' '\n' | sort -V | uniq)"
for i in $tags
do
    if [ "$i" == "$tag" ]
    then
        red "Tag '$tag' is taken. Choose a new tag different from these ones:"
        red "$(echo $tags | tr ' ' '\n' | sort -V | uniq)" && exit 1
    fi
done
if [ -z "$tags" ]
then
    yellow "These remotes returned no tag:"
    yellow "$(echo $remotes | tr ' ' '\n')"
    yellow "Are your SSH keys unset?"
fi

if (! docker images > /dev/null 2>&1 )
then
  red "The docker daemon does not appear to be running." && exit 1
fi

existing_images="$(docker images -q '2dii_pacta' || exit 1)"
if [ -n "$existing_images" ]
then
    red "Existing docker images match '2dii_pacta':"
    docker images 2dii_pacta
    red "E.g.: Remove '2dii_pacta:latest' with: docker rmi 2dii_pacta:latest" && exit 1
fi

if [ "$dir_start" == "." ]
then
    dir_start="$(pwd)"
fi

wd="$(basename $dir_start)"
if [ ! "$wd" == "transitionmonitor_docker" ]
then
    red "Your current working directory is not 'transitionmonitor_docker': $dir_start" && exit 1
fi



cd $dir_temp
for repo in $repos
do
    remote="${url}${repo}.git"
    git clone -b master "$remote" --depth 1 || exit 2
    echo
done

# Tag and log
for repo in $repos
do
    green "Tagging $(basename $repo) with $tag"
    git -C "$repo" tag -a "$tag" -m "Release pacta $tag" HEAD || exit 2
    echo

    green "$(git -C $repo log --pretty='%h %d <%an> (%cr)' | head -n 1)"
    echo
done

# Copy Dockerfile alongside pacta siblings and build the image
cp "${dir_start}/Dockerfile" "$dir_temp"
docker build --tag 2dii_pacta:"$tag" --tag 2dii_pacta:latest .
echo

cd $dir_start



image_tar_gz="2dii_pacta_v$tag.tar.gz"
green "Saving 2dii_pacta into $image_tar_gz ..."
docker save 2dii_pacta | gzip -q > "$image_tar_gz"
echo

green "Done :-)"
exit 0
