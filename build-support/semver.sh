#!/bin/sh
#
# If run inside a git repository will return a valid semver based on
# the semver formatted tags. For example if the current HEAD is tagged
# at 0.0.1, then the version echoed will simply be 0.0.1. However, if
# the tag is say, 3 patches behind, the tag will be in the form
# `0.0.1+build.3.0ace960`. This is basically, the current tag a
# monotonically increasing commit (the number of commits since the
# tag, and then a git short ref to identify the commit.
#
# You may also pass this script the a `release` argument. If that is
# the case it will exit with a non-zero value if the current head is
# not tagged.
#
set -e

commit_count=
version_candidate=
version_tag=
vsn=

get_version_candidate()
{
    version_regex='tag: (v([^,\)]+)|([0-9]+(\.[0-9]+)*))'
    tag_lines=$(git log --oneline --decorate | fgrep "tag: ")
    version_match=$(echo "$tag_lines" | perl -e "<> =~ /\($version_regex\)/ && print \${1}")

    if [ -n "$version_match" ]; then
        if [ $(echo "$version_match" | perl -e '<> =~ /(.)/ && print ${1}') = 'v' ]; then
            version_tag="$version_match"
            version_candidate=$(echo "$tag_lines" | perl -e "<> =~ /\($version_regex\)/ && print \${2}")
        else
            version_tag=$(echo "$tag_lines" | perl -e "<> =~ /\($version_regex\)/ && print \${3}")
            version_candidate="$version_tag"
        fi
    else
        version_tag=""
        version_candidate="0.0.0"
    fi
}

get_commit_count()
{
    if [ $version_tag = "" ]; then
        commit_count=$(git rev-list HEAD | wc -l)
    else
        commit_count=$(git rev-list ${version_tag}..HEAD | wc -l)
    fi
    commit_count=$(echo $commit_count | tr -d ' 't)
}

build_version()
{
    if [ $commit_count = 0 ]; then
        vsn=$version_candidate
    else
        local ref=$(git log -n 1 --pretty=format:'%h')
        vsn="${version_candidate}+build.${commit_count}.${ref}"
    fi
}

check_tag()
{
    if [ "$1" = "release" ]; then
        if [[ $commit_count != 0 ]]; then
            echo "The current head *must* be tagged"
            exit 127
        fi
    fi
}

get_version_candidate
get_commit_count
build_version
check_tag "$1"

echo $vsn
