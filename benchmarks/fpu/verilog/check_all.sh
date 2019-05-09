#!/bin/zsh

function blue()  { print -P "%F{blue}%B$1%b%f" }
function green() { print -P "%F{green}%B$1%b%f" }
function red()   { print -P "%F{red}%B$1%b%f" }

MODULES=( pre_norm pre_norm_fmul add_sub27 mul_r2 div_r2 post_norm )

for m in ${MODULES[@]}; do
	f="${m}.v"
	if [[ ! -f "$f" ]]; then
		red "$f does not exists, aborting ..."
		exit 1
	fi

	blue "################################################################################"
	blue "testing $f"
	blue "################################################################################"

	verylog -M $m $f $@

	if [[ $? -ne 0 ]]; then
		red "running $f failed !"
		exit 1
	fi
done

green "################################################################################"
green "ALL TESTS PASSED !!!"
green "################################################################################"

