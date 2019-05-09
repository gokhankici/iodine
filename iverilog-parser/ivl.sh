#!/bin/zsh

if [[ $# -eq 0 ]]; then
    echo "usage: ${0:t} [args to ivl ... ] <verilog file>"
    exit 1
fi

FILENAME="${@[-1]}"
shift -p 1

if [[ ! -f $FILENAME ]]; then
    echo "'$FILENAME' is not a regular file"
    exit 1
fi

THIS_DIR=${0:A:h}
IVL=$THIS_DIR/ivl
IVLPP=$THIS_DIR/ivlpp/ivlpp

# ##############################################################################
# Preprocessing 
# ##############################################################################

PREPROC_OUT=$(mktemp /tmp/ivl-preproc-out.XXXXXX)

if [[ -z "$PREPROC_OUT" ]]; then
	echo "preproc output filename is empty, aborting ..." >&2
	exit 1
fi

$IVLPP "$FILENAME" > "$PREPROC_OUT"
last_err=$?

if [[ $last_err -ne 0 ]]; then
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo "!!! ERROR WHILE PREPROCESSING !!!"
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
	cat "$PREPROC_OUT"
	rm -f "$PREPROC_OUT"
    exit 1
fi

# ##############################################################################
# Parsing
# ##############################################################################

PARSE_OUT=$(mktemp /tmp/ivl-parse-out.XXXXXX)

if [[ -z "$PARSE_OUT" ]]; then
	echo "parse output filename is empty, aborting ..." >&2
	exit 1
fi

$IVL $@ -O "$PARSE_OUT" "$PREPROC_OUT"
last_err=$?

if [[ $last_err -ne 0 ]]; then
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo "!!! ERROR WHILE PARSING !!!"
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo "partial output:"
    cat "$PARSE_OUT"

	echo
	echo
	echo "################################################################################"
	echo "################################################################################"
	echo "################################################################################"
	if [[ -x "$(command -v xclip)" ]]; then
		echo "$IVL $@ -O \"$PARSE_OUT\" \"$PREPROC_OUT\"" | head -c -1 | xclip -selection c
		echo "copied last cmd to clipboard"
	else
		echo "cmd: $IVL $@ -O \"$PARSE_OUT\" \"$PREPROC_OUT\""
	fi

	# rm -f "$PREPROC_OUT" "$PARSE_OUT"
    exit $last_err
fi

# ##############################################################################
# Bodged annotation support
# ##############################################################################

(
echo
echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
echo
echo "% annotations"
echo

sed -n --posix 's|^[ \t]*//[ \t]*@annot{\(.*\)}[^}]*|\1.|p' $FILENAME
) >> "$PARSE_OUT"

cat "$PARSE_OUT"
rm -f "$PREPROC_OUT" "$PARSE_OUT"

