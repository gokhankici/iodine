#!/bin/zsh

zparseopts -D -E -- \
	-linecount=LINECOUNT \
	-timing=TIMING

THIS_DIR=${0:A:h}
CONF_FILE=$THIS_DIR/../configuration.sh
source $CONF_FILE

BMKS_DIR="$IVL_DIR/benchmarks"
MIPS_DIR="$BMKS_DIR/472-mips-pipelined"
CRYPTO_DIR="$BMKS_DIR/crypto_cores"

FILES=( \
	"$MIPS_DIR/mips_pipeline.v" \
	"$BMKS_DIR/yarvi/shared/yarvi.v" \
	"$CRYPTO_DIR/sha_core/trunk/rtl/sha256.v" \
	"$BMKS_DIR/fpu/verilog/fpu.v" \
	"$BMKS_DIR/fpu2/divider/divider.v" \
	"$CRYPTO_DIR/RSA4096/ModExp2/ModExp.v"\
	)

NAMES=( \
	"mips"    \
	"yarvi"   \
	"sha256"  \
	"fpu"     \
	"fpu-div" \
	"modexp"  \
	)

N=10

if   [[ -n "$LINECOUNT" ]]; then
	for f in ${(@)FILES}; do
		echo '=============================='
		echo "    ${f:t}"
		echo '=============================='

		$THIS_DIR/line_count.sh "$f"


		echo '=============================='
		echo
	done
elif [[ -n "$TIMING" ]]; then
	for name in ${(@)NAMES}; do
		echo '=============================='
		echo "    ${name}"
		echo '=============================='

		$THIS_DIR/get_average.sh "$name" $N

		echo '=============================='
		echo
	done
else
	echo 'error'
	exit 1
fi
