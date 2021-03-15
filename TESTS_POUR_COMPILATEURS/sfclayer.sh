#!/bin/bash
set -x
${1:-ftn} -c sfclayer_funcs.F90
${1:-ftn} -c sfclayer_mod.F90
