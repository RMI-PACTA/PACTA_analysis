#!/usr/bin/env bash
tmp=$(tempfile)
head -n 211 web_tool_script_2.R >> "$tmp"
Rscript --vanilla web_tool_script_1.R '${1:-TestPortfolio_Input}'
Rscript --vanilla "$tmp" '${1:-TestPortfolio_Input}'

