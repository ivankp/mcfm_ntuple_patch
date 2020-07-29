PDF = mcfm_ntuple_patch.pdf

TEX = pdflatex -interaction=batchmode -shell-escape
# BIB = bibtex -terse
BIB = biber -q

.ONESHELL:
SHELL = bash

.PHONY: all clean

all: $(PDF)

$(PDF): %.pdf: %.tex
	@md5() { md5sum $*.$$1 2> /dev/null; }
	for (( i=1, n=1; i<=n; ++i )); do
	  printf "\e[32;1m$$i\e[0m\n"
	  aux_md5=$$(md5 aux)
	  bcf_md5=$$(md5 bcf)
	  # run LaTeX
	  $(TEX) $* > /dev/null || { let i=0; break; }
	  # check if need to run multiple times
	  if (( i == 1 )) && ( # update bibliography
	    ls -1t $? | grep '\.bib$$' > /dev/null \
	    || [ "$$bcf_md5" != "$$(md5 bcf)" ]
	  ); then
	    printf "\e[32;1mbib\e[0m\n"
	    $(BIB) $* || { let i=0; break; }
	    (( ++n ))
	  elif [ "$$aux_md5" != "$$(md5 aux)" ]; then
	    # rerun if aux file updated
	    (( ++n ))
	  fi
	done
	if (( i == 0 )); then
	  printf "\e[31mCompilation failed\e[0m\n"
	  awk '/Warning/{a=1}/^$$/{a=0}a' $*.log \
	  | sed "s/.*Warning.*/\x1b[33m&\x1b[0m/"
	  awk '/^!/{a=1}/}$$/{print;a=0}a' $*.log \
	  | sed "s/^!.*/\x1b[31m&\x1b[0m/"
	fi

clean:
	@rm -rfv *.aux *.toc *.out *.log *.lof *.lot \
	  *.nav *.snm *.bbl *.blg *.bcf *.run.xml _minted-* \
	  $(PDF)
