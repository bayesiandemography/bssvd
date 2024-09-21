

.PHONY: all
all: ../data/asfr_subset.rda \
     ../data/oecd_lfp_subset.rda \
     documentation


../data/asfr_subset.rda: hfd/asfr_subset.R \
  hfd/asfrRR.txt.zip
	Rscript $^ $@

../data/oecd_lfp_subset.rda: lfp/oecd_lfp_subset.R
	Rscript $^ $@


## update help files

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"


.PHONY: clean
clean:
	rm -rf ../data
	mkdir ../data
