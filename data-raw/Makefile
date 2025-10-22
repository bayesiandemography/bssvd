

.PHONY: all
all: ../data/asfr_subset.rda \
     ../data/oecd_lfp_subset.rda \
     ../data/country_code.rda \
     documentation


../data/asfr_subset.rda: hfd/asfr_subset.R \
  hfd/asfrRR.txt.zip
	Rscript $^ $@

../data/oecd_lfp_subset.rda: lfp/oecd_lfp_subset.R
	Rscript $^ $@

../data/country_code.rda: concordances/country_code.R \
  concordances/un_m49.csv
	Rscript $^ $@

../data/marital_status.rda: concordances/marital_status.R \
  concordances/marital_status.csv
	Rscript $^ $@


## update help files

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"


.PHONY: clean
clean:
	rm -rf ../data
	mkdir ../data
