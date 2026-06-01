

.PHONY: all
all: ../data/asfr_subset.rda \
     ../data/country_code.rda \
     ../data/oecd_lfp_subset.rda \
     ../data/un_csa_subset.rda \
     documentation


../data/asfr_subset.rda: hfd/asfr_subset.R \
  hfd/asfrRR.txt.zip
	Rscript $^ $@

../data/country_code.rda: concordances/country_code.R \
  concordances/un_m49.csv
	Rscript $^ $@

../data/oecd_lfp_subset.rda: lfp/oecd_lfp_subset.R
	Rscript $^ $@

../data/un_csa_subset.rda: csa/un_csa_subset.R \
  csa/UNdata_Export_20251022_221132488.csv.zip
	Rscript $^ $@



## update help files

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"


.PHONY: clean
clean:
	rm -rf ../data
	mkdir ../data
