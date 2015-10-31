all: hw3.html

hw3.html: hw3.Rmd boroughs.json data.Rdata 
	Rscript -e "library(rmarkdown); render('hw3.Rmd')"
  
boroughs.json: model.R data.Rdata
	Rscript model.R
         
data.Rdata: geocode.R ~cr173/Sta523/data/nyc/intersections/intersections.shp /home/vis/cr173/Sta523/data/nyc/pluto/pluto.Rdata ~cr173/Sta523/data/nyc/nyc_311_index.sqlite
	Rscript geocode.R

clean:
	rm -f boroughs.json
	rm -f hw3.html

.PHONY: all clean

