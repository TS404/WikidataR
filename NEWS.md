# WikidataR 2.3.0
* New `disambiguate_QIDs()` function for manual item-by-item disambiguation

# WikidataR 2.2.1
* Outputs as tibbles by default with vector options where relevant

# WikidataR 2.2.0
* Additional 'website' output format option for `write_wikidata()` to submit to Quickstatements website
* Patches to make writing wikidata statements with dates more reliable

# WikidataR 2.1.5
* Updated release on CRAN
* Expected property values and constraints from 2.1.0 now just stored as a data file, since onAttach function caused issues

# WikidataR 2.1.0
* Upon loading the package, it will check whether property values are expected to be strings, numbers, dates or QIDs

# WikidataR 2.0.0
* `write_wikidata()` allows you to write to wikidata via the 'quickstatements' format
* Outputs as tibbles where relevant
* Switch maintatiner to Thomas Shafee

# WikidataR 1.4.0
* First release on CRAN!
* `extract_claims()` allows you to, well, extract claims.
* SPARQL syntax bug with some geo queries now fixed (thanks to Mikhail Popov)

# WikidataR 1.3.0
* Vectorisation of `get_*()` functions

# WikidataR 1.2.0
* Geographic data for entities that exist relative to other Wikidata items can now be retrieved
  with get_geo_entity and get_geo_box, courtesy of excellent Serena Signorelli's excellent
  QueryWikidataR package.
* A bug in printing returned objects is now fixed.

# WikidataR 1.1.0
* You can now retrieve multiple random properties or items with get_random_item and get_random_property

# WikidataR 1.0.1
* Various documentation and metadata improvements.

# WikidataR 1.0.0
* Fix a bug in `get_*()` functions due to a parameter name mismatch
* Print methods added by Christian Graul

# WikidataR 0.5.0
* This is the initial release! See the explanatory vignettes.
