# Miletus_Buildingcatalogue
Transforming Data from [Field Desktop](https://github.com/dainst/idai-field) for the [Map of Miletus](https://geoserver.dainst.org/maps/5764) on [iDAI.geoserver](https://geoserver.dainst.org/).

This repository exists purely for the purpose of transparency. It contains scripts that reformat the output of the Building Catalogue (*Bauwerkskatalog*) from the Miletus Database in [Field Desktop](https://github.com/dainst/idai-field). 

The purpose is to transform information from the database and merge it with other spatial data. On the one hand, the building catalogue is used to produce a map guide for the [Map of Miletus](https://geoserver.dainst.org/maps/5764) housed on the DAI GeoNode ([iDAI.geoserver](https://geoserver.dainst.org/)) (`R/import_transform.R`). On the other hand, the dating of buildings and walls is reformatted and merged with a vector dataset to make the visualization of all reconstructions and plans possible in the same map (`R/duplicate_drawings.R`). 

Feel free to use this as a model for similar projects, though reformatting is tailored to the needs of the Miletus Excavation and will hardly fit other data models. 

## Dependencies / Packages

Required packages are `idaifieldR` [(GitHub)](https://github.com/lsteinmann/idaifieldR), `tidyverse`, `rgdal`, `sf`, `utf8`.

## Funding
This repository was produced during our work for the [Miletus Excavation](https://www.miletgrabung.uni-hamburg.de/) in the course of the DFG/ANR-funded project ["Life Forms in the Megapolis: Miletus in the Longue Durée"](https://www.kulturwissenschaften.uni-hamburg.de/ka/forschung/lebensformen-megapolis.html) headed by Christof Berns and Julien Zurbach. 

## License
The License in the repository only affects the script itself. The map and its layers (of which exports are included as data in this repository) are protected under the conditions of a [CC BY-NC-SA 4.0-License (Creative Commons Attribution-NonCommercial-ShareAlike 4.0)](https://creativecommons.org/licenses/by-nc-sa/4.0/). If you have any questions regarding the license, or require a specific permission for your intended purpose, please let us know at: [miletgrabung@uni-hamburg.de](mailto:miletgrabung@uni-hamburg.de). We would greatly appreciate any notification of publication or use of the map in any case.
