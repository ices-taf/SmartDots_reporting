# SmartDots Maturity Report template

## Description

This report is generated for **maturity staging exchanges and workshops**, either run internally or internationally coordinated via ICES/WGBIOP.


## Getting started

The settings for the report are contained in the [config.json](config.json) file, in the bootstrap/initial/data folder. Currently, the code requires direct access to the SmartDots database and so must be run by someone with the appropriate access.  To run the template:

```r
library(icesTAF)
# download data
taf.bootstrap(taf = TRUE)
# run analysis and create reports
sourceAll()
```

To change the configuration and run the project, the script `worksheet.R`
is provided to set up the config file when changeing eventID for example.


## Running the maturity report template

Typical report pulls can be done directly from the event page on the SmartDots web-API. The repository and code is available for development and debugging purposes. They can also be used and adapted for institute-specific SmartDots API derivatives that do not transit through ICES systems and databases. 


## Help

For help or clarification regarding the reports, contact Patrícia Gonçalves [Email](mailto:patricia@ipma.pt) or Côme Denechaud [Email](mailto:come.denechaud@hi.no).


## Authors

* Patrícia Gonçalves
* Côme Denechaud
* Colin Millar


