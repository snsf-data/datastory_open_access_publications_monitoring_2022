# Datastory: *OA monitoring 2022: strong increase and continued demand for article funding*

*By the end of 2022, the percentage of open access publications produced by SNSF-funded projects had risen to 81%. The SNSF funded several million francs for the publication of journal articles.*

[English](https://data.snf.ch/stories/open-access-publications-monitoring-2022-en.html)\
[German](https://data.snf.ch/stories/open-access-publikationen-monitoring-2022-de.html)\
[French](https://data.snf.ch/stories/publications-en-libre-acces-monitoring-2022-fr.html)

**Author(s)**: Tobias Philipp, Marlene Lanzerstorfer, Simon Gorin

**Publication date**: 13.11.2024

# Data description

The data used in this data story are available in the folder `data`. There is one dataset called `publications_2022_feb_2024.csv` and another one called `apc_2022_2023_jul_2024.csv`.

### `publications_2022_feb_2024.csv`

The data are a combination of:

-   scientific publications reported by SNSF-funded researchers and published in 2021 that are accessible via the [datasets section](https://data.snf.ch/datasets) of the SNSF Data Portal.

-   scientific publications retrieved from the [Crossref](https://www.crossref.org/) database and indicated as supported by the SNSF.

-   scientific publications retrieved from the [Dimensions](https://app.dimensions.ai/discover/publication) database and indicated as supported by the SNSF.

To determine the Open Access status of the publications present in the dataset, we queried the [Unpaywall API](https://unpaywall.org/products/api) using the DOI of the publications.

This final dataset contains information about 15267 publications related to the SNSF funding activities. Please note that 896 publications for which the Open Access status could not be determined are not included in the analysis reported in the links at the beginning of this readme. However, these publications remains available in the dataset. Each row corresponds to a single publication. Here is a description of the variables:

-   `doi`: the Digital Object Identifier of the publication.

-   `id`: the ID of the publication (each publication can have several IDs that can be parsed using `;`).

-   `title`: the title of the publication.

-   `publication_year`: the publication year of the publication.

-   `publication_date`: the publication date of the publication (YYYY-MM-DD).

-   `type`: the type of publication (a publication can have different types when different sources point to the same location; the types can be parsed using `,`).

-   `main_discipline_level1`: the research area (`Div 1` = Social sciences and humanities; `Div 2` = Mathematics, natural and engineering sciences; `Div 3` = Life sciences; `No SNSF discipline associated` = the publication was indicated as supported by the SNSF but could not be linked to an SNSF grant).

-   `snsf_grant_number`: the SNSF unique identifier of the grant associated to the publication (can be used in the [search grant](https://data.snf.ch/grants) of the SNSF Data Portal).

-   `source`: whether the information were retrieved via the SNSF Data Portal, Crossref, Dimension, or a combination of them (`DP` = Data portal and the source can be parsed using `,`).

-   `up_is_oa`: whether the publication is Open Access according to Unpaywall.

-   `up_journal_is_oa`: whether the publication journal is Open Access according to Unpaywall.

-   `up_journal_issns`: the publication journal ISSN number (retrieved via Unpaywall) (a journal can have several ISSN numbers that can be parsed using `,`).

-   `up_publisher`: the publication publisher (retrieved via Unpaywall).

-   `up_updated`: timestamp of when the information about the publication where updated in Unpaywall database (YYYY-MM-DD:TIME).

-   `up_year`: the publication year of the publication (retrieved via Unpaywall).

-   `up_oa_status`: the Open Access status of the publication (retrieved via Unpaywall).

-   `up_version`: the version of the publication (retrieved via Unpaywall).

-   `up_evidence`: the evidence used by Unpaywall to determine the Open Access status of the publication.

-   `up_host_type`: whether the Open Access version of the publication is hosted by the `publisher` or in a `repository` (retrieved via Unpaywall).

-   `up_license`: the license associated to the publication (retrieved via Unpaywall).

-   `oa_status`: the Open Access status of the publication, defined based on the different information. retrieved via Unpaywall and the SNSF regulations (`unknown`, `hybrid`, `green`, `gold`, `other OA`, or `closed`).

### `apc_2022_2023_jul_2024.csv`

The data consists of Article Processing Charges (APC) paid between 2022 and 2023. Here is a description of the variables:

-   `Title`: the title of the publication.

-   `Doi`: the Digital Object Identifier of the publication (when available).

-   `Publisher Name`: the name of the publisher of the publication.

-   `Journal Title`: the title of the journal that published the publication.

-   `License`: the license associated with the publication.

-   `Total Amount`: the total amount in CHF paid by the SNSF for APC.

-   `Payment Year`: the year when the payment took place.

-   `Payment Date`: the date when the payment took place.

-   `Institution grid id`: the grid id of the institution (when available) to which the author requesting the APC funding was affiliated.
