% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openFDA.R
\name{openFDA}
\alias{openFDA}
\title{Send requests to the openFDA API}
\usage{
openFDA(
  search = "",
  sort = NULL,
  count = NULL,
  limit = 1000,
  skip = NULL,
  endpoint = "drug-drugsfda",
  api_key = get_api_key(),
  paging = rlang::peek_option("openFDA.paging"),
  warn_on_http_error = TRUE
)
}
\arguments{
\item{search}{A character vector which will be passed to
\code{format_search_term()}. If search is \code{""} (the default), openFDA will
retrieve all records with no filtering. An error will be thrown if any
elements of \code{search} are missing (\code{NA}).}

\item{sort}{A single string or scalar named character vector describing how
to sort the results. The \code{sort} term should either be formatted as
\code{"[FIELD]:[asc/desc]"}, or \code{c("[FIELD]" = "[asc/desc]")}. For example, to
sort results in the Drugs@FDA endpoint by ascending submission status
dates. If values other than \code{"asc"}, or \code{"desc"} are supplied, the function
will throw an error.}

\item{count}{A single string denoting a field on which to count results. If
\code{NULL} (the default), results will be returned in full to the user. Specify
this parameter if you want to count occurrences of results within your
search term - go to \url{https://open.fda.gov/apis/query-syntax/} for more
information.}

\item{limit}{A single integerish value describing the limit on the number of
records to retrieve. An error will be thrown if \code{limit} is more than \code{1000}
(the default).}

\item{skip}{A single integer describing how many records should be skipped.
If more records are skipped than are found in your search, the openFDA
API will return a 404 error.}

\item{endpoint}{A single-length character vector describing which openFDA
endpoint to target.
\itemize{
\item \code{"animalandveterinary-event"}: Adverse event reports for
animal/veterinary drugs.
\item \code{"drug-event"}: Adverse event reports from \href{https://open.fda.gov/data/faers/}{FDA Adverse Event Reporting System}
\item \code{"drug-label"}: Drug documentation in the \href{https://www.fda.gov/industry/fda-data-standards-advisory-board/structured-product-labeling-resources}{Structured Product Labeling}
(SPL) format.
\item \code{"drug-ndc"}: Data from the \href{https://open.fda.gov/data/ndc/}{National Drug Code Directory} (NDC).
\item \code{"drug-enforcement"}: Data from the \href{https://open.fda.gov/data/res/}{FDA Recall Enterprise System} about drug products.
\item \code{"drug-drugsfda"}: Data on products approved for human use by the FDA
since 1939, with mostly complete data after 1998.
\item \code{"drug-shortages"}: Data on
\href{https://open.fda.gov/data/drugshortages/}{drug shortages} as compiled by
the FDA.
\item \code{"device-510k"}: Data from 510(k) submissions to the FDA regarding
medical devices.
\item \code{"device-classification"}: Data from the FDA \href{https://open.fda.gov/data/product-classification/}{Product Classification Database} for medical
devices.
\item \code{"device-enforcement"}: Data from the \href{https://open.fda.gov/data/res/}{FDA Recall Enterprise System} (RES) for medical devices.
\item \code{"device-event"}: Data on adverse events associated with FDA-approved
medical devices.
\item \code{"device-pma"}: Pre-market approval data for medical devices submitted to
the FDA.
\item \code{"device-recall"}: Data on product recalls for medical devices which
violate FDA law.
\item \code{"device-registrationlisting"}: Data on FDA \href{https://www.fda.gov/medical-devices/how-study-and-market-your-device/device-registration-and-listing}{Device Registrations and Listings}.
\item \code{"device-covid19serology"}: Data from \href{https://open.fda.gov/apis/device/covid19serology/}{independent evaluations of COVID-19 serological tests}.
\item \code{"device-udi"}: Data from the FDA's Global Unique Device Identification
Database (GUDID).
\item \code{"food-enforcement"}: Data from the \href{https://open.fda.gov/data/res/}{FDA Recall Enterprise System} for food products.
\item \code{"food-event"}: Data from the \href{https://open.fda.gov/data/caers/}{Center for Food Safety and Applied Nutrition Adverse Event Reporting System}.
\item \code{"other-historicaldocument"}: The majority of FDA Press Releases, from
1913 to 2014.
\item \code{"other-nsde"}: Data from the \href{https://www.fda.gov/industry/structured-product-labeling-resources/nsde}{National Drug Code Structured Product Labeling Data Elements}.
\item \code{"other-substance"}: Data regarding substances - individual molecules,
proteins, nucleic acids, and more.
\item \code{"other-unii"}: Data from the \href{https://www.fda.gov/industry/fda-data-standards-advisory-board/fdas-global-substance-registration-system}{FDA's Global Substance Registration System}.
\item \code{"tobacco-problem"}: Data on problems (e.g. damage, defects,
contamination, bad smell) with tobacco products.
}

This argument is case-sensitive. By default, the package will target the
Drugs@FDA endpoint (\code{"drugs-drugsfda"}).}

\item{api_key}{A single-length character vector with your openFDA API key.
By default this is the result of \code{get_api_key()}. If \code{api_key} is an empty
string, an error will be thrown.}

\item{paging}{A single string describing whether results should be paged,
and whether relevant alerts should be printed to the console. By default,
\code{openFDA()} uses the string stored in \code{options("openFDA.paging")}, which
has the value \code{"ask"}. Permissible values include:
\itemize{
\item \code{"ask"} - \code{openFDA()} will warn you that pagination is required and ask
if you want this to be done. Depending on user input, either a single
\code{httr2} response object or a list of \code{httr2} response objects will be
returned
\item \code{"yes"} - information on pagination will be printed to the console and
done automatically; a list of responses will be returned
\item \code{"no"} - information on pagination will be printed to the console but
will not be performed; only the first response will be returned
\item \code{"yes-quiet"} - same as \code{paging = "yes"} except no alert is printed to
the console
\item \code{"no-quiet"} - same as \code{paging = "no"} except no alert is printed to
the console
}}

\item{warn_on_http_error}{A scalar logical value. If \code{TRUE} (the default),
common openFDA HTTP errors will cause explanatory warnings to be printed
If \code{FALSE}, the underlying httr2 response object will be returned with
no extra warnings.}
}
\value{
Either an \code{httr2} response object from \code{\link[httr2:req_perform]{httr2::req_perform()}} or list
of these objects, depending on whether pagination was required. You can use
\code{\link[httr2:resp_body_raw]{httr2::resp_body_json()}} to extract JSON data from these responses.
}
\description{
Send requests to the openFDA API
}
\examples{
if (httr2::secret_has_key("OPENFDA_KEY")) {
  set_api_key(httr2::secret_decrypt(
    "TEaDtqdFMq9_Montij5p9IY6T57IyqkbF8IYFVOpk-ttxotFUNdJSxgccAnkq4nQhplaf-r3deQ",
    "OPENFDA_KEY"
  ))

  resp <- openFDA(search = "openfda.manufacturer_name:gilead*",
                  limit = 2,
                  skip = 10,
                  paging = "no-quiet")

  # The function returns an `httr2` object
  print(resp)
}

# Bad inputs will cause informative errors - here, a bad API key is supplied
try(
  openFDA(search = "openfda.manufacturer_name:gilead*",
          api_key = "BAD_API_KEY",
          limit = 1)
)
}
\references{
Kass-Hout TA, Xu Z, Mohebbi M, Nelsen H, Baker A, LEvine J, Johansen E,
Bright RA. \strong{OpenFDA: an innovative platform providing access to a wealth of
FDA's publicly available data} \emph{J Am Med Inform Assoc} 2016,
\strong{23(3):596-600.} \doi{10.1093/jamia/ocv153}
}
\seealso{
\code{\link[=format_search_term]{format_search_term()}} documents how input \code{search} vectors are
converted to openFDA API searches. Check out the openFDA
\link[=openFDA_options]{options} documentation to see how to set paging behaviour
across multiple \code{openFDA()} calls.
}
