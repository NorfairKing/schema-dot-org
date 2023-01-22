# Schema.org in Haskell

This is a library to deal with data in the form of the schemas from [schema.org](https://schema.org).

## Library design

### Code generation

[schema.org](https://schema.org) contains schemas about all sorts of data.
The schemas are all machine-readable, great.

However, there are a LOT of schemas; more than 2000!
This means that we must be careful with generating code, otherwise we might end up with a library that takes ages to compile.

We have a few options:

* Generate as little code per schema as possible.
  This will be a trade-off.
* Only generate code for _some_ schemas.
  [Google publishes which data they support](https://developers.google.com/search/docs/appearance/structured-data/search-gallery), so those schemas could be a good choice.
* Have users generate the library with exactly the schemas they need.
  This works for applications but not for libraries.


### Usage

Schemas are most commonly used in microdata/RDFa, and JSON-LD.
Two types of users could be using this library: Consumers and producers.

Webmasters could be producers, for example, when they annotate web pages with structured data in one of these forms.
Producers want to be as strict as possible with what they produce.
For example, producers want to make sure that dates are in the exact right format every time.

A web scraper could be a consumer, for example, when they extract structured data in one of these forms from web pages.
Consumers want to be as lenient as possible with what they can consume.
For example, consumers don't want to fail to consume entirely if a date is spelled wrong.
