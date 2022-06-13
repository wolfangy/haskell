# Chapter 3 Developing an application: Stock quotes

Chapter covers:

* Designing a standalone multi-module program with dependencies

* Dealing with dates, text and command-line args

* Parsing CSV file and plotting charts

* Employing type classes


## 3.1 Setting the scene

```mermaid
graph TD;
    Input(CSV file) --> Data(Data collection in memory)
    Data --> Chart(Chart - SVG file)
    Data --> Info(Statistical Information)
    Chart --> Report(Full report - HTML)
    Data --> Report
    Info --> Report
```

What we should do in this project:

* Process command-line arguments

* Read quote data from a CSV file

* Compute statistics

* Plot charts

* Prepare reports on statistical info in text and HTML

```mermaid
graph TD;
    Main([Main]) --> Data([QuoteData])
    Main --> Chars([Charts])
    Main --> SReport([StatReport])
    Main --> HReport([HtmlReport])
    Chars --> Data
    SReport --> Data
    HReport --> SReport
    HReport --> Data
    Main --> Params([Params])
```
