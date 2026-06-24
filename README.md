## Why I Built This

I've been involved with horse show organizations for more than 15 years, serving in a variety of leadership and volunteer roles. One challenge that seems to come up every year is tracking exhibitor eligibility for year-end awards.

Calculating points is only part of the process. Many organizations also have requirements related to membership status, minimum show participation, class eligibility, or other organization-specific rules. These requirements are often tracked manually and can be difficult for both exhibitors and show management to monitor throughout the season.

As a result, exhibitors sometimes discover at the end of the year that they don't meet a qualification requirement they assumed they had satisfied. Likewise, show secretaries and year-end awards committees can spend a significant amount of time validating standings and eligibility.

I built HorseShowTrackR to help address that problem.

The goal is to provide exhibitors with a clear view of their current standings while also tracking eligibility requirements throughout the season. By making that information visible early and often, exhibitors can make informed decisions before year-end awards are finalized.

## Data Source

HorseShowTrackR is designed specifically to work with exports from ShowPro horse show management software.

ShowPro provides text-based export files containing show results, exhibitor information, horse information, and related competition data. Python-based ETL processes import and standardize these files before loading them into a DuckDB database for analysis and reporting.

While the current implementation is built around ShowPro exports, the overall architecture could be adapted to support additional show management systems in the future.

## Key Capabilities

-   Track year-end point standings
-   Monitor award eligibility throughout the season
-   Validate membership requirements
-   Track minimum show attendance and participation requirements
-   Provide exhibitors with real-time insight into qualification status
-   Reduce administrative effort required for year-end awards processing
-   Improve transparency for exhibitors and organization leadership

## Application Architecture

``` mermaid
flowchart TD

    A[ShowPro Export Files<br/>TXT Format]

    A --> B[Python Import Scripts]

    B --> C[Standardize Exhibitor Data]
    B --> D[Standardize Horse Data]
    B --> E[Standardize Show Results]

    C --> F[(DuckDB)]
    D --> F
    E --> F

    F --> G[Points Engine]
    F --> H[Eligibility Engine]

    G --> I[R Shiny Dashboard]
    H --> I

    I --> J[Current Standings]
    I --> K[Qualification Status]
    I --> L[Membership Validation]
    I --> M[Participation Tracking]
```
