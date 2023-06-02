City of Goodeats: Commonalities and Trends Among Poor-Performing
Establishments (2012-2018)
================
Andrew Jaroncyk
2023-05-22

# Introduction

This report analyzes Average Inspection Scores of all Food
Establishments in the City of Goodeats between 2012 and 2018. By
protocol, every Food Establishment in the City of Goodeats must be
inspected at least once in a year. This first Annual Inspection is
called the *Initial Inspection*. Protocol is to then conduct periodic
re-inspections of an Establishment that fails an Initial or
re-Inspection until (1) the Establishment passes Inspection; (2) the
Inspector deems the results acceptable enough not to require further
re-inspection; or (3) the Establishment is shut down or is required to
undergo a Hearing.

For the purposes of this analysis, the definition of a “Poor
Performance” Food Establishment is defined as a Food Establishment
receiving a “C” Grade on a an alphabetic scale from A to C, where A is
the highest and C is the lowest. The Inspection Score determines what
Letter Grade a Food Establishment receives:

1)  **Grade A**: Inspection Score $\geq$ 94
2)  **Grade B**: Inspection Score $\geq$ 81 and Inspection Score $\leq$
    93
3)  **Grade C**: Inspection Score $\leq$ 80

In this analysis, 2012-2018 Average Inspection Scores will be examined
between all Food Establishments and “Poor Performing Food
Establishments” exclusively. The relationship of these Scores with
respect to what Food License Category and the Grade that is determined
based on their Inspection Score will be examined. Furthermore, the
Average Inspection Score over this timeframe with respect to the
Violation Level of a Violation will also be explored. To conclude, an
analysis of frequent Violations that occurred over this timeframe will
be conducted.

``` r
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

# R Libraries
library(reticulate)
```

    ## Warning: package 'reticulate' was built under R version 4.2.3

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.2.3

    ## Warning: package 'ggplot2' was built under R version 4.2.3

    ## Warning: package 'tibble' was built under R version 4.2.3

    ## Warning: package 'tidyr' was built under R version 4.2.3

    ## Warning: package 'readr' was built under R version 4.2.3

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## Warning: package 'forcats' was built under R version 4.2.3

    ## Warning: package 'lubridate' was built under R version 4.2.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(stats)
library(plyr)
```

    ## ------------------------------------------------------------------------------
    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)
    ## ------------------------------------------------------------------------------
    ## 
    ## Attaching package: 'plyr'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(ggplot2)
library(ggthemes)
library(ggpubr)
```

    ## Warning: package 'ggpubr' was built under R version 4.2.3

    ## 
    ## Attaching package: 'ggpubr'
    ## 
    ## The following object is masked from 'package:plyr':
    ## 
    ##     mutate

``` r
library(stringr)
library(broom)
```

    ## Warning: package 'broom' was built under R version 4.2.3

``` r
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(gtsummary)
```

    ## Warning: package 'gtsummary' was built under R version 4.2.3

    ## #BlackLivesMatter
    ## 
    ## Attaching package: 'gtsummary'
    ## 
    ## The following object is masked from 'package:plyr':
    ## 
    ##     mutate

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.2.3

    ## Loading required package: lattice

    ## Warning: package 'lattice' was built under R version 4.2.3

    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(dynlm)
```

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 4.2.3

    ## 
    ## Attaching package: 'zoo'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(forecast)
```

    ## Warning: package 'forecast' was built under R version 4.2.3

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo 
    ## 
    ## Attaching package: 'forecast'
    ## 
    ## The following object is masked from 'package:ggpubr':
    ## 
    ##     gghistogram

``` r
library(readr)
library(magrittr)
```

    ## 
    ## Attaching package: 'magrittr'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
library(tibble)
library(lubridate)
library(Hmisc)
```

    ## Warning: package 'Hmisc' was built under R version 4.2.3

    ## 
    ## Attaching package: 'Hmisc'
    ## 
    ## The following objects are masked from 'package:plyr':
    ## 
    ##     is.discrete, summarize
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     src, summarize
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

``` r
library(GGally)
```

    ## Warning: package 'GGally' was built under R version 4.2.3

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
library(ggiraph)
```

    ## Warning: package 'ggiraph' was built under R version 4.2.3

``` r
library(cowplot)
```

    ## 
    ## Attaching package: 'cowplot'
    ## 
    ## The following object is masked from 'package:ggpubr':
    ## 
    ##     get_legend
    ## 
    ## The following object is masked from 'package:ggthemes':
    ## 
    ##     theme_map
    ## 
    ## The following object is masked from 'package:lubridate':
    ## 
    ##     stamp

``` r
library(glue)
library(htmltools)
```

    ## Warning: package 'htmltools' was built under R version 4.2.3

``` r
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'
    ## 
    ## The following object is masked from 'package:Hmisc':
    ## 
    ##     subplot
    ## 
    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, mutate, rename, summarise
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter
    ## 
    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(parallelly)
```

    ## Warning: package 'parallelly' was built under R version 4.2.3

``` r
library(parallel)
library(future.apply)
```

    ## Warning: package 'future.apply' was built under R version 4.2.3

    ## Loading required package: future

    ## Warning: package 'future' was built under R version 4.2.3

    ## 
    ## Attaching package: 'future'
    ## 
    ## The following object is masked from 'package:caret':
    ## 
    ##     cluster

``` r
library(skimr)
```

    ## Warning: package 'skimr' was built under R version 4.2.3

``` r
library(foreach)
```

    ## 
    ## Attaching package: 'foreach'
    ## 
    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when

``` r
library(doParallel)
```

    ## Loading required package: iterators

``` r
library(parallelMap)
library(patchwork)
```

    ## 
    ## Attaching package: 'patchwork'
    ## 
    ## The following object is masked from 'package:cowplot':
    ## 
    ##     align_plots

``` r
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(colorspace)
```

    ## Warning: package 'colorspace' was built under R version 4.2.3

``` r
# Load the Raw Data Source
FoodEstablishmentViolations <- read_csv("food+establishment+violations.csv")
```

    ## Rows: 324898 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (13): ISSDTTM, EXPDTTM, LICSTATUS, LICENSECAT, DESCRIPT, RESULT, RESULTD...
    ## dbl  (2): LICENSENO, PROPERTY_ID
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
FoodEstablishmentGrades <- read_csv("food+establishment+grades.csv")
```

    ## Rows: 72243 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (8): ISSDTTM, EXPDTTM, LICSTATUS, LICENSECAT, DESCRIPT, RESULT, RESULTDT...
    ## dbl (3): LICENSENO, SUM_VIOLATIONS, SCORE
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Transform `FoodEstablishmentViolations` Dataset into Cleaned Dataset
FoodEstablishmentViolationsClean <- FoodEstablishmentViolations %>%
    mutate(
      LICENSENO = as.character(LICENSENO),
      PROPERTY_ID = as.character(PROPERTY_ID),
      across(contains("DTTM"), ~as.POSIXct(., format = "%m/%d/%Y %H:%M")),
      VIOLLEVEL = case_when(
        VIOLLEVEL == '1919' ~ "",
        VIOLLEVEL == "*" ~ "Non-Critical Violation (Least Severe)",
        VIOLLEVEL == "**" ~ "Critical Violation",
        VIOLLEVEL == "***" ~ "Foodborne Critical Violation (Most Severe)"
      ),
      MonthYear = floor_date(RESULTDTTM, unit = "month")
    ) %>%
    mutate(MonthYear = make_datetime(year(MonthYear), month(MonthYear)))


# Transform `FoodEstablishmentGrades` Dataset into Cleaned Dataset
FoodEstablishmentGradesClean <- FoodEstablishmentGrades %>%
    mutate(
      LICENSENO = as.character(LICENSENO),
      across(contains("DTTM"), ~as.POSIXct(., format = "%m/%d/%Y %H:%M")),
      MonthYear = floor_date(RESULTDTTM, unit = "month")
    ) %>%
    mutate(MonthYear = make_datetime(year(MonthYear), month(MonthYear)
    ))


# Merge the cleaned Violations and Grades Datasets
JoinedData <- inner_join(FoodEstablishmentGradesClean, FoodEstablishmentViolationsClean, by = "LICENSENO")
```

    ## Warning in inner_join(FoodEstablishmentGradesClean, FoodEstablishmentViolationsClean, : Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 213 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
PoorPerformerData <- JoinedData %>%
  filter(GRADE == "C")
```

# Analysis

## Insight 1: Average Inspection Scores

For **Grade A** Establishments, the average Inspection Score from 2012
to 2018 is $99.0$. Across *all* Licensce Categories, the average
Inspection Score for these Establishments exceed 98.5. Mobile Food
Trucks or Vendors (MFWs) tended to score the highest over this
timeframe, and Retail Food Establishments (RF) scored the lowest, but by
less than $0.5$ points than the other two Licensce Categories, Eating &
Drinking Establishments (FS; i.e. Restaurants) with and without Take-Out
Service (FT for Restaurants with Take-Out service).

For **Grade B** Establishments, the average Inspection Score from 2012
to 2018 is $87.1$. Similar to Grade A Establishments, there is very
little variability in the scores for these License Categories. MFW
Licenses scored the highest over this timeframe on average, and FS
Licenses scored the lowest over this timeframe, but only $0.2$ points
lower than the next-lowest License Level, FT.

For **Grade C** Establishments, the average Inspection Score from 2012
to 2018 is $58.3$. There is some noticeable variability in the scores
over this timeframe. Most of the variability in the average Inspection
Score over this time frame is associated with MFW Licenses, where it
ranged from approximately $36$ to approximately $71$. This $35$ point
range in this License Category alone saw a major dip in 2014, falling
$39$% between 2013 and 2014 from $59$ to $36$. It then sharply rose to
$67$ in 2015, an $86$% increase. Across all License Categories between
2012 and 2018, there is a general positive trend in the average
Inspection Score, suggesting that Inspections are effective in improving
the conditions of these poorer performing Establishments.

When segmenting the Average Inspection Score through Violation Levels
(Non-Critical, Critical, Foodborne Critical) for Poor Performers over
this timeframe, the following insights can be seen:

- For **Non-Critical Violations**, there is a general, increasing trend
  in the Average Inspection Score across all License Categories in
  poor-performing Food Establishments. Despite the positive progress
  overall, this is an opportunity for improvement for the City of
  Goodeats to proactively identify and resolve any Non-Critical
  Violations that are preventing certain Food Establishments from
  attaining a higher grade. Average Food Inspection Scores for these
  Violation Levels are around $55$ to $65$ for the most part, but
  License Categories such as MFW and RF see Inspection Scores approach
  and even exceed $70$, showing that improvements by these Food
  Establishments are being made. However, Food Services with and without
  Take-Out options over this six year timeframe have not seen as
  dramatic increases in their Inspection Scores and tend to be in the
  $50$ to $60$ range, showing that there is significant areas for
  improvement in these Food Establishments to make adjustments similar
  to MFW and RF Food Establishments.

- For **Critical Violations**, there is a general, increasing trend in
  the Average Inspection Score across all License Categories in
  poor-performing Food Establishments, which is cause for concern.
  Despite this overall trends, improvements can be made to have these
  Average Inspection Scores in the future increase towards levels closer
  to Grades A and B. MFW Licenses see the largest variability in their
  Average Inspection Scores, with Average Inspection Scores here going
  as low as $40$ as seen in 2014 and as high as $75$ in 2017. All
  License Types in this Violation Level experienced a decrease in their
  Average Inspection Score in 2014 but wiped it away with a positive
  Average Score increase in 2015.

- For **Foodborne Critical Violations**, there is a general, increasing
  trend in the Average Inspection Score across all License Categories in
  poor-performing Food Establishments. Despite the overall positive
  progress over the six years, there is always room for improvement,
  especially with Food Services with Take-Out. Since 2015, FT Licenses
  have had an Average Inspection Score of $65$, and this plateau
  suggests that a possible re-visitation of these Violations for these
  particular License Categories may be necessary. Specifically,
  Foodborne Critical Violations should be revised and structured in a
  way for FT Licenses to make the appropriate adjustments to become in
  compliance and thus raise their Average Inspection Score. In addition,
  the variability found in MFW Licenses again is very significant, where
  it appears in the last couple of years that the Average Inspection
  Score has leveled off. However, reducing this variability to an
  acceptable range, such as that found in RF Licenses, gives MFW
  Licenses opportunities to avoid having Foodborne Critical Violations
  and thus increase their Average Inspection Score.

- For **No Violation Level**, there is a general, increasing trend in
  the Average Inspection Score across all Licence Categories in
  poor-performing Food Establishments. All License Categories rebounded
  nicely from 2014, especially MFW Licenses, and since 2014 have been in
  the $60$ to $70$ range, showing that the Inspection Process in 2014
  increased the number of areas where these Food Establishments are in
  compliance with local laws and ordinances.

``` r
# Line Chart: Average Inspection Score by Year, Faceted by Grade and Grouped by License Category, 2012 - 2018
(JoinedData %>%
  mutate(Year = as.numeric(format(RESULTDTTM.x, "%Y"))) %>%
  group_by(Year, LICENSECAT.x, GRADE) %>%
  summarise(AverageInspectionScore = mean(SCORE, na.rm = TRUE),
            PassRate = (mean(VIOLSTATUS == "Pass", na.rm = TRUE)),
            FailRate = 1 - PassRate
            ) %>%
  mutate(PassRate = percent(PassRate),
         FailRate = percent(FailRate)
         ) %>%
  ggplot(aes(x = Year, y = AverageInspectionScore, color = LICENSECAT.x)) + 
  geom_line() + 
  facet_wrap(~GRADE) + 
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) + 
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Average Inspection Score by Year",
       subtitle = "2012-2018",
       x = "Year",
       y = "Average Inspection Score",
       color = "License Category") +
  theme(legend.position = "bottom",
        axis.title.x = element_text(),
        axis.title.y = element_text(),
        panel.spacing = unit(2, "lines")) + 
  theme_bw()
)
```

![](Mass-EOTTS-Data-Challenge_files/figure-gfm/Average%20Inspection%20Score%20Line%20Charts-1.png)<!-- -->

``` r
# Line Chart: Average Inspection Score by Year for Poor Performers, Faceted by Violation Level and Grouped by License Category, 2012 - 2018
(PoorPerformerData %>%
  mutate(Year = as.numeric(format(RESULTDTTM.x, "%Y"))) %>%
  group_by(Year, LICENSECAT.x, VIOLLEVEL) %>%
  summarise(AverageInspectionScore = mean(SCORE, na.rm = TRUE),
            PassRate = (mean(VIOLSTATUS == "Pass", na.rm = TRUE)),
            FailRate = 1 - PassRate
            ) %>%
  mutate(PassRate = percent(PassRate),
         FailRate = percent(FailRate)
         ) %>%
  ggplot(aes(x = Year, y = AverageInspectionScore, color = LICENSECAT.x)) + 
  geom_line() + 
  facet_wrap(~VIOLLEVEL) + 
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) + 
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Average Inspection Score by Year for Poor Performers by Violation Level",
       subtitle = "2012-2018",
       x = "Year",
       y = "Average Inspection Score",
       color = "License Category") +
  theme(legend.position = "bottom",
        axis.title.x = element_text(),
        axis.title.y = element_text(),
        panel.spacing = unit(2, "lines")) + 
  theme_bw()
)
```

![](Mass-EOTTS-Data-Challenge_files/figure-gfm/Average%20Inspection%20Score%20Line%20Charts-2.png)<!-- -->

## Insight 2: Top Inspection Violations

Across *all* of the City of Goodeats’ Food Establishments between 2012
and 2018, three of the most common Violations that Inspectors noted
were:

- **37-6-501.11-.12**: This Violation occurs when a Food Establishment
  in the City of Goodeats is noted by an Inspector to have “Improper
  Maintenance of Walls and/or Ceilings.” This Violation was especially
  prevalent between 2012 and 2016, where roughly $2,834$ of these
  Violations were noted yearly across all Food Establishments. With
  these buildings improperly maintained, it appears that from 2017 on
  that Owners of these Food Establishments made the necessary repairs
  and updates, reducing the total amount of these Violations to $1,749$,
  a $38%$ decrease from the four year average of these Violation filings
  from 2012-2016.

- **36-6-501.11-.12**: This Violation occurs when a Food Establishment
  in the City of Goodeats is noted by an Inspector to have “Improper
  Maintenance of Floors.” Between 2012 and 2014, these Violation
  occurrences were relatively small, averaging about $363$ per year.
  Between 2015 and 2017, however, the average number of these Violations
  per year increased $1,053$ per year, a $185+$% increase when comparing
  the three year range from 2012 to 2014. That number of Violations
  sharply fell to $345$ Violations in 2018, suggesting that Owners also
  made repairs and updates to their floors as well as their other
  infrastructure assets to better protect customers and employees from
  accidental slips and falls.

- **15-4-202.16**: This Violation occurs when a Food Establishment in
  the City of Goodeats is noted by an Inspector to have issues regarding
  “Non-Food Contact Surfaces.” Between 2012 and 2018, there were
  approximately $1,037$ of these Violations, on average. 2012 and 2016
  were the two primary years that this Violation occurred more
  frequently than usual.

``` r
# Top Violations, 2012 - 2018; Chart & Table
(JoinedData %>%
 mutate(Year = as.character(as.numeric(format(RESULTDTTM.x, "%Y")))) %>%
 na.omit() %>%
 group_by(Year, LICENSENO, VIOLATION, VIOLDESC) %>%
 summarise(ViolationCount = n()) %>%
 arrange(Year, desc(ViolationCount)) %>%
 top_n(10, wt = ViolationCount) %>%
 ungroup() %>%
 select(-LICENSENO) %>%
 select(Year, VIOLATION, VIOLDESC, ViolationCount) %>%
 group_by(Year, VIOLATION, VIOLDESC) %>%
 summarise(ViolationCount = sum(ViolationCount)) %>%
 spread(key = Year, value = ViolationCount)
)
```

    ## # A tibble: 90 × 9
    ## # Groups:   VIOLATION [90]
    ##    VIOLATION           VIOLDESC `2012` `2013` `2014` `2015` `2016` `2017` `2018`
    ##    <chr>               <chr>     <int>  <int>  <int>  <int>  <int>  <int>  <int>
    ##  1 01-3-101/701.11     Spoilag…   2529   2409   1925   2147   2094   1818    922
    ##  2 01-3-201.11         Approve…   1880   1736   1504   1606   1502   1286    718
    ##  3 01-3-202.11-.17     Recievi…    254    231    193    161    199    190     80
    ##  4 01-3-202.12         Approve…     57     49     40     46     62     51     31
    ##  5 01-3-202.18         Shellst…    612    491    560    609    524    491    241
    ##  6 01-3-203.12         Tags & …    771    684    722    694    617    588    295
    ##  7 01-3-602.11 B2      Labelin…   3681   3326   2963   3094   3574   3056   1565
    ##  8 02-3-201.11F        Safe Fo…    791    705    556    666    628    596    263
    ##  9 02-3-602.11-.12/3-… Food Co…  14307  13774  12280  13278  14548  12257   5958
    ## 10 03-3-402.11-.12     Parasit…    110    115    107     85     82     75     27
    ## # ℹ 80 more rows

``` r
(JoinedData %>%
  mutate(Year = as.character(as.numeric(format(RESULTDTTM.x, "%Y")))) %>%
  na.omit() %>%
  group_by(Year, LICENSENO, VIOLATION, VIOLDESC) %>%
  summarise(ViolationCount = n()) %>%
  arrange(Year, desc(ViolationCount)) %>%
  group_by(Year) %>%
  top_n(10, wt = ViolationCount) %>%
  ungroup() %>%
  select(-LICENSENO) %>%
  select(Year, VIOLATION, VIOLDESC, ViolationCount) %>%
  group_by(Year, VIOLATION) %>%
  summarise(ViolationCount = sum(ViolationCount)) %>%
  tidyr::pivot_wider(names_from = Year, values_from = ViolationCount, values_fill = 0) %>%
  mutate(TotalCount = rowSums(across(-VIOLATION))) %>%
  arrange(desc(TotalCount)) %>%
  slice_max(TotalCount, n = 10) %>%
  tidyr::pivot_longer(cols = -c(VIOLATION, TotalCount), names_to = "Year", values_to = "ViolationCount") %>%
  ggplot(aes(x = Year, y = ViolationCount, fill = VIOLATION)) +
  geom_col() +
  geom_text(aes(label = ViolationCount), position = position_stack(vjust = 0.5), color = "black", size = 3) + 
  labs(title = "Top 10 Violations by Year",
       x = "Year",
       y = "Violation Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.margin = margin(1, 1, 1, 1, "cm"))
)
```

![](Mass-EOTTS-Data-Challenge_files/figure-gfm/Top%20Violation%20Analysis-1.png)<!-- -->

``` r
# Top Violations for Poor Performers, 2012 - 2018; Chart & Table

(PoorPerformerData %>%
 mutate(Year = as.character(as.numeric(format(RESULTDTTM.x, "%Y")))) %>%
 na.omit() %>%
 group_by(Year, LICENSENO, VIOLATION, VIOLDESC) %>%
 summarise(ViolationCount = n()) %>%
 arrange(Year, desc(ViolationCount)) %>%
 top_n(10, wt = ViolationCount) %>%
 ungroup() %>%
 select(-LICENSENO) %>%
 select(Year, VIOLATION, VIOLDESC, ViolationCount) %>%
 group_by(Year, VIOLATION, VIOLDESC) %>%
 summarise(ViolationCount = sum(ViolationCount)) %>%
 spread(key = Year, value = ViolationCount)
)
```

    ## # A tibble: 90 × 9
    ## # Groups:   VIOLATION [90]
    ##    VIOLATION           VIOLDESC `2012` `2013` `2014` `2015` `2016` `2017` `2018`
    ##    <chr>               <chr>     <int>  <int>  <int>  <int>  <int>  <int>  <int>
    ##  1 01-3-101/701.11     Spoilag…    509    539    526    587    588    450    289
    ##  2 01-3-201.11         Approve…    418    382    396    421    384    311    238
    ##  3 01-3-202.11-.17     Recievi…     56     58     53     34     66     53     24
    ##  4 01-3-202.12         Approve…     10     10      6     14     21     11     10
    ##  5 01-3-202.18         Shellst…    151    122    181    191    139    129     62
    ##  6 01-3-203.12         Tags & …    207    158    230    201    180    147     75
    ##  7 01-3-602.11 B2      Labelin…    757    740    714    718    962    739    389
    ##  8 02-3-201.11F        Safe Fo…    156    130    114    158    121    118     74
    ##  9 02-3-602.11-.12/3-… Food Co…   3095   3272   3378   3720   4255   3020   1904
    ## 10 03-3-402.11-.12     Parasit…     32     23     45     26     18     18     10
    ## # ℹ 80 more rows

``` r
(PoorPerformerData %>%
  mutate(Year = as.character(as.numeric(format(RESULTDTTM.x, "%Y")))) %>%
  na.omit() %>%
  group_by(Year, LICENSENO, VIOLATION, VIOLDESC) %>%
  summarise(ViolationCount = n()) %>%
  arrange(Year, desc(ViolationCount)) %>%
  group_by(Year) %>%
  top_n(10, wt = ViolationCount) %>%
  ungroup() %>%
  select(-LICENSENO) %>%
  select(Year, VIOLATION, VIOLDESC, ViolationCount) %>%
  group_by(Year, VIOLATION) %>%
  summarise(ViolationCount = sum(ViolationCount)) %>%
  tidyr::pivot_wider(names_from = Year, values_from = ViolationCount, values_fill = 0) %>%
  mutate(TotalCount = rowSums(across(-VIOLATION))) %>%
  arrange(desc(TotalCount)) %>%
  slice_max(TotalCount, n = 10) %>%
  tidyr::pivot_longer(cols = -c(VIOLATION, TotalCount), names_to = "Year", values_to = "ViolationCount") %>%
  ggplot(aes(x = Year, y = ViolationCount, fill = VIOLATION)) +
  geom_col() +
  geom_text(aes(label = ViolationCount), position = position_stack(vjust = 0.5), color = "black", size = 3) + 
  labs(title = "Top 10 Violations from Poor Performers by Year",
       x = "Year",
       y = "Violation Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1, 1, 1, 1, "cm"))
)
```

![](Mass-EOTTS-Data-Challenge_files/figure-gfm/Top%20Violation%20Analysis-2.png)<!-- -->

# Conclusion

In conclusion, the City of Goodeats continues to make positive strides
in keeping Food Establishments of all kinds safe for Customers,
Employees, and Owners. The higher-performing Food Establishments, namely
Grade A and Grade B Food Establishments, show little variability in
their average Inspection Scores between 2012 and 2018 across all License
Categories. In addition, strides have been made by the City of Goodeats
and its Inspection Team in identifying Violations in regard to the
physical infrastructure of the Food Establishment and on Non-Food
Contact Surfaces. Continuing efforts must still be made, however, to
alleviate concerns in the poorer-performing Food Establishments
(i.e. Grade C Food Establishments), especially with MFW Licenses. Across
all Violation Levels, there is a general, positive increase in the
Average Inspection Score, but a significant amount of work must be done
from an Inspection standpoint to (a) reduce the overall variability in
the Average Inspection Score found in MFW Licenses and (b) have all
License Categories increase their Average Inspection Scores towards
levels seen in Grades A and B.
