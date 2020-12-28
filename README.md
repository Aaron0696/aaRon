---
title: "aaRon Test Document"
author: "Aaron0696"
output:
  html_document: 
    toc: yes
    toc_float: yes
    highlight: haddock
    code_folding: show
    keep_md: true
---


```r
# keep this chunk to activate theme
aaRon::use.style()
```

<!--html_preserve--><style type="text/css">a {
    font-weight: bold;
}

.pull-right {
    float: left !important
}

.title, .author, .date {
  text-align: center
}

.warnings {
    background: white;
    margin-bottom: 20px;
    width: auto;
    padding: 10px;
    height: 40px;
    color: darkred;
    text-align: left;
    font-size: 18px;
    border-left-style: solid;
    border-left-width: 10px;
}

pre code {
    color: black;
}

pre:not([class]) {
    background-color: #e7f1f5;
}

pre {
    background-color: #d7e4e43d;
}

.halftable {
    width: 50%;
}

div.main-container {
    max-width: 1600px;
}

ol ol, ol ul, ul ol, ul ul {
    margin-bottom: 10px;
}

body {
  font-family: Arial, Helvetica, sans-serif;
}

.h1, .h2, .h3, h1, h2, h3 {
    margin-top: 20px;
    margin-bottom: 10px;
    font-weight: bold;
    color: black;
}

@media only screen and (min-width: 768px)
{
  div.tocify {
    width: 10%;
    max-width: 260px;
    max-height: 95%;
    font-size: 12px;
  }

  .col-sm-8 {
    width: 85%;
  }

  .col-sm-4 {
    width: 15%;
    padding-left: 10px;
  }
}


.tocify {
    position: fixed;
    border: 0px solid #333;
    border-radius: 0px;
}

.toc-content {
    padding-left: 10px;
    padding-right: 10px;
    float: right;
}

.tocify-subheader {
    text-indent: 0px;
    font-size: 11px;
}

.list-group-item.active, .list-group-item.active:focus, .list-group-item.active:hover {
    z-index: 2;
    color: #fff;
    background-color: black;
    border-color: black;
}

th, td {
  font-size: 12px;
}

tbody tr:hover {
  background: #e7f1f5;
}

*::-webkit-scrollbar {
  width: 3px;
  height: 3px;
}
*::-webkit-scrollbar-track {
  background: whitesmoke;
}
*::-webkit-scrollbar-thumb {
  background-color: black;
  border-radius: 20px;
}</style><!--/html_preserve-->


```r
library(aaRon)
library(psych)
library(lavaan)
```

# fct2num()


```r
a <- factor(c("Agree","Disagree","Agree","Agree","Disagree"))
fct2num(a)
```

```
## Levels: Agree, Disagree
```

```
## [1] 1 2 1 1 2
```

```r
fct2num(a, start = 0) # if likert scales begin at 0
```

```
## Levels: Agree, Disagree
```

```
## [1] 0 1 0 0 1
```

# efa.diag()


```r
efa.diag(HolzingerSwineford1939[,7:15])
```

```
## KMO:  0.75224 
## Bartlett's Test Of Sphericity:  904.1 , df =  36 , pvalue =  1.9121e-166
```

# tall.loadings()


```r
myefa <- fa(Harman74.cor$cov, 4, fm = "wls")
```

```
## Loading required namespace: GPArotation
```

```r
tall.loadings(myefa$Structure)
```

```
## # A tibble: 85 x 3
##    ITEM                  NAME  VALUE
##    <chr>                 <chr> <dbl>
##  1 VisualPerception      WLS1  0.36 
##  2 Cubes                 WLS1  0.24 
##  3 PaperFormBoard        WLS1  0.290
##  4 Flags                 WLS1  0.37 
##  5 GeneralInformation    WLS1  0.79 
##  6 PargraphComprehension WLS1  0.82 
##  7 SentenceCompletion    WLS1  0.84 
##  8 WordClassification    WLS1  0.68 
##  9 WordMeaning           WLS1  0.86 
## 10 Addition              WLS1  0.28 
## # ... with 75 more rows
```

```r
tall.loadings(myefa$loadings)
```

```
## # A tibble: 38 x 3
##    ITEM                  NAME  VALUE
##    <chr>                 <chr> <dbl>
##  1 GeneralInformation    WLS1  0.76 
##  2 PargraphComprehension WLS1  0.8  
##  3 SentenceCompletion    WLS1  0.87 
##  4 WordClassification    WLS1  0.56 
##  5 WordMeaning           WLS1  0.86 
##  6 Deduction             WLS1  0.33 
##  7 ProblemReasoning      WLS1  0.31 
##  8 SeriesCompletion      WLS1  0.3  
##  9 ArithmeticProblems    WLS1  0.290
## 10 Addition              WLS2  0.86 
## # ... with 28 more rows
```

# aggregateScale()


```r
mydata <- data.frame(a = c(1:8,NA,NA), b = 1:10, c = rep(NA,10))
mydata
```

```
##     a  b  c
## 1   1  1 NA
## 2   2  2 NA
## 3   3  3 NA
## 4   4  4 NA
## 5   5  5 NA
## 6   6  6 NA
## 7   7  7 NA
## 8   8  8 NA
## 9  NA  9 NA
## 10 NA 10 NA
```

```r
aggregateScale(mydata)
```

```
##  [1]  2  4  6  8 10 12 14 16 NA NA
```

```r
aggregateScale(mydata, NA.threshold = 0.2) # less than 0.2 missingness
```

```
##  [1] NA NA NA NA NA NA NA NA NA NA
```

# vlookup()


```r
mydata <- data.frame(Type = rep(1:3,5), Rand = 4:6)
mytable <- data.frame(Type = 1:3, Magnitude = c("Low","Medium","High"))
vlookup(data = mydata,
        lookup_table = mytable,
        data_col = "Type",
        lookup_col = "Type",
        lookup_value = "Magnitude")
```

```
##    Type Rand Magnitude
## 1     1    4       Low
## 6     2    5    Medium
## 11    3    6      High
## 4     1    4       Low
## 9     2    5    Medium
## 14    3    6      High
## 3     1    4       Low
## 8     2    5    Medium
## 13    3    6      High
## 2     1    4       Low
## 7     2    5    Medium
## 12    3    6      High
## 5     1    4       Low
## 10    2    5    Medium
## 15    3    6      High
```

# auto.class()


```r
# return numeric
auto.class(1:100)
```

```
##   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
##  [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
##  [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
##  [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
##  [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
##  [91]  91  92  93  94  95  96  97  98  99 100
```

```r
# return factor
auto.class(rep(1:7,10))
```

```
##  [1] 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3
## [39] 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7
## Levels: 1 2 3 4 5 6 7
```

```r
# return character
auto.class(c(1:100,"hello"))
```

```
##   [1] "1"     "2"     "3"     "4"     "5"     "6"     "7"     "8"     "9"    
##  [10] "10"    "11"    "12"    "13"    "14"    "15"    "16"    "17"    "18"   
##  [19] "19"    "20"    "21"    "22"    "23"    "24"    "25"    "26"    "27"   
##  [28] "28"    "29"    "30"    "31"    "32"    "33"    "34"    "35"    "36"   
##  [37] "37"    "38"    "39"    "40"    "41"    "42"    "43"    "44"    "45"   
##  [46] "46"    "47"    "48"    "49"    "50"    "51"    "52"    "53"    "54"   
##  [55] "55"    "56"    "57"    "58"    "59"    "60"    "61"    "62"    "63"   
##  [64] "64"    "65"    "66"    "67"    "68"    "69"    "70"    "71"    "72"   
##  [73] "73"    "74"    "75"    "76"    "77"    "78"    "79"    "80"    "81"   
##  [82] "82"    "83"    "84"    "85"    "86"    "87"    "88"    "89"    "90"   
##  [91] "91"    "92"    "93"    "94"    "95"    "96"    "97"    "98"    "99"   
## [100] "100"   "hello"
```

```r
# return factor
auto.class(c("Strongly Agree","Agree","Disagree","Strongly Disagree"))
```

```
## [1] Strongly Agree    Agree             Disagree          Strongly Disagree
## Levels: Agree Disagree Strongly Agree Strongly Disagree
```

```r
# use with lapply to apply across entire dataframe
mydata <- data.frame(A = 1:100, B = c(1:99,"hello"))
mydata <- data.frame(lapply(mydata, FUN = auto.class))
str(mydata)
```

```
## 'data.frame':	100 obs. of  2 variables:
##  $ A: num  1 2 3 4 5 6 7 8 9 10 ...
##  $ B: chr  "1" "2" "3" "4" ...
```

# code.reverse()


```r
mydata <- data.frame(myscale = c("Disagree", "Neutral", "Agree", "Neutral","Agree"))
code.reverse(vector = mydata$myscale, original_levels = c("Disagree","Neutral","Agree"))
```

```
## [1] Agree    Neutral  Disagree Neutral  Disagree
## Levels: Disagree Neutral Agree
```

# prettyalpha()


```r
myalpha <- alpha(HolzingerSwineford1939[,7:15])
prettyalpha(myalpha)
```



**Cronbach Alpha**:



|   | RAW_ALPHA | STD.ALPHA | G6(SMC) | AVERAGE_R | S/N  | ASE  | MEAN |  SD  | MEDIAN_R |
|:--|:---------:|:---------:|:-------:|:---------:|:----:|:----:|:----:|:----:|:--------:|
|   |   0.76    |   0.76    |  0.81   |   0.26    | 3.17 | 0.02 | 4.22 | 0.66 |   0.21   |


**Alpha Values If Certain Items Were Dropped**:



|   | RAW_ALPHA | STD.ALPHA | G6(SMC) | AVERAGE_R | S/N  | ALPHA SE | VAR.R | MED.R |
|:--|:---------:|:---------:|:-------:|:---------:|:----:|:--------:|:-----:|:-----:|
|x1 |   0.72    |   0.73    |  0.78   |   0.25    | 2.64 |   0.02   | 0.04  | 0.19  |
|x2 |   0.76    |   0.76    |  0.81   |   0.29    | 3.22 |   0.02   | 0.04  | 0.22  |
|x3 |   0.75    |   0.75    |  0.80   |   0.27    | 2.97 |   0.02   | 0.04  | 0.21  |
|x4 |   0.72    |   0.72    |  0.76   |   0.24    | 2.55 |   0.03   | 0.03  | 0.21  |
|x5 |   0.72    |   0.73    |  0.76   |   0.25    | 2.64 |   0.02   | 0.03  | 0.21  |
|x6 |   0.71    |   0.72    |  0.76   |   0.24    | 2.53 |   0.03   | 0.03  | 0.21  |
|x7 |   0.77    |   0.76    |  0.80   |   0.29    | 3.25 |   0.02   | 0.03  | 0.22  |
|x8 |   0.75    |   0.75    |  0.79   |   0.27    | 2.95 |   0.02   | 0.04  | 0.21  |
|x9 |   0.73    |   0.73    |  0.78   |   0.25    | 2.68 |   0.02   | 0.04  | 0.18  |


**Item-Level Statistics**:



|   |  N  | RAW.R | STD.R | R.COR | R.DROP | MEAN |  SD  |
|:--|:---:|:-----:|:-----:|:-----:|:------:|:----:|:----:|
|x1 | 301 | 0.66  | 0.65  | 0.59  |  0.52  | 4.94 | 1.17 |
|x2 | 301 | 0.45  | 0.44  | 0.32  |  0.28  | 6.09 | 1.18 |
|x3 | 301 | 0.53  | 0.53  | 0.44  |  0.37  | 2.25 | 1.13 |
|x4 | 301 | 0.70  | 0.69  | 0.68  |  0.58  | 3.06 | 1.16 |
|x5 | 301 | 0.67  | 0.65  | 0.65  |  0.53  | 4.34 | 1.29 |
|x6 | 301 | 0.71  | 0.69  | 0.69  |  0.60  | 2.19 | 1.10 |
|x7 | 301 | 0.41  | 0.43  | 0.34  |  0.25  | 4.19 | 1.09 |
|x8 | 301 | 0.51  | 0.54  | 0.46  |  0.37  | 5.53 | 1.01 |
|x9 | 301 | 0.62  | 0.64  | 0.57  |  0.49  | 5.37 | 1.01 |

# prettylavaan()


```r
# the famous Holzinger and Swineford (1939) example
HS.model <-  "visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9"
fit <- cfa(HS.model, data = HolzingerSwineford1939)
prettylavaan(fit, output_format = "datatable")
```

```
## Converged: TRUE 
## Iterations: 35 
## Original Sample Size: 301 
## Effective Sample Size: 301
## 
## Fit Indices:
##        NPAR  CHISQ DF PVALUE   CFI   TLI  NNFI   NFI RMSEA  SRMR    AIC    BIC
## Values   21 85.306 24      0 0.931 0.896 0.896 0.907 0.092 0.065 7517.5 7595.3
## 
## 
## 
## Parameter Estimates:
```

<!--html_preserve--><div id="htmlwidget-623281842a4e8aa2030b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-623281842a4e8aa2030b">{"x":{"filter":"top","filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.269\" data-max=\"1\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173\" data-max=\"1.18\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.165\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.518\" data-max=\"17.014\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"disabled\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"],["visual","visual","visual","textual","textual","textual","speed","speed","speed","x1","x2","x3","x4","x5","x6","x7","x8","x9","visual","textual","speed","visual","visual","textual"],["=~","=~","=~","=~","=~","=~","=~","=~","=~","~~","~~","~~","~~","~~","~~","~~","~~","~~","~~","~~","~~","~~","~~","~~"],["x1","x2","x3","x4","x5","x6","x7","x8","x9","x1","x2","x3","x4","x5","x6","x7","x8","x9","visual","textual","speed","textual","speed","speed"],[0.772,0.424,0.581,0.852,0.855,0.838,0.57,0.723,0.665,0.404,0.821,0.662,0.275,0.269,0.298,0.676,0.477,0.558,1,1,1,0.459,0.471,0.283],[1,0.554,0.729,1,1.113,0.926,1,1.18,1.082,0.549,1.134,0.844,0.371,0.446,0.356,0.799,0.488,0.566,0.809,0.979,0.384,0.408,0.262,0.173],[0,0.1,0.109,0,0.065,0.055,0,0.165,0.151,0.114,0.102,0.091,0.048,0.058,0.043,0.081,0.074,0.071,0.145,0.112,0.086,0.074,0.056,0.049],[null,5.554,6.685,null,17.014,16.703,null,7.152,7.155,4.833,11.146,9.317,7.779,7.642,8.277,9.823,6.573,8.003,5.564,8.737,4.451,5.552,4.66,3.518],[null,0,0,null,0,0,null,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>LHS<\/th>\n      <th>OP<\/th>\n      <th>RHS<\/th>\n      <th>STD.ALL<\/th>\n      <th>EST<\/th>\n      <th>SE<\/th>\n      <th>Z<\/th>\n      <th>PVALUE<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


```r
# using robust estimators
robustfit <- cfa(HS.model, data = HolzingerSwineford1939, estimator = "MLM")
# request for robust fit indices
prettylavaan(robustfit, output_format = "datatable", robust = TRUE)
```

```
## Converged: TRUE 
## Iterations: 35 
## Original Sample Size: 301 
## Effective Sample Size: 301
## 
## Fit Indices:
##            NPAR    CHISQ       DF   PVALUE      CFI      TLI     NNFI      NFI
## Naive    21.000   85.306   24.000    0.000    0.931    0.896    0.896    0.907
## Scaled       21   80.872       24        0    0.925    0.887    0.887    0.898
## Robust       21                               0.932    0.897    0.897         
##           RMSEA     SRMR      AIC      BIC
## Naive     0.092    0.065 7517.490 7595.339
## Scaled    0.089                           
## Robust    0.091                           
## 
## 
## 
## Parameter Estimates:
```

<!--html_preserve--><div id="htmlwidget-17b4f569fb89f0f9190e" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-17b4f569fb89f0f9190e">{"x":{"filter":"top","filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.269\" data-max=\"1\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0.173\" data-max=\"1.18\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.167\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"3.139\" data-max=\"16.762\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"0\" data-max=\"0.002\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"],["visual","visual","visual","textual","textual","textual","speed","speed","speed","x1","x2","x3","x4","x5","x6","x7","x8","x9","visual","textual","speed","visual","visual","textual"],["=~","=~","=~","=~","=~","=~","=~","=~","=~","~~","~~","~~","~~","~~","~~","~~","~~","~~","~~","~~","~~","~~","~~","~~"],["x1","x2","x3","x4","x5","x6","x7","x8","x9","x1","x2","x3","x4","x5","x6","x7","x8","x9","visual","textual","speed","textual","speed","speed"],[0.772,0.424,0.581,0.852,0.855,0.838,0.57,0.723,0.665,0.404,0.821,0.662,0.275,0.269,0.298,0.676,0.477,0.558,1,1,1,0.459,0.471,0.283],[1,0.554,0.729,1,1.113,0.926,1,1.18,1.082,0.549,1.134,0.844,0.371,0.446,0.356,0.799,0.488,0.566,0.809,0.979,0.384,0.408,0.262,0.173],[0,0.103,0.115,0,0.066,0.06,0,0.152,0.132,0.138,0.107,0.085,0.05,0.058,0.046,0.079,0.074,0.068,0.167,0.121,0.083,0.082,0.055,0.055],[null,5.359,6.367,null,16.762,15.497,null,7.758,8.169,3.968,10.554,9.985,7.423,7.688,7.7,10.168,6.567,8.332,4.837,8.109,4.635,4.966,4.762,3.139],[null,0,0,null,0,0,null,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.002]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>LHS<\/th>\n      <th>OP<\/th>\n      <th>RHS<\/th>\n      <th>STD.ALL<\/th>\n      <th>EST<\/th>\n      <th>SE<\/th>\n      <th>Z<\/th>\n      <th>PVALUE<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


```r
# for piping to kableExtra for further editing
# note: library(kableExtra) may mess up formatting of normal kable tables in the same Rmd document
# refer to https://github.com/haozhu233/kableExtra/issues/265
# format = "html" is required to work with kableExtra
library(kableExtra)
mylist <- prettylavaan(robustfit, output_format = "kableExtra", format = "html", robust = TRUE)
mylist$Param %>% kable_styling(font_size = 9)
```

<table class="table" style="font-size: 9px; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> LHS </th>
   <th style="text-align:center;"> OP </th>
   <th style="text-align:center;"> RHS </th>
   <th style="text-align:center;"> STD.ALL </th>
   <th style="text-align:center;"> EST </th>
   <th style="text-align:center;"> SE </th>
   <th style="text-align:center;"> Z </th>
   <th style="text-align:center;"> PVALUE </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> visual </td>
   <td style="text-align:center;"> =~ </td>
   <td style="text-align:center;"> x1 </td>
   <td style="text-align:center;"> 0.772 </td>
   <td style="text-align:center;"> 1.000 </td>
   <td style="text-align:center;"> 0.000 </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> visual </td>
   <td style="text-align:center;"> =~ </td>
   <td style="text-align:center;"> x2 </td>
   <td style="text-align:center;"> 0.424 </td>
   <td style="text-align:center;"> 0.554 </td>
   <td style="text-align:center;"> 0.103 </td>
   <td style="text-align:center;"> 5.359 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> visual </td>
   <td style="text-align:center;"> =~ </td>
   <td style="text-align:center;"> x3 </td>
   <td style="text-align:center;"> 0.581 </td>
   <td style="text-align:center;"> 0.729 </td>
   <td style="text-align:center;"> 0.115 </td>
   <td style="text-align:center;"> 6.367 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> textual </td>
   <td style="text-align:center;"> =~ </td>
   <td style="text-align:center;"> x4 </td>
   <td style="text-align:center;"> 0.852 </td>
   <td style="text-align:center;"> 1.000 </td>
   <td style="text-align:center;"> 0.000 </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> textual </td>
   <td style="text-align:center;"> =~ </td>
   <td style="text-align:center;"> x5 </td>
   <td style="text-align:center;"> 0.855 </td>
   <td style="text-align:center;"> 1.113 </td>
   <td style="text-align:center;"> 0.066 </td>
   <td style="text-align:center;"> 16.762 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> textual </td>
   <td style="text-align:center;"> =~ </td>
   <td style="text-align:center;"> x6 </td>
   <td style="text-align:center;"> 0.838 </td>
   <td style="text-align:center;"> 0.926 </td>
   <td style="text-align:center;"> 0.060 </td>
   <td style="text-align:center;"> 15.497 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> speed </td>
   <td style="text-align:center;"> =~ </td>
   <td style="text-align:center;"> x7 </td>
   <td style="text-align:center;"> 0.570 </td>
   <td style="text-align:center;"> 1.000 </td>
   <td style="text-align:center;"> 0.000 </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;"> speed </td>
   <td style="text-align:center;"> =~ </td>
   <td style="text-align:center;"> x8 </td>
   <td style="text-align:center;"> 0.723 </td>
   <td style="text-align:center;"> 1.180 </td>
   <td style="text-align:center;"> 0.152 </td>
   <td style="text-align:center;"> 7.758 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> speed </td>
   <td style="text-align:center;"> =~ </td>
   <td style="text-align:center;"> x9 </td>
   <td style="text-align:center;"> 0.665 </td>
   <td style="text-align:center;"> 1.082 </td>
   <td style="text-align:center;"> 0.132 </td>
   <td style="text-align:center;"> 8.169 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> x1 </td>
   <td style="text-align:center;"> ~~ </td>
   <td style="text-align:center;"> x1 </td>
   <td style="text-align:center;"> 0.404 </td>
   <td style="text-align:center;"> 0.549 </td>
   <td style="text-align:center;"> 0.138 </td>
   <td style="text-align:center;"> 3.968 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> x2 </td>
   <td style="text-align:center;"> ~~ </td>
   <td style="text-align:center;"> x2 </td>
   <td style="text-align:center;"> 0.821 </td>
   <td style="text-align:center;"> 1.134 </td>
   <td style="text-align:center;"> 0.107 </td>
   <td style="text-align:center;"> 10.554 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> x3 </td>
   <td style="text-align:center;"> ~~ </td>
   <td style="text-align:center;"> x3 </td>
   <td style="text-align:center;"> 0.662 </td>
   <td style="text-align:center;"> 0.844 </td>
   <td style="text-align:center;"> 0.085 </td>
   <td style="text-align:center;"> 9.985 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> x4 </td>
   <td style="text-align:center;"> ~~ </td>
   <td style="text-align:center;"> x4 </td>
   <td style="text-align:center;"> 0.275 </td>
   <td style="text-align:center;"> 0.371 </td>
   <td style="text-align:center;"> 0.050 </td>
   <td style="text-align:center;"> 7.423 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> x5 </td>
   <td style="text-align:center;"> ~~ </td>
   <td style="text-align:center;"> x5 </td>
   <td style="text-align:center;"> 0.269 </td>
   <td style="text-align:center;"> 0.446 </td>
   <td style="text-align:center;"> 0.058 </td>
   <td style="text-align:center;"> 7.688 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> x6 </td>
   <td style="text-align:center;"> ~~ </td>
   <td style="text-align:center;"> x6 </td>
   <td style="text-align:center;"> 0.298 </td>
   <td style="text-align:center;"> 0.356 </td>
   <td style="text-align:center;"> 0.046 </td>
   <td style="text-align:center;"> 7.700 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> x7 </td>
   <td style="text-align:center;"> ~~ </td>
   <td style="text-align:center;"> x7 </td>
   <td style="text-align:center;"> 0.676 </td>
   <td style="text-align:center;"> 0.799 </td>
   <td style="text-align:center;"> 0.079 </td>
   <td style="text-align:center;"> 10.168 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> x8 </td>
   <td style="text-align:center;"> ~~ </td>
   <td style="text-align:center;"> x8 </td>
   <td style="text-align:center;"> 0.477 </td>
   <td style="text-align:center;"> 0.488 </td>
   <td style="text-align:center;"> 0.074 </td>
   <td style="text-align:center;"> 6.567 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> x9 </td>
   <td style="text-align:center;"> ~~ </td>
   <td style="text-align:center;"> x9 </td>
   <td style="text-align:center;"> 0.558 </td>
   <td style="text-align:center;"> 0.566 </td>
   <td style="text-align:center;"> 0.068 </td>
   <td style="text-align:center;"> 8.332 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> visual </td>
   <td style="text-align:center;"> ~~ </td>
   <td style="text-align:center;"> visual </td>
   <td style="text-align:center;"> 1.000 </td>
   <td style="text-align:center;"> 0.809 </td>
   <td style="text-align:center;"> 0.167 </td>
   <td style="text-align:center;"> 4.837 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> textual </td>
   <td style="text-align:center;"> ~~ </td>
   <td style="text-align:center;"> textual </td>
   <td style="text-align:center;"> 1.000 </td>
   <td style="text-align:center;"> 0.979 </td>
   <td style="text-align:center;"> 0.121 </td>
   <td style="text-align:center;"> 8.109 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> speed </td>
   <td style="text-align:center;"> ~~ </td>
   <td style="text-align:center;"> speed </td>
   <td style="text-align:center;"> 1.000 </td>
   <td style="text-align:center;"> 0.384 </td>
   <td style="text-align:center;"> 0.083 </td>
   <td style="text-align:center;"> 4.635 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> visual </td>
   <td style="text-align:center;"> ~~ </td>
   <td style="text-align:center;"> textual </td>
   <td style="text-align:center;"> 0.459 </td>
   <td style="text-align:center;"> 0.408 </td>
   <td style="text-align:center;"> 0.082 </td>
   <td style="text-align:center;"> 4.966 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> visual </td>
   <td style="text-align:center;"> ~~ </td>
   <td style="text-align:center;"> speed </td>
   <td style="text-align:center;"> 0.471 </td>
   <td style="text-align:center;"> 0.262 </td>
   <td style="text-align:center;"> 0.055 </td>
   <td style="text-align:center;"> 4.762 </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> textual </td>
   <td style="text-align:center;"> ~~ </td>
   <td style="text-align:center;"> speed </td>
   <td style="text-align:center;"> 0.283 </td>
   <td style="text-align:center;"> 0.173 </td>
   <td style="text-align:center;"> 0.055 </td>
   <td style="text-align:center;"> 3.139 </td>
   <td style="text-align:center;"> 0.002 </td>
  </tr>
</tbody>
</table>
