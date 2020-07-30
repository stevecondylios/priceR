
# priceR

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/priceR)](https://cran.r-project.org/package=priceR)
[![Travis build
status](https://travis-ci.org/stevecondylios/priceR.svg?branch=master)](https://travis-ci.org/stevecondylios/priceR)
<!-- badges: end -->

`priceR` contains 4 types of capabilties:

  - *Exchange Rates* - easily retrieve exchange rates for immediate use
  - *Inflation* - easily inflate past (nominal) values into present day
    (real) prices
  - *Regular Expressions* - easily extract common pricing patterns from
    free text
  - *Formatting* - easily handle currencies in written work, including
    Rmarkdown documents

`rawr` takes care of this.

# Installation

Install from CRAN

``` r
# install.packages("priceR")
library(priceR)
```

# Capabilties

#### Exchange Rates

Retrieve exchange rate between a currency and 170 others:

``` r
exchange_rate_latest("USD") 
```

    ## For full currency exchange rate API documentation visit:
    ##  https://exchangerate.host/#/#docs
    ##  (this message will only appear once per session)

    ## Daily USD exchange rate as at end of day 2020-07-30 GMT

    ##     currency one_usd_is_equivalent_to
    ## 1        AED             3.672950e+00
    ## 2        AFN             7.685000e+01
    ## 3        ALL             1.053230e+02
    ## 4        AMD             4.816162e+02
    ## 5        ANG             1.789906e+00
    ## 6        AOA             5.579170e+02
    ## 7        ARS             7.219630e+01
    ## 8        AUD             1.395576e+00
    ## 9        AWG             1.800000e+00
    ## 10       AZN             1.702500e+00
    ## 11       BAM             1.660375e+00
    ## 12       BBD             2.000000e+00
    ## 13       BDT             8.455636e+01
    ## 14       BGN             1.661655e+00
    ## 15       BHD             3.769710e-01
    ## 16       BIF             1.921518e+03
    ## 17       BMD             1.000000e+00
    ## 18       BND             1.371907e+00
    ## 19       BOB             6.895510e+00
    ## 20       BRL             5.170700e+00
    ## 21       BSD             1.000000e+00
    ## 22       BTC             9.100000e-05
    ## 23       BTN             7.446418e+01
    ## 24       BWP             1.138316e+01
    ## 25       BYN             2.415270e+00
    ## 26       BZD             2.009941e+00
    ## 27       CAD             1.335117e+00
    ## 28       CDF             1.959919e+03
    ## 29       CHF             9.131260e-01
    ## 30       CLF             2.782300e-02
    ## 31       CLP             7.574005e+02
    ## 32       CNH             7.002929e+00
    ## 33       CNY             7.000600e+00
    ## 34       COP             3.719201e+03
    ## 35       CRC             5.799180e+02
    ## 36       CUC             1.000000e+00
    ## 37       CUP             2.575000e+01
    ## 38       CVE             9.415000e+01
    ## 39       CZK             2.226360e+01
    ## 40       DJF             1.780500e+02
    ## 41       DKK             6.325111e+00
    ## 42       DOP             5.821332e+01
    ## 43       DZD             1.279265e+02
    ## 44       EGP             1.597140e+01
    ## 45       ERN             1.500293e+01
    ## 46       ETB             3.514251e+01
    ## 47       EUR             8.498810e-01
    ## 48       FJD             2.124100e+00
    ## 49       FKP             7.708660e-01
    ## 50       GBP             7.708660e-01
    ## 51       GEL             3.085000e+00
    ## 52       GGP             7.708660e-01
    ## 53       GHS             5.763717e+00
    ## 54       GIP             7.708660e-01
    ## 55       GMD             5.180000e+01
    ## 56       GNF             9.612892e+03
    ## 57       GTQ             7.678242e+00
    ## 58       GYD             2.084629e+02
    ## 59       HKD             7.750150e+00
    ## 60       HNL             2.465885e+01
    ## 61       HRK             6.369900e+00
    ## 62       HTG             1.110824e+02
    ## 63       HUF             2.934830e+02
    ## 64       IDR             1.464300e+04
    ## 65       ILS             3.403720e+00
    ## 66       IMP             7.708660e-01
    ## 67       INR             7.484650e+01
    ## 68       IQD             1.190000e+03
    ## 69       IRR             4.210716e+04
    ## 70       ISK             1.351400e+02
    ## 71       JEP             7.708660e-01
    ## 72       JMD             1.467357e+02
    ## 73       JOD             7.090000e-01
    ## 74       JPY             1.051725e+02
    ## 75       KES             1.077000e+02
    ## 76       KGS             7.716009e+01
    ## 77       KHR             4.096000e+03
    ## 78       KMF             4.190002e+02
    ## 79       KPW             9.000000e+02
    ## 80       KRW             1.194182e+03
    ## 81       KWD             3.058290e-01
    ## 82       KYD             8.310100e-01
    ## 83       KZT             4.166509e+02
    ## 84       LAK             9.065000e+03
    ## 85       LBP             1.507713e+03
    ## 86       LKR             1.852258e+02
    ## 87       LRD             1.992749e+02
    ## 88       LSL             1.642341e+01
    ## 89       LYD             1.372841e+00
    ## 90       MAD             9.337500e+00
    ## 91       MDL             1.669235e+01
    ## 92       MGA             3.839067e+03
    ## 93       MKD             5.230719e+01
    ## 94       MMK             1.363608e+03
    ## 95       MNT             2.844480e+03
    ## 96       MOP             7.960321e+00
    ## 97       MRO             3.570000e+02
    ## 98       MRU             3.730000e+01
    ## 99       MUR             3.990000e+01
    ## 100      MVR             1.541000e+01
    ## 101      MWK             7.402109e+02
    ## 102      MXN             2.200300e+01
    ## 103      MYR             4.242499e+00
    ## 104      MZN             7.080000e+01
    ## 105      NAD             1.655500e+01
    ## 106      NGN             3.875000e+02
    ## 107      NIO             3.463989e+01
    ## 108      NOK             9.051000e+00
    ## 109      NPR             1.191397e+02
    ## 110      NZD             1.505843e+00
    ## 111      OMR             3.850070e-01
    ## 112      PAB             1.000000e+00
    ## 113      PEN             3.505567e+00
    ## 114      PGK             3.465380e+00
    ## 115      PHP             4.913398e+01
    ## 116      PKR             1.665244e+02
    ## 117      PLN             3.745495e+00
    ## 118      PYG             6.927203e+03
    ## 119      QAR             3.641000e+00
    ## 120      RON             4.107900e+00
    ## 121      RSD             9.995000e+01
    ## 122      RUB             7.259320e+01
    ## 123      RWF             9.559614e+02
    ## 124      SAR             3.750807e+00
    ## 125      SBD             8.255762e+00
    ## 126      SCR             1.777538e+01
    ## 127      SDG             5.532500e+01
    ## 128      SEK             8.742700e+00
    ## 129      SGD             1.376274e+00
    ## 130      SHP             7.708660e-01
    ## 131      SLL             9.755000e+03
    ## 132      SOS             5.768274e+02
    ## 133      SRD             7.458000e+00
    ## 134      SSP             1.302600e+02
    ## 135      STD             2.167294e+04
    ## 136      STN             2.100000e+01
    ## 137      SVC             8.726066e+00
    ## 138      SYP             5.120655e+02
    ## 139      SZL             1.641807e+01
    ## 140      THB             3.144292e+01
    ## 141      TJS             1.028588e+01
    ## 142      TMT             3.510000e+00
    ## 143      TND             2.749000e+00
    ## 144      TOP             2.272000e+00
    ## 145      TRY             6.968500e+00
    ## 146      TTD             6.743919e+00
    ## 147      TWD             2.931100e+01
    ## 148      TZS             2.317000e+03
    ## 149      UAH             2.758620e+01
    ## 150      UGX             3.679585e+03
    ## 151      USD             1.000000e+00
    ## 152      UYU             4.239682e+01
    ## 153      UZS             1.017926e+04
    ## 154      VEF             2.484876e+05
    ## 155      VES             2.493720e+05
    ## 156      VND             2.314066e+04
    ## 157      VUV             1.149670e+02
    ## 158      WST             2.633628e+00
    ## 159      XAF             5.574855e+02
    ## 160      XAG             4.142100e-02
    ## 161      XAU             5.100000e-04
    ## 162      XCD             2.702550e+00
    ## 163      XDR             7.115560e-01
    ## 164      XOF             5.574855e+02
    ## 165      XPD             4.730000e-04
    ## 166      XPF             1.014178e+02
    ## 167      XPT             1.086000e-03
    ## 168      YER             2.503001e+02
    ## 169      ZAR             1.660760e+01
    ## 170      ZMW             1.820797e+01
    ## 171      ZWL             3.220000e+02

``` r
# View full list of available currencies with currencies()
```

Write output to a specific file

# Issues and Feature Requests

When reporting an issue, please include:

  - Example code that reproduces the **observed** behavior.
  - An explanation of what the **expected** behavior is.
  - A specific url you’re attempting to retrieve R code from (if that’s
    what your issue concerns)

For feature requests, raise an issue with the following:

  - The desired functionality
  - Example inputs and desired output

# Pull Requests

Pull requests are welcomed. Before doing so, please create an issue or
email me with your idea.

Any new functions should follow the conventions established by the the
package’s existing functions. Please ensure

  - Functions are sensibly named
  - The **intent** of the contribution is clear
  - At least one example is provided in the documentation
