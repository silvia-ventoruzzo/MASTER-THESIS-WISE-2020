# On the Role of Weather in Predicting Online Sales of Different Product Categories: Clustering of German weather stations

This repository contains the files for the the spatio-temporal clustering of weather stations that was part of the master thesis project "On the Role of Weather in Predicting Online Sales of Different Product Categories" to obtain the M.Sc. in Statistics at the Humboldt University of Berlin. 

## Description

Weather exerts a large in influence on our daily life. Some studies found that weather conditions impact our mood ([[3]](#3)) and behavior ([[5]](#5); [[7]](#5)). The weather also influences our motivation to shop outside in brick-and-mortar stores or online at home. 

The study investigated the relationship between weather conditions on the country-wide sales performance of an online retailer. Because of the company's country-wide reach, Germany was firstly divided into four weather regions using clustering methods which take into account both the time series element of the weather data and the geographical location of the weather stations. Afterwards, I compared Bayesian state-space models to evaluate the impact of weather on online sales forecast quality for different product categories.

Since the sales data used in the project is confidential, this repository will only contain the scripts for the regionalization part of the thesis.

## Data
The data for this part of the project was download from the [German Meteorological Service](https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/). It is not included in the repository because of its size.

## Content

Only clustering methods were chosen which could take into account both the time series element of the weather data and the geographical location of the weather stations. In particular, the focus was on techniques which were already implemented in `R`. However, two of the four used did not have a (complete) implementation, and they were therefore written by me.

1. HiClimR [[1]](#1): package `HiClimR` 
2. ClustGeo [[2]](#2): package `ClustGeo`
3. Bootstrap ClustGeo [[4]](#4): implementation can be found in [Helpers/bootstrap_hclustgeo.R](https://github.com/silvia-ventoruzzo/MASTER-THESIS-WISE-2020/blob/master/Helpers/bootstrap_hclustgeo.R)
4. CorClustST [[6]](#6): implementation can be found in [Helpers/CorClustST.R](https://github.com/silvia-ventoruzzo/MASTER-THESIS-WISE-2020/blob/master/Helpers/CorClustST.R)

## References

<a id="1">[1]</a> Badr, H. S., Zaitchik, B. F., & Dezfuli, A. K. (2015). A tool for hierarchical climate regionalization. Earth Science Informatics, 8(4), 949-958.

<a id="2">[2]</a> Chavent, M., Kuentz-Simonet, V., Labenne, A., & Saracco, J. (2018). ClustGeo: an R package for hierarchical clustering with spatial constraints. Computational Statistics, 33(4), 1799-1822.

<a id="3">[3]</a> Denissen, J. J., L. Butalid, L. Penke, and M. A. Van Aken (2008): The effects of weather on daily mood: a multilevel approach. Emotion, 8, 662.

<a id="4">[4]</a> Distefano, V., Mameli, V., & Poli, I. (2020). Identifying spatial patterns with the Bootstrap ClustGeo technique. Spatial Statistics, 38, 100441.

<a id="5">[5]</a> Eisinga, R., P. H. Franses, and M. Vergeer (2011): Weather conditions and daily television use in the Netherlands, 1996-2005, International journal of Biometeorology, 55, 555-564.

<a id="6">[6]</a> Hüsch, M., Schyska, B. U., & von Bremen, L. (2018). CorClustST—Correlation-based clustering of big spatio-temporal datasets. Future Generation Computer Systems.

<a id="7">[7]</a>  Lee, J. J., F. Gino, and B. R. Staats (2014): Rainmakers: Why bad weather means good productivity. Journal of Applied Psychology, 99, 504.
