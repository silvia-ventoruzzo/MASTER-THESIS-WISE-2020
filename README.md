# On the Role of Weather in Predicting Online Sales of Different Product Categories: Clustering of German weather stations

This repository contains the files for the the spatio-temporal clustering of weather stations that was part of the master thesis project "On the Role of Weather in Predicting Online Sales of Different Product Categories" to obtain the M.Sc. in Statistics at the Humboldt University of Berlin. 

## Description

My master thesis was aimed at examining whether weather information will reduce sales forecast errors of online sales of a German retailer differentiated by product category. The second goal was to divide Germany into weather regions, which was done by aggregating weather stations into clusters. Since the rest of the thesis is confidential, this repository will only contain the scripts for the regionalization part. 

## Content

Only clustering methods were chosen which could take into account both the time series element of the weather data and the geographical location of the weather stations. In particular, the focus was on techniques which were already implemented in `R`. However, two of the four used did not have a (complete) implementation, and they were therefore written by me.

1. HiClimR [[1]](#1): package `HiClimR` 
2. ClustGeo [[2]](#2): package `ClustGeo`
3. Bootstrap ClustGeo [[3]](#3): implementation can be found in [Helpers/bootstrap_hclustgeo.R](https://github.com/silvia-ventoruzzo/MASTER-THESIS-WISE-2020/blob/master/Helpers/bootstrap_hclustgeo.R)
4. CorClustST [[4]](#4): implementation can be found in [Helpers/CorClustST.R](https://github.com/silvia-ventoruzzo/MASTER-THESIS-WISE-2020/blob/master/Helpers/CorClustST.R)

## References

<a id="1">[1]</a> Badr, H. S., Zaitchik, B. F., & Dezfuli, A. K. (2015). A tool for hierarchical climate regionalization. Earth Science Informatics, 8(4), 949-958.

<a id="2">[2]</a> Chavent, M., Kuentz-Simonet, V., Labenne, A., & Saracco, J. (2018). ClustGeo: an R package for hierarchical clustering with spatial constraints. Computational Statistics, 33(4), 1799-1822.

<a id="3">[3]</a> Distefano, V., Mameli, V., & Poli, I. (2020). Identifying spatial patterns with the Bootstrap ClustGeo technique. Spatial Statistics, 38, 100441.

<a id="4">[4]</a> Hüsch, M., Schyska, B. U., & von Bremen, L. (2018). CorClustST—Correlation-based clustering of big spatio-temporal datasets. Future Generation Computer Systems.
