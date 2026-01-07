Large-scale Evidence Generation and Evaluation for Heterogeneity of Treatment Effect for Type 2 Diabetes Mellitus Drugs (LEGEND-T2DM)
=============================================================================

<img src="https://img.shields.io/badge/Study%20Status-Results%20Available-yellow.svg" alt="Study Status: Results Available">

- Analytics use case(s): **Population-Level Estimation**
- Study type: **Clinical Application**
- Tags: **-**
- Study lead: **Hsin Yi Chen**, **George Hripcsak**
- Study lead forums tag: **--**
- Study start date: **1 May 2025**
- Study end date: **-**
- Protocol: **[HTML document](https://github.com/ohdsi-studies/LegendT2dmArpah/blob/main/docs/Protocol.html)**
- Publications: **-**
- Results explorer: **-**

Requirements
============

- A database in [Common Data Model version 5](https://github.com/OHDSI/CommonDataModel) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, Google BigQuery, or Microsoft APS.
- R version 4.0.5
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com)
- 100 GB of free disk space

How to run
==========
1. Follow [these instructions](https://ohdsi.github.io/Hades/rSetup.html) for setting up your R environment, including RTools and Java.

2. Open your study package in RStudio. Use the following code to install all the dependencies:

	```r
	install.packages("renv")
	renv::activate()
	renv::restore()
	```

3. Once all dependencies are installed, you can execute the analysis by modifying and using the code provided in `StrategusCodeToRun.R`.

4. Heterogeneity analysis code and code to produce characterization tables are located in `extras/Characterization Tables` and `extras/HTE Code`

License
=======
The `LegendT2dm` package is licensed under Apache License 2.0

Development
===========
`LegendT2dmArpah` was developed in ATLAS and R Studio.

### Development status

Study complete.


