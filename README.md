# <ins>NJDEP Road Salt Application</ins>

<img width="1433" alt="featured" src="https://user-images.githubusercontent.com/36116239/202060491-095f8a92-0a66-483b-b46d-4a1050928c75.png">

### <ins>Project Background</ins>
The de-icing and anti-icing applications we apply to paved surfaces eventually make their way into streams and aquifers – increasing the salinity above healthy levels for people, fish and wildlife. At high concentrations, salt can be fatal to some aquatic animals. The presence of salt can also result in salty pockets forming near the bottom of a lake, which creates a biological dead zone.

With this in mind, the Road Salt Application was developed using the R shiny package to guide the NJDEP's Division of Water Monitoring in creating a one-stop dashboard with key statistics addressing this problem.

The data used for this project was collected from 3,644 monitoring stations throughout NJ for the years 1997-2018. There were three major pollutants that were measured: specific conductance, total dissolved solids, and chloride.

### <ins>Data Cleaning</ins>
The data was cleaned and preprocessed using R programming language. The cleaning process involved removing missing values, duplicates, and outliers. The data was also transformed to a format that is suitable for visualization and analysis.

The specific steps taken to clean the data are as follows:

Removed missing values: Any rows with missing values were removed from the dataset.

Removed duplicates: Any duplicate rows were removed from the dataset.

Removed outliers: Any extreme values that were deemed unrealistic were removed from the dataset.

Transformed data: The data was transformed into a format that is suitable for visualization and analysis.

Overall, the data cleaning process ensured that the data used in this project is accurate, consistent, and reliable.

### <ins>App Features</ins>
The NJDEP Road Salt Application provides several features to help users understand the impact of road salt on water quality in New Jersey. These features include:

Data exploration: Users can explore the data by selecting different monitoring stations, pollutants, and years. The app provides several visualizations, including scatterplots, boxplots, and histograms, to help users understand the distribution of the data.

Trend analysis: Users can analyze trends in the data by selecting different pollutants and monitoring stations. The app provides several visualizations, including time series plots and linear regression models, to help users understand how pollutant levels have changed over time.

Map visualization: Users can view a map of New Jersey with all monitoring stations plotted on it. The app provides the ability to filter by pollutant and year, allowing users to see how pollutant levels vary across the state.

Dashboard view: The app includes a dashboard view that provides key statistics on specific conductance, total dissolved solids, and chloride for the selected monitoring station and year. This view is designed to provide a quick overview of the data and help users identify areas of concern.

Overall, the NJDEP Road Salt Application is designed to provide users with a comprehensive view of road salt's impact on water quality in New Jersey.

### <ins>How to run application</ins>
The application can be tested by going to the following [website](https://kzolea695.shinyapps.io/NJDEP_Roadsalt_app/)
