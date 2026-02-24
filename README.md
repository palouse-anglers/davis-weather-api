# davis-weather-api
This R project provides tools to access and process weather station data from **Davis Instruments WeatherLink API (v2)**. It allows users to retrieve station metadata, current and historical weather observations, and transform them into clean, tidy R data frames for analysis.

## Features

- Access **WeatherLink v2 API** endpoints for stations, sensors, and weather data.
- Support for **one or more station UUIDs**.
- Automatically handles missing values (`NULL`) and converts them to `NA`.
- Adds a **local timestamp** (`realts`) based on station timezone for easier analysis.
- Filter data by **sensor type** (e.g., outdoor temperature, humidity, wet bulb, etc.).
- Ready for plotting, modeling, or exporting.