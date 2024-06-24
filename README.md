R.PEREZ (June 2024)

 # RadiationLog Viewer

-   [Description](#description)
-   [Installation](#install)
-   [Usage](#usage)


## Description {#description}

This Shiny app allows users to format and visualize the data collected data collected on a SD card of phymea data logger connected to PAR sensors. Users can select the data files t o visualize and select the different output recorded by the logger

## Installation {#install}
To run this app, ensure you have installed R and R studio software (https://posit.co/).
Then open the file Light_sensors.Rproj with RStudio. This will help R locate all the necessary data on your computer to run the code.

Once RStudio is open, launch the file App_visuRadiation.R. The code will appear in the RStudio console; click on the "Run App" button in the top right corner of the console window. 

The following packages will be automatically installed:

    shiny
    shinythemes
    shinycssloaders
    lubridate
    stringr
    tidyr
    dplyr
    viridis
    plotly
    
## Usage {#usage} 

Important: the `PAR Time.csv` file must be located in the `0-data` folder. This file must be updated with the name of the data files and the setting and replacing times of the SD card in the logger.

On the right, you'll find a `Browse` button to select the file to read. You can select one a multiple files (using the ship button when clicking on the files). Then the dates range of the selected file(s) will be displayed, in order to easily change the period of data you want to visualize (the default is the whole period). Finally you can select the variable of interest: PAR (default), Far red, Temperature and Battery. The graphic will be automatically updated depending on the selected attributes. On the plot have access to the tools of plotly package, such as zoom or download the graphic for instance.
