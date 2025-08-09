# Cal Poly MBB Summer Tracking Web App

A Shiny web application for tracking Cal Poly Men’s Basketball summer practice and scrimmage statistics.

## Features

- **Stats Tab**: View offensive and defensive success rates and opportunities by player and team.
- **Counting Stats Tab**: Displays paint touches per possession and other counting stats.
- **Trends Tab**: Visualize trends for selected stats over time for specific players or the team.
- **File Selection**: Dynamically choose which CSV files to include for analysis.
- **Custom Styling**: Uses Cal Poly's official colors and Montserrat font.

## Running the App Locally

1. **Clone the repository:**
   ```bash
   git clone https://github.com/YOUR-USERNAME/CalPolyMBB-SummerTracking.git
   cd CalPolyMBB-SummerTracking
   ```

2. **Open in RStudio** and ensure the following packages are installed:
   ```r
   install.packages(c("shiny", "DT", "dplyr", "readr", "ggplot2", "bs4Dash", "bslib", "shinyWidgets"))
   ```

3. **Run the app** in R:
   ```r
   shiny::runApp()
   ```

---

## Folder Structure

```plaintext
CalPolyMBB-SummerTracking
├── Defense/                 # Default defensive stat CSVs (optional)
├── Offense/                 # Default offensive stat CSVs (optional)
├── server.R                  # Main Shiny server logic
├── ui.R                      # Main Shiny UI layout
├── stat_definitions.R        # Centralized stat group and mapping definitions
├── app.log                   # Optional runtime logs
├── Summer Stat Tracking Site.Rproj
├── .gitignore
└── README.md                 # You are here
```

---

## Deployment

The app is deployed on RShiny servers.

---

## Author

**Yashwant Sathish Kumar** – Director of Analytics, Cal Poly Men’s Basketball  
