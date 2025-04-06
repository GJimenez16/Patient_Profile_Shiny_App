# Patient Profile Shiny App


# {Clinical Dashboard} 

This Shiny application is a modular dashboard built to summarize key metrics from clinical trial data in a clean and dynamic way.  

The app leverages:
- `{bslib}` & `{thematic}` for modern theming
- `{shiny}` modules for reusable components
- `{DT}` for interactive tables
- `{tidyverse}` for data manipulation

---

## App Structure

### Modules

| Module | Purpose |
|--------|---------|
| `mod_valuebox` | Reusable value box component (optional icon support) |

---

## How to Run

```r
# Install required packages
renv::restore()

# Run the app
shiny::runApp()
