# SEA-AD RShiny Neuropathology Viewer
RShiny app to visualize neuropathology images

## Local Use
Clone the repository from GitHub or download the source code from the [release page](https://github.com/AllenInstitute/seaad-neuropath-shiny/releases/tag/v0.1.0).

```sh
git clone --branch v0.1.0 --depth 1 git@github.com:AllenInstitute/seaad-neuropath-shiny.git
```

Run the app in RStudio / using R. 
```sh
install.packages("shiny")
library(shiny)
setwd("<GitHub repo folder>")
shiny::runApp("./basic_app")
```

## Goals
- Increased stain, region, donor comparison
- Easy to maintain and update
- RShiny host

WIP:
- incorporate quantitative np for donor selection
- reorganize layer selection information
- sync views for zooming in within the same region
- Cap the donor count: show how many are being shown (10?)

## Version information
- **Currently stable:** v0.1.0 (local)

#### 260910 Image Preview
<img height="100" alt="home_example" src="https://github.com/user-attachments/assets/6ee0317f-44b7-4cfa-a3c7-94e271e239fd"/> &emsp; <img height="100" alt="compare_regions_example" src="https://github.com/user-attachments/assets/8c05b479-49d4-4736-97f9-4995c4af7e86" /> &emsp; <img height="100" alt="about_page" src="https://github.com/user-attachments/assets/43bf4748-8844-4c3f-81de-febcb2e299b8" />


