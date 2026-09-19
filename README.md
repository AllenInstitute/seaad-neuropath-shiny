# SEA-AD RShiny Neuropathology Viewer
RShiny app to visualize neuropathology images

## Local Use
Clone the repository from GitHub or download the source code from the [release page](https://github.com/AllenInstitute/seaad-neuropath-shiny/releases/tag/v0.2.0).

```sh
git clone --branch v0.2.0 --depth 1 git@github.com:AllenInstitute/seaad-neuropath-shiny.git
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
- Use quantitative np for donor selection 
- Easy to maintain and update
- RShiny host

## Version information
- **Currently stable:** v0.2.0 (local)

#### 260918 Image Preview
<img height="100" alt="Screenshot 2026-09-18 at 6 47 11 PM" src="https://github.com/user-attachments/assets/d5f59e1e-d3f4-4238-b743-7a8b75a0593b" />
<img height="100" alt="Screenshot 2026-09-18 at 6 48 38 PM" src="https://github.com/user-attachments/assets/90b43c61-d32d-48da-a15f-bd82145cb8c2" />
<img height="100" alt="Screenshot 2026-09-18 at 6 48 28 PM" src="https://github.com/user-attachments/assets/1664e74e-11d1-4a18-be30-1bc97b119ac3" />


<img height="100" alt="Screenshot 2026-09-18 at 6 49 06 PM" src="https://github.com/user-attachments/assets/0a92e288-8eb5-4e26-aa00-3da677ab7ed5" />
<img height="100" alt="Screenshot 2026-09-18 at 6 49 41 PM" src="https://github.com/user-attachments/assets/4e5a5a69-c51c-4c96-81f9-a13ea8a8da51" />



