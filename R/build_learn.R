source("R/build_utils.R")

build_learn_page <- function() {
  ensure_site_dirs()
  write_shared_css()
  
  page <- tags$html(
    lang = "en",
    page_head("Learn SIF | SIF Signature Explorer"),
    tags$body(
      class = "learn-page",
      site_header(active = "learn"),
      
      tags$main(
        class = "page-shell",
        
        tags$section(
          class = "learn-hero",
          tags$div(class = "section-eyebrow", "Discover Solar-Induced Fluorescence"),
          tags$h1(
            class = "learn-title",
            "A more direct view into the living rhythm of vegetation"
          ),
          tags$p(
            class = "learn-lead",
            "Solar-induced fluorescence, or SIF, is a faint glow emitted by plants during photosynthesis. ",
            "Because it is linked to plant function rather than structure, it offers a way to better ",
            "understand vegetation productivity, stress, and seasonal dynamics across ecosystems from space."
          )
        ),
        
        tags$section(
          class = "section-block",
          tags$h2(class = "section-title", "What is SIF?"),
          tags$p(
            class = "section-text",
            "When chlorophyll absorbs sunlight, the absorbed energy is partitioned among photochemistry, heat dissipation, ",
            "and fluorescence. A very small fraction is re-emitted as solar-induced fluorescence. ",
            "That weak signal can be observed remotely and used as an indicator of photosynthetic activity. "
          ),
          
          tags$div(
            class = "figure-block",
            
            tags$img(
              src = "assets/SIF.gif",
              class = "learn-gif",
              alt = "Illustration of solar-induced fluorescence process"
            )
            
          ),
          
          tags$div(
            class = "card-grid",
            
            tags$article(
              class = "info-card",
              tags$div(class = "info-card-number", "01"),
              tags$h3("Plants absorb light"),
              tags$p(
                "Leaves absorb incoming solar radiation, especially in wavelengths linked to chlorophyll activity. ",
                "This captured energy powers the photosynthetic machinery."
              )
            ),
            
            tags$article(
              class = "info-card",
              tags$div(class = "info-card-number", "02"),
              tags$h3("A tiny fraction is re-emitted"),
              tags$p(
                "Not all absorbed energy becomes carbon uptake. A small part is re-emitted as solar-induced fluorescence, ",
                "creating a measurable optical signal."
              )
            ),
            
            tags$article(
              class = "info-card",
              tags$div(class = "info-card-number", "03"),
              tags$h3("That signal reveals function"),
              tags$p(
                "Because SIF is closely linked to photosynthetic processes, it can often reflect productivity and stress ",
                "more directly than greenness alone."
              )
            )
          )
        ),
        
        tags$section(
          class = "section-block",
          tags$h2(class = "section-title", "The physics behind the SIF signal."),
          tags$p(
            class = "section-text",
            "SIF emerges from chlorophyll excitation. After light is absorbed, some energy is used in photochemistry, ",
            "some is dissipated as heat, and some is re-emitted as fluorescence. ",
            "This fluorescence typically appears as a red and far-red glow, with characteristic peaks near 685 nm and 740 nm. ",
            "Because these emissions are tied to the functioning of the photosynthetic apparatus, SIF provides a physiological signal, ",
            "not just a structural one."
          ),
          
          tags$figure(
            class = "figure-block",
            tags$img(
              src = "assets/sifsign.png",
              class = "learn-figure",
              alt = "Characteristic chlorophyll fluorescence signal with peaks near 685 and 740 nanometers"
            ),
            tags$figcaption(
              class = "figure-caption",
              "Characteristic chlorophyll fluorescence emission peaks in the red and far-red spectral regions."
            )
          )
        ),
        
        tags$section(
          class = "section-block",
          tags$h2(class = "section-title", "How can we observe SIF from space?"),
          tags$p(
            class = "section-text",
            "Although SIF is a faint signal, it can be retrieved passively from space using high-resolution spectrometers. ",
            "In this project, we use satellite-based observations from the TROPOMI instrument, which provides global measurements of solar-induced fluorescence at 743 nm with daily coverage. ",
            "These observations are aggregated into weekly time series and grouped by vegetation type and climate zone to derive representative seasonal signatures. ",
            "This ensures a transparent and reproducible framework for comparing vegetation functioning across ecosystems while preserving their temporal dynamics. ",
            "Further details on the data sources, processing steps, and methodology are available on the ",
            tags$a("About page", href = "about.html", style = "color: var(--accent);"),
            "."
          ),
          
          tags$div(
            class = "highlight-panel",
            tags$p(
              class = "highlight-quote",
              "SIF lets us observe not only how green vegetation is, but how actively it is functioning."
            ),
            tags$p(
              class = "highlight-note",
              "This is what makes SIF particularly valuable for remote sensing of crop productivity, ecosystem activity, seasonal change, and stress response."
            )
          )
        ),
        
        tags$section(
          class = "section-block",
          tags$h2(class = "section-title", "What does SIF bring compared to common vegetation indices?"),
          tags$p(
            class = "section-text",
            "Traditional vegetation indices such as normalized difference vegetation index (NDVI) are extremely useful, but they capture canopy structure and greenness. ",
            "SIF adds something different: a window into vegetation function. ",
            "This makes it especially valuable when vegetation remains green but photosynthesis is changing because of drought, heat, air pollution, phenology, or atmospheric demand."
          ),
          
          tags$div(
            class = "comparison-grid",
            
            tags$article(
              class = "comparison-card",
              tags$h3("NDVI and similar indices"),
              tags$ul(
                tags$li("Excellent for canopy greenness and structure"),
                tags$li("Useful, robust, and widely adopted"),
                tags$li("Can decouple from productivity under stress"),
                tags$li("May stay high even when physiology is changing")
              )
            ),
            
            tags$article(
              class = "comparison-card comparison-card-accent",
              tags$h3("SIF"),
              tags$ul(
                tags$li("More directly linked to photosynthetic function"),
                tags$li("Sensitive to productivity and stress dynamics"),
                tags$li("Can better track changes when canopies remain green"),
                tags$li("Supports climate- and vegetation-specific interpretation")
              )
            )
          ),
          
          tags$div(
            class = "center-callout",
            
            tags$p(
              class = "center-callout-title",
              "See this comparison in action"
            ),
            
            tags$p(
              class = "center-callout-text",
              "You can compare how SIF, gross primary productivity (GPP), and NDVI relationships change across vegetation types and climate zones ",
              "- observe how SIF remains closely linked to productivity where greenness diverge."
            ),
            
            tags$a(
              href = "relationships.html",
              class = "btn btn-primary",
              "Go to Relationships"
            )
          )
        ),
        
        tags$section(
          class = "footer-cta",
          
          tags$h2(class = "section-title", "Ready to explore the signatures?"),
          
          tags$p(
            "This platform brings together scientific context, comparative analysis, and interactive exploration in one place. ",
            "We designed it to make SIF easier to understand, easier to compare with commonly used vegetation indices, ",
            "and easier to apply across crops and climate zones through a clear and portable web interface."
          ),
          
          tags$div(
            class = "learn-actions",
            tags$a(href = "explorer.html", class = "btn btn-primary", "Go to Explorer"),
            tags$a(href = "about.html", class = "btn btn-secondary", "Go to About")
          )
        ),
        
        tags$section(
          class = "section-block references-block",
          tags$h2(class = "section-title", "Selected references"),
          tags$ul(
            class = "reference-list",
            
            tags$li(
              "Guanter, L., et al. (2015). ",
              tags$em("Potential of the TROPOspheric Monitoring Instrument (TROPOMI) onboard the Sentinel-5 Precursor for the monitoring of terrestrial chlorophyll fluorescence"),
              " ",
              tags$a("(read)", href = "https://amt.copernicus.org/articles/8/1337/2015/", target = "_blank", style = "color: var(--accent);")
            ),
            
            tags$li(
              "Mohammed, G. H., et al. (2019). ",
              tags$em("Remote sensing of solar-induced chlorophyll fluorescence (SIF) in vegetation: 50 years of progress"),
              " ",
              tags$a("(read)", href = "https://www.sciencedirect.com/science/article/abs/pii/S0034425719301816", target = "_blank", style = "color: var(--accent);")
            ),
            
            tags$li(
              "Köhler, P., et al. (2018). ",
              tags$em("Global Retrievals of Solar-Induced Chlorophyll Fluorescence With TROPOMI: First Results and Intersensor Comparison to OCO-2"),
              " ",
              tags$a("(read)", href = "https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2018GL079031", target = "_blank", style = "color: var(--accent);")
            ),
            
            tags$li(
                "Mamić, L., et al. (2025). "
              ,
              tags$em("Solar-induced fluorescence as a robust proxy for vegetation productivity across climate zones and vegetation types in the United States."),
              " ",
              tags$a(
                "(read)",
                href = "https://www.sciencedirect.com/science/article/pii/S2352938525003131",
                target = "_blank",
                style = "color: var(--accent);"
              )
            )
          )
        )
      ),
      
      tags$script(HTML("
document.addEventListener('DOMContentLoaded', function() {
  requestAnimationFrame(() => {
    document.body.classList.add('page-loaded');
  });

  const internalLinks = document.querySelectorAll('a[href$=\".html\"]');
  internalLinks.forEach(link => {
    link.addEventListener('click', function(e) {
      const href = link.getAttribute('href');
      if (!href) return;

      e.preventDefault();
      document.body.classList.remove('page-loaded');

      setTimeout(() => {
        window.location.href = href;
      }, 220);
    });
  });
});
"))
    )
  )
  
  save_html(page, file = "docs/learn.html", background = "black")
  message("Learn page written to docs/learn.html")
}

build_learn_page()

