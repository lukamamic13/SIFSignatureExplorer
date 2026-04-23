# build_site.R
# Generates the landing page for GitHub Pages using only R

if (!requireNamespace("htmltools", quietly = TRUE)) install.packages("htmltools")

library(htmltools)

# ---------------------------
# Paths
# ---------------------------
dir.create("docs", showWarnings = FALSE)
dir.create("docs/assets", recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# CSS
# ---------------------------
css <- "
:root {
  --bg: #000000;
  --bg-soft: #07110c;
  --panel: rgba(255, 255, 255, 0.05);
  --panel-border: rgba(185, 255, 210, 0.14);
  --text: #f5f7f6;
  --muted: #b6c7be;
  --accent: #9dffb3;
  --accent-strong: #72f59a;
  --shadow: 0 20px 60px rgba(0, 0, 0, 0.45);
  --radius: 22px;
  --max-width: 1200px;
}

* {
  box-sizing: border-box;
}

html, body {
  margin: 0;
  padding: 0;
  min-height: 100%;
  background: var(--bg);
  color: var(--text);
  font-family: 'Inter', sans-serif;
}

body{
  opacity: 0;
  transform: translateY(10px);
  transition: opacity .55s ease, transform .55s ease;
}

body.page-loaded{
  opacity: 1;
  transform: translateY(0);
}

.site-nav a,
.btn,
.multi-select-trigger{
  transition: opacity .2s ease, transform .2s ease, color .2s ease, background .2s ease, border-color .2s ease;
}

.site-nav a:hover,
.btn:hover{
  transform: translateY(-1px);
}


body.landing-page {
  overflow: hidden;
}

a {
  color: inherit;
  text-decoration: none;
}

.hero {
  position: relative;
  min-height: 100vh;
  width: 100%;
  background: var(--bg);
}

.hero-media {
  position: absolute;
  inset: 0;
  overflow: hidden;
}

.hero-gif {
  width: 100%;
  height: 100%;
  object-fit: cover;
  object-position: center;
  filter: brightness(1.2) contrast(1.15) saturate(1.25);
  transform: scale(1.02);
}

.hero-overlay {
  position: absolute;
  inset: 0;
  background:
    linear-gradient(to bottom, rgba(0, 0, 0, 0.15), rgba(0, 0, 0, 0.45)),
    radial-gradient(circle at center, rgba(20, 80, 40, 0.08), rgba(0, 0, 0, 0.5));
}

.hero-content {
  position: relative;
  z-index: 2;
  min-height: 100vh;
  max-width: var(--max-width);
  margin: 0 auto;
  padding: 40px 24px 56px;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  text-align: center;
}

.hero-badge {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  padding: 10px 16px;
  margin-bottom: 20px;
  border: 1px solid var(--panel-border);
  border-radius: 999px;
  background: rgba(0, 0, 0, 0.32);
  color: var(--accent);
  font-size: 0.85rem;
  font-weight: 500;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  backdrop-filter: blur(8px);
}

.hero-title {
  margin: 0;
  font-size: clamp(2.5rem, 6vw, 5.5rem);
  font-weight: 700;
  line-height: 0.96;
  letter-spacing: -0.04em;
  max-width: 900px;
  text-shadow: 0 8px 30px rgba(0, 0, 0, 0.35);
}

.hero-subtitle {
  max-width: 760px;
  margin: 18px 0 0;
  color: var(--muted);
  font-size: clamp(1rem, 2vw, 1.25rem);
  line-height: 1.7;
}

.hero-actions {
  margin-top: 34px;
  display: flex;
  gap: 14px;
  flex-wrap: wrap;
  justify-content: center;
}

.btn {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  min-width: 180px;
  padding: 15px 24px;
  border-radius: 999px;
  font-size: 0.98rem;
  font-weight: 600;
  transition: transform 0.2s ease, box-shadow 0.2s ease, background 0.2s ease, border-color 0.2s ease;
}

.btn:hover {
  transform: translateY(-2px);
}

.btn-primary {
  color: #031108;
  background: linear-gradient(135deg, var(--accent), var(--accent-strong));
  box-shadow: 0 10px 30px rgba(114, 245, 154, 0.22);
}

.btn-primary:hover {
  box-shadow: 0 16px 38px rgba(114, 245, 154, 0.32);
}

@media (max-width: 768px) {
  .hero-content {
    padding: 24px 18px 40px;
  }

  .hero-subtitle {
    line-height: 1.55;
  }

  .btn {
    width: 100%;
    max-width: 280px;
  }
}
"

writeLines(css, "docs/styles.css")

# ---------------------------
# HTML page generator
# ---------------------------
build_index_page <- function(hero_file = "assets/hero.gif") {
  
  page <- tags$html(
    lang = "en",
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
      tags$title("SIF Signature Explorer"),
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap",
        rel = "stylesheet"
      ),
      tags$link(rel = "stylesheet", href = "styles.css")
    ),
    tags$body(
      class = "landing-page",
      tags$main(
        class = "hero",
        
        tags$div(
          class = "hero-media",
          tags$img(
            src = hero_file,
            alt = "Global SIF animation",
            class = "hero-gif"
          ),
          tags$div(class = "hero-overlay")
        ),
        
        tags$div(
          class = "hero-content",
          tags$div(class = "hero-badge", "SIF Signature Explorer"),
          tags$h1(class = "hero-title", "The Pulse of Photosynthesis"),
          tags$p(
            class = "hero-subtitle",
            "How does vegetation shape our planet with solar-induced fluorescence?"
          ),
          tags$div(
            class = "hero-actions",
            tags$a(
              href = "learn.html",
              class = "btn btn-primary",
              "Discover More"
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
  
  save_html(page, file = "docs/index.html", background = "black")
}

# ---------------------------
# Build files
# ---------------------------
# Change this if your file is jpg/jpeg/png for now
build_index_page(hero_file = "assets/hero.gif")

message("Landing page written to docs/index.html")
message("CSS written to docs/styles.css")
message("Put your animation file inside docs/assets/ as hero.gif")

