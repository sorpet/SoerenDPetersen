# CV Site Build & Development

## Overview

This project is a static CV website generated from Markdown and HTML content. Source Markdown lives in `content_md/` and is pre-rendered to HTML into `content_html/`. It uses:
- R scripts (in `scripts/*.R`) to generate some HTML snippets
- A Node.js build step to pre-render Markdown (`.md`) files to HTML
- A centralized client-side loader (`scripts/script.js`) for inserting content

## Prerequisites

- Node.js (v14+ recommended)
- (Optional) R and renv to run existing R scripts

## Setup

1. Install Node.js dependencies:
   ```bash
   npm install
   ```
2. Pre-render Markdown to HTML:
   ```bash
   npm run build:md
   ```
3. Generate any R-based content (if needed):
   ```bash
   Rscript scripts/generate_publications.R
   # or run your existing R scripts
   ```

## Usage

- Edit source Markdown in `content_md/*.md` and re-run `npm run build:md` to regenerate HTML fragments (output HTML lands in `content_html/`).
- Open `main.html` in your browser (no server required).
- The client-side script (`scripts/script.js`) will load content from `content/*.html`.

## Development Notes

- All inline styles have been moved to `style.css`. Add new styles in CSS classes.
- CSS custom properties (variables) are defined in `:root` for easy theming.
- Mobile responsiveness is handled via a simple media query at max-width 600px.
- Accessibility improvements:
  - Use `alt` attributes on images.
  - Use `aria-labelledby` and `<nav>` in future enhancements.

## Future Enhancements

- Add a link-checker or test suite to catch broken links before deployment.
- Optimize assets (SVG, JPEG, PNG) with tools like `svgo` or `imagemin`.
- Consolidate content filenames to use a consistent naming convention (e.g., hyphens).