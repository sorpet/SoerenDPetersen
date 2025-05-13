#!/usr/bin/env node
/**
 * buildMd.js
 * Simple Node.js script to pre-render Markdown files in the content directory to HTML.
 */
const fs = require('fs');
const path = require('path');
const { marked } = require('marked');

// Directory containing Markdown source files
const srcDir = path.join(__dirname, '../content_md');
// Directory where HTML fragments will be written
const outDir = path.join(__dirname, '../content_html');

function buildMarkdown() {
  // Ensure source directory exists
  if (!fs.existsSync(srcDir)) {
    console.error(`Markdown source directory not found: ${srcDir}`);
    process.exit(1);
  }
  // Ensure output directory exists
  if (!fs.existsSync(outDir)) {
    fs.mkdirSync(outDir, { recursive: true });
  }
  const files = fs.readdirSync(srcDir);
  files.forEach(file => {
    if (path.extname(file) === '.md') {
      const mdPath = path.join(srcDir, file);
      const base = path.basename(file, '.md');
      const htmlPath = path.join(outDir, `${base}.html`);
      try {
        let md = fs.readFileSync(mdPath, 'utf-8');
        // Strip first-level header
        md = md.replace(/^#.*\n/, '');
        const html = marked.parse(md);
        fs.writeFileSync(htmlPath, html);
        console.log(`Generated ${htmlPath}`);
      } catch (err) {
        console.error(`Error processing ${mdPath}:`, err);
      }
    }
  });
}

buildMarkdown();