const date = new Date();
const options = { year: 'numeric', month: 'long' };
document.getElementById('cv-date').textContent = date.toLocaleDateString('en-GB', options);

// Helper to load and render content sections
function loadSection(id, url, { stripHeader = false, parseMarkdown = false, math = false } = {}) {
  fetch(url)
    .then(res => {
      if (!res.ok) throw new Error(`HTTP error ${res.status}`);
      return res.text();
    })
    .then(text => {
      let content = text;
      if (stripHeader) {
        content = content.replace(/^#.*\n/, '');
      }
      if (parseMarkdown) {
        content = marked.parse(content);
      }
      document.getElementById(id).innerHTML = content;
      if (math && window.MathJax) {
        MathJax.typeset();
      }
    })
    .catch(err => console.error(`Failed to load ${url}:`, err));
}

// Load all sections (pre-rendered HTML for Markdown files via build step)
loadSection('personal-data', 'content/personal_data.html');
loadSection('research-content', 'content/research_profile.html', { math: true });
loadSection('leadership-content', 'content/leadership.html');
loadSection('funding-content', 'content/funding.html');
loadSection('education-content', 'content/education.html');
loadSection('experience-content', 'content/work_experience.html');
loadSection('teaching-courses-content', 'content/teaching_courses.html');
loadSection('teaching-students-content', 'content/teaching_students.html');
loadSection('communication-conferences-content', 'content/communication_conferences.html');
loadSection('communication-workshops-content', 'content/communication_workshops.html');
loadSection('publication-summary-content', 'content/publication_summary.html');
loadSection('publications-content', 'content/publications.html');
