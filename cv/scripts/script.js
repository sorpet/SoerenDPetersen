const date = new Date();
const options = { year: 'numeric', month: 'long' };
document.getElementById('cv-date').textContent = date.toLocaleDateString('en-GB', options);

// Load and render personal data, stripping first-level Markdown header
fetch("content/personal_data.html")
  .then(res => res.text())
  .then(html => {
    document.getElementById("personal-data").innerHTML = html;
  })
  .catch(err => console.error("Could not load personal data:", err));


// Load and render research profile, stripping first-level Markdown header
fetch("content/research_profile.md")
  .then(res => res.text())
  .then(md => {
    const cleaned = md
      .replace(/^#.*\n/, '');             // strip top-level header
      // .replace(/\\beta/g, '$\\beta$');   // wrap \beta in $...$
    document.getElementById("research-content").innerHTML = marked.parse(cleaned);
    MathJax.typeset();                   // ✅ this triggers MathJax rendering
  })
  .catch(err => console.error("Could not load research profile:", err));


// Load and render leadership section, stripping first-level Markdown header
fetch("content/leadership.md")
  .then(res => res.text())
  .then(md => {
    const cleaned = md.replace(/^#.*\n/, ''); // removes top-level heading
    document.getElementById("leadership-content").innerHTML = marked.parse(cleaned);
  })
  .catch(err => console.error("Could not load leadership content:", err));

fetch("content/funding.md")
  .then(res => res.text())
  .then(md => {
    const cleaned = md.replace(/^#.*\n/, ''); // remove top-level header
    document.getElementById("funding-content").innerHTML = marked.parse(cleaned);
  })
  .catch(err => console.error("Could not load funding content:", err));

fetch('content/education.html')
  .then(res => res.text())
  .then(md => {
    document.getElementById('education-content').innerHTML = md;
  });

fetch('content/work_experience.html')
  .then(res => res.text())
  .then(md => {
    document.getElementById('experience-content').innerHTML = md;
  });

fetch('content/teaching_courses.html')
  .then(res => res.text())
  .then(md => {
    document.getElementById('teaching-courses-content').innerHTML = marked.parse(md);
  });

fetch('content/teaching_students.html')
  .then(res => res.text())
  .then(md => {
    document.getElementById('teaching-students-content').innerHTML = marked.parse(md);
  });


fetch('content/communication_conferences.html')
  .then(res => res.text())
  .then(md => {
    document.getElementById('communication-conferences-content').innerHTML = md;
  });

fetch('content/communication_workshops.html')
  .then(res => res.text())
  .then(md => {
    document.getElementById('communication-workshops-content').innerHTML = md;
  });

fetch('content/publications.html')
  .then(response => response.text())
  .then(data => {
    document.getElementById('publications-content').innerHTML = data;
  });
