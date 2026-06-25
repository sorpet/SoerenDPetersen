here <- function(...) file.path(getwd(), ...)

message("🔧 Loading utilities...")
source(here("R", "utils_cv.R"))

message("📄 Generating CV sections...")
source(here("scripts", "generate_education.R"))
source(here("scripts", "generate_teaching.R"))
source(here("scripts", "generate_funding.R"))
source(here("scripts", "generate_work_experience.R"))
source(here("scripts", "generate_publications.R"))
source(here("scripts", "generate_publications_summary.R"))
source(here("scripts", "generate_personal_data.R"))
source(here("scripts", "generate_other_communication.R"))

message("✅ All sections generated!")
