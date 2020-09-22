# Create a temporary directory for the output
output <- fs::path(tempdir(), "output")
fs::dir_create(output)
# It's still empty
fs::dir_ls(output)

# yaml <- c(
#   "---",
#   "title: 'Climate Scenario Analysis Report'",
#   "output: bookdown::gitbook",
#   "css: 2dii_gitbook_style.css",
#   "site: bookdown::bookdown_site",
#   "documentclass: book",
#   "---"
# )

yaml <- c(
  '--- ',
  'pagetitle: "Interactive Portfolio Report"',
  'output: ',
  ' bookdown::gitbook:',
  '   css: "2dii_gitbook_style.css"',
  '   toc_depth: 1',
  '   split_by: "none"',
  '   number_sections: TRUE',
  '   config:',
  '     fontsettings: false',
  '     info: false',
  '     sharing: false',
  '     toc:',
  '       collapse: section',
  '---'
)

report_source <- "~/git/report-template"

index_rmd <- fs::dir_ls(report_source, regexp = "index[.]Rmd")
other_rmd <- fs::dir_ls(report_source, regexp = "_0.*[.]Rmd")
other_rmd_lines <- lapply(other_rmd, readLines, encoding = "UTF-8")
template <- unlist(append(yaml, other_rmd_lines))

# Create a temporary directory where to render a book
tmp <- path(tempdir(), "tmp")
if (!fs::dir_exists(tmp)) fs::dir_create(tmp)

# Write the compiled template rmarkdown document
index_rmd_tmp <- fs::path(report_source, "index.Rmd")
writeLines(template, index_rmd_tmp)

config <- fs::dir_ls(report_source, regexp = "[.]css|[.]yml|[.]yaml")
fs::file_copy(config, path(tmp, fs::path_file(config)), overwrite = TRUE)
fs::dir_ls(tmp)

withr::with_dir(tmp, {
  bookdown::render_book(
    path(tmp, "index.Rmd"), 
    output_dir = output
  )
})

fs::dir_ls(output)
fs::file_move(
  path(output, "book.html"), 
  path(output, "template.html")
)
fs::dir_ls(output)

report <- path(output, "report")
fs::dir_create(report)

create_interactive_report(
  repo_path,
  template_dir = output,
  company_charts_dir,
  output_dir = report,
  project_name,
  investor_name,
  portfolio_name,
  start_year,
  select_scenario = scenario,
  portfolio_allocation_method,
  scenario_geography,
  twodi_sectors = c('Power', 'Automotive', 'Shipping', 'Oil&Gas', 'Coal', 'Steel', 'Cement', 'Aviation'),
  gbtech_sectors = c('Power', 'Automotive', 'Oil&Gas', 'Coal'),
  green_techs = c('RenewablesCap', 'HydroCap', 'NuclearCap', 'Hybrid', 'Electric'),
  tech_roadmap_sectors = c('Automotive', 'Power', 'Oil&Gas', 'Coal'),
  audit_file,
  emissions,
  equity_results_portfolio,
  bonds_results_portfolio,
  equity_results_company,
  bonds_results_company,
  equity_results_map,
  bonds_results_map,
  indicies_equity_results_portfolio,
  indicies_bonds_results_portfolio,
  peers_equity_results_portfolio,
  peers_bonds_results_portfolio
)

# Explore the output report
fs::dir_ls(report)
# browseURL(fs::path(report, "index.html"))

# Cleanup
dir_delete(report)
dir_delete(output)
dir_delete(tmp)

file_delete(path(report_source, "template.Rmd"))
