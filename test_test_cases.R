library(fs)
library(readr)
library(yaml)

test_cases_dir <- "~/Dropbox (2° Investing)/2° Investing Team/People/Jacob/p2020_test_cases"
stopifnot(fs::dir_exists(test_cases_dir))

test_cases_csvs <- fs::dir_ls(test_cases_dir, regexp = "[.]csv")

test_cases_output_dir <- "test_cases"

try(fs::dir_delete(test_cases_output_dir))

for (i in seq_along(test_cases_csvs)) {
  filepath <- test_cases_csvs[[i]]

  test_case <- read_csv(filepath, col_types = cols())
  filename <- fs::path_file(fs::path_ext_remove(filepath))
  portfolio_name <- unique(test_case$Portfolio.Name)
  investor_name <- unique(test_case$Investor.Name)

  # if no unique, valid portfolio_name or investor_name, use the filename
  if (length(portfolio_name) != 1) {
    portfolio_name <- filename
  }
  if (length(investor_name) != 1) {
    investor_name <- filename
  }

  yaml_data <-
    list(default = list(
      parameters =
        list(
          portfolio_name_in = portfolio_name,
          investor_name_in = investor_name,
          peer_group = "pensionfund",
          language = "EN",
          project_code = "CHPA2020"
        )
    ))


  out_dir <- fs::path(test_cases_output_dir, portfolio_name)

  fs::dir_create(out_dir, recurse = TRUE)

  sub_directories_needed <-
    c(
      "00_Log_Files", "10_Parameter_File", "20_Raw_Inputs",
      "30_Processed_Inputs", "40_Results", "50_Outputs"
    )

  lapply(sub_directories_needed, function(sub_dir) {
    fs::dir_create(fs::path(out_dir, sub_dir), recurse = TRUE)
  })

  write_csv(test_case, fs::path(out_dir, "20_Raw_Inputs", paste0(portfolio_name, ".csv")))
  write_yaml(yaml_data, fs::path(out_dir, "10_Parameter_File", paste0(portfolio_name, "_PortfolioParameters.yml")), indent = 4)
  fs::file_copy(
    "working_dir/10_Parameter_File/AnalysisParameters.yml",
    fs::path(out_dir, "10_Parameter_File", "AnalysisParameters.yml")
  )
}


report_git_status <-
  function(repo_roots = ".") {
    for (repo_root in repo_roots) {
      cli::cli_h1(paste0("repo status for: ", repo_root))

      system2("git", c("-C", repo_root, "fetch"), stdout = FALSE)

      repo <- git2r::repository(repo_root)
      local_head <- git2r::revparse_single(repo, "HEAD")
      upstream_head <- git2r::revparse_single(repo, "origin/HEAD")
      local_repo_head <- git2r::repository_head(repo)
      upstream_branch <- git2r::branch_get_upstream(local_repo_head)
      remote_url <- git2r::branch_remote_url(upstream_branch)
      repo_ahead_behind <- git2r::ahead_behind(local_head, upstream_head)

      cat(paste0("Upstream branch: ", upstream_branch$name, " from ", remote_url), "\n")
      if (!all(repo_ahead_behind == 0)) {
        cat(cli::col_red(paste0(c("ahead: ", "behind: "), repo_ahead_behind, collapse = " - "), "\n"))
        cat(paste0("Remote HEAD: ", format(upstream_head)), "\n")
        cat(paste0("Local HEAD: ", format(local_head)), "\n")
      } else {
        cat(cli::col_green("in sync with remote origin"), "\n")
      }
      print(git2r::status(repo))

      owner <- "2DegreesInvesting"
      repo_name <- basename(repo_root)
      open_pr_list <-
        gh::gh("/repos/:owner/:repo/pulls",
          owner = owner, repo = repo_name,
          state = "open", .limit = Inf
        )

      if (length(open_pr_list) > 0) {
        infos <-
          lapply(open_pr_list, function(pr) {
            paste0("#", pr$number, " - ", pr$title, ifelse(pr$draft, " (DRAFT)", ""))
          })
        cat(paste0("Open PRs (", length(open_pr_list), "):\n", paste0(infos, collapse = "\n")), "\n")
      }
    }
  }

report_git_status(
  c(
    "../PACTA_analysis",
    "../create_interactive_report",
    "../StressTestingModelDev",
    "../pacta-data",
    "../user_results"
  )
)


for (csv_num in seq_along(test_cases_csvs)) {
  filepath <- test_cases_csvs[[csv_num]]
  test_case <- read_csv(filepath, col_types = cols())
  filename <- fs::path_file(fs::path_ext_remove(filepath))
  portfolio_name <- unique(test_case$Portfolio.Name)
  sub_directory <- portfolio_name

  # if no unique, valid portfolio_name, use an empty string
  if (length(portfolio_name) != 1) {
    portfolio_name <- ""
  }

  # if no unique, valid portfolio_name, use the filename
  if (length(sub_directory) != 1) {
    sub_directory <- filename
  }

  out_dir <- fs::path(test_cases_output_dir, sub_directory)

  portfolio_name_ref_all <- portfolio_name
  portfolio_root_dir <- out_dir

  cli::cli_h1(paste0("running for: ", portfolio_name_ref_all, " (test #", csv_num, ")"))
  cli::cli_h1(paste0("in: ", portfolio_root_dir))

  try(source("web_tool_script_1.R"))
  try(source("web_tool_script_2.R"))
  try(source("web_tool_script_3.R"))
}
