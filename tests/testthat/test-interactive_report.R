test_that("peergroup correct",{

  expect(peer_group %in% c("pensionfund", "assetmanager", "bank" ,"insurance"))

})

test_that("language valid",{

  expect(language_select %in% c("EN", "DE", "FR"))

})


test_that("template folder exists for selected language",{

  expect(dir.exists(paste0(template_path, template_name)))

})

test_that("template file exists for selected language",{

  expect(file.exists(paste0(template_path, template_name, "/template.html")))

})


test_that("peers file exist",{

  expect(file.exists(paste0(analysis_inputs_path, "Peers_equity_results_portfolio.rda")) &
           file.exists(paste0(analysis_inputs_path, "Peers_bonds_results_portfolio.rda")))

})

test_that("index file exist",{

  expect(file.exists(paste0(analysis_inputs_path, "Indices_bonds_portfolio.rda")) &
           file.exists(paste0(analysis_inputs_path, "Indices_equity_portfolio.rda")))

})
