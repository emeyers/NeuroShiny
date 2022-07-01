

create_analysis_project <- function(project_name,
                                    directory_name = file.path("..", "decoding_analysis_projects")) {


  full_project_path <- file.path(directory_name, project_name)

  if (exists(full_project_path)) {
    stop(paste0("A project called ", full_project_path,
                " already exists. Please specify a different project name and/or directory_name"))
  }


  # create the project directory
  dir.create(full_project_path, recursive = TRUE)


  # create the data directories
  dir.create(file.path(full_project_path, "data"))
  dir.create(file.path(full_project_path, "data", "raster_data"))
  dir.create(file.path(full_project_path, "data", "binned_data"))

  file.create(file.path(full_project_path, "data", "raster_data", ".gitkeep"))
  file.create(file.path(full_project_path, "data", "binned_data", ".gitkeep"))


  # create result directories
  dir.create(file.path(full_project_path, "results", "decoding_results"), recursive = TRUE)

  dir.create(file.path(full_project_path, "results", "decoding_results", "r_markdown"))
  dir.create(file.path(full_project_path, "results", "decoding_results", "r_markdown_pdfs"))
  dir.create(file.path(full_project_path, "results", "decoding_results", "r_scripts"))
  dir.create(file.path(full_project_path, "results", "decoding_results", "decoding_result_files"))

  file.create(file.path(full_project_path, "results", "decoding_results", "r_markdown", ".gitkeep"))
  file.create(file.path(full_project_path, "results", "decoding_results", "r_markdown_pdfs", ".gitkeep"))
  file.create(file.path(full_project_path, "results", "decoding_results", "r_scripts", ".gitkeep"))
  file.create(file.path(full_project_path, "results", "decoding_results", "decoding_result_files", ".gitkeep"))


}




# Should add functions to push/pull the project files to a GitHub repository...













