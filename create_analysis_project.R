
# To create a project on GitHub using the code below one can run: 
#    create_analysis_project("test_decoding")  # create the relevant folder
#    create_github_repo("test_decoding")   # create a new repo on GitHub 


# Perhaps there is another package that can do this too?  
# (usethis can be one needs to use R projects)
# For pushing and pulling to the repo, one can use gert...




create_analysis_project <- function(project_name,
                                    directory_name = file.path("projects")) {
                                    #directory_name = file.path("..", "decoding_analysis_projects")) {


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




# might be better to do most of this through the gert package (or gh package)
# https://docs.ropensci.org/gert/reference/git_commit.html
# https://github.com/r-lib/gh

# Also see https://github.com/r-lib/gitcreds




# Creates a new GitHub repo based in a directory name

# Need to install and set up the GitHub command line utilities
# - Installation on mac: brew install gh
# - Setting up login info:  gh auth login

create_github_repo <- function(project_name,
                           directory_name = file.path("projects"),
                           visibility = "public") {
 
  # visibility must be either: "public", "private" or "internal"
  # (not sure what "internal" does)
  
  
  full_project_path <- file.path(directory_name, project_name)
  
  
  # create a new git repo
  gert::git_init(path = full_project_path)
  
  
  # create a new GitHub repository
  # needs the GitHub CLI installed to run this (https://cli.github.com/)
  # would be good if I could create a repo without needed the GitHub CLI
  # need to set up command line login which requires use of command line so this is not good :(
  system2("gh", paste0("repo create --source ", full_project_path, " --", visibility), stdout = TRUE, stderr = TRUE)
  
  
  
  # add a README.md
  
  # create the README.md, add a title and short information about the repository
  file.create(file.path(full_project_path, "README.md"))
  
  # could add some default text to the README.md as well...
  
  
  gert::git_add(repo = full_project_path, files = "README.md")
  
  
  # Add a main branch  (not sure what the gert version of this is)
  system2("git", paste0("-C ", full_project_path, " branch -M main"), stdout = TRUE, stderr = TRUE)  
  
  
  # Do a first commit
  gert::git_commit(repo = full_project_path, "initial commit adding README.md")
  
  
  system2("git", paste("-C ", full_project_path,  " push origin main"))
  
  
  # set the origin and main for future pulling and pushing
  gert::git_branch_set_upstream("origin/main", repo = full_project_path)
  

  # add the data/ and results/ folders to GitHub
  gert::git_add(repo = full_project_path, files = "data")
  gert::git_add(repo = full_project_path, files = "results")
  gert::git_commit_all(repo = full_project_path, message = "Add data/ and results/ filders")
  gert::git_push(repo = full_project_path)
  
  
}








# Adds all files in the repo that are less than 100 MB
# commit the files to the repo
# pushes the files to GitHub

# use this instead: 
# gert::git_push(repo = ...)

git_push_all <- function(project_name,
                         directory_name = file.path("projects")) {
  
  
  full_repo_path <- file.path(directory_name, project_name)
  
  
  # Need to check all files that are less than 100 MB and add them to the repo
  
  
  
  # use gert functions to commit add, commit and push all files less than 100 MB
  
  
  
  
  # Give a warning for all files that are greater than 100 MB and are not pushed
  
  
}












