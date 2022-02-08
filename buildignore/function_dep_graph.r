#remotes::install_github("datastorm-open/DependenciesGraphs")
require(DependenciesGraphs)
?funDependencies

deps <- funDependencies("package:cheem", "cheem_ls")
plot(deps)
