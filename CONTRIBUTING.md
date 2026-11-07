# Contributing to the NOAA Fisheries Integrated Modeling System

External contributions and feedback are important to the development and future maintenance of the Fisheries Integrated Modeling System (FIMS) and are welcome. This section provides contributing guidelines and workflows for developers of FIMS and the ecosystem surrounding FIMS. All contributors, both internal and external, are required to abide by the [Code of Conduct](https://github.com/NOAA-FIMS/.github/blob/main/CODE_OF_CONDUCT.md).

## TLDR --- I just have a question

Please use [GitHub Discussions](https://github.com/orgs/NOAA-FIMS/discussions) if you have a question. There is a special category set up for [Q&A](https://github.com/orgs/NOAA-FIMS/discussions/categories/q-a). But, you should also check out the other categories to see if your question has already been answered.

## Found a :bug:

Please use [GitHub Issues](https://github.com/NOAA-FIMS/FIMS/issues/new?template=bug_report.yml) to submit a :bug: report. If you have an idea for a well-defined enhancement to FIMS, please use GitHub to submit a [Feature request](https://github.com/NOAA-FIMS/FIMS/issues/new?template=feature_request.yml). Before submitting either of these tickets, please search the [existing issues](https://github.com/NOAA-FIMS/FIMS/issues?q=is%3Aissue) to see if something identical or similar has already been submitted. Once created, Issues are automatically tagged with the `status: triage_needed` tag.

## Contributing code

### Non-code contributions

Not all contributions are code! Writing documentation, teaching, and other activities are also key parts of contributing to software that are much appreciated. For more information about open source contributions, see [the Open Source Guide's How to Contribute page](https://opensource.guide/how-to-contribute/). A good place to start is by looking for open issues with the [good first issue label](https://github.com/NOAA-FIMS/FIMS/contribute). These issues are generally small and well-defined, making them a good starting point for new contributors. We encourage collaborators to feel welcome to contribute in any way they can, and we will do our best to incorporate these contributions into the FIMS projects. Please see the [feature branches](#feature-branches), [pull requests](#pull-requests), and [style guide](#style-guide) sections below for how to make your contributions to FIMS. We thank you in advance for being part of the team. 

Additionally, if you would like to make contributions to the [NOAA-FIMS website](https://noaa-fims.github.io/) (write a blog, etc.), please follow this [contributing guide](https://github.com/NOAA-FIMS/noaa-fims.github.io/blob/main/CONTRIBUTING.md).

### Adding a new module

If you are adding a new module to FIMS (e.g., a new selectivity function, recruitment model, or other population dynamics component), start with the guide titled [Adding a New Module](https://noaa-fims.github.io/FIMS/articles/adding-new-module.html).

That guide explains the current architecture in detail. The quick checklist below is intended to help you keep track of the files and workflows you will usually need to touch.

#### Quick checklist for new module work

**R implementation**

- [ ] Write the module function in the file for its family, e.g., `R/selectivity.R`, with a roxygen comment giving the equation, the scale of each parameter, and `@noRd`.
- [ ] Add 1 line to the family's lookup table at the bottom of the same file, keyed by the `module_type`.
- [ ] Do not branch on a parameter's value, and follow the rules in the Adding a New Module vignette for values that depend on parameters (`RTMB::ADoverload()`, `RTMB::matrix()`, `RTMB::rowSums()`).

**Defaults and initialization**

- [ ] Update `R/setup_default_parameters.R` so default values and estimation types are generated correctly.
- [ ] If the module needs inputs that are not parameters, add an `inputs` function, as `growth_modules` does in `R/growth.R`. `initialize_model_data()` in `R/model_data.R` calls it.

**Testing**

- [ ] Add testthat tests with known values for the module function, using `FIMS:::use_testthat_template()`.
- [ ] Add testthat tests for user-facing behavior through `initialize_fims()` and `fit_fims()`, with edge cases where relevant.

**Validation and follow-up**

- [ ] Complete the tasks found in the [Standard contributor checks](#standard-contributor-checks) section
- [ ] Update the [Adding a New Module vignette](https://noaa-fims.github.io/FIMS/articles/adding-new-module.html) itself if your changes alter the recommended workflow.
- [ ] Provide feedback to the developers on how helpful the vignette was to your development process.

### Feature branches

Once an issue is approved and prioritized for work, a feature branch off of the main branch, e.g., `parametric-discards`, can be created to address the issue. Feature branches can be made within the repository or on a fork of the repository. There are no hard and fast rules for branch names but we tend to use dashes to separate words. Often while working on this new feature branch, there will be updates to the main branch. These updates must be brought in using `git rebase`. Branches that have merge commits will not be accepted into the code base. Please see the [GitHub documentation for rebasing](https://docs.github.com/en/get-started/using-git/about-git-rebase) for more information.

Along the development process it is important to add tests and ensure that the code is well documented. Below are the standard contributor checks to follow so that the code is ready for a pull request:

#### Standard contributor checks

* Spell check the package using [cspell](https://cspell.org/). Install with `npm install -g cspell` and run `cspell "**/*.{R,Rmd,md}"` from the package root. Add any terms needed to [`NOAA-FIMS/.github/cspell.json`](https://github.com/NOAA-FIMS/.github/blob/main/cspell.json).
* Build the roxygen documentation using `devtools::document()`. See the [r-lib](https://roxygen2.r-lib.org/) for all available tags and best practices.
* Implement the suite of testthat tests using `devtools::test()`.
* Run `styler::style_pkg()` to style R code.
* Run `usethis::use_tidy_description()` to update the DESCRIPTION file.
* Run `devtools::check()` to ensure the package can be compiled and R tests pass. If there are failing tests, run `devtools::test(filter = "file_name")` (where "test-file_name.R" is the testthat file containing failing tests) and edit code/tests to troubleshoot tests. During development, run `devtools::build()` locally to build the package more frequently and faster.
* Build the package down site using `pkgdown::build_site()`.
* Run the code coverage report using `covr::report()` to ensure that all of the code is covered, with the goal of maintaining 80 percent code coverage.

### Pull requests

Once the work is complete, a pull request should be created to bring the changes into the appropriate branch. Pull requests from forks will be reviewed under the same guidelines as pull requests from internal branches. Review of the pull request will be both automated and manual, where all pull requests must pass automated tests and syntax checks before being merged in addition to being reviewed by at least one developer. [Code review](https://github.com/features/code-review) ensures health and continuous improvement of the FIMS codebase, while simultaneously helping FIMS developers become familiar with the codebase. [Google's code review guide](https://google.github.io/eng-practices/review/) provides a useful set of guidelines for both reviewers and code authors. Each PR is accompanied by a [checklist of major considerations for code reviews](https://github.com/NOAA-FIMS/FIMS/blob/main/.github/workflows/pr-checklist.yml) to guide reviewers through the PR. Additional guidance can be found at [Conventional Comments](https://conventionalcomments.org/) for formatting/structuring your reviewer comments. Navigate to the [Perforce Blog](https://www.perforce.com/blog/qac/9-best-practices-for-code-review) for nine best practices for code review and use the [FIMS Style Guide](#style-guide) to settle any style arguments.

An automated workflow ([pr-format.yml](https://github.com/NOAA-FIMS/FIMS/blob/main/.github/workflows/pr-format.yml)) runs when you open or update a pull request. This workflow checks R code formatting. If you are contributing from the main repository, the workflow will automatically create a pull request with formatting changes that need to be merged into your feature branch. If you are contributing from a fork, the workflow will add a comment to your pull request with detailed instructions on how to format your code locally, since the automated workflow cannot push changes to forks.

Versioning follows [semantic versioning](https://semver.org/) to establish the major, minor, and patch number for a release. Often, multiple pull requests are included in a single release, which will be initiated by a member of the FIMS Implementation Team. Release notes for each version can be found in the [release notes](https://github.com/NOAA-FIMS/FIMS/releases).

## Style guide

Style guides are used to ensure the code is consistent, easy to use (e.g., read, share, and verify) and ultimately easier to write. We use the [tidyverse style guide](https://style.tidyverse.org/) for R code.


Commit messages communicate details about changes that have occurred to collaborators and improve team efficiency. The best guidance for how to create an excellent commit message can be found on [Conventional Commits](https://www.conventionalcommits.org), which is our style guide for commit messages. See [Conventional Comments](https://conventionalcomments.org/) for how to format reviewer comments.
