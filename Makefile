DP_DATA_ROOT ?= $(abspath ../dp-data)
export DP_DATA_ROOT

.PHONY: restore analysis figures paper manuscript format lint test check ci-docker clean

restore:
	Rscript -e 'renv::restore(prompt = FALSE)'

analysis:
	Rscript scripts/run_all.R

figures: analysis
	Rscript scripts/figures.R

paper: figures
	$(MAKE) manuscript

manuscript:
	Rscript -e 'options(tinytex.install_packages = FALSE); rmarkdown::render("ms/main.Rmd", knit_root_dir = getwd(), quiet = TRUE)'

format:
	Rscript -e 'styler::style_dir("R"); styler::style_dir("scripts"); styler::style_dir("tests")'

lint:
	Rscript -e 'l <- unlist(lapply(c("R", "scripts", "tests"), lintr::lint_dir), recursive = FALSE); print(l); quit(status = as.integer(length(l) > 0))'

test:
	Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

check: paper lint test

ci-docker:
	docker run --rm -e DP_DATA_ROOT=/dp-data -v "$(DP_DATA_ROOT):/dp-data:ro" \
		-v "$(CURDIR):/project" -w /project rocker/verse:4.6.0 \
		bash -lc "Rscript -e 'install.packages(\"renv\", repos = \"https://cloud.r-project.org\")' && make restore check"

clean:
	Rscript -e 'unlink("build", recursive = TRUE)'
