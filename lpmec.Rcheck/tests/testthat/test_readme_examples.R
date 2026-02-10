# Tests that extract and run R code blocks from the README
# Ensures README examples stay in sync with actual package behavior

skip_on_cran()

# Helper: extract R code blocks from a markdown file
extract_r_code_blocks <- function(readme_path) {
  lines <- readLines(readme_path)
  blocks <- list()
  in_block <- FALSE
  current_block <- character(0)
  block_lang <- ""

  for (line in lines) {
    if (!in_block && grepl("^```", line)) {
      # Capture the language tag (everything after ```)
      lang <- trimws(sub("^```", "", line))
      in_block <- TRUE
      block_lang <- lang
      current_block <- character(0)
    } else if (in_block && grepl("^```\\s*$", line)) {
      in_block <- FALSE
      # Only keep blocks that are R or unlabeled (potential R)
      if (block_lang %in% c("", "r", "R")) {
        blocks <- c(blocks, list(list(lang = block_lang, code = current_block)))
      }
    } else if (in_block) {
      current_block <- c(current_block, line)
    }
  }
  blocks
}

# Helper: determine if a code block is runnable in tests
is_runnable_block <- function(block) {
  code_text <- paste(block$code, collapse = "\n")
  # Skip install commands
  if (grepl("install_github|install\\.packages", code_text)) return(FALSE)
  # Skip bash/bibtex/non-R blocks
  if (block$lang == "") {
    # Unlabeled blocks that contain R code patterns are still R
    if (!grepl("(library|lpmec|<-|rnorm|matrix)", code_text)) return(FALSE)
  }
  # Skip blocks requiring numpyro backend
  if (grepl("numpyro", code_text)) return(FALSE)
  # Skip build_backend
  if (grepl("build_backend", code_text)) return(FALSE)
  TRUE
}

# Locate README relative to package root
readme_path <- file.path(
  system.file(package = "lpmec"),
  "..", "..", "..", "README.md"
)
# Also try the common development layout
if (!file.exists(readme_path)) {
  # When running via devtools::test(), the working dir is often the package root
  candidates <- c(
    file.path("..", "..", "README.md"),       # lpmec/tests/testthat -> root
    file.path("..", "..", "..", "README.md"),
    "README.md"
  )
  for (cand in candidates) {
    if (file.exists(cand)) {
      readme_path <- cand
      break
    }
  }
}

# Skip all tests if README can't be found (e.g., installed from CRAN tarball)
if (!file.exists(readme_path)) {
  skip("README.md not found (not running from source tree)")
}

blocks <- extract_r_code_blocks(readme_path)
runnable <- Filter(is_runnable_block, blocks)

test_that("README.md contains runnable R code blocks", {
  expect_true(length(runnable) >= 2,
              label = "README should have at least 2 runnable R code blocks")
})

for (i in seq_along(runnable)) {
  code_text <- paste(runnable[[i]]$code, collapse = "\n")
  # Strip comment-only lines for a readable label
  first_code_line <- grep("^[^#]", runnable[[i]]$code, value = TRUE)[1]
  label <- if (!is.na(first_code_line)) {
    trimws(substr(first_code_line, 1, 60))
  } else {
    paste("block", i)
  }

  test_that(paste0("README code block ", i, " runs without error: ", label), {
    # Use a small n_boot/n_partition to keep tests fast
    code_text_fast <- gsub("n_boot\\s*=\\s*\\d+", "n_boot = 2", code_text)
    code_text_fast <- gsub("n_partition\\s*=\\s*\\d+", "n_partition = 1", code_text_fast)

    env <- new.env(parent = globalenv())
    expect_no_error(
      eval(parse(text = code_text_fast), envir = env)
    )
  })
}
