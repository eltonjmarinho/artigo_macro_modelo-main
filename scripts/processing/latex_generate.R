# Gera tabelas em LaTeX a partir dos relatórios CSV produzidos pelos modelos GMM

suppressWarnings({
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
})

library(dplyr)
library(readr)
library(stringr)

reports_dir <- "reports"
latex_dir <- file.path(reports_dir, "latex")

if (!dir.exists(reports_dir)) {
  stop("A pasta 'reports' não existe. Execute os modelos antes de gerar o LaTeX.")
}

if (!dir.exists(latex_dir)) {
  dir.create(latex_dir, recursive = TRUE)
  message("Pasta 'reports/latex' criada.")
}

safe_read <- function(path) {
  if (!file.exists(path)) {
    message(sprintf("Arquivo %s não encontrado. Pulando.", path))
    return(NULL)
  }
  suppressMessages(readr::read_csv(path, show_col_types = FALSE, progress = FALSE))
}

latex_escape <- function(texts) {
  if (is.null(texts)) return("")
  vapply(texts, function(txt) {
    if (is.na(txt)) return("")
    txt <- gsub("\\\\", "\\\\textbackslash{}", txt)
    txt <- gsub("([%$#_{}&])", "\\\\\\1", txt, perl = TRUE)
    txt <- gsub("~", "\\\\textasciitilde{}", txt, fixed = TRUE)
    gsub("\\^", "\\\\textasciicircum{}", txt)
  }, character(1))
}

find_p_value_column <- function(df) {
  candidates <- c("Pr(>|t|)", "Pr(>|z|)", "Pr(>|Z|)", "p.value", "p_value", "pvalue", "p-val", "pval")
  match <- candidates[candidates %in% names(df)]
  if (length(match) == 0) return(NULL)
  match[1]
}

format_with_stars <- function(estimate, p_value, digits = 4) {
  estimate <- suppressWarnings(as.numeric(estimate))
  p_value <- suppressWarnings(as.numeric(p_value))

  if (length(estimate) == 0 || is.na(estimate)) {
    return("")
  }

  if (length(p_value) == 0) {
    p_value <- NA_real_
  }

  stars <- ifelse(is.na(p_value), "",
                  ifelse(p_value < 0.01, "***",
                         ifelse(p_value < 0.05, "**",
                                ifelse(p_value < 0.1, "*", ""))))
  sprintf(paste0("%0.", digits, "f%s"), estimate, stars)
}

format_se <- function(se, digits = 4) {
  se <- suppressWarnings(as.numeric(se))
  if (length(se) == 0 || is.na(se)) return("")
  sprintf(paste0("(%0.", digits, "f)"), se)
}

format_test_cell <- function(statistic, p_value, digits_stat = 3, digits_p = 3) {
  stat_num <- suppressWarnings(as.numeric(statistic))
  p_num <- suppressWarnings(as.numeric(p_value))

  has_stat <- length(stat_num) > 0 && !is.na(stat_num)
  has_p <- length(p_num) > 0 && !is.na(p_num)

  if (!has_stat && !has_p) {
    return("")
  }

  stat_str <- if (has_stat) formatC(stat_num, digits = digits_stat, format = "f") else ""

  if (!has_p) {
    return(stat_str)
  }

  p_str <- sprintf("(p=%s)", formatC(p_num, digits = digits_p, format = "f"))

  if (!has_stat) {
    return(sprintf("\\begin{tabular}[c]{@{}c@{}}%s\\end{tabular}", p_str))
  }

  sprintf("\\begin{tabular}[c]{@{}c@{}}%s\\\\%s\\end{tabular}", stat_str, p_str)
}

pick_metric_column <- function(df, ...) {
  candidates <- c(...)
  candidates <- candidates[!is.null(candidates)]
  candidates <- candidates[candidates %in% names(df)]
  if (length(candidates) == 0) return(NULL)
  candidates[1]
}

sanitize_suffix <- function(text) {
  if (is.null(text) || is.na(text)) {
    return("grupo")
  }

  slug <- text %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")

  if (slug == "") "grupo" else slug
}

build_model_labels <- function(df, extra_cols = NULL, include_income = TRUE) {
  if (!"model_variant" %in% names(df)) {
    df$model_variant <- NA_character_
  }
  parts <- list()
  if (!is.null(extra_cols)) {
    for (col in extra_cols) {
      if (col %in% names(df)) {
        parts[[length(parts) + 1]] <- df[[col]]
      }
    }
  }
  if (include_income && "income_group" %in% names(df)) {
    parts[[length(parts) + 1]] <- df$income_group
  }
  parts[[length(parts) + 1]] <- df$model_variant

  if (length(parts) == 0) {
    return(rep("Modelo", nrow(df)))
  }

  part_mat <- do.call(cbind, parts)
  apply(part_mat, 1, function(row_vals) {
    row_vals <- row_vals[!is.na(row_vals) & row_vals != ""]
    if (length(row_vals) == 0) {
      "Modelo"
    } else {
      paste(row_vals, collapse = " - ")
    }
  })
}

generate_coef_table <- function(df, caption, filename, dep_label = "log_gdp", include_income = TRUE, test_df = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    message(sprintf("Sem dados para gerar %s", filename))
    return()
  }

  if (!"model_label" %in% names(df)) {
    df <- df %>% mutate(model_label = build_model_labels(df, extra_cols = c("area_label"), include_income = include_income))
  }

  if (!is.null(test_df)) {
    if (!"model_label" %in% names(test_df)) {
      test_df <- test_df %>% mutate(model_label = build_model_labels(test_df, extra_cols = c("area_label"), include_income = include_income))
    }
  }

  if (is.factor(df$model_label)) {
    level_order <- levels(df$model_label)
    used_levels <- unique(as.character(df$model_label))
    ordered_levels <- level_order[level_order %in% used_levels]
    df$model_label <- as.character(df$model_label)
  } else {
    ordered_levels <- NULL
  }
  if (!is.null(test_df) && "model_label" %in% names(test_df) && is.factor(test_df$model_label)) {
    test_df$model_label <- as.character(test_df$model_label)
  }

  term_levels <- unique(df$term)
  model_levels <- if (!is.null(ordered_levels) && length(ordered_levels) > 0) ordered_levels else unique(df$model_label)
  p_value_col <- find_p_value_column(df)

  header <- c(
    "\\begin{table}[H]",
    "\\centering",
    sprintf("\\caption{%s}", caption),
    "\\resizebox{\\textwidth}{!}{%",
    sprintf("\\begin{tabular}{l*{%d}{c}}", length(model_levels)),
    "\\hline",
    sprintf(" & \\multicolumn{%d}{c}{\\textbf{Variável dependente: %s}} \\\\", length(model_levels), dep_label),
    sprintf("\\cline{2-%d}", length(model_levels) + 1),
      paste0(
        " & ",
        paste(sprintf("(%d)", seq_along(model_levels)), collapse = " & "),
        " ",
        "\\\\")
  )

  body <- c("\\hline")

  for (term in term_levels) {
    row_data <- df[df$term == term, , drop = FALSE]
    est_row <- c(latex_escape(term))
    se_row <- c("")

    for (model in model_levels) {
      cell <- row_data[row_data$model_label == model, , drop = FALSE]
      if (nrow(cell) == 0) {
        est_row <- c(est_row, "")
        se_row <- c(se_row, "")
      } else {
        p_val <- if (!is.null(p_value_col) && p_value_col %in% names(cell)) cell[[p_value_col]][1] else NA_real_
        est_row <- c(est_row, format_with_stars(cell$Estimate[1], p_val))
        se_row <- c(se_row, format_se(cell$`Std. Error`[1]))
      }
    }

    body <- c(body,
              paste(est_row, collapse = " & "), "\\\\",
              paste(se_row, collapse = " & "), "\\\\")
  }

  if (!is.null(test_df)) {
    metric_specs <- list(
      list(
        label = "Teste Sargan",
        stat_col = "sargan_stat",
        p_col = "sargan_p_value",
        fallback_stat = "hansen_stat",
        fallback_p = "hansen_p_value"
      ),
      list(
        label = "Autocorrelação AR(2)",
        stat_col = "ar2_statistic",
        p_col = "ar2_p_value"
      )
    )

    body <- c(body, "\\hline")

    for (spec in metric_specs) {
      stat_col <- pick_metric_column(test_df, spec$stat_col, spec$fallback_stat)
      p_col <- pick_metric_column(test_df, spec$p_col, spec$fallback_p)

      values <- c(spec$label)
      for (model in model_levels) {
        cell <- test_df[test_df$model_label == model, , drop = FALSE]
        stat_val <- if (!is.null(stat_col) && stat_col %in% names(cell)) cell[[stat_col]][1] else NA_real_
        p_val <- if (!is.null(p_col) && p_col %in% names(cell)) cell[[p_col]][1] else NA_real_
        values <- c(values, format_test_cell(stat_val, p_val))
      }
      body <- c(body, paste(values, collapse = " & "), "\\")
    }
  }

  col_span <- length(model_levels) + 1
  footer <- c(
    "\\hline",
    sprintf("\\multicolumn{%d}{l}{\\footnotesize Nota: *p$<0.1$; **p$<0.05$; ***p$<0.01$.} \\\\", col_span),
    sprintf("\\multicolumn{%d}{l}{\\footnotesize Fonte: Elaborado pelos autores.} \\\\", col_span),
    "\\end{tabular}",
    "}",
    "\\end{table}"
  )

  latex_lines <- c(header, body, footer)
  latex_path <- file.path(latex_dir, filename)
  writeLines(latex_lines, latex_path)
  message(sprintf("Tabela salva em %s", latex_path))
}

generate_test_table <- function(df, caption, filename, include_income = TRUE) {
  if (is.null(df) || nrow(df) == 0) {
    message(sprintf("Sem dados para gerar %s", filename))
    return()
  }

  df <- df %>% mutate(model_label = build_model_labels(df, extra_cols = c("area_label"), include_income = include_income))

  model_levels <- unique(df$model_label)

  metric_specs <- list(
    list(
      label = "Teste Sargan",
      stat_col = "sargan_stat",
      p_col = "sargan_p_value",
      fallback_stat = "hansen_stat",
      fallback_p = "hansen_p_value"
    ),
    list(
      label = "Autocorrelação AR(2)",
      stat_col = "ar2_statistic",
      p_col = "ar2_p_value"
    ),
    list(
      label = "Teste Wu-Hausman F",
      stat_col = "wu_statistic",
      p_col = "wu_p_value"
    )
  )

  header <- c(
    "\\begin{table}[H]",
    "\\centering",
    sprintf("\\caption{%s}", caption),
    "\\resizebox{\\textwidth}{!}{%",
    sprintf("\\begin{tabular}{l*{%d}{c}}", length(model_levels)),
    "\\hline",
    paste0(" & ", paste(sprintf("(%d) %s", seq_along(model_levels), model_levels), collapse = " & "), " \\\\")
  )

  body <- c("\\hline")

  for (spec in metric_specs) {
    stat_col <- pick_metric_column(df, spec$stat_col, spec$fallback_stat)
    p_col <- pick_metric_column(df, spec$p_col, spec$fallback_p)

    values <- c(spec$label)
    for (model in model_levels) {
      cell <- df[df$model_label == model, , drop = FALSE]
      stat_val <- if (!is.null(stat_col) && stat_col %in% names(cell)) cell[[stat_col]][1] else NA_real_
      p_val <- if (!is.null(p_col) && p_col %in% names(cell)) cell[[p_col]][1] else NA_real_
      values <- c(values, format_test_cell(stat_val, p_val))
    }
    body <- c(body, paste(values, collapse = " & "), "\\\\")
  }

  col_span <- length(model_levels) + 1
  footer <- c(
    "\\hline",
    sprintf("\\multicolumn{%d}{l}{\\footnotesize Fonte: Elaborado pelos autores.} \\\\", col_span),
    "\\end{tabular}",
    "}",
    "\\end{table}"
  )

  latex_lines <- c(header, body, footer)
  latex_path <- file.path(latex_dir, filename)
  writeLines(latex_lines, latex_path)
  message(sprintf("Tabela de testes salva em %s", latex_path))
}

generate_income_group_tables <- function(df, caption_prefix, filename_prefix, dep_label = "log_gdp", tests_df = NULL) {
  if (is.null(df) || !"income_group" %in% names(df)) {
    return()
  }

  groups <- unique(df$income_group)
  groups <- groups[!is.na(groups)]

  has_tests <- !is.null(tests_df) && "income_group" %in% names(tests_df)

  for (grp in groups) {
    subset_df <- df[df$income_group == grp, , drop = FALSE]
    if (nrow(subset_df) == 0) next

    subset_tests <- NULL
    if (has_tests) {
      subset_tests <- tests_df[tests_df$income_group == grp, , drop = FALSE]
      if (nrow(subset_tests) == 0) subset_tests <- NULL
    }

    caption <- sprintf("%s (%s)", caption_prefix, grp)
    fname <- sprintf("%s_%s.tex", filename_prefix, sanitize_suffix(grp))

    generate_coef_table(
      subset_df,
      caption = caption,
      filename = fname,
      dep_label = dep_label,
      include_income = FALSE,
      test_df = subset_tests
    )
  }
}

income_file <- file.path(reports_dir, "gmm_system_models_income.csv")
income_tests_file <- file.path(reports_dir, "gmm_system_models_income_tests.csv")
areas_file <- file.path(reports_dir, "gmm_system_models_income_areas.csv")
areas_tests_file <- file.path(reports_dir, "gmm_system_models_income_areas_tests.csv")

income_df <- safe_read(income_file)
income_tests_df <- safe_read(income_tests_file)
areas_df <- safe_read(areas_file)
areas_tests_df <- safe_read(areas_tests_file)

if (!is.null(income_df)) {
  income_order <- c("Low Income", "Lower-Middle Income", "Upper-Middle Income", "High Income")
  present_order <- income_order[income_order %in% unique(income_df$income_group)]
  if (length(present_order) == 0) {
    present_order <- unique(income_df$income_group)
  }

  income_df <- income_df %>%
    mutate(
      model_label = factor(income_group, levels = present_order, ordered = TRUE)
    )

  if (!is.null(income_tests_df)) {
    income_tests_df <- income_tests_df %>%
      mutate(
        model_label = factor(income_group, levels = present_order, ordered = TRUE)
      )
  }

  generate_coef_table(
    income_df,
    caption = "Modelos GMM por grupo de renda",
    filename = "gmm_income_table.tex",
    include_income = FALSE,
    test_df = income_tests_df
  )
}

if (!is.null(areas_df)) {
  generate_income_group_tables(
    areas_df,
    caption_prefix = "Modelos GMM por subcomponentes de liberdade",
    filename_prefix = "gmm_income_areas_table",
    tests_df = areas_tests_df
  )
}

message("Geração das tabelas em LaTeX concluída.")
