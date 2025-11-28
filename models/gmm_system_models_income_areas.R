# --- Estimação de Modelos GMM Sistêmicos por Grupo de Renda com Subcomponentes de Liberdade ---

# Suppress warnings immediately (incluindo carregamento de pacotes); restaura ao sair
old_warn <- getOption("warn")
options(warn = -1)
on.exit(options(warn = old_warn), add = TRUE)

suppressWarnings({
  cat("Carregando pacotes necessários (plm, dplyr, AER)...\n")
  library(plm)
  library(dplyr)
  library(AER)

  cat("Carregando e preparando a base de dados...\n")
  data_full <- tryCatch({
    read.csv("data/processed/data_model.csv") %>%
      mutate(
        log_gdp = log(gdp),
        log_lab_part = log(lab_part),
        log_edu = log(edu),
        log_inv = log(inv)
      ) %>%
      arrange(iso3c, year) %>%
      group_by(iso3c) %>%
      mutate(
        log_gdp_lag1 = lag(log_gdp, 1),
        log_gdp_lag2 = lag(log_gdp, 2),
        log_gdp_lag3 = lag(log_gdp, 3)
      ) %>%
      ungroup() %>%
      filter(
        is.finite(log_gdp) &
          is.finite(log_inv) &
          is.finite(log_lab_part) &
          is.finite(log_edu) &
          !is.na(wb_income_group)
      )
  }, error = function(e) {
    stop("Erro ao carregar base: ", e$message)
  })

  cat("Base de dados carregada.\n")

  area_vars <- c("area1", "area2", "area3", "area4", "area5")
  area_labels <- c(
    area1 = "Area 1 Size of Government",
    area2 = "Area 2 Legal System & Property Rights (ajustada por gênero)",
    area3 = "Area 3 Sound Money",
    area4 = "Area 4 Freedom to trade internationally",
    area5 = "Area 5 Regulation"
  )

  run_wu_hausman <- function(df, formula) {
    iv_fit <- try(AER::ivreg(formula = formula, data = df), silent = TRUE)

    if (inherits(iv_fit, "try-error")) {
      return(NULL)
    }

    diagnostics <- summary(iv_fit, diagnostics = TRUE)$diagnostics

    if (is.null(diagnostics)) {
      return(NULL)
    }

    label <- if ("Wu-Hausman F" %in% rownames(diagnostics)) {
      "Wu-Hausman F"
    } else if ("Wu-Hausman" %in% rownames(diagnostics)) {
      "Wu-Hausman"
    } else {
      NULL
    }

    if (is.null(label)) {
      return(NULL)
    }

    stats <- diagnostics[label, , drop = FALSE]

    pull_value <- function(col) {
      if (col %in% colnames(stats)) stats[1, col] else NA_real_
    }

    list(
      statistic = pull_value("statistic"),
      p_value = pull_value("p-value"),
      df1 = pull_value("df1"),
      df2 = pull_value("df2")
    )
  }

  report_hansen_j <- function(model) {
    hansen <- tryCatch(sargan(model), error = function(e) NULL)

    if (is.null(hansen)) {
      cat("Hansen J-test: não foi possível calcular para este grupo.\n")
      return(invisible(NULL))
    }

    stat <- hansen$statistic
    pval <- hansen$p.value
    df <- hansen$parameter

    stat_txt <- ifelse(is.null(stat) || is.na(stat), NA_real_, as.numeric(stat))
    pval_txt <- ifelse(is.null(pval) || is.na(pval), NA_real_, as.numeric(pval))
    df_txt <- ifelse(is.null(df) || is.na(df), "-", formatC(as.numeric(df), format = "f", digits = 0))

    if (is.na(stat_txt) || is.na(pval_txt)) {
      cat("Hansen J-test: não foi possível calcular para este grupo.\n")
    } else {
      cat(sprintf("Hansen J-test: J = %.3f (df=%s), p-valor = %.4f\n", stat_txt, df_txt, pval_txt))
    }
  }

  groups <- unique(data_full$wb_income_group)
  cat("\nGrupos de renda: ", paste(groups, collapse = ", "), "\n", sep = "")

  for (area_var in area_vars) {
    cat(sprintf("\n=== Resultados usando %s (%s) ===\n", area_labels[[area_var]], area_var))

    gmm_formula <- as.formula(sprintf(
      "log_gdp ~ lag(log_gdp, 1) + log_inv + log_lab_part + log_edu + %s | lag(log_gdp, 2:3)",
      area_var
    ))

    wu_formula <- as.formula(sprintf(
      "log_gdp ~ log_gdp_lag1 + log_inv + log_lab_part + log_edu + %s | log_gdp_lag2 + log_gdp_lag3",
      area_var
    ))

    for (g in groups) {
      cat("\n--- Estimando modelo para grupo:", g, "---\n")

      df <- data_full %>%
        filter(
          wb_income_group == g,
          is.finite(.data[[area_var]])
        )

      if (length(unique(df$year)) < 6) {
        cat("T muito pequeno para GMM. Pulando.\n")
        next
      }

      if (length(unique(df$iso3c)) < 5) {
        cat("Poucos países neste grupo. Pulando.\n")
        next
      }

      panel_df <- pdata.frame(df, index = c("iso3c", "year"))

      out <- try(
        pgmm(
          formula = gmm_formula,
          data = panel_df,
          effect = "twoways",
          model = "onestep",
          transformation = "ld",
          collapse = TRUE
        ),
        silent = TRUE
      )

      if (inherits(out, "try-error")) {
        cat("System GMM falhou. Tentando Difference GMM...\n")

        out <- try(
          pgmm(
            formula = gmm_formula,
            data = panel_df,
            effect = "twoways",
            model = "onestep",
            transformation = "d",
            collapse = TRUE
          ),
          silent = TRUE
        )
      }

      if (inherits(out, "try-error")) {
        cat("Não foi possível estimar nenhum modelo GMM para este grupo.\n")
      } else {
        print(summary(out, robust = FALSE))
        report_hansen_j(out)

        wu_res <- run_wu_hausman(df, wu_formula)

        if (is.null(wu_res) || is.na(wu_res$statistic)) {
          cat("Teste Wu-Hausman: não foi possível calcular para este grupo.\n")
        } else {
          df1_txt <- ifelse(is.na(wu_res$df1), "-", formatC(wu_res$df1, format = "f", digits = 0))
          df2_txt <- ifelse(is.na(wu_res$df2), "-", formatC(wu_res$df2, format = "f", digits = 0))
          cat(
            sprintf(
              "Teste Wu-Hausman F: estatística = %.3f (df1=%s, df2=%s), p-valor = %.4f\n",
              wu_res$statistic,
              df1_txt,
              df2_txt,
              wu_res$p_value
            )
          )
        }
      }
    }
  }

  cat("\n--- Análise GMM por áreas concluída. ---\n")
})
